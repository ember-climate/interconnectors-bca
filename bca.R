# Version 1.1, 03/12/2020
# Version 1.0 peer reviewed by Euan Graham, 19/12/2019
# Changes since version 1.0:
# - Data coverage extended to 01/01/2015 - 31/12/2019
# - Carbon price join changed. Data changed from weekly to weekday. 
# - 

# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

# Create bespoke plot theme
plot_theme <- theme_minimal() +
  theme(text = element_text(colour="grey20",size=12),
        legend.text = element_text(colour="grey20", size=12),
        axis.text = element_text(size=12, colour="grey20"),
        axis.title = element_text(size=12, colour="grey20"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour="grey70", size=0.5),
        axis.text.x=element_text(angle=0,hjust=0.5),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.margin = unit(c(1,1,1,1), "cm")
)

# Sandbag colour scheme
SB_cols <- c("#FFCB33", 
             "#5C7397", 
             "#B9D4D1", 
             "#D94C4C", 
             "#989D61", 
             "#F9DC75", 
             "#6A8784",
             "#FF9D80", 
             "#BD6DA7")

# ==== Read and clean data ====

Price <- read.csv("data/Prices_Jan15-Dec19.csv",
                    header=T,
                    sep=",",
                    check.names=T,
                    strip.white=T,
                    colClasses=c("character", rep("numeric",22))
)
#colnames(Flows) <- str_replace_all(colnames(Flows)," ","") #Get rid of spaces in column names
#Flows$Date <- force_tz(Flows$Date, "CET") #Force time zone to be CET
Price$Date <- dmy_hm(Price$Date, tz="CET") #force CET time zone
Price <- Price[!is.na(Price$Date),]


# Electricity flows
Flows <- read.csv("data/hourly_crossborder_physical_flows_01-01-2015_31-12-2019.csv",
                  header=T,
                  sep=",",
                  check.names=T,
                  strip.white=T,
                  skip=4,
                  colClasses=c("character",rep("numeric",166))
                  )
#colnames(Flows) <- str_replace_all(colnames(Flows)," ","") #Get rid of spaces in column names
#Flows$Date <- force_tz(Flows$Date, "CET") #Force time zone to be CET
Flows$Date <- dmy_hm(Flows$Date, tz="CET") #force CET time zone
Flows <- Flows[!is.na(Flows$Date),]

# Electricity emissions factors
#EFfile <- "data/Electricity_EF.csv"
#EFfile <- "data/Electricity_EF_imputed.csv" # Assumes 2015 Carbon intensities for 2016, and 2017 intensities for 2018 and 2019
EFfile <- "data/Electricity_EF_imputed_sep_ukr.csv" # As above but treats Ukraine as three separate regions
EF <- read.csv(file=EFfile,
                header=T,
                strip.white=T,
                na.strings=c("NA", "N/A"),
                skip=0,
                colClasses=c("character", "numeric", "numeric")
                )
# Convert country names to be the same as in Flows data, for matching later
EF$country[EF$country %in% "Russia"] <- "RussianFederation"
EF$country[EF$country %in% "Bosnia and Herzegovina"] <- "BosniaHerzegovina"
EF$country[EF$country %in% "North Macedonia"] <- "NorthMacedonia"

# ETS Carbon price
Cprice <- read.csv(file="data/eua-price.csv",
                    header=T,
                    strip.white=T,
                    na.strings=c("NA", "N/A"),
                    skip=0,
                    colClasses=c("character", "numeric")
)
colnames(Cprice) <- c("ETS_Date", "ETS_Price")
# Convert Carbon Price date (character) into date type
Cprice$ETS_Date <- date(ymd_hms(Cprice$ETS_Date))

# key to match price zones to countries
price_areas <- read.csv(file="data/price_areas_key.csv",
                         header=T,
                         strip.white=T,
                         na.strings=c("NA", "N/A"),
                         skip=0,
                         colClasses=c(rep("character",4))
)

# Flows between Spain - Morocco (not available through ENTSO-E)
es_flow_imp <- read.csv2(file="data/morocco_to_spain_hourly_jan15_dec19.csv",
                         sep=";",
                         header=T,
                         strip.white=T,
                         na.strings=c("NA", "N/A"),
                         skip=0,
                         colClasses=c(rep("character",6)) #Read every field as character and convert after
)
es_flow_imp <- mutate(es_flow_imp, Direction="Import", From="Morocco", To="Spain")
es_flow_exp <- read.csv2(file="data/spain_to_morocco_hourly_jan15_dec19.csv",
                         sep=";",
                         header=T,
                         strip.white=T,
                         na.strings=c("NA", "N/A"),
                         skip=0,
                         colClasses=c(rep("character",6)) #Read every field as character and convert after
)
es_flow_exp <- mutate(es_flow_exp, Direction="Export", From="Spain", To="Morocco")
es_flow <- bind_rows(es_flow_imp, es_flow_exp) #Join together
es_flow$geoid <- NULL #delete empty field
es_flow$geoname <- NULL #delete empty field
es_flow$value <- as.numeric(es_flow$value) #convert to numeric
es_flow$Date <- ymd_hms(es_flow$datetime, tz="CET") #force CET time zone
es_flow <- rename(es_flow, Flow = value)
es_flow$Flow <- abs(es_flow$Flow) #make all flow values positive
# es_flow now has the same headings, with data in the same format, as 'Flows' data frame will have after wrangling.  

# monthly data from the Turkish ESO (mostly useful for 2015-2017)
Tur_imp <- read_excel(path="data/Turkey_monthly_imports_2018.xls",
                    sheet="forR",
                    col_names=T,
                    trim_ws=T,
                    #na=c("NA", "N/A"),
                    skip=0,
                    col_types=c("text","text",rep("numeric",9)) 
)
Tur_exp <- read_excel(path="data/Turkey_monthly_exports_2018.xls",
                      sheet="forR",
                      col_names=T,
                      trim_ws=T,
                      #na=c("NA", "N/A"),
                      skip=0,
                      col_types=c("text","text",rep("numeric",9)) 
)


######################################
# ==== Data wrangling ====
######################################

# ==== GROSS flow data ====

# Countries in the EU ETS that have a non-EU neighbour
ETS_countries_of_interest <- c("Spain", 
                           "Greece", 
                           "Croatia", 
                           "Bulgaria", 
                           "Lithuania", 
                           "Finland", 
                           "Estonia", 
                           "Latvia", 
                           "Hungary", 
                           "Romania", 
                           "Poland", 
                           "Slovakia",
                           "Italy")
# Non-EU countries that have an EU ETS neighbour
non_ETS_countries_of_interest <- c("Morocco",
                                   "RussianFederation",
                                   "Turkey",
                                   "NorthMacedonia",
                                   "Albania",
                                   "Ukraine",
                                   "Serbia",
                                   "Montenegro",
                                   "Belarus",
                                   "BosniaHerzegovina",
                                   "RepublicofMoldova",
                                   "Kaliningrad")
# All countries of interest
countries_of_interest <- c(ETS_countries_of_interest, non_ETS_countries_of_interest)

# Macedonia. FYROM changes name in the data on 1st April 2019. These lines transfer FYROM data before than date into NorthMacedonia columns, and delete FYROM columns 
nm_rows <- which(Flows$Date < ymd_hms("2019/04/01 00:00:00"))
Flows$NorthMacedonia.Bulgaria[nm_rows] <- Flows$FormerYugoslavRepublicofMacedonia.Bulgaria[nm_rows]
Flows$NorthMacedonia.Greece[nm_rows] <- Flows$FormerYugoslavRepublicofMacedonia.Greece[nm_rows]
Flows$NorthMacedonia.Serbia[nm_rows] <- Flows$FormerYugoslavRepublicofMacedonia.Serbia[nm_rows]
Flows$Bulgaria.NorthMacedonia[nm_rows] <- Flows$Bulgaria.FormerYugoslavRepublicofMacedonia[nm_rows]
Flows$Greece.NorthMacedonia[nm_rows] <- Flows$Greece.FormerYugoslavRepublicofMacedonia[nm_rows]
Flows$Serbia.NorthMacedonia[nm_rows] <- Flows$Serbia.FormerYugoslavRepublicofMacedonia[nm_rows]
Flows[,grepl("FormerYugoslavRepublicofMacedonia", colnames(Flows))] <- NULL

# convert flow data to long format and filter down to flows between countres of interest
Flows <- Flows %>% gather(Countries, Flow, -Date) %>% 
  separate(Countries, c("From", "To"), sep="\\.") %>%
#  filter(Date < as.POSIXct("2020/01/01 00:00:00", tz="CET")) %>%
  mutate(Direction = ifelse((From %in% non_ETS_countries_of_interest) & (To %in% ETS_countries_of_interest), 
                            "Import",
                            ifelse((From %in% ETS_countries_of_interest) & (To %in% non_ETS_countries_of_interest), "Export", NA))) %>%
  filter(!is.na(Direction))

# Modify Ukraine for 3 different grid sections
# Modify Russia to distinguish Kaliningrad enclave
#TEST: 
table(Flows$From[grepl("Ukraine",Flows$From)], Flows$To[grepl("Ukraine",Flows$From)])
Flows$From[Flows$From=="Ukraine" & Flows$To=="Poland"] <- "Ukraine-Dob"
Flows$From[Flows$From=="Ukraine" & Flows$To=="Slovakia"] <- "Ukraine-Bur"
Flows$From[Flows$From=="Ukraine" & Flows$To=="Hungary"] <- "Ukraine-Bur"
Flows$To[Flows$From=="Poland" & Flows$To=="Ukraine"] <- "Ukraine-Dob"
Flows$To[Flows$From=="Slovakia" & Flows$To=="Ukraine"] <- "Ukraine-Bur"
Flows$To[Flows$From=="Hungary" & Flows$To=="Ukraine"] <- "Ukraine-Bur"
Flows$From[Flows$From=="RussianFederation" & Flows$To=="Lithuania"] <- "Kaliningrad"
Flows$To[Flows$From=="Lithuania" & Flows$To=="RussianFederation"] <- "Kaliningrad"

# TESTS
table(Flows$From[grepl("Ukraine",Flows$From)], Flows$To[grepl("Ukraine",Flows$From)])
table(Flows$From[Flows$To=="Lithuania"])
table(Flows$To[Flows$From=="Lithuania"])

# Add data for Spain to Flows
Flows <- bind_rows(Flows, es_flow[c("Date", "To", "From", "Flow", "Direction")])

# Re-shape price data
Price <- Price %>% gather(Code, Price, -Date) 

# Bring map code into Flows using (destination) country name.
Flows <- left_join(Flows, price_areas[c("Country","MapCode")],by=c("To"="Country"))
# TEST: 
table(Flows$To[Flows$Direction=="Import"], Flows$MapCode[Flows$Direction=="Import"], useNA = "always")

# Bring prices into Flows data frame, using combo of Date/time and country code to match.
Flows <- left_join(Flows, Price, by=c("Date"="Date", "MapCode"="Code"))
sum(is.na(Flows$Price[Flows$Direction=="Import"]))

# Create Year variable in Flows
Flows <- mutate(Flows, Year = year(Date))

# Convert to Macedonia name. 
#Flows$From[Flows$From=="FormerYugoslavRepublicofMacedonia"] <- "North Macedonia"
#Flows$To[Flows$To=="FormerYugoslavRepublicofMacedonia"] <- "North Macedonia"

# TESTS
# Test that all Imports have an associated price. No Imports should be TRUE. 
# 5 are TRUE. All around the time of clock changes. something funny is going on with that. It has a negligible impact
table(Flows$Direction, is.na(Flows$Price))
table(Flows$Direction, is.na(Flows$MapCode))

# How many missing flow values?
sum(is.na(Flows$Flow)) # 76016 on 24/10/2019. 163597 on 24/10/2019 (because Italy.Montenegro has been added?)
table(Flows$From[is.na(Flows$Flow)], Flows$To[is.na(Flows$Flow)]) # most NA values are Rom->Ukr (3093), Tur->Bul (4557), Bul->Mac (6598), Mac->Bul (21417), Bul->Tur (32263)
# when are the missing flow values? 
table(year(Flows$Date[is.na(Flows$Flow)])) #over half (50,000) in 2015 and 2016.
# Same for 2019 only
table(Flows$From[is.na(Flows$Flow) & Flows$Year==2019], Flows$To[is.na(Flows$Flow) & Flows$Year==2019])
# Bul->Tur (1093), Gre-> Mac (2042), Bul->Mac (3134), Tur->Bul (1093), Mac->Bul (3133), Mac->Gre (2042)
#hist(Flows$Date[is.na(Flows$Flow) & Flows$Year==2019 & Flows$From=="North Macedonia"], breaks=365)
#hist(Flows$Date[is.na(Flows$Flow) & Flows$Year==2019 & Flows$To=="North Macedonia"], breaks=365)
# The flows to and from North Macedonia are mostly missing from mid-Oct to the end of the year, accounting for most of the missing values.
# The Flows I'm most interested in are from Turkey to Bulgaria, which seem quite complete. 1093hrs = 45days. 
# when are the missing Bul -> Tur values?
# completely missing in 2015-2017. 56% in 2018. 12% missing in 2019. 
table(Flows$Year[Flows$From=="Bulgaria" & Flows$To=="Turkey"], is.na(Flows$Flow[Flows$From=="Bulgaria" & Flows$To=="Turkey"]))

# Re-shape Turkey data into long format
Tur_imp <- Tur_imp %>% 
  gather(key=Year, value=Flow, -Month, -Country) %>%
  mutate(Direction="Import")
Tur_exp <- Tur_exp %>%
  gather(key=Year, value=Flow, -Month, -Country) %>%
  mutate(Direction="Export")
Tur_flow <- rbind(Tur_imp, Tur_exp) %>%
  mutate(Month_num = month(as.Date(str_c("01",Month,Year,sep="-"), format="%d-%B-%Y"),label=F)) %>%
  mutate(Flow = Flow*1000.0) #Convert to MWh, like other flow data.

# Impute the ETS data, to give a price every day
Cprice_imp <- tibble(ETS_Date=seq(ymd("2015/01/01"),ymd("2019/12/31"),1),
                      ETS_Price=NA
                      )
for (i in seq(1:nrow(Cprice_imp))) {
  j <- 0
  # returns the location of the date in Cprice with the smallest absolute differnce to the ith date in Cprice_imp
  j <- which.min(abs(Cprice$ETS_Date - Cprice_imp$ETS_Date[i]))
  Cprice_imp$ETS_Price[i] <- Cprice$ETS_Price[j]
}

#Flows data frame now contains the hourly flow between each pair of countries we're interested in
# Direction: whether the flow is Import to the EU or Export
# Price: the price in the import country at the same time (only available for EU imports)
# Date: is assumed to be in CET. There is an issue when matching price to flow around time changes. Missing Prices in Flow data are around clock changes. 

# ==== Calculate value and Carbon of each flow ====


# Value calculation. Value (Euro) = Flow(MWh) x Price (Euro/MWh)
Flows <- mutate(Flows, Value = (Flow*Price))

# Bring Emissions factors (MtCO2/GWh) in for years available, for exporting and importing countries, using year and country to match
Flows <- left_join(Flows, EF, by=c("From"="country", "Year"="year"))
colnames(Flows)[colnames(Flows)=="ef"] <- "EF_From"
Flows <- left_join(Flows, EF, by=c("To"="country", "Year"="year"))
colnames(Flows)[colnames(Flows)=="ef"] <- "EF_To"
# Check each From country has a EF (all should be FALSE)
table(Flows$From, is.na(Flows$EF_From))
# same for To
table(Flows$To, is.na(Flows$EF_To))

# Carbon emitted in 'From' countries = Energy flow x emissions factor of origin country
Flows <- mutate(Flows, CO2_From=Flow*EF_From/1000.0)
# The equivalent Carbon that would have been emitted, if the imported energy had been generated in the receiving 'To' country
Flows <- mutate(Flows, CO2_To=Flow*EF_To/1000.0)

# Calculate the Carbon cost = Carbon in flow x Carbon Price
# Match each hour to the nearest date in the Cprice data
# Create variable for day date of each hour 
Flows <- mutate(Flows, Day = date(Date))
# Join with ETS price data (daily)
Flows <- left_join(Flows, Cprice_imp, by=c("Day"="ETS_Date"))
# Test for missing values. should be zero
sum(is.na(Flows$ETS_Price))
Flows$Day <- NULL
#plot(Flows$ETS_Date[Flows$ETS_Date > ymd_hms("2019-09-30 00:00:00")], Flows$ETS_Price[Flows$ETS_Date > ymd_hms("2019-09-30 00:00:00")])
# Carbon cost in origin country
Flows <- mutate(Flows, Crev_From=CO2_From*ETS_Price) # Mt CO2 x EUR/t = EUR (millions)
# Carbon cost for the same energy in destination country
Flows <- mutate(Flows, Crev_To=CO2_To*ETS_Price) # Mt CO2 x EUR/t = EUR (millions)

# The Flows data frame has the structure: 
# - each row represents one hour for one combination of countries, in variables 'To' and 'From' (all combinations of Eu and non-EU are covered). 
# - each rows has info about that ONE WAY flow betwen those two countries for that hour

# ==== NET flow data ====
# Re-shape Flows data frame for calculating net flows.

# check every row in Flows has a direction
table(Flows$Direction, useNA="always")
# Match the import and export parts of Flows, by country and hour.
cols <- c("Date", "From", "To", "Flow", "Direction", "MapCode", "Price", "Year", "Value")
NetFlows <- left_join(filter(Flows[,cols], Direction %in% "Import"), 
                     filter(Flows[,c("Date", "To", "From", "Flow", "Direction")], Direction %in% "Export"),
                     by=c("From"="To", "To"="From", "Date"="Date")) %>%
  rename(FlowImp = Flow.x,
         FlowExp = Flow.y)
# assign zero to missing flow values.
NetFlows$FlowImp[is.na(NetFlows$FlowImp)] <- 0
NetFlows$FlowExp[is.na(NetFlows$FlowExp)] <- 0
# Calculate net flow and net value for every country-country connection.
NetFlows <- mutate(NetFlows, NetFlow = FlowImp - FlowExp,
                   NetValue = NetFlow*Price)
                   #NetImport = ifelse(NetFlow > 0, NetFlow, 0))

# Bring in Carbon intensities, again by matching
NetFlows <- left_join(NetFlows, EF, by=c("From"="country", "Year"="year"))
colnames(NetFlows)[colnames(NetFlows)=="ef"] <- "EF_From"
NetFlows <- left_join(NetFlows, EF, by=c("To"="country", "Year"="year"))
colnames(NetFlows)[colnames(NetFlows)=="ef"] <- "EF_To"
# Check each 'From' and 'To' country has a EF
table(Flows$From, is.na(Flows$EF_From))
table(Flows$To, is.na(Flows$EF_To))

# Carbon calculations
# Carbon emissions of imports
NetFlows <- mutate(NetFlows, 
                   CarbImp = FlowImp*EF_From/1000.0,
                   CarbExp = FlowExp*EF_To/1000.0) %>%
            mutate(NetCarb = CarbImp - CarbExp)

# Create date, for aggregating
NetFlows$Date_day <- date(NetFlows$Date)

# NetFlow has information on Net flows. It is structured as
# - each row represents one hour, for two countries ('To'= EU country, 'From'= non-EU country)
# - each row contains info on the flow in both diections for that pair of countries

######################################
# ==== Summary tables ====
######################################

# All yearly and monthly summaries have been corrected for the dodgy data of Bulgaria -> Turkey.

# Aggregate to annual flows between every pair of counties
Flows_by_pair_year <- Flows %>% 
#  select(-Date) %>% 
  group_by(Year, From, To) %>% 
  summarise(Energy = sum(Flow, na.rm=T)/1000000.0, # energy flow TWh
            Value = sum(Value, na.rm=T)/1000000.0, # value of energy flow
            CO2 = sum(CO2_From, na.rm=T), # CO2 emitted in generating energy
            CO2_equiv = sum(CO2_To, na.rm=T), # CO2 that would have been emitted if generated in destination country
            Crev = sum(Crev_From, na.rm=T), # CO2 revenue at ETS price
            Crev_equiv = sum(Crev_To, na.rm=T) # CO2 revenue of equiv CO2 at ETS price
  ) %>% ungroup()
# for every year, replace flow between Turkey and Bulgaria with downloaded data from Turkish TSO
for (y in 2015:2018) {
  # indices of affected rows. bt = Bulgaria to Turkey. tb = Turkey to Bulgaria
  bt <- (Flows_by_pair_year$From=="Bulgaria" & Flows_by_pair_year$To=="Turkey" & Flows_by_pair_year$Year==y)
  tb <- (Flows_by_pair_year$From=="Turkey" & Flows_by_pair_year$To=="Bulgaria" & Flows_by_pair_year$Year==y)
  Flows_by_pair_year$Energy[bt] <- sum(Tur_flow$Flow[Tur_flow$Direction=="Import" & Tur_flow$Country=="Bulgaria" & Tur_flow$Year==y], na.rm=T)/1000000.0 #TWh
  Flows_by_pair_year$Energy[tb] <- sum(Tur_flow$Flow[Tur_flow$Direction=="Export" & Tur_flow$Country=="Bulgaria" & Tur_flow$Year==y], na.rm=T)/1000000.0 #TWh
  Flows_by_pair_year$CO2[bt] <- Flows_by_pair_year$Energy[bt]*EF$ef[EF$country=="Bulgaria" & EF$year==y]*1000.0
  Flows_by_pair_year$CO2[tb] <- Flows_by_pair_year$Energy[tb]*EF$ef[EF$country=="Turkey" & EF$year==y]*1000.0
  #Need to do CO2_equiv and Crev
}
# Add direction variable to summarised data
Flows_by_pair_year <- mutate(Flows_by_pair_year, Direction=ifelse(From %in% ETS_countries_of_interest, "Export", "Import"))
# View result
table(Flows_by_pair_year$From, Flows_by_pair_year$Direction)


# Imports at country-country level, per year
Import_by_pair_year <- Flows_by_pair_year %>% 
  filter(Direction=="Import")


# Aggregate to each exporting (origin, non-EU) country
Import_by_country_year <- Flows_by_pair_year %>% 
  # Re-combine "Ukraine-..." regions into one country
  mutate(From = ifelse(grepl("Ukraine", From),"Ukraine",From)) %>%
  filter(Direction=="Import") %>%
  group_by(Year, From) %>% 
  summarise(Energy = sum(Energy, na.rm=T), # energy flow
            Value = sum(Value, na.rm=T), # value of energy flow
            CO2 = sum(CO2, na.rm=T), # CO2 emitted in generating energy
            CO2_equiv = sum(CO2_equiv, na.rm=T), # CO2 that would have been emitted if generated in destination country
            Crev = sum(Crev, na.rm=T), # CO2 revenue at ETS price
            Crev_equiv = sum(Crev_equiv, na.rm=T) # CO2 revenue of equiv CO2 at ETS price
  ) %>%
  ungroup() 

Import_by_EUcountry_year <- Flows_by_pair_year %>% 
  filter(Direction=="Import") %>%
  group_by(Year, To) %>% 
  summarise(Energy = sum(Energy, na.rm=T), # energy flow
            Value = sum(Value, na.rm=T), # value of energy flow
            CO2 = sum(CO2, na.rm=T), # CO2 emitted in generating energy
            CO2_equiv = sum(CO2_equiv, na.rm=T), # CO2 that would have been emitted if generated in destination country
            Crev = sum(Crev, na.rm=T), # CO2 revenue at ETS price
            Crev_equiv = sum(Crev_equiv, na.rm=T) # CO2 revenue of equiv CO2 at ETS price
  ) %>%
  ungroup() 

# annual flows in and out of the EU (superseeded by Net_by_year)
Flows_by_year <- Flows_by_pair_year %>%
  group_by(Year, Direction) %>%
  summarise(Energy = sum(Energy, na.rm=T), # energy flow
            Value = sum(Value, na.rm=T), # value of energy flow
            CO2 = sum(CO2, na.rm=T), # CO2 emitted in generating energy
            CO2_equiv = sum(CO2_equiv, na.rm=T), # CO2 that would have been emitted if generated in destination country
            Crev = sum(Crev, na.rm=T), # CO2 revenue at ETS price
            Crev_equiv = sum(Crev_equiv, na.rm=T) # CO2 revenue of equiv CO2 at ETS price
  ) %>%
  ungroup()


# Only flows into EU ETS
Import_2019 <- filter(Import_by_pair_year, Year==2019)
# Only flows out of EU ETS
#Export_2019 <- filter(Export_by_pair_year, Year==2019)


# Annual exchanges between each EU country and each connected non-EU neighbour 
Net_by_pair_year <- NetFlows %>% 
  #  select(-Date) %>%
  group_by(Year, From, To) %>% 
  summarise(EnergyImp = sum(FlowImp, na.rm=T)/1000000.0,
            EnergyExp = sum(FlowExp, na.rm=T)/1000000.0,
            Energy = sum(NetFlow, na.rm=T)/1000000.0, # net energy flow
            Value = sum(NetValue, na.rm=T)/1000000.0, # net value of energy flow
            CarbImp = sum(CarbImp, na.rm=T), # gross carbon imported
            CarbExp = sum(CarbExp, na.rm=T), # gross carbon exported
            Carbon = sum(NetCarb, na.rm=T) # net carbon imported
  ) %>% ungroup()
# replace annual Turkey-Bulgaria data with data downloaded from Turkish TSO
for (y in 2015:2018) {
  # i = indices of affected rows
  i <- (Net_by_pair_year$Year==y & Net_by_pair_year$From=="Turkey" & Net_by_pair_year$To=="Bulgaria")
  # Imports in the net data = exports in the Turkish TSO data, and vice versa
  Net_by_pair_year$EnergyImp[i] <- sum(Tur_flow$Flow[Tur_flow$Direction=="Export" & Tur_flow$Country=="Bulgaria" & Tur_flow$Year==y], na.rm=T)/1000000.0
  Net_by_pair_year$EnergyExp[i] <- sum(Tur_flow$Flow[Tur_flow$Direction=="Import" & Tur_flow$Country=="Bulgaria" & Tur_flow$Year==y], na.rm=T)/1000000.0
  Net_by_pair_year$Energy[i] <- Net_by_pair_year$EnergyImp[i] - Net_by_pair_year$EnergyExp[i]
  Net_by_pair_year$CarbImp[i] <- Net_by_pair_year$EnergyImp[i]*EF$ef[EF$country=="Turkey" & EF$year==y]*1000.0 
  Net_by_pair_year$CarbExp[i] <- Net_by_pair_year$EnergyExp[i]*EF$ef[EF$country=="Bulgaria" & EF$year==y]*1000.0
  Net_by_pair_year$Carbon[i] <- Net_by_pair_year$CarbImp[i] - Net_by_pair_year$CarbExp[i]
}


# annual exchanges between each non-EU country and all EU neighbours
Net_by_country_year <- Net_by_pair_year %>% 
  #  select(-Date) %>%
  mutate(From = ifelse(grepl("Ukraine",From), "Ukraine", From)) %>%
  group_by(Year, From) %>% 
  summarise(EnergyImp = sum(EnergyImp, na.rm=T),
            EnergyExp = sum(EnergyExp, na.rm=T),
            Energy = sum(Energy, na.rm=T), # net energy flow
            Value = sum(Value, na.rm=T), # net value of energy flow
            CarbImp = sum(CarbImp, na.rm=T), # gross carbon imported
            CarbExp = sum(CarbExp, na.rm=T), # gross carbon exported
            Carbon = sum(Carbon, na.rm=T) # net carbon imported
  ) %>% ungroup()


# Aggregate to years, all countries
Net_by_year <- Net_by_pair_year %>% 
  #  select(-Date) %>%
  group_by(Year) %>% 
  summarise(EnergyImp = sum(EnergyImp, na.rm=T),
            EnergyExp = sum(EnergyExp, na.rm=T),
            Energy = sum(Energy, na.rm=T), # net energy flow
            Value = sum(Value, na.rm=T), # net value of energy flow
            CarbImp = sum(CarbImp, na.rm=T), # gross carbon imported
            CarbExp = sum(CarbExp, na.rm=T), # gross carbon exported
            Carbon = sum(Carbon, na.rm=T) # net carbon imported
  ) %>% ungroup()

# Just 2019
Net_2019 <- filter(Net_by_pair_year, Year==2019) %>%
  mutate(Imports = ifelse(Energy > 0, Energy, 0),
         C_Imports = ifelse(Carbon > 0, Carbon, 0))


# Monthly exchanges between each EU country and each connected non-EU neighbour
Net_by_month_pair <- NetFlows %>% 
  mutate(#From = ifelse(From=="RussianFederation", "Russia", From), #Re-name, for presentation
         #From = ifelse(From=="BosniaHerzegovina", "Bosnia & Herzegovina", From), #Re-name, for presentation
         #From = ifelse(grepl("Ukraine",From), "Ukraine", From), # Replace "Ukraine-Bur" and "Ukraine-Dob" with "Ukraine"
         Month = month(Date, label=T, abbr=T)) %>% #New month variable, as text
  group_by(Year, Month, From, To) %>% 
  summarise(EnergyImp = sum(FlowImp, na.rm=T)/1000000.0,
            EnergyExp = sum(FlowExp, na.rm=T)/1000000.0,
            Energy = sum(NetFlow, na.rm=T)/1000000.0, # net energy flow
            Value = sum(NetValue, na.rm=T)/1000000.0, # net value of energy flow
            CarbImp = sum(CarbImp, na.rm=T), # gross carbon imported
            CarbExp = sum(CarbExp, na.rm=T), # gross carbon exported
            Carbon = sum(NetCarb, na.rm=T) # net carbon imported
  ) %>% ungroup() %>% 
  mutate(my_text=str_c("01",Month,Year,sep="-"), #01-Month-Year as text
         my=as.Date(my_text,format="%d-%b-%Y"), #01-Month-Year as date
         Month_num = month(my, label=F)) %>% #Number of month, for plotting
  ungroup()
# Turkey adjustment. For 2015 to 2018, and each month within, add net flow calculated from separate Turkey data
# Net flow is import - export from EU perspective. The Turkey data is from Tur perspective, i.e., the opposite
for (y in 2015:2018) {
  for (m in 1:12) {
    i <- Net_by_month_pair$From=="Turkey" & Net_by_month_pair$To=="Bulgaria" & Net_by_month_pair$Year==y & Net_by_month_pair$Month_num==m
    Net_by_month_pair$EnergyImp[i] <- Tur_flow$Flow[Tur_flow$Direction=="Export" & Tur_flow$Country=="Bulgaria" & Tur_flow$Year==y & Tur_flow$Month_num==m]/1000000.0 
    Net_by_month_pair$EnergyExp[i] <- Tur_flow$Flow[Tur_flow$Direction=="Import" & Tur_flow$Country=="Bulgaria" & Tur_flow$Year==y & Tur_flow$Month_num==m]/1000000.0 
    Net_by_month_pair$Energy[i] <- Net_by_month_pair$EnergyImp[i] - Net_by_month_pair$EnergyExp[i]
    Net_by_month_pair$CarbImp[i] <- Net_by_month_pair$EnergyImp[i]*EF$ef[EF$country=="Turkey" & EF$year==y]*1000.0
    Net_by_month_pair$CarbExp[i] <- Net_by_month_pair$EnergyExp[i]*EF$ef[EF$country=="Bulgaria" & EF$year==y]*1000.0
    Net_by_month_pair$Carbon[i] <- Net_by_month_pair$CarbImp[i] - Net_by_month_pair$CarbExp[i]
  }
}

# Monthly exchanges between each non-EU country and all EU neighbours
Net_by_month_country <- Net_by_month_pair %>%
  mutate(From = ifelse(grepl("Ukraine",From), "Ukraine", From)) %>%
  mutate(From = ifelse(From=="RussianFederation", "Russia", From)) %>%
  mutate(From = ifelse(From=="BosniaHerzegovina", "Bosnia & Herzegovina", From)) %>%
  mutate(From = ifelse(From=="Kaliningrad", "Russia", From)) %>%
  mutate(From = ifelse(From=="NorthMacedonia", "North Macedonia", From)) %>%
  group_by(Year, Month, From) %>% 
  summarise(EnergyImp = sum(EnergyImp, na.rm=T),
            EnergyExp = sum(EnergyExp, na.rm=T),
            Energy = sum(Energy, na.rm=T), # net energy flow
            Value = sum(Value, na.rm=T), # net value of energy flow
            CarbImp = sum(CarbImp, na.rm=T), # gross carbon imported
            CarbExp = sum(CarbExp, na.rm=T), # gross carbon exported
            Carbon = sum(Carbon, na.rm=T) # net carbon imported
  ) %>% ungroup() %>%
  mutate(my_text=str_c("01",Month,Year,sep="-"), #01-Month-Year as text
         my=as.Date(my_text,format="%d-%b-%Y"), #01-Month-Year as date
         Month_num = month(my, label=F)) %>% #Number of month, for plotting
  ungroup()

# Daily exchanges between each pair of countries
Net_by_pair_day <- NetFlows %>% 
  #  select(-Date) %>%
  group_by(Date_day, From, To) %>% 
  summarise(EnergyImp = sum(FlowImp, na.rm=T)/1000000.0,
            EnergyExp = sum(FlowExp, na.rm=T)/1000000.0,
            Energy = sum(NetFlow, na.rm=T)/1000000.0, # net energy flow
            Value = sum(NetValue, na.rm=T)/1000000.0, # net value of energy flow
            CarbImp = sum(CarbImp, na.rm=T), # gross carbon imported
            CarbExp = sum(CarbExp, na.rm=T), # gross carbon exported
            Carbon = sum(NetCarb, na.rm=T) # net carbon imported
  ) %>% ungroup()


# Write summarised data to csv files
#write.csv(Flows_2019, file="Flows_2019.csv")
write.csv(Net_by_year, file="data/net_flows_by_year.csv", row.names=F)
#write.csv(Import_2019, file="data/Import_flows_2019.csv", row.names=F)
#write.csv(Net_2019, file="data/Net_flows_2019.csv", row.names=F)
write.csv(Flows_by_pair_year, file="data/Flows_by_country_pairs.csv", row.names=F)
#write.csv(Import_by_pair_year, file="data/Imports_by_country_pairs.csv", row.names=F)
write.csv(Net_by_pair_year, file="data/Net_flows_by_country_pairs.csv", row.names=F)
write.csv(Net_by_country_year, file="data/Net_flows_year_by_origin_country.csv", row.names=F)
write.csv(select(Net_by_month_pair, -my_text, -my, -Month_num), file="data/Net_monthly_flows_by_pairs.csv", row.names=F)
write.csv(Net_by_pair_day, file="data/Net_daily_flows_by_pairs.csv", row.names=F)
write.csv(Import_by_EUcountry_year, file="data/Import_by_EUcountry_year.csv", row.names=F)

######################################
# ==== PLOTS ====
######################################

# ==== PLOT timeline (monthly) of net flows by non-EU region ====================

png(file="plots/Monthly_net_by_region_timeline_highres.png",width=1200,height=950,res=200,type='cairo')
source("region_flows_time.R")
dev.off()

# ==== PLOT absolute and net flow by year ====

png(file="plots/Net_flow_by_year_highres.png",width=1200,height=750,res=160,type='cairo')
source("annual_flows_total.R")
dev.off()

