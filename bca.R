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

# Price data
Price <- read_excel(path="data/Prices_Jan15-Dec19.xlsx",
                    sheet="Prices",
                    col_names=T,
                    trim_ws=T,
                    #na=c("NA", "N/A"),
                    skip=8,
                    col_types=c("date",rep("numeric",22)) # Might be better to read date as character, for matching purposes
)
Price$Date <- force_tz(Price$Date, "CET") # Force time zone to be CET

# Electricity flows
Flows <- read_excel(path="data/Hourly_Daily_Monthly_CrossBorderPhysicalFlow_01-2015_12-2019.xlsx",
                    sheet="HourlyForR",
                    col_names=T,
                    trim_ws=T,
                    #na=c("NA", "N/A"),
                    skip=5,
                    col_types=c("date",rep("numeric",166)) # Might be better to read date as character, for matching purposes
                    )
colnames(Flows) <- str_replace_all(colnames(Flows)," ","") #Get rid of spaces in column names
Flows$Date <- force_tz(Flows$Date, "CET") #Force time zone to be CET

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
  filter(Date < as.POSIXct("2020/01/01 00:00:00", tz="CET")) %>%
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
table(Flows$To, Flows$MapCode, useNA = "always")

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
# 4 are. All around the time of clock changes. something funny is going on with that. It only has a tiny impact
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

# Western Balkan countries in the data.
WBalk <- c("Albania", "BosniaHerzegovina", "NorthMacedonia", "Serbia", "Montenegro")

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


# ==== Plots ====

# Dates for 2019 adjustment
Date2 <- ymd_hms("2018-12-31 23:59:59", tz="CET")
Date1 <- ymd_hms("2018-09-30 23:59:59", tz="CET")

# ==== PLOT net flows by month, by region ====================


png(file="plots/Monthly_net_by_region_timeline_highres.png",width=1200,height=950,res=200,type='cairo')
ggplot(filter(Net_by_month_country, !From %in% "Montenegro"), aes(x=my, y=Energy*1000.0)) +
  facet_wrap(~From,3,3) +
  #geom_line(aes(group=Region, colour=Region)) +
  geom_line(aes(group=From, colour=From), size=1) +
  geom_line(data=tibble(x=as.Date(c("01-01-2015","31-12-2019"), format="%d-%m-%Y"),y=c(0,0)),
            aes(x=x,y=y),
            size=0.5, colour="grey20",alpha=0.7) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=55,hjust=1),
        title = element_text(colour="grey20", size=12),
        text = element_text(colour="grey20", size=12),
        plot.title = element_text(hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  labs(x="", y="Net monthly exchange (GWh)", 
       colour = "Non-EU country"
       ) +
  scale_colour_manual(values = SB_cols) +
  scale_y_continuous(breaks=c(-500, 0, 500, 1000, 1500), labels=c("500", "0", "500", "1000", "1500")) +
  guides(colour=FALSE)
dev.off()


# ==== PLOT gross EU imports by origin country over time =====

#estimates for the last quarter of 2019 - won't need these eventually!
Belarus_2019_adj <- sum(Flows$Flow[Flows$Direction=="Import" & (Flows$From == "Belarus") & (Flows$Date < Date2) & (Flows$Date > Date1)], na.rm=T)/1000000.0
Russia_2019_adj <- sum(Flows$Flow[Flows$Direction=="Import" & (Flows$From == "RussianFederation") & (Flows$Date < Date2) & (Flows$Date > Date1)], na.rm=T)/1000000.0
Turkey_2019_adj <- sum(Tur_flow$Flow[Tur_flow$Direction=="Export" & Tur_flow$Year==2018 & Tur_flow$Month %in% c("October", "November", "December")], na.rm=T)/1000.0
Ukraine_2019_adj <- sum(Flows$Flow[Flows$Direction=="Import" & grepl("Ukraine",Flows$From) & (Flows$Date < Date2) & (Flows$Date > Date1)], na.rm=T)/1000000.0
Morocco_2019_adj <- sum(Flows$Flow[Flows$Direction=="Import" & (Flows$From == "Morocco") & (Flows$Date < Date2) & (Flows$Date > Date1)], na.rm=T)/1000000.0

# create two new data points fo each country. The 2018 imports, and projected 2019 imports
Belarus_2018 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2018 & Import_by_country_year$From=="Belarus"])
Belarus_2019 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2019 & Import_by_country_year$From=="Belarus"]) + Belarus_2019_adj
Russia_2018 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2018 & Import_by_country_year$From=="RussianFederation"])
Russia_2019 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2019 & Import_by_country_year$From=="RussianFederation"]) + Russia_2019_adj
Turkey_2018 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2018 & Import_by_country_year$From=="Turkey"])
Turkey_2019 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2019 & Import_by_country_year$From=="Turkey"]) + Turkey_2019_adj
Ukraine_2018 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2018 & Import_by_country_year$From=="Ukraine"])
Ukraine_2019 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2019 & Import_by_country_year$From=="Ukraine"]) + Ukraine_2019_adj
Morocco_2018 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2018 & Import_by_country_year$From=="Morocco"])
Morocco_2019 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2019 & Import_by_country_year$From=="Morocco"]) + Morocco_2019_adj

# Western Balkans agregate data
WB_2019_adj <- sum(Flows$Flow[Flows$Year==2018 & Flows$Direction=="Import" & (Flows$From %in% WBalk) & (Flows$Date < Date2) & (Flows$Date > Date1)], na.rm=T)/1000000.0
WB_Imp2015 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2015 & Import_by_country_year$From %in% WBalk])
WB_Imp2016 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2016 & Import_by_country_year$From %in% WBalk])
WB_Imp2017 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2017 & Import_by_country_year$From %in% WBalk])
WB_Imp2018 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2018 & Import_by_country_year$From %in% WBalk])
WB_Imp2019 <- sum(Import_by_country_year$Energy[Import_by_country_year$Year==2019 & Import_by_country_year$From %in% WBalk]) + WB_2019_adj

#select data with Western Balkans combined or not
#Import_by_country_year %>% # Not combined
plot_data1 <- ungroup(Import_by_country_year) %>% select(Year, From, Energy) %>% 
  filter(Year != 2019) %>%
  mutate(Type="Actual") %>% 
  add_row(Year=2015, From="Western Balkans", Energy = WB_Imp2015, Type="Actual") %>%
  add_row(Year=2016, From="Western Balkans", Energy = WB_Imp2016, Type="Actual") %>%
  add_row(Year=2017, From="Western Balkans", Energy = WB_Imp2017, Type="Actual") %>%
  add_row(Year=2018, From="Western Balkans", Energy = WB_Imp2018, Type="Actual") %>%
  add_row(Year=2018, From="Western Balkans", Energy = WB_Imp2018, Type="Proj") %>%
  add_row(Year=2019, From="Western Balkans", Energy = WB_Imp2019, Type="Proj") %>%
  add_row(Year=2018, From="Belarus" , Energy=Belarus_2018, Type="Proj") %>%
  add_row(Year=2019, From="Belarus", Energy=Belarus_2019, Type="Proj") %>%
  add_row(Year=2018, From="RussianFederation" , Energy=Russia_2018, Type="Proj") %>%
  add_row(Year=2019, From="RussianFederation", Energy=Russia_2019, Type="Proj") %>%
  add_row(Year=2018, From="Turkey" , Energy=Turkey_2018, Type="Proj") %>%
  add_row(Year=2019, From="Turkey", Energy=Turkey_2019, Type="Proj") %>%
  add_row(Year=2018, From="Ukraine" , Energy=Ukraine_2018, Type="Proj") %>%
  add_row(Year=2019, From="Ukraine", Energy=Ukraine_2019, Type="Proj") %>%
  add_row(Year=2018, From="Morocco" , Energy=Morocco_2018, Type="Proj") %>%
  add_row(Year=2019, From="Morocco", Energy=Morocco_2019, Type="Proj") %>%
  filter(!From %in% WBalk) # Remove individual WBalk countries

png(file="plots/Gross_Imports_by_region_2015-19.png",width=800,height=500,res=120,type='cairo')
ggplot(data = plot_data1,
       aes(x=Year, y=Energy)) +
  geom_line(aes(colour=From, linetype=Type), size=1.5) +
  #theme_classic() +
  plot_theme +
  #theme(axis.line.x.bottom=) +
# theme(panel.grid.major.y = element_line(colour="grey60", size=0.5)) +
  xlim(c(2015,2019)) +
  labs(x="", y="Energy (TWh)", 
       title="Gross electricity imports into EU ETS by origin", 
       colour = "Country") +
  scale_color_brewer(palette = "Set1", labels = c("RussianFederation"="Russia")) +
  scale_y_continuous(breaks=c(0,5,10,15), limits = c(0,15)) +
  guides(linetype=FALSE)
dev.off()


# ==== PLOT Net imports by origin over time ====

Belarus_net_q42018 <- sum(NetFlows$NetFlow[(NetFlows$From == "Belarus") & (NetFlows$Date < Date2) & (NetFlows$Date > Date1)], na.rm=T)/1000000.0
Russia_net_q42018 <- sum(NetFlows$NetFlow[(NetFlows$From == "RussianFederation") & (NetFlows$Date < Date2) & (NetFlows$Date > Date1)], na.rm=T)/1000000.0
Ukraine_net_q42018 <- sum(NetFlows$NetFlow[grepl("Ukraine", NetFlows$From) & (NetFlows$Date < Date2) & (NetFlows$Date > Date1)], na.rm=T)/1000000.0
Morocco_net_q42018 <- sum(NetFlows$NetFlow[(NetFlows$From == "Morocco") & (NetFlows$Date < Date2) & (NetFlows$Date > Date1)], na.rm=T)/1000000.0
Turkey_net_q42018 <- sum(Tur_flow$Flow[Tur_flow$Direction=="Export" & Tur_flow$Year==2018 & Tur_flow$Month_num > 9], na.rm=T)/1000.0 - sum(Tur_flow$Flow[Tur_flow$Direction=="Import" & Tur_flow$Year==2018 & Tur_flow$Month_num > 9], na.rm=T)/1000.0

# create two new data points fo each country. The 2018 net imports, and projected 2019 net imports
Belarus_net2018 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2018 & Net_by_country_year$From=="Belarus"])
Belarus_net2019 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2019 & Net_by_country_year$From=="Belarus"]) + Belarus_net_q42018
Russia_net2018 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2018 & Net_by_country_year$From=="RussianFederation"])
Russia_net2019 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2019 & Net_by_country_year$From=="RussianFederation"]) + Russia_net_q42018
Turkey_net2018 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2018 & Net_by_country_year$From=="Turkey"])
Turkey_net2019 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2019 & Net_by_country_year$From=="Turkey"]) + Turkey_net_q42018
Ukraine_net2018 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2018 & Net_by_country_year$From=="Ukraine"])
Ukraine_net2019 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2019 & Net_by_country_year$From=="Ukraine"]) + Ukraine_net_q42018
Morocco_net2018 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2018 & Net_by_country_year$From=="Morocco"])
Morocco_net2019 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2019 & Net_by_country_year$From=="Morocco"]) + Morocco_net_q42018

# Western Balkans aggregated and adjusted data
WB_net_q42018 <- sum(NetFlows$NetFlow[(NetFlows$Year==2018) & (NetFlows$From %in% WBalk) & (NetFlows$Date < Date2) & (NetFlows$Date > Date1)], na.rm=T)/1000000.0
WB_net2015 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2015 & Net_by_country_year$From %in% WBalk])
WB_net2016 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2016 & Net_by_country_year$From %in% WBalk])
WB_net2017 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2017 & Net_by_country_year$From %in% WBalk])
WB_net2018 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2018 & Net_by_country_year$From %in% WBalk])
WB_net2019 <- sum(Net_by_country_year$Energy[Net_by_country_year$Year==2019 & Net_by_country_year$From %in% WBalk]) + WB_net_q42018

plot_data5 <- ungroup(Net_by_country_year) %>% select(Year, From, Energy) %>% 
  filter(Year != 2019) %>%
  mutate(Type="Actual") %>% 
  add_row(Year=2015, From="Western Balkans", Energy = WB_net2015, Type="Actual") %>%
  add_row(Year=2016, From="Western Balkans", Energy = WB_net2016, Type="Actual") %>%
  add_row(Year=2017, From="Western Balkans", Energy = WB_net2017, Type="Actual") %>%
  add_row(Year=2018, From="Western Balkans", Energy = WB_net2018, Type="Actual") %>%
  add_row(Year=2018, From="Western Balkans", Energy = WB_net2018, Type="Proj") %>%
  add_row(Year=2019, From="Western Balkans", Energy = WB_net2019, Type="Proj") %>%
  add_row(Year=2018, From="Belarus" , Energy=Belarus_net2018, Type="Proj") %>%
  add_row(Year=2019, From="Belarus", Energy=Belarus_net2019, Type="Proj") %>%
  add_row(Year=2018, From="RussianFederation" , Energy=Russia_net2018, Type="Proj") %>%
  add_row(Year=2019, From="RussianFederation", Energy=Russia_net2019, Type="Proj") %>%
  add_row(Year=2018, From="Turkey" , Energy=Turkey_net2018, Type="Proj") %>%
  add_row(Year=2019, From="Turkey", Energy=Turkey_net2019, Type="Proj") %>%
  add_row(Year=2018, From="Ukraine" , Energy=Ukraine_net2018, Type="Proj") %>%
  add_row(Year=2019, From="Ukraine", Energy=Ukraine_net2019, Type="Proj") %>%
  add_row(Year=2018, From="Morocco" , Energy=Morocco_net2018, Type="Proj") %>%
  add_row(Year=2019, From="Morocco", Energy=Morocco_net2019, Type="Proj") %>%
  filter(!From %in% WBalk) # Remove individual WBalk countries

png(file="plots/Net_Imports_by_region_2015-19.png",width=800,height=500,res=120,type='cairo')
ggplot(data = plot_data5,
       aes(x=Year, y=Energy)) +
  geom_line(aes(colour=From, linetype=Type), size=1.5) +
  #theme_classic() +
  plot_theme +
  #theme(axis.line.x.bottom=) +
  # theme(panel.grid.major.y = element_line(colour="grey60", size=0.5)) +
  xlim(c(2015,2019)) +
  labs(x="", y="Net imports (TWh)", 
       title="Net electricity imports into EU ETS by origin region", 
       colour = "Country") +
  scale_color_brewer(palette = "Set1", labels = c("RussianFederation"="Russia")) +
  scale_y_continuous(breaks=c(-10, -5, 0,5,10,15), limits = c(-10,15)) +
  guides(linetype=FALSE)
dev.off()

# ==== PLOT absolute and net flow by year ====

png(file="plots/Net_flow_by_year_highres.png",width=1200,height=750,res=160,type='cairo')
Flows_by_year %>% 
  select(Year, Energy, Direction) %>%
  mutate(Energy=ifelse(Direction=="Export", -1.0*Energy, Energy)) %>%
ggplot(aes(x=Year)) +
  geom_bar(aes(y=Energy, fill=Direction), stat="identity", width=0.8) +
  labs(y="Electricity (TWh)",
       x="",
       fill="") +
  geom_point(data=Net_by_year[c("Year", "Energy")], aes(x=Year, y=Energy, size=3), colour="grey40") +
  geom_line(data=Net_by_year[c("Year", "Energy")], aes(x=Year, y=Energy), colour="grey40", size=0.5) +
  plot_theme +
  scale_y_continuous(breaks = c(-30,-20,-10,0,10,20,30),
                     labels = c("30","20","10","0","10","20","30")) +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019),
                     labels = c("2015", "2016", "2017", "2018", "2019"),
                     ) +
  scale_fill_manual(guide = guide_legend(reverse=TRUE), 
                      values = c("#FF9D80", "#B9D4D1")) +
  scale_size(name="", labels=c("Net imports"))
dev.off()

# ==== SLOPE plot of carbon intensities ====

slope_data <- NetFlows %>% filter(Year==2019) %>%
  mutate(From=ifelse(From=="Ukraine-Bur", "UkraineB",From),
         From=ifelse(From=="Ukraine-Dob", "UkraineD",From),
         Pair=str_c(From,"-",To)) %>%
  group_by(Pair) %>% 
  summarise(EF_From = mean(EF_From, na.rm=T)*1000000.0,
            EF_To = mean(EF_To, na.rm=T)*1000000.0) %>%
  ungroup() %>%
  mutate(From = gsub("([A-z]+)\\-[A-z]+","\\1",Pair),
         To = gsub("[A-z]+\\-([A-z]+)","\\1",Pair),
         From = ifelse(From=="RussianFederation", "Russia", From),
         From = ifelse(From=="BosniaHerzegovina", "Bosnia &\nHerzegovina", From),
         From = ifelse(From=="UkraineB", "Ukraine (Bur)", From),
         From = ifelse(From=="UkraineD", "Ukraine (Dob)", From),
         #EF_diff = ifelse(EF_To < EF_From, "light","dark"),
         EF_diff = "light",
         EF_diff = ifelse(From=="Russia" & To=="Finland","top5",EF_diff),
         EF_diff = ifelse(From=="Ukraine (Bur)" & To=="Hungary","top5",EF_diff), 
         EF_diff = ifelse(From=="Russia" & To=="Lithuania","top5",EF_diff),
         EF_diff = ifelse(From=="Macedonia" & To=="Greece","top5",EF_diff),
         EF_diff = ifelse(From=="Turkey" & To=="Greece","top5",EF_diff))

png(file="plots/EF_compare_slope.png",width=650,height=700,res=120,type='cairo')
ggplot(data=slope_data) +
  geom_segment(aes(x = 1,
                   xend = 2.2,
                   y = EF_From,
                   yend = EF_To,
                   group = Pair,
                   col = EF_diff),
               alpha=0.8,
               size=0.6) +
  geom_segment(data=filter(slope_data, EF_diff == "top5"),
               aes(x = 1,
                   xend = 2.2,
                   y = EF_From,
                   yend = EF_To,
                   group = Pair,
                   col = EF_diff),
               alpha=1.0,
               size=1.2) +
  scale_color_manual(values = c("dark"="#468189","light"="#9DBEBB","top5"="#EB4034"), guide = "none")  +
  #E68C74 - dirty salmon
  #9DBEBB - light orig
  #468189 - dark orig
  # remove all axis stuff
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  geom_line(data=tibble(x = c(0.4,0.4,1,1,2.2,2.2),
                        y = c(-50, 1150, -50, 1150, -50, 1150),
                        group = c("scale", "scale", "left", "left", "right", "right")),
            aes(x=x, y=y, group=group),
            size=0.5,
            col="grey70") +
  geom_text(aes(x=x, y=y, label = label),
            data = tibble(x = c(1.0,2.2), 
                          y = c(1200,1200),
                          label = c("non-EU", "EU")),
            col = "grey30", size=5
            ) +
  geom_text(data=tibble(x=c(0.4), y=c(1200), label=c("gCO2/kWh")),
            aes(x=x, y=y, label=label),
            size=3.5, hjust="center", col= "grey30"
  ) +
  geom_text(aes(x=1-0.05,
                y=EF_From,
                label=From),
            col = "grey30", hjust = "right"
            ) +
  geom_text(data=tibble(x=rep(0.37,12),
                        y=seq(0,1100,100),
                        label=as.character(seq(0,1100,100))),
            aes(x=x, y=y,label=label),
            col="grey30", hjust="right", size=3.5
            ) +
  geom_text(aes(x=2.2+0.05,
                y=EF_To,
                label=To),
            col = "grey30", hjust = "left"
            ) +
  scale_x_continuous(limits = c(0.2, 2.5)) +
  geom_point(aes(x = 1, 
                 y = EF_From), size = 4.5,
             col = "white") +
  # add the white outline for the points at each rate for women
  geom_point(aes(x = 2.2, 
                 y = EF_To), size = 4.5,
             col = "white") +
  # add the actual points at each rate for men
  geom_point(aes(x = 1, 
                 y = EF_From), size = 4,
             col = "grey60") +
  # add the actual points at each rate for men
  geom_point(aes(x = 2.2, 
                 y = EF_To), size = 4,
             col = "grey60") +
  geom_text(data=tibble(x=1.6,
                        y=1320,
                        label="Grid carbon intensities\nof connected grids"),
            aes(x=x,y=y,label=label),
            size=5, colour="grey30")
dev.off()
#ggslope


# ==== PLOT absolute and net flow by country - country link ====
# NOT ADJUSTED FOR TURKEY
#png(file="plots/Net_flow_by_pair.png",width=1000,height=500,res=120,type='cairo')
#ungroup(Net_2019) %>% mutate(From = ifelse(From=="RussianFederation", "Russia", From),
#                                            From = ifelse(From=="BosniaHerzegovina", "Bosnia Herzegovina", From),
#                                            Pair=str_c(From, "-", To)) %>%
#  select(Pair, EnergyExp, EnergyImp) %>%
#  gather(key="Direction",value="Flow",-Pair) %>%
#  mutate(Flow = ifelse(Direction=="EnergyExp", -1.0*Flow, Flow)) %>%
#ggplot(aes(x=Pair)) +
#  geom_bar(aes(y=Flow, fill=Direction), stat="identity", width=0.7) +
#  labs(title="Electricity flows between EU ETS states and non-EU neighbours, Q1-3 2019",
#       x="",
#       y="Gross energy flow (TWh)",
#       fill="") +
#  plot_theme +
#  theme(axis.text.x=element_text(angle=55,hjust=1),
#        panel.grid.major.y = element_line(colour="grey70", size=0.5)) +
#        #panel.grid.major.x = element_line(colour="grey90", size=0.5, linetype="dotted")) +
#  scale_y_continuous(breaks = c(-2,0,2,4,6),
#                     labels = c("2","0","2","4","6")) +
#  scale_fill_discrete(guide = guide_legend(reverse=TRUE),
#                      labels = c("EnergyExp"="Export",
#                                 "EnergyImp"="Import")
#  )
#dev.off()

# ==== PLOT absolute and net Carbon by country-country link ====
# NOT ADJUSTED FOR NEW TURKEY DATA
#png(file="plots/Net_carbon_by_pair.png",width=1000,height=500,res=120,type='cairo')
#ungroup(Net_2019) %>% mutate(From = ifelse(From=="RussianFederation", "Russia", From),
#                             From = ifelse(From=="BosniaHerzegovina", "Bosnia Herzegovina", From),
#                             Pair=str_c(From, "-", To)) %>%
#  select(Pair, CarbonExp, CarbonImp) %>%
#  gather(key="Direction",value="Carb",-Pair) %>%
#  mutate(Carb = ifelse(Direction=="CarbonExp", -1.0*Carb, Carb)) %>%
#  ggplot(aes(x=Pair)) +
#  geom_bar(aes(y=Carb, fill=Direction), stat="identity", width=0.7) +
#  labs(title="CO2 flows between EU ETS states and non-EU neighbours, Q1-3 2019",
#       x="",
#       y=" CO2 (Mt)",
#       fill="") +
#  plot_theme +
#  theme(axis.text.x=element_text(angle=55,hjust=1),
#        panel.grid.major.y = element_line(colour="grey70", size=0.5)) +
#  #panel.grid.major.x = element_line(colour="grey90", size=0.5, linetype="dotted")) +
#  scale_y_continuous(breaks = c(-2,0,2,4,6),
#                     labels = c("2","0","2","4","6")) +
#  scale_fill_discrete(guide = guide_legend(reverse=TRUE),
#                      labels = c("CarbonExp"="Export",
#                                 "CarbonImp"="Import")
#  )
#dev.off()


# ==== PLOT gross Carbon flows with non-EU neighbours ====
# NOT ADJUSTED FOR NEW TURKEY DATA
#png(file="plots/Carbon_flows_non-EU.png",width=1000,height=500,res=120,type='cairo')
#ungroup(Net_2019) %>% mutate(From = ifelse(From=="RussianFederation", "Russia", From),
#                             From = ifelse(From=="BosniaHerzegovina", "Bosnia Herzegovina", From)) %>%
#  select(From, CarbonExp, CarbonImp) %>%
#  gather(key="Direction",value="Carb",-From) %>%
#  mutate(Carb = ifelse(Direction=="CarbonExp", -1.0*Carb, Carb)) %>%
#  ggplot(aes(x=From)) +
#  geom_bar(aes(y=Carb, fill=Direction), stat="identity", width=0.7) +
#  labs(title="CO2 from electricity, exchanged with non-EU neighbours, Q1-3 2019",
#       x="",
#       y=" CO2 (Mt)",
#       fill="") +
#  plot_theme +
#  theme(axis.text.x=element_text(angle=55,hjust=1),
#        panel.grid.major.y = element_line(colour="grey70", size=0.5)) +
#  #panel.grid.major.x = element_line(colour="grey90", size=0.5, linetype="dotted")) +
#  scale_y_continuous(breaks = c(-2,0,2,4,6,8),
#                     labels = c("2","0","2","4","6","8"),
#                     limits = c(-2,8)) +
#  scale_fill_discrete(guide = guide_legend(reverse=TRUE),
#                      labels = c("CarbonExp"="Out of EU",
#                                 "CarbonImp"="Into EU")
#  )
#dev.off()


# ==== CO2 of imported energy vs equivalent in EU country ====
# By year - NOT ADJUSTED 2019
# NOT ADJUTED FOR NEW TUREY DATA
#plot_data3 <- ungroup(Import_by_country_year) %>% 
#  select(Year, CO2, CO2_equiv) %>%
#  gather(EmEq, Carbon, -Year) %>%
#  group_by(Year, EmEq) %>%
#  summarise(C_tot=sum(Carbon, na.rm=T)) %>%
#  mutate(EmEq = ifelse(EmEq=="CO2", "Emitted", "EU Equivalent"))
#
#png(file="plots/co2_compare_year.png",width=800,height=500,res=120,type='cairo')
#ggplot(data=plot_data3,
#       aes(x=Year, y=C_tot, fill=EmEq)) +
#  geom_bar(stat="identity", position="dodge", width=0.8) +
#  labs(title="CO2 emissions of imported energy in exporting vs importing countries",
#       x="Year",
#       y=expression(paste("CO"[2]," (Mt)")),
#       fill="") + 
#  plot_theme +
#  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019),
#                     labels = c("2015", "2016", "2017", "2018", "2019*"))
#dev.off()
#  #theme_minimal() +
#  #theme(title = element_text(size=10),
#  #      panel.background = element_blank(),
#  #      panel.grid = element_blank(),
#  #      panel.grid.major.y = element_line(colour="grey60", size=0.5))
#
## By country
#plot_data4 <- ungroup(Import_by_country_year) %>%
#  filter(Year==2018) %>%
#  select(From, CO2, CO2_equiv) %>%
#  gather(EmEq, Carbon, -From) %>%
#  mutate(EmEq = ifelse(EmEq=="CO2", "Emitted", "EU Equivalent"))
#
#png(file="plots/co2_compare_country.png",width=800,height=450,res=120,type='cairo')
#ggplot(data=plot_data4,
#       aes(x=reorder(From, -Carbon), y=Carbon, fill=EmEq)) +
#  geom_bar(stat="identity", position="dodge", width=0.8) +
#  plot_theme +
#  theme(axis.text.x=element_text(angle=55,hjust=1),
#        panel.grid.major.y = element_line(colour="grey70", size=0.5)) +
#  labs(title="CO2 emissions of imported energy in 2018: export vs import country",
#       x="Exporting country",
#       y=expression(paste("CO"[2]," (Mt)")),
#       fill="") +
#  scale_x_discrete(labels=c("RussianFederation"="Russia", "BosniaHerzegovina"="Bosnia &\n Herzegovina"))
#dev.off()
  #plot_theme +
  #theme(axis.text.x=element_text(angle=45,hjust=1))
  #theme_minimal() +
  #theme(title = element_text(size=10),
  #      axis.text.x=element_text(angle=45,hjust=1),
  #      panel.background = element_blank(),
  #      panel.grid = element_blank(),
  #      panel.grid.major.y = element_line(colour="grey60", size=0.5))


# ==== Scatter: connected capacity vs CO2 intensity ====
# Not Updated. 
#Connection_cap <- tibble(Country = c("Albania",
#                               "Belarus",
#                               "BosniaHerzegovina",
#                               "Macedonia",
#                               "Morocco",
#                               "RussianFederation",
#                               "Serbia",
#                               "Turkey",
#                               "Ukraine"),
#                    Capacity = c(533, 
#                                 4553, 
#                                 5429, 
#                                 2582,
#                                 1359,
#                                 5650,
#                                 4682,
#                                 4385,
#                                 11513)
#)
#Flows_exp_2018 %>% 
#  mutate(Year=2018) %>%
#  left_join(EF, by=c(Year="year", Exp="country")) %>%
#  left_join(Connection_cap, by=c(Exp="Country")) %>%
#  mutate(Max=Capacity*365.25*24/1000000.0) %>%
#  mutate(ef = ef*1000000.0) %>%
#  ggplot(aes(x=Capacity, y=EnergyImp, label=Exp)) +
#  geom_point(aes(colour=ef), size=3) + 
#  scale_colour_gradient(low="white", high="red") +
#  geom_text(nudge_y = -0.5) +
#  labs(title="2018",
#       x="Connection Capacity (MW)",
#       y="Energy import (TWh)",
#       colour=expression(paste("gCO"[2],"/kWh"))) +
#  theme_minimal()
  
  



                    