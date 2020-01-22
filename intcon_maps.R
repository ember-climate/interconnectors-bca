library(maps)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(mapplots)
#library(scatterpie)
library(readxl)

# ==== Colour palette ====
#"#4169E1", #(BLUE)
#"#FF0000", #(RED)
#"#FFD700", #(GOLD)
#"#006400", #(GREEN)
#"#FFE4C4", #(BISQUE)
#"#800000", #(MAROON)
#"#FF7F50", #(ORANGE)
#"#ADFF2F", #(LIME GREEN)
#"#9370DB", #(DEEP PURPLE)
#"#BC8F8F", #(ROSE BROWN)
#"#FF1493", #(DEEP PINK)
#"#00CED1", #(DARK TURQUOISE)
#"#FFC0CB", #(PINK)
#"#708090", #(SLATE GREY)
#"#8B4513", #(SADDLE BROWN)
#"#3CB371", #(MEDIUM SEA GREEN)
#"#800080", #(PURPLE)
#"#BDB76B", #(DARK KHAKI)
#"#D8BFD8", #(THISTLE)
#"#DCDCDC", #(GAINSBORO – GREY)
#"#6A5ACD", #(SLATE BLUE)
#"#B0C4DE", #(LIGHT STEEL BLUE)
#"#E6E6FA", #(LAVENDER)
#"#FFFFE0") #(LIGHT PALE YELLOW)

my_colours <- c("Operating"="grey20",
                #"Under construction"="deeppink",
                "Planned"="darkorange")

# ==== Read Lat-long data ====

# Locations and statuses of coal plants, by country
coal <- read_excel("data/Global_Coal_Plant_Tracker_July_2019_12July.xlsx",
                   sheet="Coal units 2",
                   col_names=T,
                   trim_ws=T
                   #.name_repair = make.names
                   )
colnames(coal) <- make.names(colnames(coal))
coal <- mutate(coal, Status_mod = Status)
coal$Status_mod[coal$Status=="announced"] <- "Planned"
coal$Status_mod[coal$Status=="Pre-permit"] <- "Planned"
coal$Status_mod[coal$Status=="Permitted"] <- "Planned"
coal$Status_mod[coal$Status=="permitted"] <- "Planned"
coal$Status_mod[coal$Status=="construction"] <- "Planned"
coal$Status_mod[coal$Status=="Construction"] <- "Planned"
coal$Status_mod[coal$Status=="Announced"] <- "Planned"
coal$Status_mod[coal$Status=="operating"] <- "Operating"
#coal$Status_mod[coal$Status=="Operating"] <- "Current"
coal$Capacity..MW. <- as.numeric(coal$Capacity..MW.)
coal <- coal %>% 
  arrange(Status_mod) %>%
  group_by(TrackerLOC, Country, Status_mod) %>%
  summarise(Capacity = sum(Capacity..MW., na.rm=T),
            Longitude = mean(Longitude, na.rm=T),
            Latitude = mean(Latitude, na.rm=T)
            ) %>%
  ungroup()

# Interconnector start and end coordinates
intcon <- read.csv("data/interconnector_coords_simple4.csv",
                    header=T,
                    strip.white=T,
                    na.strings=c("NA", "N/A"),
                    skip=0,
                    colClasses=c("character", "character", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "numeric")
) 
intcon$Status[intcon$Status=="Under construction"] <- "Planned"
intcon <- intcon %>% mutate(Cap_lon= ifelse(cap_p==0, Lon+0.5*(Lon_end-Lon), Lon+0.7*(Lon_end-Lon)),
                            Cap_lat = ifelse(cap_p==0, Lat+0.5*(Lat_end-Lat), Lat+0.7*(Lat_end-Lat)),
                            Cap_lon_p = ifelse(cap_e==0, Lon+0.5*(Lon_end-Lon), Lon+0.3*(Lon_end-Lon)),
                            Cap_lat_p = ifelse(cap_e==0, Lat+0.5*(Lat_end-Lat), Lat+0.3*(Lat_end-Lat))
                            )

ccodes <- read.csv("data/countries2.csv",
                   header=T,
                   strip.white=T,
                   na.strings=c("NA", "N/A"),
                   skip=0,
                   colClasses=c("character", "numeric", "numeric", "character")
) %>% rename(lab_lat = latitude,
             lab_lon = longitude,
             country_code = country) %>%
  mutate(name = ifelse(name=="United Kingdom", "UK", name))
ccodes$lab_lat[ccodes$name=="Italy"] <- 41
ccodes$lab_lon[ccodes$name=="Italy"] <- 15
ccodes$lab_lat[ccodes$name=="Romania"] <- 45.5
ccodes$lab_lon[ccodes$name=="Romania"] <- 23
ccodes$lab_lat[ccodes$name=="Bulgaria"] <- 42.6
ccodes$lab_lon[ccodes$name=="Bulgaria"] <- 23.4
ccodes$lab_lat[ccodes$name=="Hungary"] <- 46.4
ccodes$lab_lon[ccodes$name=="Hungary"] <- 19

# List of country maps to retrieve for each country of interest
neighbours <- list("Turkey"=c("Turkey", "Bulgaria", "Greece", "Albania", "Macedonia", "Serbia", "Kosovo", "Romania"),
                   "Morocco"=c("Morocco", "Spain", "Portugal", "Algeria"),
                   "WBalk"=c("Austria","Albania", "Bosnia and Herzegovina", "Serbia", "Macedonia", "Kosovo", "Montenegro", "Croatia", "Slovenia", "Bulgaria", "Romania", "Greece", "Hungary", "Italy", "North Macedonia", "Bosnia & Herzegovina"),
                   "Ukraine"=c("Ukraine", "Bulgaria", "Moldova", "Serbia", "Hungary", "Slovakia", "Romania", "Poland", "Belarus", "Poland", "Bosnia and Herzegovina", "Croatia"),
                   "Egypt"=c("Egypt", "Greece", "Turkey", "Cyprus", "Libya", "Israel", "Palestine", "Lebanon", "Jordan", "Syria", "Albania", "North Macedonia", "Bulgaria", "Italy", "Saudi Arabia")
                   )

# Limits for each map (Lon1, Lon2, Lat1, Lat2)
limits <- list("Turkey"=c(22, 33.5, 38.5, 44.3),
               "WBalk"=c(14, 23.5, 39, 46.6),
               "Morocco"=c(-11, 0, 30, 41),
               "Ukraine"=c(18.5, 32, 45, 52),
               "Egypt"=c(22, 36, 26, 39)
               )
            
EF <- read.csv(file="data/Electricity_EF_imputed_sep_ukr.csv",
               header=T,
               strip.white=T,
               na.strings=c("NA", "N/A"),
               skip=0,
               colClasses=c("character", "numeric", "character")
)
EF$ef <- as.numeric(EF$ef)
#EF$country[EF$country=="North Macedonia"] <- "Macedonia"

# ==== Create maps =====
# Map plotting function
#source("make_map3.R")

png(file="plots/WBalk_map_highres.png",width=1200,height=975,res=200,type='cairo')
#make_map(country="WBalk", range=c(1,4), pie_rad=0.4, code_size=5)
source("wbalk_map.R")
p
#make_map3(country="WBalk", range=c(1,4), code_size=3.5)
dev.off()

png(file="plots/Tur_map_highres.png",width=1200,height=750,res=200,type='cairo')
#make_map(country="Turkey", range=c(2,10), pie_rad=0.7, code_size=6)
#make_map3(country="Turkey", range=c(2,10), code_size=5)
source("Tur_map.R")
p
dev.off()

png(file="plots/Ukr_map_highres.png",width=1200,height=740,res=200,type='cairo')
#make_map(country="Turkey", range=c(2,10), pie_rad=0.7, code_size=6)
#make_map3(country="Turkey", range=c(2,10), code_size=5)
source("Ukraine_map.R")
p
dev.off()

png(file="plots/Mor_map_highres.png",width=1200,height=1030,res=200,type='cairo')
#make_map(country="Turkey", range=c(2,10), pie_rad=0.7, code_size=6)
source("Mor_map.R")
p
dev.off()
#WBalk_map <- make_map(country="WBalk") #It doesn't like it yet! 29/10/2019

png(file="plots/Egy_map_highres.png",width=1200,height=1000,res=200,type='cairo')
#make_map(country="Turkey", range=c(2,10), pie_rad=0.7, code_size=6)
source("Egy_map.R")
p
dev.off()


