library(ggplot2)
library(maps)
library(dplyr)
library(ggthemes)
#library(mapplots)
library(readxl)

intcon <- read.csv("data/interconnector_coords_simple2.csv",
                   header=T,
                   strip.white=T,
                   na.strings=c("NA", "N/A"),
                   skip=0,
                   colClasses=c("character", "character", "numeric", "numeric", "character")
)

EU <- c("Finland", 
        "Belgium", 
        "Germany", 
        "Spain", 
        "France", 
        "Sweden",
        "Spain",
        "Portugal",
        "Italy",
        "Austria",
        "Greece",
        "Poland",
        "Bulgaria",
        "Romania",
        "Hungary",
        "Latvia",
        "Lithuania",
        "Estonia",
        "UK",
        "Ireland",
        "Slovenia",
        "Slovakia",
        "Denmark",
        "Netherlands",
        "Croatia",
        "Czech Republic",
        "Cyprus",
        "Luxembourg")
non_EU_ETS <- c("Norway", "Switzerland")
EU_ETS <- c(EU, non_EU_ETS) 
EU_ETS_con <- c("Finland", 
                "Spain", 
                "Spain",
                "Portugal",
                "Italy",
                "Greece",
                "Poland",
                "Bulgaria",
                "Romania",
                "Hungary",
                "Latvia",
                "Lithuania",
                "Estonia",
                "Slovakia",
                "Croatia")
non_EU <- c("Turkey",
            "Egypt",
            "Russia",
            "Belarus",
            "Morocco",
            "Tunisia",
            "Ukraine",
            "Bosnia and Herzegovina",
            "Serbia",
            "Montenegro",
            "Moldova",
            "Macedonia",
            "Albania",
            "Israel",
            "Libya")
countries <- read.csv("data/countries2.csv",
                      header=T,
                      strip.white=T,
                      na.strings=c("NA", "N/A"),
                      skip=0,
                      colClasses=c("character", "numeric", "numeric", "character")
) %>% rename(lab_lat = latitude,
             lab_lon = longitude,
             country_code = country) %>%
  mutate(name = ifelse(name=="United Kingdom", "UK", name))


my_colours <- c("Existing"="grey20",
                "Planned"="dodgerblue2"
                #"Planned"="darkorange"
                )

# Create Burtshyn island shape file
source("burt.R")

# Get world map data
map_dat <- map_data("world")
# give Burtshyn island a unique group number
burt <- mutate(burt, group=max(map_dat$group, na.rm=T)+1)
# Combine with world map shape files
#map_dat <- bind_rows(map_dat) %>%
map_dat <- mutate(map_dat, ETS = ifelse(region %in% EU_ETS, "EU ETS", 
                      ifelse(region %in% non_EU, "Current or future connected neighbour", "Other")))

# Make Baltic shape file by finding unique points? in all three, i.e., not on the borders?

png(file="plots/summary_map_highres.png",width=1200,height=1550,res=220,type='cairo')
sum_map2 <- ggplot(data=map_dat) +
  geom_polygon(aes(x=long, y = lat, group = group, fill=ETS), 
               #fill=ETS, 
               alpha=0.5,
               linetype="solid",
               size=0.3,
               color="grey60") +
  geom_polygon(data=burt, aes(x=long, y=lat, group=group), 
               fill="plum1", 
               colour="grey20",
               linetype="dotted",
               size=0.5,
               alpha=0.0) +
  geom_text(data=countries, 
            aes(x=lab_lon, y=lab_lat, label=country_code), 
            size=2.2, colour="grey40") +
  geom_line(data=intcon, aes(x=Lon, y=Lat, group=ID, colour=Status), size=0.7, lineend = "round") +
  #  geom_line(data=filter(intcon, Type=="HVDC", Status=="Existing"), aes(x=Lon, y=Lat, group=ID, colour=Type), size=0.6) +
  #  geom_line(data=filter(intcon, Type=="220kV", Status=="Existing"), aes(x=Lon, y=Lat, group=ID, colour=Type), size=0.5) +
  #  geom_line(data=filter(intcon, Type=="330kV", Status=="Existing"), aes(x=Lon, y=Lat, group=ID, colour=Type), size=0.5) +
  #  geom_line(data=filter(intcon, Type=="750kV", Status=="Existing"), aes(x=Lon, y=Lat, group=ID, colour=Type), size=0.6) +
  #  geom_line(data=filter(intcon, Type=="150kV", Status=="Existing"), aes(x=Lon, y=Lat, group=ID, colour=Type), size=0.4) +
#  geom_line(data=filter(intcon, Status=="Planned"), aes(x=Lon, y=Lat, group=ID), colour="deeppink", size=0.4) +
  theme_void() +
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.text = element_text(size=11)
        #legend.key.size = unit(0.5, "cm"),
        ) +
  #guides(colour = guide_legend(override.aes = list(size=2))) +
  coord_cartesian(xlim = c(-9,35), ylim = c(29,69.5)) +
  scale_colour_manual(name="", values = my_colours, guide = guide_legend(override.aes = list(size=2))) +
  scale_fill_manual(name="",
                    values = c(#"EU ETS connected"="darkolivegreen1",
                               "EU ETS"="darkseagreen2",
                               "EU ETS"="#D2F3D8",
                               #"Current or future connected neighbour"="plum1",
                               "Current or future connected neighbour"="#F7D0EC",
                               #F3D0E9
                               "Other"="grey80"),
                    breaks = c("EU ETS", "Current or future connected neighbour")
  )
  #scale_linetype_manual(values=c("solid", "solid"), guide=FALSE)
sum_map2
dev.off()

# save as plotly objct and preview
#sum_map2_ly <- ggplotly(sum_map2)
#sum_map2_ly
