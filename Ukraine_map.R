map_dat <- map_data("world") %>% 
  filter(region %in% neighbours[["Ukraine"]]) %>% 
  mutate(region=ifelse(region=="Macedonia", "North Macedonia", region)) %>%
  left_join(filter(EF, year==2019), by=c("region"="country")) %>%
  mutate(ef = ef*1000000.0)
i <- filter(intcon, Map=="Ukraine")
c <- filter(coal, Country %in% c("Ukraine", "Moldova"), Status_mod %in% c("Operating", "Planned"))
#c3 <- filter(codes, Map==map)
lim <- limits[["Ukraine"]]
# Alter country codes
ccodes$lab_lon[ccodes$name=="Ukraine"] <- 29
ccodes$lab_lat[ccodes$name=="Ukraine"] <- 49.5
ccodes$lab_lon[ccodes$name=="Romania"] <- 25
ccodes$lab_lat[ccodes$name=="Romania"] <- 47
ccodes$lab_lon[ccodes$name=="Poland"] <- 21
ccodes$lab_lat[ccodes$name=="Poland"] <- 51
ccodes$lab_lon[ccodes$name=="Hungary"] <- 20
ccodes$lab_lat[ccodes$name=="Hungary"] <- 47

source("burt.R")
burt <- burt %>% mutate(region="Ukraine-Bur", ef=1080)
map_dat <- bind_rows(map_dat, burt)

p <- ggplot() +
  # Countries
  geom_polygon(data = map_dat, 
               aes(x=long, y = lat, group = group, fill = ef), 
               #fill = "grey",
               alpha=0.5,
               linetype="solid",
               size=0.4,
               colour="grey55") +
  # Outline of Burtshyn island
  geom_polygon(data=burt, aes(x=long, y=lat, group=group), 
               fill="grey", 
               colour="grey20",
               linetype="33",
               size=0.8,
               alpha=0.0) +
  # Existing Interconnectors
  geom_segment(data=filter(i, Status=="Existing"), aes(x=Lon, y=Lat, xend=Lon_end, yend=Lat_end, group=ID), colour="grey20", size=1.2, lineend = "round") +
  # Ukraine - Romania interconnector
  geom_segment(data=tibble(Lon=23.0,
                           Lat=48.4,
                           Lon_end=23.5,
                           Lat_end=47.6,
                           ID="Ukr-Rom"),
               aes(x=Lon, y=Lat, xend=Lon_end, yend=Lat_end, group=ID), 
               colour="grey20", 
               size=1.2, 
               lineend = "round") +
  # New Interconnectors
  geom_segment(data=filter(i, Status=="Planned"), aes(x=Lon, y=Lat, xend=Lon_end, yend=Lat_end, group=ID), linetype="22", colour="firebrick1", size=1.2, lineend = "round") +
  # Coal plants
  geom_point(data=c, aes(x=Longitude, y=Latitude, size=Capacity, colour=Status_mod), alpha=0.7, position=position_jitter(width = 0.01, height = 0.01)) +
  # Capacity bubbles
  # Existing
  geom_point(data=filter(i, Status=="Existing"), aes(x=Cap_lon, y=Cap_lat, stroke=sqrt(cap_e)), shape=21, fill="white", colour="grey20", size=7) +
  geom_text(data=filter(i, Status=="Existing"), aes(x=Cap_lon, y=Cap_lat, label=round(cap_e,1)), size=3.2, colour="grey20", fontface = "bold") +
  # Existing and Expanding
  geom_point(data=filter(i, Status=="Existing", cap_p > 0), aes(x=Cap_lon_p, y=Cap_lat_p, stroke=cap_p), shape=21, size=7, colour="firebrick1", fill="white") +
  geom_text(data=filter(i, Status=="Existing", cap_p > 0), aes(x=Cap_lon_p, y=Cap_lat_p, label=round(cap_p,1)), size=3.2, colour="firebrick1", fontface = "bold") +
  # New
  geom_point(data=filter(i, Status=="Planned"), aes(x=Cap_lon_p, y=Cap_lat_p, stroke=sqrt(cap_p)), shape=21, fill="white", colour="firebrick1", size=7) +
  geom_text(data=filter(i, Status=="Planned"), aes(x=Cap_lon_p, y=Cap_lat_p, label=round(cap_p,1)), size=3.2, colour="firebrick1", fontface = "bold") +
  # Country codes
  geom_text(data=ccodes, aes(x=lab_lon, y=lab_lat, label=country_code), size=4, colour="grey20") +
  theme_void() +
  theme(legend.text = element_text(size=9),
        legend.title = element_text(size=10, face="bold")
  ) +
  scale_colour_manual(name="Coal plants", values = my_colours, guide = "none") +
  scale_fill_gradient(low = "white", high = "slateblue1",
                      na.value = "grey60",
                      breaks = c(250, 500, 750, 1000),
                      name=expression(paste("gCO"[2],"/kWh")),
                      guide = guide_colorbar(frame.colour = "grey20", 
                                             ticks.colour = "grey50", 
                                             title.position="top",
                                             title.hjust = 0,
                                             barheight = 4)) +
  scale_size(range=c(1,6), 
             name="Capacity (GW)", 
             breaks=c(1000, 2000, 3000), 
             labels=c("1","2","3"),
             guide = guide_legend(override.aes = list(colour="grey20"))) +
  coord_cartesian(xlim = c(lim[1], lim[2]), ylim = c(lim[3], lim[4]))