map_dat <- map_data("world") %>% 
  filter(region %in% neighbours[["Egypt"]]) %>% 
  mutate(region=ifelse(region=="Macedonia", "North Macedonia", region)) %>%
  left_join(filter(EF, year==2019), by=c("region"="country")) %>%
  mutate(ef = ef*1000000.0)
i <- filter(intcon, Map=="Egypt")
c <- filter(coal, Country %in% c("Israel", "Libya", "Egypt"), Status_mod %in% c("Operating", "Planned"))
#c3 <- filter(codes, Map==map)
lim <- limits[["Egypt"]]
# Alter country codes
ccodes$lab_lon[ccodes$name=="Egypt"] <- 29
ccodes$lab_lat[ccodes$name=="Egypt"] <- 29
ccodes$lab_lon[ccodes$name=="Libya"] <- 23
ccodes$lab_lat[ccodes$name=="Libya"] <- 30

p <- ggplot() +
  # Countries
  geom_polygon(data = map_dat, 
               aes(x=long, y = lat, group = group, fill = ef), 
               #fill = "grey",
               alpha=0.5,
               linetype="solid",
               size=0.4,
               colour="grey55") +
  # Existing Interconnectors
  geom_segment(data=filter(i, Status=="Existing"), aes(x=Lon, y=Lat, xend=Lon_end, yend=Lat_end, group=ID), colour="grey20", size=1.2, lineend = "round") +
  # New Interconnectors
  geom_segment(data=filter(i, Status=="Planned"), aes(x=Lon, y=Lat, xend=Lon_end, yend=Lat_end, group=ID), linetype="22", colour="firebrick1", size=1.2, lineend = "round") +
  # Coal plants
  geom_point(data=c, aes(x=Longitude, y=Latitude, size=Capacity, colour=Status_mod), alpha=0.7, position=position_jitter(width = 0.1, height = 0.1, seed=115)) +
  geom_point(data=c, aes(x=Longitude, y=Latitude, size=Capacity), shape=1, colour="grey20", alpha=0.7, position=position_jitter(width = 0.1, height = 0.1, seed=115)) +
  # Capacity bubbles
  # Existing
  geom_point(data=filter(i, Status=="Existing"), aes(x=Cap_lon, y=Cap_lat, stroke=sqrt(cap_e)), shape=21, fill="white", colour="grey20", size=8) +
  geom_text(data=filter(i, Status=="Existing"), aes(x=Cap_lon, y=Cap_lat, label=round(cap_e,1)), size=3.6, colour="grey20", fontface = "bold") +
  # Existing and Expanding
  geom_point(data=filter(i, Status=="Existing", cap_p > 0), aes(x=Cap_lon_p, y=Cap_lat_p, stroke=cap_p), shape=21, size=8, colour="firebrick1", fill="white") +
  geom_text(data=filter(i, Status=="Existing", cap_p > 0), aes(x=Cap_lon_p, y=Cap_lat_p, label=round(cap_p,1)), size=3.6, colour="firebrick1", fontface = "bold") +
  # New
  geom_point(data=filter(i, Status=="Planned"), aes(x=Cap_lon_p, y=Cap_lat_p, stroke=sqrt(cap_p)), shape=21, fill="white", colour="firebrick1", size=8) +
  geom_text(data=filter(i, Status=="Planned"), aes(x=Cap_lon_p, y=Cap_lat_p, label=round(cap_p,1)), size=3.6, colour="firebrick1", fontface = "bold") +
  # Country codes
  geom_text(data=ccodes, aes(x=lab_lon, y=lab_lat, label=country_code), size=4, colour="grey20") +
  theme_void() +
  theme(legend.text = element_text(size=11),
        legend.title = element_text(size=11, face="bold")
  ) +
  scale_colour_manual(name="Coal plants", values = my_colours, guide = guide_legend(override.aes = list(size=4))) +
  scale_fill_gradient(low = "white", high = "slateblue1",
                      na.value = "grey60",
                      breaks = c(200, 400, 600),
                      name=expression(paste("gCO"[2],"/kWh")),
                      guide = guide_colorbar(frame.colour = "grey20", 
                                             ticks.colour = "grey50", 
                                             title.position="top",
                                             title.hjust = 0,
                                             barheight = 4)) +
  scale_size(range=c(2,9), 
             name="Capacity (GW)", 
             #breaks=waiver(),
             breaks=c(3000, 4000, 5000), 
             labels=c("3","4","5")
             ) +
  coord_cartesian(xlim = c(lim[1], lim[2]), ylim = c(lim[3], lim[4]))