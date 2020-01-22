# Create shapefile for Burtshyn island

m_d <- map_data("world") %>% filter(region %in% "Ukraine")
grp <- max(m_d$group, na.rm=T) + 1 
burt <- filter(m_d, lat<50, long<24.5) %>% mutate(group=grp, subregion="burtshyn")
ord <- max(m_d$order, na.rm=T)

#ggplot(data=map_dat2, aes(x=long, y=lat, group=group, fill=group)) + geom_polygon()

burt <- burt %>% add_row(long=24.7, lat=49.8, group=grp, order=ord+1, region="Ukraine", subregion=NA) %>%
  add_row(long=25.2, lat=49.4, group=grp, order=ord+2, region="Ukraine", subregion=NA) %>%
  add_row(long=24.65, lat=48.85, group=grp, order=ord+3, region="Ukraine", subregion=NA) %>%
  add_row(long=24.2, lat=48.95, group=grp, order=ord+4, region="Ukraine", subregion=NA) %>%
  add_row(long=24.1, lat=48.7, group=grp, order=ord+5, region="Ukraine", subregion=NA) %>%
  add_row(long=24.6, lat=48.50, group=grp, order=ord+6, region="Ukraine", subregion=NA)
  
#map_dat2 <- bind_rows(map_dat, burt)
#ggplot(data=map_dat2, aes(x=long, y=lat, group=group, fill=group)) + geom_polygon()
