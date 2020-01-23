# Bar chart of imports/export into EU ETS as a whole

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

