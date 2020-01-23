# ==== PLOT net flows by month, by region ====================

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
