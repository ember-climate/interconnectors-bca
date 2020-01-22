# slope chart version 2!

slope_data_new <- read_excel(path="data/new_int.xlsx",
                             sheet="data",
                             col_names=T,
                             trim_ws=T,
                             #na=c("NA", "N/A"),
                             skip=0,
                             col_types=c(rep("text",3),rep("numeric",3))
)

slope_data_new <- slope_data_new %>% mutate(EF_diff = ifelse(EF_To < EF_From, "up","down"),
                                            EF_delta = EF_From - EF_To) 


slope_data_new <- slope_data_new[order(slope_data_new$EF_delta),]
sep <-310
ncon <- nrow(slope_data_new)
slope_data_new$y1 <- seq(0,(ncon-1)*sep,sep)
slope_data_new$y2 <- slope_data_new$y1
#slope_data$y2 <- slope_data$y1 + slope_data$EF_delta


png(file="plots/EF_chart_new_connect_highres.png",width=1100,height=1100,res=150,type='cairo')
ggplot(data=slope_data_new) +
  geom_segment(aes(x = 1,
                   xend = 2.0,
                   y = y1,
                   yend = y2,
                   group = Pair,
                   size=Capacity,
                   colour=EF_diff,
  ),
  alpha=1,
  ) +
  scale_color_manual(values = c("down"="#B9D4D1","up"="#FF9D80"), guide = "none")  +
  #E68C74 - dirty salmon
  #9DBEBB - light orig
  #468189 - dark orig
  # remove all axis stuff
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size=13, colour="grey20"),
        legend.text = element_text(size=12, colour="grey20"),
        legend.margin = margin(t=-15,r=2,b=5,l=2)) +
  # Left labels
  geom_text(aes(x=1-0.1,
                y=y1,
                label=From),
            col = "grey30", hjust = "right", size=4.5
  ) +
  # right labels
  geom_text(aes(x=2.0+0.1,
                y=y2,
                label=To),
            col = "grey30", hjust = "left", size=4.5
  ) +
  geom_text(aes(x=1.5, y=y1, label=Capacity),
            size=5, colour="grey20", hjust=0.5
  ) +
  geom_text(aes(x=x, y=y, label = label),
            data = tibble(x = c(0.77,2.15), 
                          y = c(1.1*max(slope_data_new$y2),1.1*max(slope_data_new$y2)),
                          label = c("non-EU", "EU")),
            col = "grey20", size=6
  ) +
  #scale_fill_viridis_c(name="Carbon\nintensity", na.value="grey80", guide="colourbar", direction=-1) +
  geom_point(aes(x = 1, 
                 y = y1,
                 fill = EF_From), shape=21, size = 8,
             col = "grey60") +
  # add the white outline for the points at each rate for women
  geom_point(aes(x = 2.0, 
                 y = y2,
                 fill = EF_To), shape=21, size = 8,
             col = "grey60") +
  # Title
  geom_text(data=tibble(x=c(1.5),
                        y=c(max(slope_data_new$y2)+1.3*sep),
                        label=c("Capacity (MW)")),
            aes(x=x,y=y,label=label),
            size=5, colour="grey30") +
  scale_x_continuous(limits = c(0.2, 2.7)) +
  scale_y_continuous(limits = c(-100, 5500)) +
  scale_fill_gradient(low = "white", high = "grey20",
                      breaks = c(250, 500, 750, 1000),
                      name=expression(paste("gCO"[2],"/kWh")),
                      guide = guide_colorbar(frame.colour = "grey20", 
                                             ticks.colour = "grey50", 
                                             title.position="top",
                                             title.hjust = 0.5,
                                             barheight = 0.5)) +
  scale_size(guide="none")
dev.off()
