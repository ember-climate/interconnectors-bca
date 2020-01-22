library(ggplot2)
library(ggpol)
library(readxl)
library(tidyr)
library(dplyr)
#library(devtools)
#devtools::install_github("zeehio/facetscales")
# Tried using this solution to set individual y-limits for the facets, but there's an issue with devtools or rlang
# https://stackoverflow.com/questions/51735481/ggplot2-change-axis-limits-for-each-individual-facet-panel

cap <- read_excel("data/coal_int_cap.xlsx",
                   sheet="data",
                   col_names=T,
                   trim_ws=T,
                   .name_repair = make.names
)

cap_long <- gather(cap[,1:5], key=cap_type, value=cap, -Country) %>%
  mutate(cap_type2 = ifelse(grepl("Coal", cap_type),"Coal power","Interconnection with EU")) %>%
  mutate(cap_tot = ifelse(cap_type2=="Coal power", -cap, cap))
#  mutate(cap_now = ifelse(grepl("Planned",cap_type), 0, cap_tot))

#make Country an ordered factor
cap_long$Country <- factor(cap_long$Country, 
                           ordered=T, 
                           levels=rev(c("Turkey", 
                                    "Russia", 
                                    "Ukraine",
                                    "Egypt",
                                    "Serbia",
                                    "Bosnia & Herzegovina",
                                    "Israel",
                                    "Morocco",
                                    "Moldova",
                                    "North Macedonia",
                                    "Montenegro",
                                    "Belarus",
                                    "Tunisia",
                                    "Libya",
                                    "Albania")))
cap_long$cap_type <- factor(cap_long$cap_type, 
                             ordered=T, 
                             levels=c("Planned.Coal.Capacity", 
                                      "Coal.Capacity",
                                      "Planned.Interconnector.Capacity",
                                      "Interconnector.Capacity")
                             )
png(file="plots/coal_int_cap_highres.png",width=1200,height=600,res=140,type='cairo')
ggplot(cap_long[order(cap_long$cap_type),], aes(x = Country, y = cap_tot/1000.0, fill = cap_type)) +
  geom_bar(stat = "identity", position="stack") + 
  #geom_segment(aes(x=14, xend=14, y=5.8, yend=6.8), 
  #             arrow = arrow(length = unit(0.15, "cm"))) +
  #geom_segment(aes(x=7, xend=7, y=0, yend=1.0), 
  #             arrow = arrow(length = unit(0.15, "cm"))) +
  facet_share(~cap_type2, dir = "h", scales = "free", reverse_num = TRUE) +
  coord_flip() +
  theme_minimal() +
  theme(title = element_text(size=11, colour="grey20"),
        axis.title = element_text(size=11, colour="grey20"),
        axis.text = element_text(size=10, colour="grey20"),
        legend.text = element_text(size=10, colour="grey20"),
        strip.text.x = element_text(size = 11, colour="grey20")
        ) +
  labs(x="", title="", fill = "", y="Capacity (GW)") +
  guides(fill=guide_legend(nrow=2)) +
  theme(legend.position = "top",
        legend.key.size = unit(0.4, "cm"),
        legend.margin = margin(t=2,r=2,b=-15,l=2),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour="grey80", size=0.5)) +
  scale_fill_manual(values=c("Coal.Capacity"="grey30",
                             "Planned.Coal.Capacity"="grey65",
                             "Interconnector.Capacity"="skyblue3",
                             #"Interconnector.Capacity"="#5C7397",
                             "Planned.Interconnector.Capacity"="lightskyblue1"
                             #"Planned.Interconnector.Capacity"="#B9D4D1"
                             ),
                    labels=c("Coal.Capacity"="Existing",
                             "Planned.Coal.Capacity"="Planned",
                             "Interconnector.Capacity"="",
                             "Planned.Interconnector.Capacity"=""))
dev.off()

