# Slope chart for carbon intensities. 


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
         From = ifelse(From=="BosniaHerzegovina", "Bosnia & Herzegovina", From),
         From = ifelse(From=="UkraineB", "Ukraine-Bur", From),
         From = ifelse(From=="UkraineD", "Ukraine-Dob", From),
         From = ifelse(From=="Kaliningrad", "Kaliningrad (Rus)", From),
         From = ifelse(From=="NorthMacedonia", "North Macedonia", From),
         EF_diff = ifelse(EF_To < EF_From, "bad","good"),
         From_txt = EF_From,
         To_txt = EF_To)
         #EF_diff = "light",
         #EF_diff = ifelse(From=="Russia" & To=="Finland","top5",EF_diff),
         #EF_diff = ifelse(From=="Ukraine (Bur)" & To=="Hungary","top5",EF_diff), 
         #EF_diff = ifelse(From=="Russia" & To=="Lithuania","top5",EF_diff),
         #EF_diff = ifelse(From=="Macedonia" & To=="Greece","top5",EF_diff),
         #EF_diff = ifelse(From=="Turkey" & To=="Greece","top5",EF_diff))

# Manual adjustments to text labels EU
slope_data$To_txt[slope_data$To=="Latvia"] <- 290
slope_data$To_txt[slope_data$To=="Slovakia"] <- 250
slope_data$To_txt[slope_data$To=="Croatia"] <- 210
slope_data$To_txt[slope_data$To=="Finland"] <- 170
slope_data$To_txt[slope_data$To=="Spain"] <- 130
#slope_data$To_txt[slope_data$To=="Hungary"] <- 380
slope_data$To_txt[slope_data$To=="Italy"] <- 340
slope_data$To_txt[slope_data$To=="Lithuania"] <- 420
slope_data$To_txt[slope_data$To=="Bulgaria"] <- 630
# non-EU
slope_data$From_txt[slope_data$From=="Russia"] <- 770
slope_data$From_txt[slope_data$From=="Ukraine-Dob"] <- 1160
slope_data$From_txt[slope_data$From=="Ukraine-Bur"] <- 1040
slope_data$From_txt[slope_data$From=="Turkey"] <- 460
slope_data$From_txt[slope_data$From=="Kaliningrad (Rus)"] <- 510
slope_data$From_txt[slope_data$From=="Montenegro"] <- 1080
slope_data$From_txt[slope_data$From=="Serbia"] <- 1120


png(file="plots/EF_compare_slope_highres.png",width=1300,height=1200,res=160,type='cairo')
ggplot(data=slope_data) +
  geom_segment(aes(x = 1,
                   xend = 2.2,
                   y = EF_From,
                   yend = EF_To,
                   group = Pair,
                   col = EF_diff),
               alpha=0.65,
               size=1.0) +
  scale_color_manual(values = c("good"="#B9D4D1","bad"="#FF9D80"), guide = "none")  +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  geom_line(data=tibble(x = c(2.8,2.8,1,1,2.2,2.2),
                        y = c(-50, 1250, -50, 1250, -50, 1250),
                        group = c("scale", "scale", "left", "left", "right", "right")),
            aes(x=x, y=y, group=group),
            size=0.8,
            col="grey70") +
  geom_text(aes(x=x, y=y, label = label),
            data = tibble(x = c(1.0,2.2), 
                          y = c(1320,1320),
                          label = c("non-EU", "EU")),
            col = "grey30", size=6.5
  ) +
  geom_text(data=tibble(x=c(2.9), y=c(1300)),
            aes(x=x, y=y), label=expression(paste("gCO"[2],"/kWh")),
            size=5, hjust="center", col= "grey30"
  ) +
  geom_text(data=tibble(x=rep(2.85,13),
                        y=seq(0,1200,100),
                        label=as.character(seq(0,1200,100))),
            aes(x=x, y=y,label=label),
            col="grey30", hjust="left", size=4.5
  ) +
  # Left labels
  geom_text(aes(x=1-0.07,
                y=From_txt,
                label=From),
            col = "grey30", hjust = "right", size=4.5
  ) +
  # Right labels
  geom_text(aes(x=2.2+0.07,
                y=To_txt,
                label=To),
            col = "grey30", hjust = "left", size=4.5
  ) +
  scale_x_continuous(limits = c(0.1, 3.2)) +
  geom_point(aes(x = 1, 
                 y = EF_From), shape=21, size = 6,
             fill="grey60", col = "white") +
  geom_point(aes(x = 2.2, 
                 y = EF_To), shape=21, size = 6,
             fill="grey60", col = "white")
  # TITLE
  #geom_text(data=tibble(x=1.55,
  #                      y=1430,
  #                      label="Carbon intensities of connected grids"),
  #          aes(x=x,y=y,label=label),
  #          size=7, colour="grey30")
dev.off()
#ggslope