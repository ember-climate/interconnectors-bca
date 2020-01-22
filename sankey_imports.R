library(plotly)
#library(processx)

data <- Import_2019 #Produced by bca.R
data$From[data$From=="RussianFederation"] <- "Russia"
data$From[data$From=="BosniaHerzegovina"] <- "Bosnia & Herzegovina"
data$From[data$From=="NorthMacedonia"] <- "North Macedonia"
data$From[grepl("Ukraine",data$From)] <- "Ukraine"
data$From[data$From=="Kaliningrad"] <- "Russia"
n <- nrow(data)
all_countries <- c(data$From, data$To)
lbl <- unique(all_countries)

#png(file="plots/sankey_imports_highres.png",width=1200,height=800,res=200,type='cairo')
p <- plot_ly(
  type = "sankey",
  orientation = "h",
  valueformat = ".1f",
  valuesuffix = "TWh",
  node = list(
    label = lbl,   # All countries
    color = c(rep("#D94C4C",10), rep("#327BBF",13)), 
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = match(data$From, lbl)-1,     # Refers to position in label above (Import country)
    target = match(data$To, lbl)-1,     # Also refers to position in label above
    value =  data$Energy    # value of connection
  )
) %>% 
  layout(
    title = "",
    font = list(
      size = 12
    )
  )
p
#orca(p, "plots/sankey_import.png")
#dev.off()
