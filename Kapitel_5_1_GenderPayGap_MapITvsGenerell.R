################################################
#
# Kapitel: 5 IT Branche-Maps
#
################################################
# 00 Pakete laden & und Working directory festlegen

library(tidyverse)
#install.packages("countrycode")
library(countrycode)
library(ggplot2)
library(ggthemes)

library(ggmap)
library(dplyr)
library(grid)
library(rworldmap)

library(gt)
library(glue)
library(stringr)

setwd("C:/Users/dig/Desktop/Weiterbildung Data Science/Modul3 Storytelling/xx Hausarbeit/Quellen")


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# 02. Gender pay gap der eu für Karte

g_pay_gap_eu <-as_tibble(read.csv2("Kapitel 04 Arbeitswelt/eurostat - gender pay gap/Eurostat_Table_sdg_05_20FlagDesc_fbf275dc-ec0b-4ddc-82d7-821f45a75d7d.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", header = TRUE, na.strings = ':'))%>%
  rename(Country = geo.time,
         '2002' = X2002,
         '2006' = X2006,
         '2007' = X2007,
         '2008' = X2008,
         '2009' = X2009,
         '2010' = X2010,
         '2011' = X2011,
         '2012' = X2012,
         '2013' = X2013,
         '2014' = X2014,
         '2015' = X2015,
         '2016' = X2016,
         '2017' = X2017
  )%>%
  gather(Year,Value,-Country)%>%
  filter(Year =='2017')



# Karte EU

# Hole Weltkarte
worldMap <- getMap()

indEU <- which(worldMap$NAME%in%g_pay_gap_eu$Country)

#Hole longitude und latitude der Grenzen von E.U. Staaten
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)
europeCoords$value <- g_pay_gap_eu$Value[match(europeCoords$region,g_pay_gap_eu$Country)]


P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))+
  labs(title = "Gender Pay Gap - Europa (2017)", caption = "Quelle: Eurostat - Gender Pay Gap 2017")

##FFFF00FF

P <- P + scale_fill_gradient(name = "Gender Pay Gap", low = "yellow", high = "red", na.value = "grey80")

P <- P + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))+
  theme(plot.title = element_text(vjust=-0.5))

# Zeichne Karte
P

#---------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------

honeypot<- read.csv2("Kapitel 05 Gender bias in IT/oecd_honeypot.io_2018 Women in Tech Index.csv", header=T, stringsAsFactors = FALSE, na.strings = '', sep = ';', dec = ',' )%>%
  select(
    "Rank",
    "Country",
    "Total.Workforce..Millions.",
    "Female.Workforce..Millions.",
    "X..Women",                 
    "Overall.Workforce.Average.Wage",
    "Women.s.Average.Wage",
    "Gender.Pay.Gap....",
    "Tech.Workforce..Thousands.",
    "X..Workforce.in.Tech",
    "Female.Tech.Workforce..Thousands.",
    "X..Women.in.Tech",
    "Female.STEM.Graduates....",                               
    "Tech.Average.Wage....",
    "Tech.Average.Wage.for.Women....",
    "Gender.Pay.Gap.in.Tech....",
    "X..Difference.of.Overall.Gender.Pay.Gap.and.Gender.Pay.Gap.in.Tech"
  )%>%
  rename(
    Total_Workforce_in_Mil = "Total.Workforce..Millions.",
    Female_Workforce_in_Mil = "Female.Workforce..Millions.",
    Female_in_Perc = "X..Women",
    AverageWage_Workforce = "Overall.Workforce.Average.Wage",
    FemaleAverageWage_Workforce = "Women.s.Average.Wage",
    GenderPayGap= "Gender.Pay.Gap....",
    Workforce_Tech= "Tech.Workforce..Thousands.",
    Workforce_Tech_in_Perc="X..Workforce.in.Tech",
    FemaleWorkforce_Tech="Female.Tech.Workforce..Thousands.",
    FemaleWorkforce_Tech_in_Perc="X..Women.in.Tech",
    FemaleSTEM_Graduates="Female.STEM.Graduates....",                               
    AverageWage_Tech="Tech.Average.Wage....",
    FemaleAverageWage_Tech="Tech.Average.Wage.for.Women....",
    GenderPayGapTech= "Gender.Pay.Gap.in.Tech....",
    DiffGenderPayGap_Overall_Tech = "X..Difference.of.Overall.Gender.Pay.Gap.and.Gender.Pay.Gap.in.Tech"
  )%>%
  mutate(Country.de=countrycode(sourcevar = Country,origin = 'country.name', destination = 'country.name.de'))

honeypot$AverageWage_Workforce<- as.numeric(str_replace(honeypot$AverageWage_Workforce,'\\.',''))
honeypot$FemaleAverageWage_Workforce<-as.numeric(str_replace(honeypot$FemaleAverageWage_Workforce,'\\.',''))
honeypot$AverageWage_Tech<-as.numeric(str_replace(honeypot$AverageWage_Tech,'\\.',''))
honeypot$FemaleAverageWage_Tech<-as.numeric(str_replace(honeypot$FemaleAverageWage_Tech,'\\.',''))
honeypot$Country.de[honeypot$Country.de == 'Vereinigte Staaten'] <- 'USA'


honeypot_map <- honeypot %>%
  select(Country,GenderPayGap,GenderPayGapTech)


## Karte honeypot Daten 2018 Gender Pay Gap allgemein

# Hole Kartendaten
worldMap02 <- getMap()

indEU02 <- which(worldMap02$NAME%in%honeypot_map$Country)

#Hole longitude und latitude der Grenzen von E.U. Staaten
europeCoords02 <- lapply(indEU02, function(i){
  df02 <- data.frame(worldMap02@polygons[[i]]@Polygons[[1]]@coords)
  df02$region =as.character(worldMap02$NAME[i])
  colnames(df02) <- list("long", "lat", "region")
  return(df02)
})

europeCoords02 <- do.call("rbind", europeCoords02)
europeCoords02$value <- honeypot_map$GenderPayGap[match(europeCoords02$region,honeypot_map$Country)]

P2 <- ggplot() + geom_polygon(data = europeCoords02, aes(x = long, y = lat, group = region, fill = value),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))+
  labs(title = "Gender Pay Gap - Europa (2018)", caption = "Quelle: Honeypot")

##FFFF00FF

P2 <- P2 + scale_fill_gradient(name = "Gender Pay Gap", low = "yellow", high = "red", na.value = "grey80")

P2 <- P2 + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))+
  theme(plot.title = element_text(vjust=-0.5))

# Zeichne Karte
P2

## Karte honeypot Daten 2018 Gender Pay Gap IT

# Hole Kartendaten
worldMap03 <- getMap()

indEU03 <- which(worldMap03$NAME%in%honeypot_map$Country)

#Hole longitude und latitude der Grenzen von E.U. Staaten
europeCoords03 <- lapply(indEU03, function(i){
  df03 <- data.frame(worldMap02@polygons[[i]]@Polygons[[1]]@coords)
  df03$region =as.character(worldMap02$NAME[i])
  colnames(df03) <- list("long", "lat", "region")
  return(df03)
})

europeCoords03 <- do.call("rbind", europeCoords03)
europeCoords03$`allgemein` <- honeypot_map$GenderPayGap[match(europeCoords03$region,honeypot_map$Country)]
europeCoords03$IT <- honeypot_map$GenderPayGapTech[match(europeCoords03$region,honeypot_map$Country)]

europeCoords04<- europeCoords03 %>%
  gather(Indicator, Value,-c(region,long,lat))


P3 <- ggplot() + geom_polygon(data = europeCoords04, aes(x = long, y = lat, group = region, fill = Value),
                              colour = "black", size = 0.1) +
  scale_fill_gradient(name = "Gender Pay Gap", low = "yellow", high = "red", na.value = "grey80")+
  facet_grid(.~Indicator)

#Europa
P3_eu<- P3+ coord_map(xlim = c(-13, 35),  ylim = c(32, 71)) +
  labs(title = "Gender Pay Gap - Europa (2018)", caption = "Quelle: Honeypot")+ 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(), axis.title = element_blank(),
    #rect = element_blank(),
    #plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")
    )+
  theme(plot.title = element_text(vjust=0.5))

#Nord_Amerika

P3_usa<- P3+ coord_map(xlim = c(-155, -50),  ylim = c(0, 71))+
  labs(title = "Gender Pay Gap - USA (2018)", caption = "Quelle: Honeypot")+ 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(), axis.title = element_blank(),
    #rect = element_blank(),
    #plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")
    )+
  theme(plot.title = element_text(vjust=0.5))

# Zeichne Karte
P3_eu

ggsave("Kapitel 05 Gender bias in IT/Bilder/Karte_gender_pay_gap_eu_2018_ITvsGenerell.jpg") 

P3_usa

ggsave("Kapitel 05 Gender bias in IT/Bilder/Karte_gender_pay_gap_usa_2018_ITvsGenerell.jpg") 
