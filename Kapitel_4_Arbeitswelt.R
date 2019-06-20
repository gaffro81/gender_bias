################################################
#
# Kapitel: 4 Arbeitswelt
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


#---------------------------------------
# 01.Gender pay gap (OECD & EU)
#Vorbereitungen:

g_pay_gap <- as_tibble(read.csv("Kapitel 04 Arbeitswelt/oecd - Gender pay gap/GENDER_EMP_16052019100552589.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"))%>%
  select(c(COU,Country,Indicator,Time,Unit,Value))%>%
  rename(Country.iso = COU)%>%
  #Kontinent&Land in Deutsch hinzufügen
  mutate(Continent=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'continent'))%>%
  mutate(Country.de=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'country.name.de'))%>%
  filter(is.na(Continent) == FALSE)

#Kontinent auf Deutsch
g_pay_gap$Continent.de[g_pay_gap$Continent=='Europe'] <- 'Europa'
g_pay_gap$Continent.de[g_pay_gap$Continent=='Asia'] <- 'Asien'
g_pay_gap$Continent.de[g_pay_gap$Continent=='Africa'] <- 'Afrika'
g_pay_gap$Continent.de[g_pay_gap$Continent=='Americas'] <- 'Amerika'
g_pay_gap$Continent.de[g_pay_gap$Continent=='Oceania'] <- 'Ozeanien'

g_pay_gap$Country.de[g_pay_gap$Country.iso=='USA'] <- 'USA'


g_pay_gap_selfemployed <- as_tibble(read.csv("Kapitel 04 Arbeitswelt/oecd - Gender pay gap/GENDER_ENT1_02052019164344259_oecd_Earning gap in self-employment, by sex.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"))%>%
  select(LOCATION, Country, Time, Value)%>%
  rename(Country.iso= LOCATION)%>%
  mutate(Continent=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'continent'))%>%
  mutate(Country.de=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'country.name.de'))

#Kontinent auf Deutsch
g_pay_gap_selfemployed$Continent.de[g_pay_gap_selfemployed$Continent=='Europe'] <- 'Europa'
g_pay_gap_selfemployed$Continent.de[g_pay_gap_selfemployed$Continent=='Asia'] <- 'Asien'
g_pay_gap_selfemployed$Continent.de[g_pay_gap_selfemployed$Continent=='Africa'] <- 'Afrika'
g_pay_gap_selfemployed$Continent.de[g_pay_gap_selfemployed$Continent=='Americas'] <- 'Amerika'
g_pay_gap_selfemployed$Continent.de[g_pay_gap_selfemployed$Continent=='Oceania'] <- 'Ozeanien'

g_pay_gap_selfemployed$Country.de[g_pay_gap_selfemployed$Country.iso=='USA'] <- 'USA'

# 01.01 Globaler Gender pay gap

Laender_17_16_15 <- g_pay_gap%>%
  select(Continent.de,Country.de,Time,Value)%>%
  filter(Time== '2017' | Time == '2016' | Time == '2015')%>%
  spread(Time,Value)


Laender_17 <- filter(Laender_17_16_15, is.na(`2017`)==FALSE)%>% 
  select(Continent.de,Country.de,`2017`)%>%
  mutate(Jahr = '2017',Indicator= 'all')%>%
  rename(Value=`2017`)

Laender_16 <- filter(Laender_17_16_15, is.na(`2017`) & is.na(`2016`)== FALSE )%>% 
  select(Continent.de,Country.de,`2016`)%>%
  mutate(Jahr = '2016',Indicator= 'all')%>%
  rename(Value=`2016`)

Laender_15 <- filter(Laender_17_16_15, is.na(`2017`)& is.na(`2016`) & is.na(`2015`)== FALSE )%>% 
  select(Continent.de,Country.de,`2015`)%>%
  mutate(Jahr = '2015',Indicator= 'all')%>%
  rename(Value=`2015`)


oecd_pay_gap<-rbind(Laender_17,Laender_16,Laender_15)

lab_col11 <- c(rep("black",17),"red",rep("black",5),"red",rep("black",4))

oecd_pay_gap %>%
  ggplot(mapping = aes(x=reorder(Country.de,Value),y=Value, fill=Jahr))+
  geom_bar(stat="identity")+
  theme_economist_white()+
  ylim(0, 50)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Global Gender pay gap der OECD Ländern")+
  coord_flip()+
  theme(axis.text.y = element_text(colour = lab_col11))

# 01.02 DE & US bei self employment

Laender_17_16_15_self <- g_pay_gap_selfemployed%>%
  select(Continent.de,Country.de,Time,Value)%>%
  filter(Time== '2016' | Time == '2015' | Time == '2014')%>%
  spread(Time,Value)


Laender_16_self <- filter(Laender_17_16_15_self, is.na(`2016`)==FALSE)%>% 
  select(Continent.de,Country.de,`2016`)%>%
  mutate(Jahr = '2016',Indicator= 'self-employed')%>%
  rename(Value=`2016`)

Laender_15_self <- filter(Laender_17_16_15_self, is.na(`2016`) & is.na(`2015`)== FALSE )%>% 
  select(Continent.de,Country.de,`2015`)%>%
  mutate(Jahr = '2015',Indicator= 'self-employed')%>%
  rename(Value=`2015`)

Laender_14_self <- filter(Laender_17_16_15_self, is.na(`2016`)& is.na(`2015`) & is.na(`2014`)== FALSE )%>% 
  select(Continent.de,Country.de,`2014`)%>%
  mutate(Jahr = '2014',Indicator= 'self-employed')%>%
  rename(Value=`2014`)


oecd_pay_gap_self<-rbind(Laender_16_self,Laender_15_self,Laender_14_self)

lab_col10 <- c(rep("black",27),"red")

oecd_pay_gap_self %>%
  ggplot(mapping = aes(x=reorder(Country.de,Value),y=Value, fill=Jahr))+
  geom_bar(stat="identity")+
  theme_economist_white()+
  #geom_hline(aes(yintercept = world_mean),color="red", linetype="dashed", size=1)+
  #geom_hline(aes(yintercept = eu_mean),color="blue", linetype="dashed", size=1)+
  ylim(-20, 70)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Global Gender pay gap der OECD Ländern - Selbstständig")+
  coord_flip()+
  theme(axis.text.y = element_text(colour = lab_col10))

oecd_pay_gap_all <- rbind(oecd_pay_gap,oecd_pay_gap_self)

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# 02. Gender pay gap der eu für Karte

g_pay_gap_eu_2 <-as_tibble(read.csv2("Kapitel 04 Arbeitswelt/eurostat - gender pay gap/Eurostat_Table_sdg_05_20FlagDesc_fbf275dc-ec0b-4ddc-82d7-821f45a75d7d.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", header = TRUE, na.strings = ':'))%>%
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
  gather(Year,Value,-Country)


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

ggsave("Kapitel 04 Arbeitswelt/Bilder/Karte_gender_pay_gap_eu_2017.jpg")  

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# 03. Deutschland und USA im Zeitverlauf
g_pay_gap%>%
  filter(Country.iso == c("DEU") | Country.iso == c("USA"))%>%
  filter(`Time` > 2005)%>%
  ggplot(mapping = aes(x=Time,y=Value, color=Country.de))+
  geom_line(aes(group = Country.de))+
  #  facet_grid(Gruppe ~.)+
  ylim(0,40)+
  theme(legend.title=element_blank(),legend.position = "bottom",axis.ticks=element_blank())+
  labs(title= "Entwicklung Gender pay gap", subtitle = "für Deutschland und USA (2010 bis 2017)", caption = "Quelle: OECD - Gender Pay Gap")+
  xlab("Jahr")+
  ylab(NULL)+
  scale_colour_manual(name = 'the colour', values =c('Deutschland'='coral3','USA'='grey37'), labels = c('Deutschland','USA'))

ggsave("Kapitel 04 Arbeitswelt/Bilder/Entwicklung Gender pay gap_de_usa_zeitreihe.jpg")  

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

# Verteilungen der Frauen in der Arbeitswelt
#---------------------------------------
# 04 Anteil Frauen/Männer an Arbeitskraft oecd:

#Arbeitswelt Zahlen - Jahr 2018
labour_force_data <- as_tibble(read.csv("Kapitel 04 Arbeitswelt/oecd/Labour_Force_Data_24052019093824679.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", header = TRUE))%>%
  select(COUNTRY,Country,Sex,Age,Series,Time,PowerCode, Value)%>%
  rename(Country.iso = COUNTRY)%>%
  filter(Sex != 'All persons')%>%
  #Kontinent&Land in Deutsch hinzufügen
  mutate(Continent=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'continent'))%>%
  mutate(Country.de=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'country.name.de'))%>%
  filter(is.na(Continent) == FALSE)

#Kontinent auf Deutsch
labour_force_data$Continent.de[labour_force_data$Continent=='Europe'] <- 'Europa'
labour_force_data$Continent.de[labour_force_data$Continent=='Asia'] <- 'Asien'
labour_force_data$Continent.de[labour_force_data$Continent=='Africa'] <- 'Afrika'
labour_force_data$Continent.de[labour_force_data$Continent=='Americas'] <- 'Amerika'
labour_force_data$Continent.de[labour_force_data$Continent=='Oceania'] <- 'Ozeanien'

labour_force_data$Country.de[labour_force_data$Country.iso=='USA'] <- 'USA'


labor_force_data.berbeitet<-labour_force_data%>%
  filter((Continent == 'Europe' | Country.iso == 'USA') & Time=='2017' & Age == 'Total')%>% 
  spread(Series,Value)%>%
  mutate(Anteil_arbeitsfähiger_an_Gesamtgeschlecht = `Labour Force`/`Population`)%>%
  mutate(Anteil_arbeitender_an_arbeitsfähigenGeschlecht = `Employment`/`Labour Force`)%>%
  select(Country.de,Sex,Anteil_arbeitsfähiger_an_Gesamtgeschlecht,Anteil_arbeitender_an_arbeitsfähigenGeschlecht)
  

labor_force_data.berbeitet_m <- labor_force_data.berbeitet%>%
  filter(Sex=='Men')

labor_force_data.berbeitet_F <- labor_force_data.berbeitet%>%
  filter(Sex=='Women')

labor_force_data.berbeitet.01<-inner_join(labor_force_data.berbeitet_F,labor_force_data.berbeitet_m,c("Country.de" = "Country.de"))

labor_force_data.berbeitet.02<-labor_force_data.berbeitet.01%>%rename(Anteil_arbeitenderFrauen_an_arbeitsfähigenFrauen=Anteil_arbeitender_an_arbeitsfähigenGeschlecht.x,
                                       Anteil_arbeitsfähigerFrauen_an_GesamtFrauen=Anteil_arbeitsfähiger_an_Gesamtgeschlecht.x,
                                       Anteil_arbeitenderMänner_an_arbeitsfähigenMännern=Anteil_arbeitender_an_arbeitsfähigenGeschlecht.y,
                                       Anteil_arbeitsfähigerMänner_an_GesamtMännern=Anteil_arbeitsfähiger_an_Gesamtgeschlecht.y)

lab_col09 <- c(rep("black",15),"red","black","red",rep("black",10))
# plot
ggplot(labor_force_data.berbeitet.02) +
  geom_segment( aes(x=reorder(Country.de,(Anteil_arbeitsfähigerFrauen_an_GesamtFrauen)), xend=Country.de, y=Anteil_arbeitsfähigerFrauen_an_GesamtFrauen*100, yend=Anteil_arbeitsfähigerMänner_an_GesamtMännern*100), color="black") +
  geom_point( aes(x=Country.de, y=Anteil_arbeitsfähigerFrauen_an_GesamtFrauen*100, color='coral3'), size=3 ) +
  geom_point( aes(x=Country.de, y=Anteil_arbeitsfähigerMänner_an_GesamtMännern*100,color='blue4'), size=3 ) +
  coord_flip()+ 
  xlab(NULL) +
  ylab("Prozent")+
  labs(title="Anteil Arbeitsfähiger an Bevölkerung nach Geschlecht", subtitle = "OECD Länder (2017)", caption = "Quelle: OECD - Labour force data 2017")+
  theme(legend.title=element_blank(),legend.position = "bottom",axis.ticks=element_blank())+
  scale_colour_manual(name = 'the colour', values =c('coral3'='coral3','blue4'='blue4'), labels = c('Anteil arbeitsfähige Frauen\nan Gesamtfrauen','Anteil arbeitsfähige Männer\nan Gesamtmännern'))+
  ylim(0,100)+
  theme(axis.text.y = element_text(colour = lab_col09))

ggsave("Kapitel 04 Arbeitswelt/Bilder/Anteil Arbeitsfähiger an Gesamtbevölkerung nach Geschlecht.jpg")

lab_col08 <- c(rep("black",22),"red",rep("black",3),"red","black")

ggplot(labor_force_data.berbeitet.02) +
  geom_segment( aes(x=reorder(Country.de,(Anteil_arbeitenderFrauen_an_arbeitsfähigenFrauen)), xend=Country.de, y=Anteil_arbeitenderFrauen_an_arbeitsfähigenFrauen*100, yend=Anteil_arbeitenderMänner_an_arbeitsfähigenMännern*100), color="black") +
  geom_point( aes(x=Country.de, y=Anteil_arbeitenderFrauen_an_arbeitsfähigenFrauen*100, color='coral3'), size=3 ) +
  geom_point( aes(x=Country.de, y=Anteil_arbeitenderMänner_an_arbeitsfähigenMännern*100, color='blue4'), size=3 ) +
  coord_flip()+ 
  xlab(NULL) +
  ylab("Prozent")+
  labs(title = "Anteil Arbeiter an Arbeitsfähigen nach Geschlecht", subtitle = "OECD Länder (2017)", caption = "Quelle: OECD - Labour force data 2017")+
  scale_colour_manual(name = 'the colour', values =c('coral3'='coral3','blue4'='blue4'), labels = c('Anteil arbeitende Frauen\nan arbeitsfähigen Frauen','Anteil arbeitende Männer\nan arbeitsfähigen Männer'))+
  theme(legend.title=element_blank(),legend.position = "bottom",axis.ticks=element_blank() )+
  ylim(0,100)+
  theme(axis.text.y = element_text(colour = lab_col08))

ggsave("Kapitel 04 Arbeitswelt/Bilder/Anteil Arbeiter an Arbeitsfähigen nach Geschlecht.jpg")  


tabelle5<- labor_force_data.berbeitet.02%>%
  filter(Anteil_arbeitenderFrauen_an_arbeitsfähigenFrauen > Anteil_arbeitenderMänner_an_arbeitsfähigenMännern)%>%
  mutate(Unterschied = (Anteil_arbeitenderFrauen_an_arbeitsfähigenFrauen - Anteil_arbeitenderMänner_an_arbeitsfähigenMännern)*100)%>%
  mutate(Anteil_arbeitenderFrauen_an_arbeitsfähigenFrauen = round(Anteil_arbeitenderFrauen_an_arbeitsfähigenFrauen*100,1),Anteil_arbeitenderMänner_an_arbeitsfähigenMännern=round(Anteil_arbeitenderMänner_an_arbeitsfähigenMännern*100,1))%>%
  arrange(desc(Unterschied))%>%
  select(Country.de,Anteil_arbeitenderFrauen_an_arbeitsfähigenFrauen,Anteil_arbeitenderMänner_an_arbeitsfähigenMännern,Unterschied)%>%
  rename(Land =Country.de,'Anteil arbeitende Frauen an arbeitsfähigen Frauen'=Anteil_arbeitenderFrauen_an_arbeitsfähigenFrauen,'Anteil arbeitende Männer an arbeitsfähigen Männern' = Anteil_arbeitenderMänner_an_arbeitsfähigenMännern,'Differenz' = Unterschied)%>%
  mutate('Differenz'=round(Differenz,2))%>%
  filter(Land=='Deutschland'|Land=='USA'|Land=='Litauen'|Land=='Lettland'|Land=='Irland')

tab_kap4_1<- tabelle5%>%
  gt()%>%
  tab_header(
    title = "Differenz der Anteile arbeitende Frauen zu arbeitende Männer",
    subtitle = "Top 3 nach Differenz, Deutschland und USA - Stand 2017")%>%
    tab_source_note("Quelle: OECD - Labour force data 2017")

gtsave(tab_kap4_1,"Kapitel 04 Arbeitswelt/Bilder/US_DE_Top3_Differenz der Anteile arbeitende Frauen zu arbeitende Männer.png")
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

# 05 Anteil Frauen an Manager Positionen:

share_fem_manager <-as_tibble(read.csv("Kapitel 04 Arbeitswelt/oecd/Share of employed who are managers, by sex.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", header = TRUE))%>%
  select(COU,Country,Indicator,Sex,Time,Unit,Value)%>%
  rename(Country.iso = COU, Year = Time)%>%
  mutate(Continent=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'continent'))%>%
  mutate(Country.de=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'country.name.de'))
  #Kontinent und Land bearbeiten
  share_fem_manager$Continent.de[share_fem_manager$Continent=='Europe'] <- 'Europa'
  share_fem_manager$Continent.de[share_fem_manager$Continent=='Asia'] <- 'Asien'
  share_fem_manager$Continent.de[share_fem_manager$Continent=='Africa'] <- 'Afrika'
  share_fem_manager$Continent.de[share_fem_manager$Continent=='Americas'] <- 'Amerika'
  share_fem_manager$Continent.de[share_fem_manager$Continent=='Oceania'] <- 'Ozeanien'
  share_fem_manager$Country.de[share_fem_manager$Country.iso=='USA'] <- 'USA'

share_fem_board_members <-as_tibble(read.csv("Kapitel 04 Arbeitswelt/oecd/Female share of seats on boards of the largest publicly listed companies.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", header = TRUE))%>%
  select(COU,Country,Indicator,Sex,Time,Unit,Value)%>%
  rename(Country.iso = COU, Year = Time)%>%
  mutate(Continent=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'continent'))%>%
  mutate(Country.de=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'country.name.de'))
#Kontinent und Land bearbeiten
share_fem_board_members$Continent.de[share_fem_board_members$Continent=='Europe'] <- 'Europa'
share_fem_board_members$Continent.de[share_fem_board_members$Continent=='Asia'] <- 'Asien'
share_fem_board_members$Continent.de[share_fem_board_members$Continent=='Africa'] <- 'Afrika'
share_fem_board_members$Continent.de[share_fem_board_members$Continent=='Americas'] <- 'Amerika'
share_fem_board_members$Continent.de[share_fem_board_members$Continent=='Oceania'] <- 'Ozeanien'
share_fem_board_members$Country.de[share_fem_board_members$Country.iso=='USA'] <- 'USA'



share_fem_manager_01 <- share_fem_manager%>%
  filter((Continent == 'Europe' | Country.iso == 'USA') & Year=='2017')%>%
  select(Country.de,Year,Sex,Value)%>%
  spread(Sex,Value)%>%
  mutate(Total=Men+Women)%>%
  mutate(Anteil_Frauen_Management = round((Women/Total)*100,1))

share_fem_manager_02 <- share_fem_manager%>%
  filter((Continent == 'Europe' | Country.iso == 'USA'))%>%
  select(Country.de,Year,Sex,Value)%>%
  spread(Sex,Value)%>%
  mutate(Total=Men+Women)%>%
  mutate(Value = round((Women/Total)*100,1))

lab_col07 <- c(rep("black",3),"red",rep("black",17),"red",rep("black",4))

share_fem_manager_01%>%
  ggplot(mapping = aes(x=reorder(Country.de,Anteil_Frauen_Management),y=Anteil_Frauen_Management))+
  geom_col(fill="coral3")+
  ylim(0, 75)+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "Anteil Frauen im Management",subtitle = "OECD Länder (2017)",caption = "Quelle: OECD - Indikator Share of employed, whoi are managers 2017")+
  geom_text(aes(label=Anteil_Frauen_Management), hjust = 1.2, colour="white")+
  coord_flip()+
  theme(axis.text.y = element_text(colour = lab_col07))


ggsave("Kapitel 04 Arbeitswelt/Bilder/Anteil Frauen im Management Europa und USA 2017.jpg")

lab_col06 <- c(rep("black",13),"red",rep("black",7),"red",rep("black",6))

share_fem_board_members%>%
  filter((Continent == 'Europe' | Country.iso == 'USA') & Year=='2017')%>%
  ggplot(mapping = aes(x=reorder(Country.de,Value),y=Value))+
  geom_bar(stat="identity", fill="coral3")+
  theme(legend.position = "none")+
  ylim(0, 50)+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "Anteil Frauen im Aufsichtsrat",subtitle =  "OECD Länder (2017)",caption = "Quelle: OECD - Indikator Female share on boards 2017")+
  geom_text(aes(label=Value), hjust = 1.2, colour="white")+
  coord_flip()+
  theme(axis.text.y = element_text(colour = lab_col06))

ggsave("Kapitel 04 Arbeitswelt/Bilder/Anteil Frauen Aufsichtsrat.jpg")

#Verlauf Manager und Board Members DE & USA

share_frauen_in_management_board<-rbind(
share_fem_board_members%>%
  filter(Country.iso == 'DEU' | Country.iso == 'USA')%>%
  select(Country.de,Year,Value)%>%
  mutate(Indicator='Anteil\nFrauen im Aufsichtsrat'),
share_fem_manager_02%>%
  filter(Country.de == 'Deutschland' | Country.de == 'USA')%>%
  select(Country.de,Year,Value)%>%
  mutate(Indicator='Anteil\nManagerinnen'))

share_frauen_in_management_board%>%
ggplot(mapping = aes(x=Year,y=Value, color=Indicator))+
  geom_line(aes(group = Indicator))+
  theme(legend.title=element_blank(),legend.position = "bottom",axis.ticks=element_blank())+
  ggtitle("")+
  xlab("Jahr")+
  ylab("Prozent")+
  facet_grid(Country.de~.)+
  labs(title = "Anteil Frauen im Aufsichtsrat & im Management",subtitle = "Deutschland und USA (2010 bis 2016)",caption = "Quelle: OECD - Indikator Female share on boards & Management 2017")

ggsave("Kapitel 04 Arbeitswelt/Bilder/Anteil Frauen Firmenleitungen_Management_US_DE_Zeitverlauf.jpg")

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# 06 world bank: Anteil Frauen an Parlamenten, in Ministerien

worldbank_indicators_data <- as_tibble(read.csv("Kapitel 04 Arbeitswelt/worldbank/63f1783b-e916-42ff-8826-d67688420843_Data.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", header = TRUE, na.strings = '..'))%>%
  rename(Indicator=Series.Name,
         '2009' = X2009..YR2009.,
         '2010' = X2010..YR2010.,
         '2011' = X2011..YR2011.,
         '2012' = X2012..YR2012.,
         '2013' = X2013..YR2013.,
         '2014' = X2014..YR2014.,
         '2015' = X2015..YR2015.,
         '2016' = X2016..YR2016.,
         '2017' = X2017..YR2017.,
         '2018' = X2018..YR2018.,
         Country=Country.Name,
         Country.iso=Country.Code
  )%>%
  gather(Years,Value,-c(Indicator,Country.iso,Country,Series.Code))%>%
  mutate(Continent=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'continent'))%>%
  mutate(Country.de=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'country.name.de'))
#Kontinent und Land bearbeiten
worldbank_indicators_data$Continent.de[worldbank_indicators_data$Continent=='Europe'] <- 'Europa'
worldbank_indicators_data$Continent.de[worldbank_indicators_data$Continent=='Asia'] <- 'Asien'
worldbank_indicators_data$Continent.de[worldbank_indicators_data$Continent=='Africa'] <- 'Afrika'
worldbank_indicators_data$Continent.de[worldbank_indicators_data$Continent=='Americas'] <- 'Amerika'
worldbank_indicators_data$Continent.de[worldbank_indicators_data$Continent=='Oceania'] <- 'Ozeanien'
worldbank_indicators_data$Country.de[worldbank_indicators_data$Country.iso=='USA'] <- 'USA'


#worldbank_indicators_definition <- as_tibble(read.csv("Kapitel 04 Arbeitswelt/worldbank/63f1783b-e916-42ff-8826-d67688420843_Definition and Source.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", header = TRUE, na.strings = '..'))


share_fem_parlament_2018 <- filter(worldbank_indicators_data,Series.Code=='SG.GEN.PARL.ZS')%>%
  select(Indicator,Country.iso,Country.de,Continent.de, Years, Value)%>%
  filter(Years==2018)%>%
  arrange(desc(Value))

share_fem_parlament_2018 <- share_fem_parlament_2018[is.na(share_fem_parlament_2018$Value)==FALSE,]%>%
  mutate(ranking = min_rank(desc(Value)))



#Gesamt:
share_fem_parlament_2018_Top5_tail5_USA_DE<- rbind(head(share_fem_parlament_2018,3),
                                                  # c('..','..','..','..','..','',''),
                                                   filter(share_fem_parlament_2018, Country.iso == 'DEU'),
                                                  # c('..','..','..','..','..','',''),
                                                   filter(share_fem_parlament_2018, Country.iso == 'USA')
                                                   #c('..','..','..','..','..','',''),
                                                   #tail(share_fem_parlament_2018,3)
                                                  )

tabelle3 <- share_fem_parlament_2018_Top5_tail5_USA_DE%>%
  select(Country.de, Continent.de,ranking,Value)%>%
  rename(Land=Country.de, Kontinent = Continent.de, Rank=ranking, 'Anteil in %' = Value)


tab_kap4_2 <- tabelle3 %>%
  gt()%>%
  tab_header(
    title = "Anteil Frauen in Parlamenten (2018)",
    subtitle = "Top 3, Deutschland & USA")%>%
    tab_source_note(source_note = "Quelle: world bank - Indikator share of parlament 2018")

gtsave(tab_kap4_2,"Kapitel 04 Arbeitswelt/Bilder/Anteil Frauen in Parlamenten2018.png")

#Europa&USA
share_fem_parlament_2018_EU_USA <-share_fem_parlament_2018%>%
  filter((Continent.de == 'Europa' | Country.iso == 'USA') & Years=='2018')

lab_col06 <- c(rep("black",8),"red",rep("black",15),"red",rep("black",19))

share_fem_parlament_2018_EU_USA%>%
  ggplot(mapping = aes(x=reorder(Country.de,Value),y=Value))+
  geom_bar(stat="identity", fill="coral3")+
  ylim(0, 50)+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "Anteil Frauen Parlament",subtitle =  "Europa und USA (2018)", caption = "Quelle:  world bank - Indikator share of parlament 2018")+
  coord_flip()+
  geom_text(aes(label=Value),hjust=1.5,colour="white")+
  theme(axis.text.y = element_text(colour = lab_col06))

ggsave("Kapitel 04 Arbeitswelt/Bilder/Anteil Frauen Parlament Europa & USA (2018).jpg") 

share_fem_parlament_DE_USA<- filter(worldbank_indicators_data,Series.Code=='SG.GEN.PARL.ZS')%>%
  select(Indicator,Country.iso,Country.de,Continent.de, Years, Value)%>%
  filter(Country.iso == 'DEU' | Country.iso == 'USA')%>%
  arrange(desc(Value))

#Zeitverlauf DE & USA
share_fem_parlament_DE_USA%>%
  ggplot(mapping = aes(x=Years,y=Value, color=Country.de))+
  geom_line(aes(group = Country.de))+
  theme(legend.title=element_blank(),legend.position = "bottom",axis.ticks=element_blank())+
  ggtitle("")+
  xlab("Jahr")+
  ylab("Prozent")+
  ylim(0,50)+
  labs(title = "Anteil Frauen im Parlament", subtitle="Deutschland & USA", caption = "Quelle: world bank - Indikator share of parlament 2018")+
  scale_colour_manual(name = 'the colour', values =c('Deutschland'='coral3','USA'='grey37'), labels = c('Deutschland','USA'))

ggsave("Kapitel 04 Arbeitswelt/Bilder/Anteil Frauen Parlament Deutschland & USA_zeitverlauf.jpg") 

share_fem_minister <- filter(worldbank_indicators_data,Series.Code=='SG.GEN.MNST.ZS')%>%
  select(Indicator,Country.iso,Country.de,Continent.de, Years, Value)%>%
  filter(Years %in% c('2017','2016','2015'))%>%
  arrange(desc(Value))

share_fem_minister<-share_fem_minister[is.na(share_fem_minister$Value) == FALSE,] 

Land_Jahr <- group_by(share_fem_minister,Country.de)%>%
  summarise(Year=max(Years))

share_fem_minister.01 <- na.omit(inner_join(share_fem_minister,Land_Jahr,c("Country.de"="Country.de","Years" = "Year"))) %>%
  arrange(desc(Value))%>%
  mutate(ranking = min_rank(desc(Value)))

#Status Quo Top3 letzte 3, USA & DE
share_fem_minister_top3_last3_de_us_2017 <-rbind(
  share_fem_minister.01%>% head(3),
#  c('..','..','..','..','..','',''),
  share_fem_minister.01%>% filter(Country.iso == 'DEU'),
 # c('..','..','..','..','..','',''),
  share_fem_minister.01%>% filter(Country.iso == 'USA')
  #c('..','..','..','..','..','',''),
  #share_fem_minister.01%>% tail(3)
)
tabelle4 <- share_fem_minister_top3_last3_de_us_2017%>%
  select(Country.de, Continent.de,ranking,Value)%>%
  rename(Land=Country.de, Kontinent = Continent.de, Rank=ranking, 'Anteil in %' = Value)

tab_kap4_3<- tabelle4%>%
  gt()%>%
    tab_header(
    title = "Anteil Frauen in Ministerpositionen (Datenbasis 2015-2018)",
    subtitle = "Top 3, letzte 3, Deutschland & USA")%>%
    tab_source_note(source_note = "Quelle: world bank - share of minister 2015-2018")

gtsave(tab_kap4_3,"Kapitel 04 Arbeitswelt/Bilder/Anteil Frauen in Ministerpositionen_2015bis2018.png")


#Minister Status Quo 2015-2018 EU
lab_col05 <- c(rep("black",23),"red",rep("black",7),"red",rep("black",12))

share_fem_minister.01%>%
  filter((Continent.de == 'Europa' | Country.iso == 'USA'))%>%
  ggplot(mapping = aes(x=reorder(Country.de,Value),y=Value))+
  geom_bar(stat="identity", fill="coral3")+
  theme_economist_white()+
  ylim(0, 70)+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "Anteil Frauen als Minister Europa & USA", subtitle = "Daten zwischen 2015-2018",caption = "Quelle: world bank - share of minister 2015-2018")+
  coord_flip()+
  geom_text(aes(label=Value),hjust=1.5,colour="white")+
  theme(axis.text.y = element_text(colour = lab_col05))

ggsave("Kapitel 04 Arbeitswelt/Bilder/Anteil Frauen als Minister Europa & USA.jpg") 
