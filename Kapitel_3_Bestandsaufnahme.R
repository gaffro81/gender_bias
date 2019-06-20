################################################
#
# Kapitel: 3 Population & Global Gender Index
#
################################################
# 00 Pakete laden & Setup Grafisches Layout

library(tidyverse)
#install.packages("countrycode")
library(countrycode)
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)

library(gt)
library(glue)

#install.packages("webshot")
library(webshot)
#install_phantomjs()

# 01 Daten einlesen & erste Aufbereitung

setwd("C:/Users/dig/Desktop/Weiterbildung Data Science/Modul3 Storytelling/xx Hausarbeit/Quellen")

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

# 01 population von UN Daten

population <- as_tibble(read.csv("Kapitel 03 Allgemein/population/Population_by_sex_UNdata_20190506_165023191.csv", stringsAsFactors = FALSE, na.strings = "..", fileEncoding = "UTF-8-BOM"))
population$Country.iso <- countrycode(sourcevar = population$Country.or.Area,origin =  'country.name', destination = 'iso3c')


population <- population %>%
  filter(Area == 'Total', Sex == 'Male' | Sex =='Female')%>%
  select(Country.iso,Country.or.Area,Year, Sex, Value)%>%
  rename(Country = Country.or.Area, Population_value = Value)%>%
  group_by(Country.iso, Country,Year,Sex)%>%
  summarise(Pop_value = max(Population_value) )%>%
  mutate(Continent=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'continent'))%>%
  mutate(Country.de=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'country.name.de'))
  

population$Country[population$Country.iso=='GBR'] <- 'United Kingdom'
#Kontinente eindeutschen

population$Continent.de[population$Continent=='Europe'] <- 'Europa'
population$Continent.de[population$Continent=='Asia'] <- 'Asien'
population$Continent.de[population$Continent=='Africa'] <- 'Afrika'
population$Continent.de[population$Continent=='Americas'] <- 'Amerika'
population$Continent.de[population$Continent=='Oceania'] <- 'Ozeanien'


#2.1 Bevölkerungsverteilung nach Geschlecht

pop2 <- population %>% 
  filter(Year == '2016')%>%
  group_by(Year,Sex, Country,Country.iso,Country.de,Continent, Continent.de)%>%
  summarise(Pop_value1 = max(Pop_value))%>%
  spread(Sex,Pop_value1)%>%
  mutate(Gesamt=Male+Female)%>%
  gather("Sex",Pop_value1,-c(Year,Country,Country.iso,Country.de,Gesamt,Continent, Continent.de))%>%
  mutate(Pop_value_percent = round(Pop_value1/Gesamt*100,1))%>%
  filter(Sex == 'Female')%>%
  arrange(desc(Pop_value_percent), Country)

pop2$Country.de[pop2$Country.de == 'Vereinigte Staaten'] = 'USA'

pop2$Pop_value_percent[pop2$Country.de == 'Litauen'] = 54.0

#Durchschnitt je Kontinent
Continent <- pop2 %>%
  filter(is.na(Continent) == FALSE )%>%
  group_by(Continent)%>%
  summarise(Durchschnitt_Kontinent=(sum(Pop_value1)/sum(Gesamt))*100)

#Durchschnitt Welt
world_mean <- population %>% 
  filter(Year == '2017')%>%
  group_by(Year,Sex)%>%
  summarise(Pop_value1 = sum(Pop_value)/1000000)%>%
  mutate(Pop_value_percent = Pop_value1/sum(Pop_value1)*100)%>%
  filter(Sex=="Female")%>%
  select(Pop_value_percent)
world_mean <- as.numeric(world_mean[1,2])

#Durchschnitt Europa
eu_mean<- as.numeric(Continent[Continent[1]=='Europe',2])  

#Durchschnitt DE
de <- as.numeric(na.omit((pop2$Pop_value1[pop2$Country.iso == 'DEU']/pop2$Gesamt[pop2$Country.iso == 'DEU'])*100))

#Durchschnitt USA
us <- as.numeric(na.omit((pop2$Pop_value1[pop2$Country.iso == 'USA']/pop2$Gesamt[pop2$Country.iso == 'USA'])*100))


#Exploration - nicht für Hausarbeit:
ggplot(pop2,mapping = aes(x=Pop_value_percent,y= Country))+
  geom_point(size=3)+
  theme_economist_white()

#---------------------------------------------------------------------

# Grafiken

#Anteil Frauen in % nach Kontinent 2017



pop2%>%
  na.omit(Continent)%>%
  filter(Sex=='Female', Year=='2016')%>%
  group_by(Continent.de,Sex)%>%
  summarize(v1 = mean(Pop_value_percent))%>%
  ggplot(mapping = aes(x=reorder(Continent.de,desc(v1)),y=v1, fill=Sex))+
  geom_col()+
  ylim(0, 100)+
  geom_hline(aes(yintercept = world_mean),color="red", linetype="dashed", size=1)+
  theme(legend.position = "none",axis.ticks=element_blank())+
  labs(title = "Anteil Frauen je Kontinent in Prozent",subtitle = "(2016)", caption = "Quelle: UN Statitik Daten zur Bevölkerung von data.un.org 2019")+
  scale_fill_manual(values = c("coral3"))+
  xlab(NULL)+
  ylab(NULL)+
  geom_text(aes(label = round(v1,1)),vjust = 1.5, colour = "white")

ggsave("Kapitel 03 Allgemein/Bilder/Anteil Frauen je Kontinent.jpg")




# Verteilungen als facet nach Kontinenten durchschnittlicher Anteil Frauen je Land aufgeteilt nach Kontinenten
pop2%>%
  na.omit(Continent)%>%
  mutate(Pop_value_percent = round(Pop_value_percent,1))%>%
ggplot(mapping = aes(x=Pop_value_percent))+
  geom_histogram(bins=180)+
  geom_vline(aes(xintercept = world_mean),color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept = eu_mean),color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept = de),color="yellow", linetype="dashed", size=1)+
  facet_grid(Continent.de~.)+
  xlab("Prozentwerte")+
  ylab("Anzahl Länder")+
  labs(title="Häufigkeit des Anteils Frauen an Bevölkerung",subtitle = "aufgeteilt nach Kontinent",caption = "Quelle: UN Statitik Daten zur Bevölkerung von data.un.org 2019")
 
ggsave("Kapitel 03 Allgemein/Bilder/Verteilung_Kontinent_Häufigkeiten_Anteil_Frauen_je_Land.jpg")

lab_col04 <- c(rep("black",19),"red",rep("black",4),"red",rep("black",20))

#Europa & USA
pop2 %>%
  filter(Continent == 'Europe' | Country.iso == 'USA')%>% 
  ggplot(mapping = aes(x=reorder(Country.de,Pop_value_percent),y=Pop_value_percent))+
  geom_bar(stat="identity", fill = ("coral3"))+
  geom_hline(aes(yintercept = eu_mean,linetype="EU-Mittelwert"),color="blue", size=1, show.legend = T)+
  ylim(0, 100)+
  xlab(NULL)+
  ylab(NULL)+
  labs(title="Anteil Frauen je Land", subtitle = "in Europa & USA in Prozent (2016)",
       caption = "Quelle: UN Statitik Daten zur Bevölkerung von data.un.org 2019")+
  geom_text(aes(label = round(Pop_value_percent,1)),hjust = 1.5, colour = "white", position = position_identity())+
  coord_flip()+
  theme(legend.title = element_blank(),legend.position = "bottom",legend.key.width = unit(1,"cm"))+
  scale_linetype_manual(values = c(2, 1),
                        guide = guide_legend(override.aes = list(color = c("blue"))))+
  theme(axis.text.y = element_text(colour = lab_col04))


ggsave("Kapitel 03 Allgemein/Bilder/Anteil Frauen je Land in EU_US.jpg")

# Hintergrundlinien weg

#DE& US & EU im Vergleich

data.frame(Country.de=c("Deutschland", "USA"),Pop_value_percent=c(de,us), stringsAsFactors = F)%>%  
  rbind(c("EU",eu_mean))%>%
  mutate(Pop_value_percent = as.numeric(Pop_value_percent))%>%
  ggplot(mapping = aes(x=reorder(Country.de,Pop_value_percent),y=Pop_value_percent))+
  geom_col(fill=("coral3"))+
  ylim(0, 100)+
  geom_hline(aes(yintercept = world_mean,linetype="Weltweiter Durchschnitt"),color="red",  size=1)+
  theme(legend.position = "bottom", legend.title = element_blank(),axis.ticks=element_blank(),legend.key.width = unit(1,"cm"))+
  labs(title = "Anteil Frauen in Prozent",subtitle = "(2016)",
       caption = "Quelle: UN Statitik Daten zur Bevölkerung von data.un.org 2019")+
  xlab(NULL)+
  ylab(NULL)+
  geom_text(aes(label = round(Pop_value_percent,1)),vjust = 1.5, colour = "white")+
  scale_linetype_manual(values = c(2, 1),
                        guide = guide_legend(override.aes = list(color = c("red"))))


ggsave("Kapitel 03 Allgemein/Bilder/Anteil Frauen DE_US_EU.jpg")


#Tabelle der Ausreißer  
ausreißer <- filter(pop2,Pop_value_percent < 40 | Pop_value_percent > 55 )%>%
  select(Country.de,Pop_value1,Gesamt,Pop_value_percent)

tab_kap3_1a<- ausreißer[,5:8] %>%
  gt()  %>%
  cols_label(Country.de = "Land", Pop_value1 = "Anzahl Frauen in Tsd.", Gesamt ="Bevölkerung in Tsd.", Pop_value_percent = "Anteil Frauen in %")%>%
  tab_header(
    title = "Länder mit Frauenanteil < 40 %",
    subtitle = "Stand 2017") %>%
  tab_source_note(source_note = "Quelle: UN Statitik Daten zur Bevölkerung von data.un.org 2019")%>%
  fmt_number(columns = vars(Pop_value1),  suffixing = FALSE, decimals = 0,scale_by = 0.001)%>%
  fmt_number(columns = vars(Gesamt),  suffixing = FALSE, decimals = 0,scale_by = 0.001)
  
gtsave(tab_kap3_1a,"Kapitel 03 Allgemein/Bilder/Ausreißer_Länder mit Frauenanteil unter 40.png")



#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

# 02 global gender index des world economic forum

global_gender_i <- as_tibble(read.csv("Kapitel 03 Allgemein/world economic forum - global gender gap/data.csv", stringsAsFactors = FALSE, na.strings = ".."))
#Spalten umbennen & tidy up
global_gender_i <- global_gender_i%>%
  rename(Country.iso = Country.ISO3,Country=Country.Name,Indicator.Code = Indicator.Id,
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
         '2018' = X2018
  )%>%
  gather("Time","Indicator.Value",-c(Country.iso,Country,Indicator.Code,Indicator,Subindicator.Type))%>%
  mutate(Continent=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'continent'))%>%
  #  filter(is.na(Continent) == FALSE)%>%
  mutate(Country.de=countrycode(sourcevar = Country.iso,origin = 'iso3c', destination = 'country.name.de'))

global_gender_i$Continent.de[global_gender_i$Continent=='Europe'] <- 'Europa'
global_gender_i$Continent.de[global_gender_i$Continent=='Asia'] <- 'Asien'
global_gender_i$Continent.de[global_gender_i$Continent=='Africa'] <- 'Afrika'
global_gender_i$Continent.de[global_gender_i$Continent=='Americas'] <- 'Amerika'
global_gender_i$Continent.de[global_gender_i$Continent=='Oceania'] <- 'Ozeanien'

global_gender_i$Country.de[global_gender_i$Country.iso=='USA'] <- 'USA'

global_gender_i_overall_rank <- global_gender_i%>%
  filter(Indicator == 'Overall Global Gender Gap Index', Subindicator.Type == 'Rank')%>%
  select(Continent.de,Country.de,Year=Time,Value =Indicator.Value)


global_gender_i_overall <- global_gender_i%>%
  filter(Indicator == 'Overall Global Gender Gap Index', Subindicator.Type == 'Index')%>%
  select(Continent.de,Country.de,Year=Time,Value =Indicator.Value)

Durchschnitt_je_Kontinent <- global_gender_i_overall%>%
  filter(is.na(Value) == FALSE)%>%
  group_by(Continent.de, Year)%>%
  summarise(Value = mean(Value))%>%
  rename(Gruppe=Continent.de)

Durchschnitt_Welt <- global_gender_i_overall%>%
  filter(is.na(Value) == FALSE)%>%
  group_by(Year)%>%
  summarise(Value = mean(Value))%>%
  mutate(Gruppe='Welt')%>%
  select(Gruppe,Year,Value)

DE <-global_gender_i_overall%>%
  filter(Country.de==c("Deutschland"))%>%
  select(Country.de,Year,Value)%>%
  rename(Gruppe=Country.de)

US <-global_gender_i_overall%>%
  filter(Country.de==c("USA"))%>%
  select(Country.de,Year,Value)%>%
  rename(Gruppe=Country.de)

Durchschnitt_DE_US_Kontinent_Welt <- bind_rows(DE,US,Durchschnitt_je_Kontinent,Durchschnitt_Welt)

# Global Gender Index Grafiken
#Hinweis 2017 fehlt in Daten

#De
global_gender_i_overall%>%
  filter(Country.de==c("Deutschland"))%>%
  ggplot(mapping = aes(x=Year,y=Value))+
  geom_line(aes(group = Country.de))+
  #stat_summary(fun.data = "mean_cl_boot", colour="red")+
#  facet_grid(Continent~.)+
  ylim(0,1)+
  theme_economist_white()


#Welt/De/USA
Durchschnitt_DE_US_Kontinent_Welt%>%
  filter(Gruppe==('Deutschland') |Gruppe==('Welt')| Gruppe==('USA') )%>%
  ggplot(mapping = aes(x=Year,y=Value, color=Gruppe))+
    geom_line(aes(group = Gruppe))+
    scale_color_manual(values = c("Deutschland" = "red",
                                           "USA" ="grey37",
                                           "Welt" ="green"))+
    ylim(0.0,0.9)+
    theme(legend.title=element_blank(),legend.position = "bottom",axis.ticks=element_blank())+
    labs(title = "Global Gender Index",subtitle = "(2006 bis 2018)",
         caption = "Quelle: world economic forum - global gender gap report 2017")+
    xlab(NULL)+
    ylab(NULL)
  
  #Wachstum zwischen 2006 und 2018
ggsave("Kapitel 03 Allgemein/Bilder/Global Gender Index_verlauf.jpg")


tabelle2 <- Durchschnitt_DE_US_Kontinent_Welt %>%
  filter(Year=='2006' | Year=='2018', Gruppe == 'Deutschland' | Gruppe == 'USA' | Gruppe == 'Welt' | Gruppe == 'Island'|Gruppe == 'Schweden'|Gruppe == 'Norwegen')%>%
  spread(Year,Value)%>%
  rename('Land oder Region' =Gruppe)%>%
  mutate('Veränderung in %' = ((`2018`/`2006`)-1)*100)
  

tab_kap3_2<- tabelle2 %>%
    gt()%>%
    tab_header(
    title = "Veränderung Global Gender Gap Index",
    subtitle = "Deutschland, USA, Welt") %>%
    tab_source_note(source_note = "Quelle: world economic forum - global gender gap report 2017")%>%
    fmt_number(columns = vars(`2006`),  suffixing = FALSE, decimals = 3,scale_by = 1)%>%
    fmt_number(columns = vars(`2018`),  suffixing = FALSE, decimals = 3,scale_by = 1)%>%
    fmt_number(columns = vars(`Veränderung in %`),  suffixing = FALSE, decimals = 1,scale_by = 1)


  
gtsave(tab_kap3_2,"Kapitel 03 Allgemein/Bilder/DE_US_Welt_Veränderung Global Gender Gap Index.png")

tabelle3 <-  global_gender_i_overall_rank%>%
  filter(Year=='2006' | Year=='2018', Country.de == 'Deutschland' | Country.de == 'USA'| Country.de == 'Island'|Country.de == 'Schweden'|Country.de == 'Norwegen')%>%
  spread(Year,Value)%>%
  rename(Kontinent=Continent.de,Land=Country.de,'Rank 2006' =`2006`,'Rank 2018'=`2018`)%>%
  arrange(`Rank 2018`)%>% 
  cbind('Index 2006'=c(0.7813,0.7994,0.8133,0.7524,0.7042))%>% 
  cbind('Index 2018'=c(0.8580,0.8350,0.8220,0.7760,0.7200))%>%
  mutate(`Index 2018`=as.numeric(`Index 2018`),`Index 2006`=as.numeric(`Index 2006`))

  
tab_kap3_3 <- tabelle3%>%
  select(Kontinent,Land,`Rank 2018`,`Index 2018`,`Rank 2006`,`Index 2006`)%>%
  rename(`Rang 2018` = `Rank 2018`,`Rang 2006` = `Rank 2006`)%>%
    gt()%>%
    tab_header(
      title = "Global Gender Gap Index & Rank 2018/2006",subtitle = "Top3 2018, Deutschland, USA")%>%
      tab_source_note(source_note = "Quelle: world economic forum - global gender gap report 2017")%>%
      fmt_number(columns = vars(`Index 2006`),  suffixing = FALSE, decimals = 2,scale_by = 1)%>%
      fmt_number(columns = vars(`Index 2018`),  suffixing = FALSE, decimals = 2,scale_by = 1)

tab_kap3_3

gtsave(tab_kap3_3,"Kapitel 03 Allgemein/Bilder/Top3_DE_US_Global Gender Gap Index & Rank 2018-2006.png")
