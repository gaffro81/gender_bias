
library(tidyverse)
#install.packages("countrycode")
library(countrycode)
library(ggthemes)

library(ggmap)
library(grid)
library(rworldmap)
library(png)

library(gt)
library(glue)
library(stringr)
#install.packages("forcats")
library(forcats)

setwd("C:/Users/dig/Desktop/Weiterbildung Data Science/Modul3 Storytelling/xx Hausarbeit/Quellen")



eu_nace2_code<- read.csv("Kapitel 05 Gender bias in IT/nace2_code_to_text.txt", sep ="|",header = T,stringsAsFactors = F, fileEncoding = 'UTF-8')

eu_arbeiter_nach_wirtschaft <- read.delim("Kapitel 05 Gender bias in IT/EU_lfsa_eegan2_2.tsv", header=T, stringsAsFactors = FALSE, fileEncoding = 'ISO-8859-13', na.strings = ' ')%>%
  separate(unit.sex.age.nace_r2.geo.time,into = c("Unit","Sex","Age", "Wirtschaftszweig-Code","Land.Code"),sep= ",")%>%
  rename(
    '2008'= "X2008",            
    '2009'= "X2009",           
    '2010'= "X2010",            
    '2011'= "X2011",            
    '2012'= "X2012",
    '2013'= "X2013", 
    '2014'= "X2014",
    '2015'= "X2015",           
    '2016'= "X2016",
    '2017'= "X2017",
    '2018'= "X2018",
    WirtschaftszweigCode="Wirtschaftszweig-Code",
    Sex.Code=Sex
  )%>%
  filter(Sex.Code!='T')%>%
  filter(WirtschaftszweigCode!='TOTAL')%>%
  filter(Age=="Y15-74")%>%
  gather(Year,Value_workforce,-c(Unit,Sex.Code,Age,WirtschaftszweigCode,Land.Code))%>%  
  select(WirtschaftszweigCode,Land.Code,Sex.Code,Year,Value_workforce)%>%
  group_by(WirtschaftszweigCode,Land.Code,Sex.Code,Year)%>%
  summarise(Value_Workforce_inTHS=sum(Value_workforce))%>%
  mutate(Land=countrycode(sourcevar = Land.Code,origin = 'eurostat', destination = 'country.name.de'))%>%
  left_join(eu_nace2_code,c("WirtschaftszweigCode"="code"))%>%
  rename(Wirtschaftszweig = description_de,Wirtschaftszweig_en = description_en)

eu_arbeiter_nach_wirtschaft$Sex[eu_arbeiter_nach_wirtschaft$Sex.Code == 'F']<- 'Frauen'
eu_arbeiter_nach_wirtschaft$Sex[eu_arbeiter_nach_wirtschaft$Sex.Code == 'M']<- 'Männer'

eu_by_industry_2018 <-as.data.frame(eu_arbeiter_nach_wirtschaft)%>%
  filter(Year=='2018' & is.na(Land)==FALSE & is.na(Wirtschaftszweig)==FALSE )%>%
  select(Land,Wirtschaftszweig,Wirtschaftszweig_en,Sex,Value_Workforce_inTHS)%>%
  spread(Sex,Value_Workforce_inTHS)%>%
  mutate(Total=Frauen + Männer)%>%
  mutate(Frauen_percent=round((Frauen/Total)*100,1))%>%
  select(Land,Wirtschaftszweig,Frauen_percent)

us_by_industrie_2018 <-read.csv2("Kapitel 05 Gender bias in IT/US_cpsaat18_Employed_persons_by_detailed_industry_sex_race.csv", header=T, stringsAsFactors = FALSE, fileEncoding = 'ISO-8859-13', na.strings = '-', sep = ';')

us_by_industrie_2018_main <- us_by_industrie_2018 %>%
  filter(Group=='Main')%>%
  mutate(Land = 'USA')%>%
  rename(Wirtschaftszweig=EU_Industry,Frauen_percent=Percent_Women_Employed.persons.by.detailed.industry_2018)%>%
  select(Land,Wirtschaftszweig,Frauen_percent)

us_eu_2018_byindustry <- rbind(eu_by_industry_2018,us_by_industrie_2018_main) 


us_eu_2018_byindustry%>%
  filter(Land=="USA"|Land=="Deutschland")%>%
  rename(Prozent = Frauen_percent)%>%
  spread(Land,Prozent)%>%
  filter(Deutschland < 80)%>%
 # filter(Deutschland > 60 | Deutschland < 40  )%>%
  arrange((Deutschland))%>%
  gt()%>%
  tab_header(
    title = "Anteil Frauen in Wirtschaftszweigen",subtitle = "USA und Deutschalnd (2018)")%>%
  tab_source_note(source_note = "Quelle: Eurostat & US Bureau of labor statistics")
  