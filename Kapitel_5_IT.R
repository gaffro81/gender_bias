################################################
#
# Kapitel: 5 IT
#
################################################
# 00 Pakete laden & und Working directory festlegen

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
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#Allgemein

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

#Anteil Frauen an Arbeitswelt vs. Anteil Frauen in Tech Arbeitswelt nach Land 2018

lab_col01 <- c(rep("black",21),"red",rep("black",12),"red",rep("black"))

honeypot%>%
  select('Rank','Country.de','Female_in_Perc','FemaleWorkforce_Tech_in_Perc')%>%
  mutate(Country.de = fct_reorder(Country.de, (FemaleWorkforce_Tech_in_Perc)))%>%
  gather(Indicator,Value,-c('Rank','Country.de'))%>%
  filter(Indicator=='FemaleWorkforce_Tech_in_Perc')%>%
  ggplot(mapping = aes(x=(Country.de),y=Value, fill="coral3"))+
  geom_bar(stat="identity")+
  theme(legend.position = "none")+
  ylim(0, 75)+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "Anteil arbeitende Frauen in IT-Unternehmen", subtitle = "Europa & USA in Prozent (2018)",
      caption = "Quelle: honeypot.io - Women in Tech Index 2018"
        )+
  geom_text(aes(label=Value),hjust=1.0, colour="white", size=3)+
  coord_flip()+
  theme(axis.text.y = element_text(colour = lab_col01))

ggsave("Kapitel 05 Gender bias in IT/Bilder/Anteil arbeitende Frauen in IT-Unternehmen.jpg")

#nur US/DE

honeypot%>%
  select('Rank','Country.de','Female_in_Perc','FemaleWorkforce_Tech_in_Perc')%>%
  rename('Anteil\narbeitende Frauen'= Female_in_Perc,'Anteil\narbeitende Frauen in IT-Branche' =FemaleWorkforce_Tech_in_Perc)%>%
  mutate(Country.de = fct_reorder(Country.de, (`Anteil\narbeitende Frauen in IT-Branche`)))%>%
  gather(Indicator,Value,-c('Rank','Country.de'))%>%
  filter(Country.de=='Deutschland'| Country.de=='USA')%>%
  ggplot(mapping = aes(x=(Country.de),y=Value, fill=Country.de))+
  geom_bar(stat="identity")+
  guides(fill=guide_legend(title="Land"))+
  theme(legend.position = "none")+
  ylim(0, 75)+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "Anteil Frauen gesamter Arbeitsmarkt vs. Anteil Frauen in IT Branche", subtitle="in Deutschland & USA (2018)",
       caption = "Quelle: honeypot.io - Women in Tech Index 2018"
       )+
  scale_fill_manual("legend", values = c("Deutschland" = "red", "USA" = "grey37"))+
  facet_grid(.~Indicator)+
  geom_text(aes(label=round(Value,1)),vjust=1.0, colour="white", size=3)

ggsave("Kapitel 05 Gender bias in IT/Bilder/US_DE_Anteil Frauen gesamter Arbeitsmarkt vs. Anteil in IT Unternehmen.jpg")

lab_col02 <- c(rep("black",20),"red",rep("black",6),"red",rep("black",13))

# Gegenüberstellung Anteil IT Arbeiter an Gesamtarbeitsmarkt nach Geschlecht
honeypot%>%
  select(Country.de,Total_Workforce_in_Mil,Workforce_Tech,FemaleWorkforce_Tech)%>%
  mutate(
    Total_Workforce=Total_Workforce_in_Mil*1000,
    MaleWorkforce_Tech = Workforce_Tech-FemaleWorkforce_Tech
  )%>%
  mutate(
    Frauen=round((FemaleWorkforce_Tech/Total_Workforce)*100,1),
    Männer=round((MaleWorkforce_Tech/Total_Workforce)*100,1)
  )%>%
  select(Country.de,Frauen,Männer)%>%
  gather(Geschlecht,Value,-Country.de)%>%
  ggplot(mapping = aes(x=reorder(Country.de,Value),y=Value,fill=Geschlecht))+
  geom_bar(stat="identity",position = "stack")+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_fill_manual("legend", values = c("Frauen" = "coral3", "Männer" = "blue4"))+
  ylim(0, 10)+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "Anteil IT Arbeitnehmer an Gesamtarbeitnehmer", subtitle = "nach Geschlecht in Prozent (2018)",
       caption = "Quelle: honeypot.io - Women in Tech Index 2018")+
  coord_flip()+
  theme(axis.text.y = element_text(colour = lab_col02))
  
ggsave("Kapitel 05 Gender bias in IT/Bilder/Anteil IT Arbeitnehmer an Gesamtarbeitnehmer nach Geschlecht.jpg")

#Verteilungen
# DE

de_beruf_geschlecht_zeitreihe <-read.csv2("Kapitel 05 Gender bias in IT/DE_destatis_verteilung nach Berufsgruppen und Geschlecht2.csv", header=T, stringsAsFactors = FALSE, fileEncoding = 'ISO-8859-13', na.strings = '-', sep = ';')%>%
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
    '2018'= "X2018"           
  )%>%
  select(-Job)%>%
  gather(Jahr,Anzahl_Arbeitnehmer,-c(Code,Wirtschaftszweig,Sex))%>%
  group_by(Code,Wirtschaftszweig,Sex,Jahr)%>%
  summarise(Anzahl= sum(Anzahl_Arbeitnehmer))

# EU

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

lab_col03 <- c(rep("black",21),"red",rep("black",8),"red",rep("black",10))

us_eu_2018_byindustry%>%
  filter(Wirtschaftszweig=='INFORMATION UND KOMMUNIKATION')%>%
  ggplot(mapping = aes(x=reorder(Land,Frauen_percent),y=Frauen_percent, fill="coral3"))+
  geom_bar(stat="identity")+
  theme(legend.position = "none")+
  geom_text(aes(label=Frauen_percent),hjust = 1.0,colour="white")+
  ylim(0, 100)+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "Anteil Frauen in Wirtschaftszweig\nInformation & Kommunikation",subtitle = "für Europa & USA in Prozent (2018)",
       caption = "Quelle: Eurostat und US Bureau of labor statistics 2018")+
  coord_flip()+
  theme(axis.text.y = element_text(colour = lab_col03))

ggsave("Kapitel 05 Gender bias in IT/Bilder/EU_USA_Anteil Frauen in Wirtschaftszweig Information & Kommunikation.jpg")


#Zeitreihe DE nach Wirtschaftszweig absolut

eu_arbeiter_nach_wirtschaft%>%
  filter(Land==('Deutschland') & WirtschaftszweigCode =='J' )%>%
  ggplot(mapping = aes(x=Year,y=Value_Workforce_inTHS, color=Sex))+
  geom_line(aes(group = Sex))+
  theme(legend.title=element_blank(),legend.position = "bottom",axis.ticks=element_blank())+
  labs(title = "Entwicklung Arbeitnehmer nach Geschlecht\nin Wirtschaftszweig Information & Kommunikation",subtitle = "Deutschland in Tsd. (2008 bis 2018)",
       caption = "Quelle: Eurostat 2018")+
  xlab("Jahr")+
  ylab(NULL)+
  scale_fill_manual("legend", values = c("Frauen" = "coral3", "Männer" = "blue4"))

ggsave("Kapitel 05 Gender bias in IT/Bilder/DEFrauen in Wirtschaftszweig Information & Kommunikation.jpg")

#Zeitreihe DE nach Wirtschaftszweig relativ
eu_anteil_frauen_in_information<- as.data.frame(eu_arbeiter_nach_wirtschaft%>%
  filter( WirtschaftszweigCode =='J' ))%>%
  filter(Land==('Deutschland') | Land.Code == 'EU28' )%>%
  select(Year,Land, Land.Code,Sex,Value_Workforce_inTHS)%>%
  spread(Sex,Value_Workforce_inTHS)%>%
  mutate(Total=Frauen+Männer)%>%
  mutate(Percent = round((Frauen/Total)*100,1))%>%
  select(Land.Code,Year,Percent)%>%
  rename(Land.de=Land.Code)

us_anteil_frauen_in_information <- read.csv2("Kapitel 05 Gender bias in IT/US_cpsaat18_percent_women_in_information.csv", header = T,stringsAsFactors = F)%>%
  select(Land.de,Year,Percent)

us_eu_de_anteil_frauen_in_information <- rbind(eu_anteil_frauen_in_information,us_anteil_frauen_in_information)

us_eu_de_anteil_frauen_in_information%>%
  mutate(Percent=as.numeric(Percent))%>%
  ggplot(mapping = aes(x=Year,y=Percent, col=Land.de))+
  geom_line(aes(group = Land.de))+
  ylim(0,75)+
  theme(legend.title=element_blank(),legend.position = "bottom",axis.ticks=element_blank())+
  labs(title = "Anteil Frauen im Zeitverlauf\nin Wirtschaftszweig Information & Kommunikation",subtitle = "in Prozent (2008 bis 2018)",
       caption = "Quelle: Eurostat und US Bureau of labor statistics 2018")+
  scale_color_manual("legend",values = c("DE"="red", "EU28"="blue", "USA"="grey37"),labels = c("Deutschland", "EU", "USA"))+
  xlab(NULL)+
  ylab(NULL)

ggsave("Kapitel 05 Gender bias in IT/Bilder/DE_US_EU_Montenegro_Anteil Frauen im Zeitverlauf in Wirtschaftszweig Information & Kommunikation.jpg")

# US
us_percent_of_Women_in_comp_occupation_zeitreihe <- read.csv2("Kapitel 05 Gender bias in IT/US_Percentage of employed women_computing_occupations2000_2018.csv", header=T, stringsAsFactors = FALSE, fileEncoding = 'ISO-8859-13', na.strings = '-', sep = ';')%>%
  rename(
    '2000'= "X2000",
    '2005'= "X2005",            
    '2009'= "X2009",           
    '2013'= "X2013", 
    '2014'= "X2014",
    '2015'= "X2015",           
    '2016'= "X2016",
    '2017'= "X2017",
    '2018'= "X2018",
    Women_incomputing_occupations_percent=Percentage.of.employed.women.in.computing.related.occupations.in.the.United.States.from.2000.to.2018
  )%>%
  gather(Year,Value,-Women_incomputing_occupations_percent)%>%
  mutate(Value=as.numeric(Value))

tab_kap_5_1 <- us_percent_of_Women_in_comp_occupation_zeitreihe%>%
  filter(Year=='2018'|Year == '2013'|Year == '2005')%>%
  rename(Berufe=Women_incomputing_occupations_percent)%>%
  spread(Year,Value)%>%
  gt()%>%
  tab_header(
    title = "Anteil Frauen in IT-Berufen",subtitle = "USA 2005/2013/2018")%>%
  tab_source_note(source_note = "Quelle: US Bureau of labor statistics")

tab_kap_5_1

gtsave(tab_kap_5_1,"Kapitel 05 Gender bias in IT/Bilder/Anteil Frauen in IT-Berufen.png")

employers_main_it_comp <- read.csv2("Kapitel 05 Gender bias in IT/US_SAP_TRADEBYTE_Distribution of employees worldwide by gender and department_tech_comp.csv", header=T, stringsAsFactors = FALSE, fileEncoding = 'ISO-8859-13', na.strings = 'NA', sep = ';')%>%
  rename(
    Value='Value.in..',
    Department=Position,
    Company=Distribution.of.employees.worldwide.2018.women
  )

employers_main_it_comp_02 <- read.csv2("Kapitel 05 Gender bias in IT/US_TRADEBYTE_Distribution of employees_by_gender_comp.csv", stringsAsFactors = FALSE, sep = ";",dec = ",")



employers_main_it_comp_03 <- employers_main_it_comp%>%
filter(Department=='Tech' & is.na(Value)==FALSE)%>%
  full_join(employers_main_it_comp_02,"Company"="Company")%>%
  select(Company,Value,Frauen)%>%
  rename(Women_Tech=Value,Women_overall = Frauen)%>%
  filter(is.na(Women_Tech)==FALSE)%>%
  mutate(Rank=min_rank((Women_Tech)))%>%
  arrange(Rank)


logo_tradebyte = readPNG(source = "Kapitel 05 Gender bias in IT/Logos/logo_tradebyte.png")
logo_apple = readPNG(source = "Kapitel 05 Gender bias in IT/Logos/logo_apple.png")
logo_ebay = readPNG(source = "Kapitel 05 Gender bias in IT/Logos/logo_EBay.png")
logo_facebook = readPNG(source = "Kapitel 05 Gender bias in IT/Logos/logo_facebook.png")
logo_google = readPNG(source = "Kapitel 05 Gender bias in IT/Logos/logo_google.png")
logo_microsoft = readPNG(source = "Kapitel 05 Gender bias in IT/Logos/logo_microsoft.png")
logo_netflix = readPNG(source = "Kapitel 05 Gender bias in IT/Logos/logo_netflix.png")
logo_paypal = readPNG(source = "Kapitel 05 Gender bias in IT/Logos/logo_paypal.png")
logo_twitter = readPNG(source = "Kapitel 05 Gender bias in IT/Logos/logo_twitter.png")
logo_uber = readPNG(source = "Kapitel 05 Gender bias in IT/Logos/logo_uber.png")

logo_tradebyte_r =  rasterGrob(logo_tradebyte, interpolate=TRUE)
logo_apple_r =  rasterGrob(logo_apple, interpolate=TRUE)
logo_ebay_r =  rasterGrob(logo_ebay, interpolate=TRUE)
logo_facebook_r =  rasterGrob(logo_facebook, interpolate=TRUE)
logo_google_r =  rasterGrob(logo_google, interpolate=TRUE)
logo_microsoft_r =  rasterGrob(logo_microsoft, interpolate=TRUE)
logo_netflix_r =  rasterGrob(logo_netflix, interpolate=TRUE)
logo_paypal_r =  rasterGrob(logo_paypal, interpolate=TRUE)
logo_twitter_r =  rasterGrob(logo_twitter, interpolate=TRUE)
logo_uber_r =  rasterGrob(logo_uber, interpolate=TRUE)

employers_main_it_comp_03%>%
  gather(Indicator,Value,-c(Company,Rank))%>%
  ggplot(mapping = aes(x=reorder(Company,Rank),y=Value, fill=Indicator))+
  geom_col(position = "dodge")+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
  )+
  ylim(0, 60)+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "Anteil Frauen in IT-Abteilungen", subtitle = "Software-Unternehmen in Prozent (2017-2019)",
       caption = "Quelle: Statista/Company reports")+
  geom_text(aes(label=Value),hjust=1.0,colour="white",position=position_dodge(0.9))+
  scale_fill_manual(values = c("Women_overall"="grey", "Women_Tech"="coral3"),labels = c("Anteil Frauen\ngesamtes UN", "Anteil Frauen\nIT Abteilungen"))+
  coord_flip()+
  annotation_custom(grob=logo_tradebyte_r, xmin=.7, xmax=1.3, ymin=-Inf, ymax=0)+
  annotation_custom(grob=logo_uber_r, xmin=1.7, xmax=2.3, ymin=-Inf, ymax=0)+
  annotation_custom(grob=logo_microsoft_r, xmin=2.7, xmax=3.3, ymin=-Inf, ymax=0)+
  annotation_custom(grob=logo_twitter_r, xmin=3.7, xmax=4.3, ymin=-Inf, ymax=0)+
  annotation_custom(grob=logo_google_r, xmin=4.7, xmax=5.3, ymin=-Inf, ymax=0)+
  annotation_custom(grob=logo_facebook_r, xmin=5.7, xmax=6.3, ymin=-Inf, ymax=0)+
  annotation_custom(grob=logo_apple_r, xmin=6.7, xmax=7.3, ymin=-Inf, ymax=0)+
  annotation_custom(grob=logo_ebay_r, xmin=7.7, xmax=8.3, ymin=-Inf, ymax=0)+
  annotation_custom(grob=logo_paypal_r, xmin=8.7, xmax=9.3, ymin=-Inf, ymax=0)+
  annotation_custom(grob=logo_netflix_r, xmin=9.7, xmax=10.3, ymin=-Inf, ymax=0)+
  scale_y_continuous(limits=c(-15,60))

ggsave("Kapitel 05 Gender bias in IT/Bilder/Anteil Frauen in Tech-Department für Software-Unternehmen.jpg")

#alte Version ohne Firmenlogos:

employers_main_it_comp_03%>%
  gather(Indicator,Value,-c(Company,Rank))%>%
  ggplot(mapping = aes(x=reorder(Company,Rank),y=Value, fill=Indicator))+
  geom_col(position = "dodge")+
  theme(legend.title = element_blank(),legend.position = "bottom")+
  ylim(0, 60)+
  xlab(NULL)+
  ylab(NULL)+
  labs(title = "Anteil Frauen in IT-Abteilungen", subtitle = "Software-Unternehmen in Prozent (2017-2019)",
       caption = "Quelle: Statista/Company reports")+
  geom_text(aes(label=Value),hjust=1.0,colour="white",position=position_dodge(0.9))+
  scale_fill_manual(values = c("Women_overall"="grey", "Women_Tech"="coral3"),labels = c("Anteil Frauen\ngesamtes UN", "Anteil Frauen\nIT Abteilungen"))+
  coord_flip()




pipeline_women_workplace <- read.csv2("Kapitel 05 Gender bias in IT/US_company_pipeline_Distribution of employees_by_gender_overall.csv", stringsAsFactors = FALSE,quote = ",")%>%
  mutate(Rank=c(6,5,4,3,2,1))%>%
gather(Sex, Value,-c(Karriere.Stufe,Rank))%>%
  mutate(Value = as.double(Value))%>%
  mutate(Value = case_when(Sex == 'Männer' ~ Value, 
                           Sex == 'Frauen' ~ Value*-1))

brks <- seq(-100, 100, 25)
lbs <- c(seq(100,0,-25),seq(25,100,25))

pipeline_women_workplace%>%
ggplot(aes(x = reorder(Karriere.Stufe,Rank), y = Value, fill = Sex)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,
                     labels = lbs)+   # Breaks + # Labels
  coord_flip() +  # Flip axes
  labs(title="Anteil Frauen/Männer je Karriere-Stufe", subtitle = "USA (2018)", caption = "Quelle: McKinsey/Lean IN- woman in workplace Report 2018") +
  theme(axis.ticks = element_blank(), legend.position = 'bottom', legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank()
        ) +  
  scale_fill_manual(values = c("Frauen"="coral3", "Männer"="blue4"),labels = c("Anteil Frauen", "Anteil Männer"))+
  xlab(NULL)+
  ylab(NULL)+
  geom_text(aes(label=abs(Value),hjust= ifelse(Value >= 0, 1.2, -0.2) ),colour="white", size = 3)

ggsave("Kapitel 05 Gender bias in IT/Bilder/US_Anteil Frauen_Männer je Karriere_Stufe.jpg")





#nicht nötig
#us_by_occupation_2018<-read.csv2("Kapitel 05 Gender bias in IT/US_cpsaat11_Employed_persons_by_detailed_occupation_sex_race.csv", header=T, stringsAsFactors = FALSE, fileEncoding = 'ISO-8859-13', na.strings = '', sep = ';')

# Verdienst
# EU
eu_verdienst_gpg_percent <- read.delim("Kapitel 05 Gender bias in IT/EU_earn_gr_gpgr2.tsv", header=T, stringsAsFactors = FALSE, fileEncoding = 'ISO-8859-13', na.strings = ':')%>%
  separate(unit.nace_r2.geo.time,into = c("Unit", "Wirtschaftszweig-Code","Land.Code"),sep= ",")%>%
  rename(
    '2007'= "X2007",            
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
    WirtschaftszweigCode="Wirtschaftszweig-Code"
  )%>%
  mutate(Land=countrycode(sourcevar = Land.Code,origin = 'eurostat', destination = 'country.name.de'))%>%
  left_join(eu_nace2_code,c("WirtschaftszweigCode"="code"))%>%
  rename(
    Wirtschaftszweig=description_de,
    Wirtschaftszweig_en=description_en
  )%>%
  gather(Year,Value_in_percent,-c(Land.Code,Land,Wirtschaftszweig,Wirtschaftszweig_en,WirtschaftszweigCode,Unit))%>%
  select(Land.Code,Land,WirtschaftszweigCode,Wirtschaftszweig,Year,Value_in_percent)%>%
  filter(is.na(Wirtschaftszweig)==FALSE)

eu_verdienst_gpg_percent$Value_in_percent<- gsub(pattern = '[a-z]',replacement = '',x = eu_verdienst_gpg_percent$Value_in_percent)
eu_verdienst_gpg_percent$Value_in_percent<- gsub(pattern = ':',replacement = '',x = eu_verdienst_gpg_percent$Value_in_percent)
eu_verdienst_gpg_percent$Value_in_percent<- as.numeric(str_trim(eu_verdienst_gpg_percent$Value_in_percent))
eu_verdienst_gpg_percent

eu_verdienst_gpg_percent%>%
  filter( Land.Code=='DE' & Year=='2017')%>%
  ggplot(mapping = aes(x=reorder(WirtschaftszweigCode,Value_in_percent),y=Value_in_percent, fill=WirtschaftszweigCode))+
  geom_col()+
  coord_flip()+
  theme(legend.position = "none")+
  labs(title = "Gender Pay Gap nach Wirtschaftszweige",subtitle = "Deutschland (2018)",
       caption = "Quelle: Eurostat 2018")+
  xlab(NULL)+
  ylab(NULL)+
  scale_fill_manual("legend",values = c("J" = "grey37",
                                       "R" ="grey",
                                       "M" ="grey",
                                       "K" ="grey",
                                       "C" ="grey",
                                       "G" ="grey",
                                       "D" ="grey",
                                       "S" ="grey",
                                       "Q" ="grey",
                                       "N" ="grey",
                                       "F" ="grey",
                                       "P" ="grey",
                                       "L" ="grey",
                                       "O" ="grey",
                                       "I" ="grey",
                                       "H" ="grey",
                                       "E" ="grey",
                                       "B" ="grey"
)
) +
  scale_x_discrete(labels = c("Bergbau",
                              "Wasserwirtschaft",
                              "Logistik",
                              "Gatronomie/Hotelerie",
                              "Öffentliche Verwaltung",
                              "Makler & Wohnungswesen",
                              "Erziehung & Unterricht",
                              "Baugewerbe",
                              "Sonsige wirtsch. DL",
                              "Gesundheit-&Sozialwesen",
                              "Sonstige DL",
                              "Energieversorgung",
                              "Handel",
                              "Verarbeitendes Gewerbe",
                              "IT",
                              "Finanz-&Versicherungs DL", 
                              "Wissenschaftl. DL", 
                              "Unterhaltung/Erholung" 
  ))+
  geom_text(aes(hjust=1.2, label=Value_in_percent), colour="white", size=3)

ggsave("Kapitel 05 Gender bias in IT/Bilder/DE_Gender Pay Gap nach Wirtschaftszweige.jpg")


honeypot_1 <- honeypot%>%
  select(Country.de,GenderPayGap,GenderPayGapTech)%>%
  mutate(Gruppe = case_when(GenderPayGap > GenderPayGapTech ~ "Overall GPG > GPG Tech", 
                            GenderPayGap <= GenderPayGapTech ~ "Overall GPG <= GPG Tech")
         )%>%
  gather(Indicator, Value, -c(Country.de,Gruppe))

honeypot_1%>%
  ggplot(mapping = aes(x=reorder(Country.de,Value),y=Value, fill=Indicator))+
  geom_bar(stat="identity", position = "dodge")+
  facet_wrap(.~Gruppe)+
  theme(legend.position = "bottom")+
  coord_flip()+
  ylab(NULL)+
  xlab(NULL)+
  labs(title="Vergleich Overall GPG vs. GPG Tech",subtitle = "in Prozent (2018)",
       caption="Quelle: honeypot.io - Women in Tech Index 2018")+
  scale_fill_manual(values = c("GenderPayGap"="gray", "GenderPayGapTech"="blue4"),labels = c("Gender Pay Gap\nOverall", "Gender Pay Gap\nIT"))
  



ggsave("Kapitel 05 Gender bias in IT/Bilder/Vergleich Overall GPG vs. GPG Tech.jpg")
  

lab_col <- c(rep("black",14),"red",rep("black",24),"red","black")


honeypot%>%
  select(Country.de,DiffGenderPayGap_Overall_Tech)%>%
  ggplot(mapping = aes(x=reorder(Country.de,DiffGenderPayGap_Overall_Tech),y=DiffGenderPayGap_Overall_Tech, fill=DiffGenderPayGap_Overall_Tech>0))+
  geom_bar(stat="identity", position = "dodge")+
  theme(legend.position = "none")+
  coord_flip()+
  ylab(NULL)+
  xlab(NULL)+
  labs(title = "Differenz zwischen allgemeinen GPG und GPG in IT",subtitle = "in % Pkt. (2018)",
          caption="Quelle: honeypot.io - Women in Tech Index 2018")+
  geom_text(aes(hjust = ifelse(DiffGenderPayGap_Overall_Tech >= 0, 1.2, -0.2), label=DiffGenderPayGap_Overall_Tech), colour="white", size=3)+
  scale_fill_manual(breaks= c("TRUE", "FALSE"), values =c( "red3", "green4"))+
  theme(axis.text.y = element_text(colour = lab_col))

ggsave("Kapitel 05 Gender bias in IT/Bilder/Differenz zwischen allgemeinen GPG und GPG in IT.jpg")  

honeypot%>%
  select(Country.de,GenderPayGap,GenderPayGapTech)%>%
  mutate(Gruppe = case_when(GenderPayGap > GenderPayGapTech ~ "Overall GPG > GPG Tech", 
                            GenderPayGap <= GenderPayGapTech ~ "Overall GPG <= GPG Tech")
  )%>%
  gather(Indicator, Value, -c(Country.de,Gruppe))%>%
  filter(Country.de=='Deutschland'|Country.de=='USA')%>%
  ggplot(mapping = aes(x=reorder(Country.de,Value),y=Value, fill=Indicator))+
  geom_col(position = "dodge")+
  theme(legend.position = "bottom", legend.title = element_blank())+
  ylab(NULL)+
  xlab(NULL)+
  geom_text(aes(label=Value),vjust=1.5,colour="white", position= position_dodge(.9))+
  labs(title = "Gegenüberstellung Gender Pay Gap\n Overall und IT",subtitle = "für Deutschland und USA in Prozent (2018)",
                              caption="Quelle: honeypot.io - Women in Tech Index 2018")+
  scale_fill_manual(values = c("GenderPayGap"="grey", "GenderPayGapTech"="grey37"),labels = c("Gender Pay Gap\nOverall", "Gender Pay Gap\nIT"))
  
ggsave("Kapitel 05 Gender bias in IT/Bilder/DE_US_Vergleich GPG und GPG in IT.jpg")  


#nicht mehr nötig
us_by_occupation_earnings_2018 <- read.csv2("Kapitel 05 Gender bias in IT/US_cpsaat39_Median_weekly_earnings_full-time_salary_workers_by_detailed_occupation_sex_2.csv", header=T, stringsAsFactors = FALSE, fileEncoding = 'ISO-8859-13', na.strings = '', sep = ';')%>%
  select(Occupation, Occupation_group,Percent.Women.Workers,Gender.pay.gap.tech)



#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
# nicht mehr nötig
#us_percent_women_industryinformation_2010_2018 <-read.csv2("Kapitel 05 Gender bias in IT/us_LNU02072293_percent_women_industry_infomation_2010_2018.csv", header=T, stringsAsFactors = FALSE, fileEncoding = 'ISO-8859-13', na.strings = '', sep = ';')
  
#Spielerein zum Schluß

mod <- us_by_occupation_earnings_2018$Gender.pay.gap.tech~us_by_occupation_earnings_2018$Percent.Women.Workers
plot(us_by_occupation_earnings_2018$Gender.pay.gap.tech~us_by_occupation_earnings_2018$Percent.Women.Workers)
abline(mod,col="red")

honeypot.cor<- cor(honeypot[3:17])
corrplot(honeypot.cor)

cor.test(honeypot$GenderPayGap,honeypot$GenderPayGapTech)

