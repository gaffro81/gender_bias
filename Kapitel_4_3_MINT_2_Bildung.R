################################################
#
# Kapitel: 4.3 Bildung/Ausbildung
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

library(gt)
library(glue)
library(stringr)

setwd("C:/Users/dig/Desktop/Weiterbildung Data Science/Modul3 Storytelling/xx Hausarbeit/Quellen")
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

#allgemein

oecd_pisa_score_by_sex_field_country <-read.csv2("Kapitel 04_3 Ausbildung MINT Fächer/oecd_pisa_Score_science, math_reading_by_sex_year.csv", header=T, stringsAsFactors = FALSE, na.strings = '-',fileEncoding = 'UTF-8-BOM')%>%
  filter(Jurisdiction=='Germany'|Jurisdiction=='United States'|Jurisdiction=='International Average (OECD)')%>%
  gather(Sex,Value,-c(Jurisdiction,Score,Year))%>%
  mutate(Value = as.numeric(Value))

oecd_pisa_score_by_sex_field_country$Jurisdiction[oecd_pisa_score_by_sex_field_country$Jurisdiction=='Germany'] <- c('Deutschland')
oecd_pisa_score_by_sex_field_country$Jurisdiction[oecd_pisa_score_by_sex_field_country$Jurisdiction=='United States'] <- c('USA')
oecd_pisa_score_by_sex_field_country$Jurisdiction[oecd_pisa_score_by_sex_field_country$Jurisdiction=='International Average (OECD)'] <- c('OECD-Durchschnitt')

labels_pisa <- c(`Averages for PISA mathematics scale` = "Mathematik",`Averages for PISA science scale`=  "Naturwissenschaft")


oecd_pisa_score_by_sex_field_country%>%
  filter(Year=='2015')%>%
  filter(Score!='Averages for PISA reading scale')%>%
  ggplot(mapping = aes(x=Jurisdiction,y=Value, fill=Sex))+
  geom_col( position = "dodge")+
  facet_grid(Score~.,labeller= as_labeller(labels_pisa))+
  xlab(NULL)+
  theme(legend.position = "bottom", legend.title = element_blank())+
  ylab(NULL)+
  ylim(0,550)+
  xlab(NULL)+
  ggtitle("Vergleich Pisa Score in Mathematik & Naturwissenschaften",subtitle = "für Deutschland, USA und OECD Durchschnitt (2015)")+
  scale_fill_manual(values = c("Female"="coral3", "Male"="blue4"),labels = c("Mädchen", "Jungen"))+
  geom_text(aes(label=Value),vjust=1.5,colour="white", position= position_dodge(.9))

ggsave("Kapitel 04_3 Ausbildung MINT Fächer/Bilder/Vergleich Pisa Score in Mathematik & Naturwissenschaften.jpg")  

oecd_studienanfaenger_information_total_anteil_frauen_level_education <-read.csv("Kapitel 04_3 Ausbildung MINT Fächer/oecd_Share of new entrants by gender in fields of education_percentWomen.csv", header=T, stringsAsFactors = FALSE, na.strings = '-',fileEncoding = 'UTF-8-BOM')%>%
  filter(Country.=='Germany' | Country.== 'United States')%>%
  filter(SEX=='F')%>%
  filter(FIELD=='T'| FIELD=='F06')%>%
  select(Country.,Field,Level.of.education,Value)%>%
  rename(Country=Country.,LevelofEducation=Level.of.education)


tabelle1<- oecd_studienanfaenger_information_total_anteil_frauen_level_education%>%
  unite(Group1,Country,Field)%>%
  spread(Group1,Value)%>%
  mutate(Rank=c(2,5,3,4,1,6))%>%
  arrange(Rank)

tabelle1$LevelofEducation <- c('kurzzeitige akademische Ausbildung','Bachelor','Masterähnliches erstes Studium','Master','Doktor','Alle Abschlüsse')

tab_kap4_3_1<- tabelle1[1:5]%>%
  gt(rowname_col = "LevelofEducation")%>%
  tab_spanner(label = "Deutschland",
              columns = vars(`Germany_Information and Communication Technologies`, `Germany_Total: All fields of education`)
              )%>%
  tab_spanner(label = "USA",
              columns = vars(`United States_Information and Communication Technologies`, `United States_Total: All fields of education`)
  )%>%
  cols_label(
    `United States_Information and Communication Technologies` = md("IT"),
    `United States_Total: All fields of education` = md("Alle Studiengänge"),
    `Germany_Information and Communication Technologies` = md("IT"),
    `Germany_Total: All fields of education` = md("Alle Studiengänge")
  )%>%
  tab_stubhead_label(label = "Abschluß") %>%
  tab_header(
    title = md("Anteil Frauen in IT Studiengängen im Vergleich zu allen Studiengängen"),
    subtitle = md("für USA und Deutschland nach Abschluß in Prozent")
  )%>%
  tab_source_note(source_note = "Quelle: OECD Studienanfänger 2015")%>%
  fmt_missing(
    columns = 1:4,
    missing_text = "-"
  )%>%
  fmt_number(
    columns = TRUE,
    decimals = 1,
    suffixing = TRUE
  )

tab_kap4_3_1

gtsave(tab_kap4_3_1,"Kapitel 04_3 Ausbildung MINT Fächer/Bilder/Anteil Frauen in IT Studiengängen im Vergleich zu allen Studiengängen.png")
