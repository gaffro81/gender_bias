################################################
#
# Kapitel: 6 Zusammenfassung
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
#install.packages("forcats")
library(forcats)

setwd("C:/Users/dig/Desktop/Weiterbildung Data Science/Modul3 Storytelling/xx Hausarbeit/Quellen")
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------


zusammenfassung <- read.csv2("Kapitel 06 Zusammenfassung/Daten für Zusammenfassung.csv", stringsAsFactors = F)%>%
  mutate(Deutschland = as.numeric(Deutschland), USA = as.numeric(USA))%>%
  rename(`Angaben in` =Angaben.in)

tab_kap6_1 <- zusammenfassung%>%
  gt(groupname_col = "Bereich")%>%
#  tab_stubhead_label(label = "Bereich") %>%
  tab_header(
    title = md("Indikatoren Überblick für USA und Deutschland"),
    subtitle = md("aktuelle verfügbare Werte")
  )%>%
  tab_source_note("Quelle: eigene Zusammenstellung auf Grundlage der vorangeganenen Kapitel")%>%
  fmt_missing(
    columns = 1:4,
    missing_text = "-"
  )%>%
  fmt_number(
    columns = 3:4,
    decimals = 1,
    suffixing = FALSE
  )

gtsave(tab_kap6_1,"Kapitel 06 Zusammenfassung/Bilder/Zusammenfassung_Indikatoren Überblick für USA und Deutschland.png")
