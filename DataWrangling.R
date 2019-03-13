#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Final project | Ubiqum + Generation.Energy 

#March 2019 - Luca Vehbiu
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(rio, dplyr, ggplot2, tidyverse, data.table, scales, 
               sf, rgdal, maptools, ggmap, spdep, rgeos, raster, 
               sp, plotrix , RColorBrewer, mapview)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Read and load the necessary data
##Big spreadsheet with energy potential demand projections (2015-2050)

data <- import("180320_Klimaatmonitor_Energieverbruik_berekeningsmap_herindelingenmap_jtv.xlsx", 
               sheet = 'TOTALEN_TJ', skip = 2) 

new_data <- read_csv("new_data.csv") #Updated gementees

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Manipulate the data

##Convert everything to numeric
load('converter.R')

NumeriConverter(data) -> final

##Remove last NA rows
finall <- subset(final[, 2:166])




##Update Municipalities

#divide provinces from municipalities
provinces <- subset(finall[395:406,])

finall <- subset(finall[1:395,])

#remove duplicate gemeentes
finall %>% filter(!(Gemeentenaam == 'Zevenaar' | 
                      Gemeentenaam == 'Leeuwarden' | 
                      Gemeentenaam == 'Súdwest-Fryslân' |
                      Gemeentenaam == 'Midden-Groningen*' | 
                      Gemeentenaam == 'Westerwolde*' | 
                      Gemeentenaam == 'Waadhoeke*')) -> finall

#recode manually new merge municipalities and the ones with *
finall$Gemeentenaam <- recode(finall$Gemeentenaam, 
                             "Ten Boer" = "Groningen",
                             'Haren' = 'Groningen',
                             'Binnenmaas' = 'Hoeksche Waard',
                             'Cromstrijen' = 'Hoeksche Waard',
                             'Korendijk' = 'Hoeksche Waard' ,
                             'Oud-Beijerland' = 'Hoeksche Waard' ,
                             'Strijen' = 'Hoeksche Waard',
                             'Leerdam' = 'Vijfheerenlanden',
                             'Vianen' = 'Vijfheerenlanden',
                             'Zederik' = 'Vijfheerenlanden',
                             'Aalburg' = 'Altena',
                             'Werkendam' = 'Altena',
                             'Woudrichem' = 'Altena',
                             'Nuth' = 'Beekdaelen',
                             'Onderbanken' = 'Beekdaelen',
                             'Schinnen' = 'Beekdaelen',
                             'Haarlemmerliede' = 'Haarlemmermeer',
                             'Spaarnwoude' = 'Haarlemmermeer',
                             'Bedum' = 'Het Hogeland',
                             'Eemsmond' = 'Het Hogeland',
                             'Winsum' = 'Het Hogeland',
                             'Grootegast' = 'Westerkwartier',
                             'Leek' = 'Westerkwartier',
                             'Marum' = 'Westerkwartier',
                             'Zuidhorn' = 'Westerkwartier',
                             'Giessenlanden' = 'Molenlanden',
                             'Molenwaard' = 'Molenlanden',
                             'Dongeradeel' = 'Noardeast-Fryslân',
                             'Ferwerderadiel' = 'Noardeast-Fryslân',
                             'Noordwijkerhout' = 'Noordwijk',
                             'Geldermalsen' = 'West Betuwe',
                             'Lingewaal' = 'West Betuwe',
                             'Neerijnen' = 'West Betuwe',
                             'Menterwolde' = 'Midden-Gronigen',
                             'Hoogezand-Sappemeer' = 'Midden-Groningen',
                             'Slochteren' = 'Midden-Groningen',
                             'Franekeradeel' = 'Waadhoeke',
                             "het Bildt" = 'Waadhoeke',
                             'Menameradiel' = 'Waadhoeke',
                             'Littenseradiel' = 'Waadhoeke',
                             'Bellingwedde' = 'Westerwolde',
                             'Vlagtwedde' = 'Westerwolde',
                             'Leeuwarden*' = 'Leeuwarden',
                             'Zevenaar*' = 'Zevenaar',
                             'Súdwest-Fryslân*' = 'Súdwest-Fryslân')

###de Marne Kollumerland, Nieuwkruisland = Missing

##Sum up Municipalities & Merge
finall %>% group_by(Gemeentenaam) %>%  summarise_if(is.numeric, .funs = sum) -> ultimo

###rename to have same column name
plyr:: rename(ultimo, c("Gemeentenaam" = "Gemeentena")) -> ultimo

###recode the gemeentes in the new data
new_data$Gemeentena <- recode(new_data$Gemeentena,
                              "'s-Gravenhage" = 'Den Haag')
###merge new_data
fundi <- merge(ultimo, new_data, by = "Gemeentena")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#AO projection + NB projections

##split to portions and totals again
fundi %>% dplyr:: select(contains("tot"), Gemeentena) -> plot

fundi %>% dplyr:: select(-contains("tot"), Gemeentena) -> pjes

##remove residual heat
pjes %>% dplyr:: select(-contains("rest")) -> pjes


#create two column with all years for AO and NB projections
load("ultimateStick_and_gather.R")
gathererAO.NB(pjes) -> final_data

#Save the data
save(final_data, file = "data.RDA")




















