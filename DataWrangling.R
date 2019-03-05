#~Final project | Ubiqum + Generation.Energy 
#~March 2019 | Luca Vehbiu

#Load libraries
require(rio)
require(dplyr)
require(ggplot2)
require(tidyverse)
require(data.table)
require(scales)
require(grid)
require(gridExtra)
require(sf)
require(rgdal)
require(maptools)
require(ggmap)
require(spdep)
require(rgeos)
require(raster)
require(sp)
require(plotrix) 
library(RColorBrewer)
library(mapview)
devtools::install_github('bbc/bbplot', force = T)#have not used p_load - fear of failure when deployed




#Read and load the necessary data
data <- import("180320_Klimaatmonitor_Energieverbruik_berekeningsmap_herindelingenmap_jtv.xlsx", 
               sheet = 'TOTALEN_TJ', skip = 2) 

electric_tj.and.kwh <- import("180320_Klimaatmonitor_Energieverbruik_berekeningsmap_herindelingenmap_jtv.xlsx", 
                              sheet = '2015_geind_nw_gem18_(m3-kwh)') 



#big spreadsheet with energy potential demand projections (2015-2050)

new_data <- read_csv("new_data.csv") 
#data frame with regions, shape_area and updated geementes (2019)

#load save RDA objects
load("bashk.RDA")   #AO projections (automation)
load("bashkNB.RDA") #NB projections (after savings)
load("before_merging.RDA")  #un-gathered final data
load("un_gathered.merged.RDA")  #merged

load('data.RDA')    #gather final data (working horse)

load('mapping.RDA') #area of NL fortified and merge (Geemente + Regio)

load('nature.RDA')  #natural resource data (to be recoded)

#Past wrangling (make the data frame in the correct format)
#############
#convert everything to numeric
data_factor <- subset(data[, 1:3])
data_rem <- subset(data[, 4:166])

data_rem[sapply(data_rem, is.character)] <- lapply(data_rem[sapply(data_rem, is.character)], 
                                                   as.numeric)

#stitch back together
final <- cbind(data_factor, data_rem)

final <- subset(final[, 2:166])

#convert geementaname to a factor
final$Gemeentenaam <- as.factor(final$Gemeentenaam)

finall <- repair_names(final)
#############

#Update Municipalities
######################

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
######################
#de Marne Kollumerland, Nieuwkruisland = Missing

#Sum up Municipalities & Merge
##############################
finall %>% group_by(Gemeentenaam) %>%  summarise_if(is.numeric, .funs = sum) -> ultimo

#rename to have same column name
plyr:: rename(ultimo, c("Gemeentenaam" = "Gemeentena")) -> ultimo

#recode the gemeentes in the new data
new_data$Gemeentena <- recode(new_data$Gemeentena,
                              "'s-Gravenhage" = 'Den Haag')
#merge new_data
fundi <- merge(ultimo, new_data, by = "Gemeentena")
##############################

#AO projection + NB projections
###############################

#split to portions and totals again
fundi %>% dplyr:: select(contains("tot"), Gemeentena) -> plot

fundi %>% dplyr:: select(-contains("tot"), Gemeentena) -> pjes

#remove residual heat
pjes %>% dplyr:: select(-contains("rest")) -> pjes

#base year
pjes %>% gather(ends_with("15_tj"), key = "baseYearAO", value = "prj15AO") %>% 
  dplyr:: select(Gemeentena, prj15AO, GM_Code, Shape__Are, Shape__Len, Res_regio, baseYearAO) -> a15

#AO projections per year
pjes %>% gather(ends_with("50AO_tj"), key = "AO50", value = "prj50AO") %>% 
  dplyr:: select(prj50AO, AO50) -> a50

pjes %>% gather(ends_with("40AO_tj"), key = "AO40", value = "prj40AO") %>% 
  dplyr:: select(prj40AO, AO40)-> a40

pjes %>% gather(ends_with("30AO_tj"), key = "AO30", value = "prj30AO") %>% 
  dplyr:: select(prj30AO, AO30) -> a30

pjes %>% gather(ends_with("25AO_tj"), key = "AO25", value = "prj25AO") %>% 
  dplyr:: select(prj25AO, AO25)-> a25

pjes %>% gather(ends_with("20AO_tj"), key = "AO20", value = "prj20AO") %>% 
  dplyr:: select(prj20AO, AO20)-> a20

#bind the ones with equal observation
do.call("cbind", list(a15, a20, a25, a30, a40, a50)) -> AOproj



#NB projections per year
pjes %>% gather(ends_with("15_tj"), key = "baseYearNB", value = "prj15NB") %>% 
  dplyr:: select(prj15NB, baseYearNB) -> b15

pjes %>% gather(ends_with("50NB_tj"), key = "NB50", value = "prj50NB") %>% 
  dplyr:: select(prj50NB, NB50) -> b50

pjes %>% gather(ends_with("40NB_tj"), key = "NB40", value = "prj40NB") %>% 
  dplyr:: select(prj40NB, NB40)-> b40

pjes %>% gather(ends_with("30NB_tj"), key = "NB30", value = "prj30NB") %>% 
  dplyr:: select(prj30NB, NB30) -> b30

pjes %>% gather(ends_with("25NB_tj"), key = "NB25", value = "prj25NB") %>% 
  dplyr:: select(prj25NB, NB25)-> b25

pjes %>% gather(ends_with("20NB_tj"), key = "NB20", value = "prj20NB") %>% 
  dplyr:: select(prj20NB, NB20)-> b20

#bind em all
do.call("cbind", list(b15, b20, b25, b30, b40, b50)) -> NBproj


#final data NB + AO
final_data <- cbind(AOproj, NBproj)

###############################
save(final_data, file = "data.RDA")




















