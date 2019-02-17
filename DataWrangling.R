require(rio)
require(dplyr)
require(ggplot2)
require(tidyverse)
require(data.table)
require(scales)
require(grid)
require(gridExtra)

#Read and load the necessary data
data <- import("180320_Klimaatmonitor_Energieverbruik_berekeningsmap_herindelingenmap_jtv.xlsx", 
               sheet = 'TOTALEN_TJ')
new_data <- read_csv("new_data.csv")

load("bashk.RDA")   #AOprojections
load("bashkNB.RDA") #NB projections
load("fundi.RDA")   #un-gathered final data
load('data.RDA')    #gather final data

#Past wrangling
#############
#remove the last rows NA from gemeentenaam
Data <- subset(data[2:408,])

#make the first row as headers
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

header.true(Data) -> final

#convert everything to numeric
data_factor <- subset(final[, 1:3])
data_rem <- subset(final[, 4:166])

data_rem[sapply(data_rem, is.character)] <- lapply(data_rem[sapply(data_rem, is.character)], 
                                                   as.numeric)

#stitch back together
final <- cbind(data_factor, data_rem)

final <- subset(final[, 2:166])

#convert geementaname to a factor
final$Gemeentenaam <- as.factor(final$Gemeentenaam)

final <- repair_names(final)
#############

#AO projection + NB projections
###############################
#base year
pjes %>% gather(ends_with("15_tj"), key = "baseYearAO", value = "prj15AO") %>% 
  select(Gemeentena, baseYearAO, prj15AO, GM_Code, Shape__Are, Shape__Len, Res_regio) -> a15

#AO projections per year
pjes %>% gather(ends_with("50AO_tj"), key = "AO50", value = "prj50AO") %>% 
  select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, prj50AO, AO50) -> a50

pjes %>% gather(ends_with("40AO_tj"), key = "AO40", value = "prj40AO") %>% 
  select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, prj40AO, AO40)-> a40

pjes %>% gather(ends_with("30AO_tj"), key = "AO30", value = "prj30AO") %>% 
  select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, prj30AO, AO30) -> a30

pjes %>% gather(ends_with("25AO_tj"), key = "AO25", value = "prj25AO") %>% 
  select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, prj25AO, AO25)-> a25

pjes %>% gather(ends_with("20AO_tj"), key = "AO20", value = "prj20AO") %>% 
  select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, prj20AO, AO20)-> a20

#bind the ones with equal observation
bashk1 <- cbind(a15, a50)
bashk2 <- cbind(a40, a30)
bashk3 <- cbind(bashk2, a25)
bashk4 <- cbind(bashk3, a20)

#all partional prjections into one dataset
bashk <- rowr:: cbind.fill(bashk1, bashk4) %>% 
  repair_names() #repair similar names for ease of column selection

#Remove duplicate columns
bashk %>% select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, baseYearAO, 
                 prj15AO, AO20, prj20AO, AO25, prj25AO, AO30, prj30AO, AO40, prj40AO, 
                 AO50,  prj50AO) -> bashk


#NB projections per year
pjes %>% gather(ends_with("15_tj"), key = "baseYearNB", value = "prj15NB") %>% 
  select(Gemeentena, baseYearNB, prj15NB, GM_Code, Shape__Are, Shape__Len, Res_regio) -> b15

pjes %>% gather(ends_with("50NB_tj"), key = "NB50", value = "prj50NB") %>% 
  select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, prj50NB, NB50) -> b50

pjes %>% gather(ends_with("40NB_tj"), key = "NB40", value = "prj40NB") %>% 
  select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, prj40NB, NB40)-> b40

pjes %>% gather(ends_with("30NB_tj"), key = "NB30", value = "prj30NB") %>% 
  select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, prj30NB, NB30) -> b30

pjes %>% gather(ends_with("25NB_tj"), key = "NB25", value = "prj25NB") %>% 
  select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, prj25NB, NB25)-> b25

pjes %>% gather(ends_with("20NB_tj"), key = "NB20", value = "prj20NB") %>% 
  select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, prj20NB, NB20)-> b20

#bind the ones with equal observation
bashk11 <- cbind(b15, b50)
bashk22 <- cbind(b40, b30)
bashk33 <- cbind(bashk22, b25)
bashk44 <- cbind(bashk33, b20)

#all partional projections into one dataset
bashkNB <- rowr:: cbind.fill(bashk11, bashk44) %>% 
  repair_names() 

bashkNB %>% select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, baseYearNB, 
                 prj15NB, NB20, prj20NB, NB25, prj25NB, NB30, prj30NB, NB40, prj40NB, 
                 NB50,  prj50NB) -> bashkNB



#final data NB + AO
final_data <- cbind(bashk, bashkNB)
final_data <- repair_names(final_data)
final_data %>% select(Gemeentena, GM_Code, Shape__Are, Shape__Len, Res_regio, 
                      baseYearAO, prj15AO, AO20, prj20AO, AO25, prj25AO, AO30, 
                      prj30AO, AO40, prj40AO, AO50,  prj50AO, baseYearNB, 
                      prj15NB, NB20, prj20NB, NB25, prj25NB, NB30, prj30NB, 
                      NB40, prj40NB, NB50,  prj50NB) -> final_data
###############################

#Update Municipalities
######################

final$Gemeentenaam <- recode(final$Gemeentenaam, 
                             "Ten Boer" = "Groningen*",
                             'Haren' = 'Groningen*',
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
                             'Neerijnen' = 'West Betuwe')
######################
#de Marne is not in the data, Kollumerland, Nieuwkruisland as well

#Sum up Municipalities and Merge
################################
final %>% group_by(Gemeentenaam) %>%  summarise_if(is.numeric, .funs = sum) -> ultimo

#rename to have same column name
plyr:: rename(ultimo, c("Gemeentenaam" = "Gemeentena")) -> ultimo

#merge new_data
fundi <- merge(ultimo, new_data, by = "Gemeentena")
################################

#split to portions and totals again
fundi %>% select(contains("tot"), Gemeentena) -> plot

fundi %>% select(-contains("tot"), Gemeentena) -> pjes


#gather for 2040 e.g
pjes %>% gather(ends_with("40AO_tj"), key = "AO40", value = "prj40AO") %>% 
  select(Gemeentena, prj40AO, AO40)-> test




g <- test %>%   
  filter(Gemeentena == "Amsterdam") %>% 
  mutate(fraction = round((prj40AO/ sum(prj40AO)), digits = 2)) %>%  
  ggplot(aes(Gemeentena, fraction, fill = AO40)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
        x = element_blank(), 
        y = element_blank()) +
  guides(fill = guide_legend(title = "Energy sources", reverse = T)) +
  theme(axis.ticks = element_blank(), 
        panel.background = element_blank(),
        axis.text.y = element_blank()) +
  scale_fill_brewer(palette = "Spectral") + 
  geom_text(aes(label = paste0(fraction, '%')), 
            position=position_dodge(width=0.9), vjust=0) +
  scale_y_continuous(labels = percent)

tg <- grobTree(textGrob("Energy Potential Distribution (TJ) - 2050 AO", 
                        y= 0.8, vjust=1, gp = gpar(fontsize=13, face=2, col="gray")),
               textGrob("Amsterdam", y= -0.3, vjust= 0, gp = gpar(fontsize= 40, face=4, col="gray")),
               cl="titlegrob")

heightDetails.titlegrob <- function(x) do.call(sum,lapply(x$children, grobHeight))
  
grid.arrange(g, top = tg)




















