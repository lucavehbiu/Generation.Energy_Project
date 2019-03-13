#natural resources and maps
nature <- import("natural_resources.csv", skip = 1)

#Function for making the first row the header
load("header.true.R")
header.true(nature) -> nature #put the right header

#recode variables the same as in the working horse(data)
nature <- plyr:: rename(nature, c("Regio's" = "Gemeentena",
                                "2015" = "surface",
                                "Onderwerp" = "resource")) #rename columns
nature <- repair_names(nature) #repair names

#make the names pretty
nature <- nature %>% filter(!(resource == "Totale oppervlakte" |
                              resource == "||Totaal buitenwater")) #remove unnecessary stuff

nature$surface[is.na(nature$surface)] -> "Unknown"


#recode nature's natural resources categories
nature$resource <- recode(nature$resource, 
                         "||Totaal  bos en open natuurlijk terrein" = "Bos en Natuur",
                         "||Totaal agrarisch terrein" = "Agrarisch",
                         "||Totaal bebouwd terrein" = "Bebouwd",
                         "||Totaal binnenwater" = "Binnenwater",
                         "||Totaal recreatieterrein" = "Overig",
                         "||Totaal semi-bebouwd terrein" = "Semi-bebouwd",
                         "Bouwterrein" = "Infrastructuur",
                         "Totaal verkeersterrein" = "Overig")  #Fixing nature file

#recode the updated municipalities
nature$Gemeentena <- recode(nature$Gemeentena,
                           "'s-Gravenhage (gemeente)" = "Den Haag",
                           "Groningen (gemeente)" = "Groningen",
                           "Utrecht (gemeente)" = "Utrecht",
                           "SÃºdwest-FryslÃ¢n" = "Súdwest-Fryslân",
                           "Beek (L.)" = "Beek",
                           "Hengelo (O.)" = "Hengelo",
                           "Laren (NH.)" = "Laren",
                           "Middelburg (Z.)" = "Middelburg",
                           "Rijswijk (ZH.)" = "Rijswijk",
                           "Stein (L.)" = "Stein",
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
                           'Vlagtwedde' = 'Westerwolde')


