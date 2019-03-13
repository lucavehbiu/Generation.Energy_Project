#population data
pop <- import("Reg_pop.csv")

#rename the columns needed to more appropriate ones
pop <- plyr:: rename(pop, 
                     c("Bevolking/Bevolkingssamenstelling op 1 januari/Totale bevolking (aantal)" = "Population",
                       "Bevolking/Bevolkingssamenstelling op 1 januari/Bevolkingsdichtheid (aantal inwoners per kmÂ²)" = "Density",
                       "Bevolking/Bevolkingssamenstelling op 1 januari/Leeftijd/Demografische druk/Totale druk (%)" = "Age",
                       "Regio's" = "Gemeentena"))

#select only the variables needed
pop <- pop %>% select(Gemeentena, Population, Density, Age)

#convert to numeric variables
pop$Population <- as.numeric(pop$Population)
pop$Density <- as.numeric(pop$Density)
pop$Age <- as.numeric(pop$Age)

#recoding population as well (typos and merged municipalities)
pop$Gemeentena <- recode(pop$Gemeentena,
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

#sum up same name gemeente
pop %>% group_by(Gemeentena) %>%  summarise_if(is.numeric, .funs = sum) -> pop

#merge the dataset
merge(fundi, pop, by = "Gemeentena") %>% 
  select("Gemeentena", "Population", "Density", "Res_regio", "Age")-> regio_pop

                         
                         







