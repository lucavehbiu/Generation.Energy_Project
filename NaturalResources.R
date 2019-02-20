#natural resources and maps

nature <- import("natural_resources.csv")

#############
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
header.true(nature) -> nature #put the right header
nature <- plyr:: rename(nature, c("Regio's" = "Gemeentena",
                                "2015" = "surface",
                                "Onderwerp" = "resource")) #rename columns
nature <- repair_names(nature) #repair names

nature <- nature %>% filter(!(resource == "Totale oppervlakte" |
                              resource == "||Totaal buitenwater")) #remove unnecessary stuff

nature$surface[is.na(nature$surface)] -> "Unknown"


#recode nature's natural resources
nature$resource <- recode(nature$resource, 
                         "||Totaal  bos en open natuurlijk terrein" = "Bos en Natuur",
                         "||Totaal agrarisch terrein" = "Agrarisch",
                         "||Totaal bebouwd terrein" = "Bebouwd",
                         "||Totaal binnenwater" = "Binnenwater",
                         "||Totaal recreatieterrein" = "Overig",
                         "||Totaal semi-bebouwd terrein" = "Semi-bebouwd",
                         "Bouwterrein" = "Infrastructuur",
                         "Totaal verkeersterrein" = "Overig")  #Fixing nature file

nature$Gemeentena <- recode(nature$Gemeentena,
                           "'s-Gravenhage (gemeente)" = "Den Haag")


#############  