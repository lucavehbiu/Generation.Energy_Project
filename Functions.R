#Functions

NumeriConverter <- function(x) {
  data_factor <- subset(data[, 1:3]) #split the numerics from categoricals
  data_rem <- subset(data[, 4:166])
  data_rem[sapply(data_rem, is.character)] <- lapply(data_rem[sapply(data_rem, is.character)], 
                                                     as.numeric)
  data <- cbind(data_factor, data_rem)
  data$Gemeentenaam <- as.factor(data$Gemeentenaam)
  
  data <- repair_names(data)
  return(data)
  
}

FortifyShapeFile <- function(x) {
  
  area@data$id <- rownames(area@data)
  area.points <- fortify(area, coords="id")
  area.fort <- plyr:: join(area.points, area@data, by="id")
}

gathererAO.NB <- function(x) {
  #AO projections per year
  pjes %>% gather(ends_with("15_tj"), key = "baseYearAO", value = "prj15AO") %>% 
    dplyr:: select(Gemeentena, prj15AO, GM_Code, Shape__Are, Shape__Len, Res_regio, baseYearAO) -> a15
  
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
}







