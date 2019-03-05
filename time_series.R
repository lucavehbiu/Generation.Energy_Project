#modeling future energy demand
require(pacman)
p_load(gridExtra, 
       tidyverse, 
       dplyr, 
       ggplot2, 
       tibbletime, 
       GGally, 
       fpp2, 
       vars, 
       lubridate, 
       imputeTS, 
       vars, 
       ggfortify, 
       tidyr, 
       DataCombine, 
       tseries, 
       seasonal, xts, DMwR)

library(Mcomp)
library(purrr)

energy <- import("total_demand.xls", skip = 1)

energy_left <- energy[, 2:8]
energy_right <- energy[, 1]


energy_left$`2010` <- recode(energy_left$`2010`, "?" = "NA")
energy_left$`2011` <- recode(energy_left$`2011`, "?" = "NA")
energy_left$`2012` <- recode(energy_left$`2012`, "?" = "NA")
energy_left$`2013` <- recode(energy_left$`2013`, "?" = "NA")
energy_left$`2014` <- recode(energy_left$`2014`, "?" = "NA")
energy_left$`2015` <- recode(energy_left$`2015`, "?" = "NA")
energy_left$`2016` <- recode(energy_left$`2016`, "?" = "NA")

energy_left[sapply(energy_left, is.character)] <- lapply(energy_left[sapply(energy_left, is.character)], 
                                                    as.numeric)

energy <- cbind(energy_right, energy_left)

energy <- plyr:: rename(energy, c("energy_right" = "Gemeentena"))


energy <- t(energy) %>% as.data.frame()
header.true(energy) -> energy

ts <- ts(energy[, 4], start = 2010, end = 2016, frequency = 1)

ts2 <- na.interpolation(ts, option = "spline")


ts %>% auto.arima(1, 2) %>% forecast(h = 10) %>% autoplot()

autoplot(ts2, facets = T) + autolayer(ts)

autoplot(forecast(ts, h = 20))

ts2 %>% stlf()


























