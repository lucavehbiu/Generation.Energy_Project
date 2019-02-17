require(sf)
require(rgdal)
require(maptools)
require(ggmap)

#working with shapefiles R
shape <- readOGR(dsn = ".", layer = "zon_pot_heide_akl_grl_10p")

shape2 <- readOGR(dsn = ".", layer = "zon_dak_pot1")

#find max area
maxArea <- max(shape2$shape_area)

biggestAreaMask <- which(shape2$shape_area == maxArea)
biggestAreaName <- shape2$name[biggestAreaMask]

area <- fortify(shape2)


allAreas <- spplot(shape2@polygons, zcol = "area", identify = T)


bashk %>% ggplot(aes(Shape__Are, Shape__Len, color = Res_regio)) + geom_point()
