
#working with shapefiles R
area <- readOGR(dsn = ".", layer = "RES_regios_2019")

#fortify dat little biatch
area.fort <- fortify(area)

#merge with area pints 
#(putting regions and geementes in the dat of the shapefile)
area@data$id <- rownames(area@data)
area.points <- fortify(area, coords="id")
area.fort <- plyr:: join(area.points, area@data, by="id")

#try plotting
ggplot(area.fort, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = c(Res_regio, Gemeentena) == mapFill)) +
  geom_path() +
  coord_equal() +
  guides(fill = guide_legend(element_blank())) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank())
  

plot(area, cex = plotrix:::rescale(area@data$Shape__Len, c(1, 4)), pch = 19)


mapFill <- list("Metropoolregio Eindhoven", "Eindhoven")



require(plotly)
devtools::install_github('ropensci/plotly')

class(area.fort)











