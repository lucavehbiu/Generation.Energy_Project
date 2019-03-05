library(leaflet)
library(tmap)
library(plotly)

#load the final meerged dataset for map
load("merged_map.RDA")

#merge sf object with the workhorse
new2 <- merge(new, final_data, all = T)

heat <- new2 %>% filter(AO50 == "e_won50AO_tj")

plot(heat["prj30AO"]) 

#shapefiles with OGR package
area <- readOGR(dsn = ".", layer = "RES_regios_2019") %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84"))

#st way of reading the shapefile (with geometries)
st_read("RES_regios_2019.shp") %>% 
  st_transform("+proj=longlat +datum=WGS84")-> new

#recode den haag
new$Gemeentena <- dplyr:: recode(new$Gemeentena, "'s-Gravenhage" = 'Den Haag')


#mapabox token
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoibHVjYXZlaGJpdSIsImEiOiJjanE1NHMzZmQyMHlsNDlzYjFkdmhhenhhIn0.WskqGyJwMpIEgWtM80hhpg')

#register the google API and get NL map
register_google(key = "AIzaSyCD0wS1CcFzcL4t-0Vh0rQQhHoG_t7cqeY")

#merge with area pints 
#(putting regions and geementes in the data of the shapefile)
area@data$id <- rownames(area@data)
area.points <- fortify(area, coords="id")
area.fort <- plyr:: join(area.points, area@data, by="id")

#join with all the projections data
area.fortt <- plyr:: join(area.fort, final_data, by="Gemeentena")


#map of the whole netherlands
nl_center <- as.numeric(geocode("Netherlands"))
NL_map <- ggmap(get_googlemap(center = nl_center, scale = 2, zoom = 8), extent = "normal")

##another method
sfMap = map = get_map(location = "Netherlands", zoom = 8, key = key)

##map of Utrecht
Utr_map = map = get_map(location = "Boxtel", zoom = 13, key = key)


area.fortt <- repair_names(area.fortt)

ggmap(Utr_map, extent = "device") + geom_density2d(data = boxtel_data, aes(x = long, y = lat), size = 0.3) + 
  stat_density2d(data = boxtel_data, 
                 aes(x = pot_tj, y = long, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

#projections bigger than whatever
  'ggmap(sfMap) +
    geom_polygon(data = area.fortt %>% filter(AO40 == "w_rest40AO_tj"), 
                 aes(fill = prj15NB, x = long, y = lat, group = group),
                 alpha = 0.5,
                 color = "black",
                 size = 0.2)'
   
#testing shit
'new <- merge(new, final_data, by = Gemeentena)
  
plot(new["prj15AO"]) 
  
  plot_ly(new)
  
  plot(st_geometry(new))
  
  ggplot() + geom_sf(data = new %>% filter(Res_regio == "Metropoolregio Eindhoven"),
                     aes(fill = Gemeentena)) + guides(fill = FALSE) 
  
  ggplotly(l)
  
  
  
  area.fortt <- repair_names(area.fortt)
  
  
  plot_ly(new, split = ~Res_regio)'
  
  
  
#boxtel shapefiles
  boxtel.area <- readOGR(dsn = ".", layer = "zon_dak_pot1") %>% 
    spTransform(CRS("+proj=longlat +datum=WGS84"))
  
  boxtel.area$id <- rownames(boxtel.area@data)
  boxtel.points <- fortify(boxtel.area, coords="id")
  boxtel_data <- plyr:: join(boxtel.points, boxtel.area@data, by="id")
  
  Box_map = map = get_map(location = "Boxtel", zoom = 14, key = key)
  
  ggmap(Box_map) +
    geom_polygon(data = boxtel_data,
                 aes(fill = typedak,x = long, y = lat, group = group))
  
  plot(new_boxtel["pot_tj2014"])
  
  
  
  
  #st way of reading the shapefile (with geometries)
  st_read("zon_dak_pot1.shp") %>% 
    st_transform("+proj=longlat +datum=WGS84")-> new_boxtel

  
    
  
mapview::mapView(new_boxtel["pot_tj"])
    



