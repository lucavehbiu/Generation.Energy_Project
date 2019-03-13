library(leaflet)
library(tmap)
library(plotly)


#mapabox token
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoibHVjYXZlaGJpdSIsImEiOiJjanE1NHMzZmQyMHlsNDlzYjFkdmhhenhhIn0.WskqGyJwMpIEgWtM80hhpg')

#register the google API and get NL map
register_google(key = "AIzaSyCD0wS1CcFzcL4t-0Vh0rQQhHoG_t7cqeY")


#shapefiles with OGR package
area <- readOGR(dsn = ".", layer = "RES_regios_2019") %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84"))


#Attach long/lat to fortified shapefile
load("fortify.R") #function
FortifyShapeFile(area) -> area.fort

#join with all base data (on Gemeentena) to attach demand to long/lat
area.fortt <- plyr:: join(area.fort, final_data, by="Gemeentena")


#ALternative way, much faster but with geometries instead of long/lat
##st way of reading the shapefile (with geometries)
st_read("RES_regios_2019.shp") %>% 
  st_transform("+proj=longlat +datum=WGS84")-> new

##merge sf object with the workhorse (add the energy demand numbers)
new2 <- merge(new, final_data, all = T)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#map of the whole netherlands
nl_center <- as.numeric(geocode("Netherlands"))
NL_map <- ggmap(get_googlemap(center = nl_center, scale = 2, zoom = 8), extent = "normal")

##map of Utrecht
Utr_map = map = get_map(location = "Boxtel", zoom = 13, key = key)


area.fortt <- repair_names(area.fortt)

'#cool thing with density
ggmap(Utr_map, extent = "device") + geom_density2d(data = boxtel_data, aes(x = long, y = lat), size = 0.3) + 
  stat_density2d(data = boxtel_data, 
                 aes(x = pot_tj, y = long, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``

#boxtel shapefiles (same method but for one muncipality)
  boxtel.area <- readOGR(dsn = ".", layer = "zon_dak_pot1") %>% 
    spTransform(CRS("+proj=longlat +datum=WGS84"))
  
  #putting long and lat
  FortifyShapeFile(boxtel.area) -> boxtel_data
  
  #st way of reading the shapefile (with geometries)
  st_read("zon_dak_pot1.shp") %>% 
    st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")-> new_boxtel

  

  #reduce with mapshaper and attach long/lat
  rmapshaper::ms_simplify(boxtel.area) -> test
  
  test$id <- rownames(test@data)
  boxtel.points <- fortify(test, coords="id")
  boxtel_data <- plyr:: join(boxtel.points, test@data, by="id")
  
  #reduce with mapshaper and attach long/lat
  rmapshaper::ms_simplify(boxtel.area) -> simplified
  
  FortifyShapeFile(simplified) -> simplified
 
 


