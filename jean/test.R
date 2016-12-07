
#test nyc mapzen

library(geojsonio)
out <- geojson_read("jean/new-york_new-york_osm_point.shp"
                    ,method="local"
                    ,parse = TRUE)

df_features <- out[['features']]
df_amenities <- cbind(df_features$properties, df_features$geometry$type, df_features$geometry$coordinates)

lat=numeric(length(df_amenities$osm_id))
lon = numeric(length(df_amenities$osm_id))
type = numeric(length(df_amenities$osm_id))
name = numeric(length(df_amenities$osm_id))
df_test=cbind(type,name,lat,lon)

for (i in 1:dim(df_amenities)[1]){
  test <- as.data.frame(df_amenities$coordinates[i])
  df_test[i,"lon"]=test[1,]
  df_test[i,"lat"]=test[2,]
  df_test[i,"type"]=df_amenities$amenity[i]
  df_test[i,"name"]=df_amenities$name[i]
  
}


df_test2 <- as.data.frame(df_test)

df_test2$lon <-as.numeric(as.character(df_test2$lon))
df_test2$lat <-as.numeric(as.character(df_test2$lat))

require("dplyr")

#Amenities
target <- c("restaurant", "fast_food", "bar", "cafe","pharmacy","bank","pub","marketplace","ice_cream","food_court","nightclub","theatre")
df_test3 <- filter(df_test2, type %in% target)

#CitiBike stations (400 seulement)
#df_test5 <- filter(df_test2, type == "bicycle_rental")


df_test4 <- subset(df_test3, lon >= -74.0513 & lon < -73.8298 & lat >= 40.6788 & lat < 40.8847)


#leaflet
library(leaflet)
m <- leaflet(data = df_test4)  %>% addTiles() %>% addCircleMarkers(~lon, ~lat,color="red",popup = ~as.character(name),clusterOptions = markerClusterOptions())
m