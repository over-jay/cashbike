#nyc mapzen
library(geojsonio)
out <- geojson_read("jean/new-york_new-york_osm_point.shp"
                    ,method="local"
                    ,parse = TRUE)

df_features <- out[['features']]
df_amenities <- cbind(df_features$properties, df_features$geometry$type, df_features$geometry)

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
df_test3 <- filter(df_test2, type%in%target)
df_test4 <- subset(df_test3, lon >= -74.0513 & lon < -73.8298 & lat >= 40.6788 & lat < 40.8847)


library(jsonlite)
#Citibike stations
station_data <- fromJSON("station_information.json") #A la racine!!!
lst_stations <- station_data[['data']]
df_stations <- as.data.frame(lst_stations)

df_tmp1<-df_stations[,c(2,4,5)]
df_tmp1$type="station"
names(df_tmp1)[names(df_tmp1)=="stations.name"] <- "name"
names(df_tmp1)[names(df_tmp1)=="stations.lat"] <- "lat"
names(df_tmp1)[names(df_tmp1)=="stations.lon"] <- "lon"
df_tmp1<-df_tmp1[,c(4,1,2,3)]
df_tmp1$category="station"

df_tmp2<-df_test4
df_tmp2$category="amenity"

df_map <- rbind(df_tmp1,df_tmp2)

library(leaflet)
pal <- colorFactor(c("navy", "red"), domain = c("amenity", "station"))
m <- leaflet(data = df_map)  %>% addTiles() %>% addCircleMarkers(~lon, ~lat,color = ~pal(category), popup = ~as.character(name),clusterOptions = markerClusterOptions()) %>% addLegend("bottomright",pal = pal, values = ~category)
m


