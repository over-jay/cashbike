require(ggmap)
library(jsonlite)



# Stations location

json_file <- "./data/stations.json"
df_data  <- fromJSON(json_file, flatten=TRUE)
# str(df_data)
# head(df_data)
df_data<-df_data[-1] # suppression de la timestamp exec

summary(df_data)
str(df_data)
head(df_data)
str(df_data$stationBeanList)
summary(df_data$stationBeanList)
df_stations <- df_data$stationBeanList
head(df_stations)
df_stations<-df_stations[,c(1,2,5,6)]

center <- c((max(df_stations[,4]) + min(df_stations[,4]) )/2,
(max(df_stations[,3]) + min(df_stations[,3]) )/2)

range<-c(max(df_stations[,4]),min(df_stations[,3]),
  max(df_stations[,3]),min(df_stations[,4]))

map <- get_map(location = center , zoom = 10,source="osm", maptype="roadtype")
ggmap(map) + geom_point(data = df_stations, aes(x = longitude , y = latitude))

install.packages("leaflet")
require (leaflet)

ml<- leaflet() %>% addTiles() %>%  fitBounds(range[1],range[2],range[4],range[3])
?addMarkers(clusterOptions = markerClusterOptions())

ml<- addCircles(ml, data = df_stations, lat = ~ latitude, lng = ~ longitude, popup = paste(df_stations$id, " - ", df_stations$stationName), clusterOptions = markerClusterOptions())
ml<- addMarkers(ml, data = df_stations, lat = ~ latitude, lng = ~ longitude, popup = paste(df_stations$id, " - ", df_stations$stationName), clusterOptions = markerClusterOptions())

ml

?addCircles()
?addTiles()
# Violations NYC Restaurant Health Violations - package(mdsr)
# require(mdsr)
# data(Violations)
# str(Violations)
# df_restaurants <- aggregate(x = Violations$camis, 
#                                  by = list(camis = Violations$camis,dba = Violations$dba, boro = Violations$boro, building = Violations$building, street = Violations$street, zipcode = Violations$zipcode, phone = Violations$phone), 
#                                  FUN = length)
# df_restaurants$address <- paste (as.character(df_restaurants$building),df_restaurants$street, "NY", sep = ",", collapse = NULL)
# 
# 
# if (!require("devtools")) install.packages("devtools")
# library(devtools)
# library(httr)
# set_config(use_proxy(url="proxy-sgt.si.socgen", port=8080, username="user",password="password"))
# devtools::install_github("hrbrmstr/nominatim")
# 
# library(nominatim)
# df_restaurants$coordinates <- osm_geocode(df_restaurants$address,key='kUHeoed2NVX83ICyejLstM2AF7Wh0GS8')
# 




