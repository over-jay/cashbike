library(jsonlite)
library(ggmap)
library(devtools)
library(ggplot2)
library(httr)
library(mdsr)

#set_config(use_proxy(url="https://proxy-sgt.si.socgen", port=8080, username="user",password="password"))

myfunc <- function(address_name) {
  base_url <- "https://search.mapzen.com/v1/search"
  res <- GET(base_url, query = list(text = address_name, api_key = "mapzen-SZRHcS5"))
  json <- content(res, as = "text")
  results <- fromJSON(json)$features
  #mapzen-4wbt2Vi
  tmp <- results$geometry
  tmp2 <- tmp$coordinates
  tmp3 <- unlist(tmp2)
  lat <- tmp3[1]
  lon <- tmp3[2]
  df=as.data.frame(cbind(lat,lon))
  return(df)
}


#Citibike stations
station_data <- fromJSON("station_information.json")
lst_stations <- station_data[['data']]
df_stations <- as.data.frame(lst_stations)

df_tmp1<-df_stations[,c(2,4,5)]
df_tmp1$type="station"
names(df_tmp1)[names(df_tmp1)=="stations.name"] <- "name"
names(df_tmp1)[names(df_tmp1)=="stations.lat"] <- "lat"
names(df_tmp1)[names(df_tmp1)=="stations.lon"] <- "lon"
df_tmp1<-df_tmp1[,c(4,1,2,3)]


#NYC restaurants
data(Violations)
df_restaurants <- aggregate(x = Violations$camis,
                            by = list(camis = Violations$camis,dba = Violations$dba, boro = Violations$boro, building = Violations$building, street = Violations$street, zipcode = Violations$zipcode, phone = Violations$phone),
                            FUN = length)

df_restaurants$address <- paste (as.character(df_restaurants$building),df_restaurants$street, "NY", sep = ",", collapse = NULL)
df_restaurants$lat=0
df_restaurants$lon=0

for (i in 1:dim(df_restaurants)[1]){
  res=myfunc(df_restaurants[i,"address"])
  df_restaurants[i,"lon"]=ifelse(nrow(res)==0,0,res[1])
  df_restaurants[i,"lat"]=ifelse(nrow(res)==0,0,res[2])
}

#suppression manuelle dans Excel des caractères "->" et des adresses à 0

df_tmp2<-df_restaurants[,c(2,10,11)]
df_tmp2$type="restaurant"
names(df_tmp2)[names(df_tmp2)=="dba"] <- "name"
df_tmp2<-df_tmp2[,c(4,1,3,2)]


#Map
df_map <- rbind(df_tmp1,df_tmp2)
df_map$lat_num <- as.numeric(df_map$lat)
df_map$lon_num <- as.numeric(df_map$lon)
df_map <- df_map[,c(-3,-4)]
names(df_map)[names(df_map)=="lat_num"] <- "lat"
names(df_map)[names(df_map)=="lon_num"] <- "lon"
df_map2 <- subset(df_map, lon >= -74.0513 & lon < -73.8298 & lat >= 40.6788 & lat < 40.8847)


#leaflet
library(leaflet)
pal <- colorFactor(c("navy", "red"), domain = c("restaurant", "station"))
m <- leaflet(data = df_map2)  %>% addTiles() %>% addCircleMarkers(~lon, ~lat,color = ~pal(type), popup = ~as.character(name),clusterOptions = markerClusterOptions()) %>% addLegend("bottomright",pal = pal, values = ~type)
m




