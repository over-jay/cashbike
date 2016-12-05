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



# Lower Manhattan Retailers
df_retailers <- read.csv("Lower_Manhattan_Retailers.csv", sep = ",", header = TRUE)
df_retailers <- df_retailers[-1,] #suppression de la seconde ligne (répétition du nom des champs)
df_retailers$address <- paste (as.character(df_retailers$CnAdrPrf_Addrline1),as.character(df_retailers$CnAdrPrf_City), as.character(df_retailers$CnAdrPrf_State), sep = ",", collapse = NULL)
df_retailers$coodirnates <- geocode(df_retailers$address)
df_retailers2 <- cbind(df_retailers,df_retailers$coodirnates)
df_retailers2 <- df_retailers2[,c(-11)]

df_tmp3<-df_retailers2[,c(1,8,11,12)]
names(df_tmp3)[names(df_tmp3)=="CnBio_Org_Name"] <- "name"
names(df_tmp3)[names(df_tmp3)=="coordinates.lat"] <- "lat"
names(df_tmp3)[names(df_tmp3)=="coordinates.lon"] <- "lon"
names(df_tmp3)[names(df_tmp3)=="Primary"] <- "type"



#Map
df_map <- rbind(df_tmp1,df_tmp2)
df_map$lat_num <- as.numeric(df_map$lat)
df_map$lon_num <- as.numeric(df_map$lon)
df_map <- df_map[,c(-3,-4)]
names(df_map)[names(df_map)=="lat_num"] <- "lat"
names(df_map)[names(df_map)=="lon_num"] <- "lon"
df_map2 <- subset(df_map, lon >= -74.0513 & lon < -73.8298 & lat >= 40.6788 & lat < 40.8847)

map <- get_map(location = c(lon=-74.011288,lat=40.706929), zoom = 11 ,source="google", maptype="roadmap")
ggmap(map) + geom_point(data = df_map, aes(x = df_map$lon , y = df_map$lat, colour = type), size=0.1)



df_map <- read.csv("MyData_031216.csv", sep = ",", header = TRUE)

#leaflet

library(leaflet)
pal <- colorFactor(c("navy", "red"), domain = c("restaurant", "station"))
m <- leaflet(data = df_map2)  %>% addTiles() %>% addCircleMarkers(~lon, ~lat,color = ~pal(type), popup = ~as.character(name),clusterOptions = markerClusterOptions())
m

#test nyc mapzen
library(geojsonio)
out <- geojson_read("new-york_new-york_buildings.geojson",parse = TRUE)
df_features <- out[['features']]




