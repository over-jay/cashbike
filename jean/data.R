
######################################## NTA  ########################################

# Data source - https://data.cityofnewyork.us/City-Government/Neighborhood-Tabulation-Areas/cpf4-rkhq/data/

library(geojsonio)
out2 <- geojson_read("nta/geo_export_0954f06e-1e23-4e30-90e2-0267a442ce03.shp"
                     ,method="local"
                     ,parse = TRUE)

df_features2 <- out2[['features']]
df_boro <- cbind(df_features2$type,
                 df_features2$id,
                 df_features2$properties$ntacode,
                 df_features2$properties$ntaname,
                 df_features2$properties$boro_name,
                 df_features2$properties$shape_leng,
                 df_features2$properties$county_fip,
                 df_features2$properties$shape_area,
                 df_features2$properties$boro_code,
                 df_features2$geometry$type,
                 df_features2$geometry$coordinates)

df_boro2 <- as.data.frame.array(df_boro)

names(df_boro2)[names(df_boro2)=="V1"] <- "type"
names(df_boro2)[names(df_boro2)=="V2"] <- "id"
names(df_boro2)[names(df_boro2)=="V3"] <- "ntacode"
names(df_boro2)[names(df_boro2)=="V4"] <- "ntaname"   #Neighborhood Tabulation Area
names(df_boro2)[names(df_boro2)=="V5"] <- "boro_name"
names(df_boro2)[names(df_boro2)=="V6"] <- "shape_leng"
names(df_boro2)[names(df_boro2)=="V7"] <- "county_fip"
names(df_boro2)[names(df_boro2)=="V8"] <- "shape_area"
names(df_boro2)[names(df_boro2)=="V9"] <- "boro_code"
names(df_boro2)[names(df_boro2)=="V10"] <- "geo_type"
names(df_boro2)[names(df_boro2)=="V11"] <- "geo_coods"

nta <- df_boro2

rm(df_boro2, df_boro,df_features2,out2)


############################# CitiBike stations #####################################

# Data source - https://gbfs.citibikenyc.com/gbfs/en/station_information.json

library(jsonlite)
station_data <- fromJSON("station/station_information.json") #A la racine!!!
lst_stations <- station_data[['data']]
df_stations <- as.data.frame(lst_stations)

df_tmp1<-df_stations[,c(2,4,5)]
df_tmp1$type="station"
names(df_tmp1)[names(df_tmp1)=="stations.name"] <- "name"
names(df_tmp1)[names(df_tmp1)=="stations.lat"] <- "lat"
names(df_tmp1)[names(df_tmp1)=="stations.lon"] <- "lon"
df_tmp1<-df_tmp1[,c(4,1,2,3)]
df_tmp1$category="station"
station <- df_tmp1

rm(station_data,lst_stations,df_tmp1,df_stations)

############################# NYC MapZen data #######################################

# Data source - https://mapzen.com/data/metro-extracts/metro/new-york_new-york/ ()

library(geojsonio)
out <- geojson_read("mapzen/new-york_new-york_osm_point.shp"
                    ,method="local"
                    ,parse = TRUE)

df_features <- out[['features']]
df_osm_data <- cbind(df_features$properties, df_features$geometry$type, df_features$geometry)

lat=numeric(length(df_osm_data$osm_id))
lon = numeric(length(df_osm_data$osm_id))
type = numeric(length(df_osm_data$osm_id))
name = numeric(length(df_osm_data$osm_id))
df_test=cbind(type,name,lat,lon)

for (i in 1:dim(df_osm_data)[1]){
  test <- as.data.frame(df_osm_data$coordinates[i])
  df_test[i,"lon"]=test[1,]
  df_test[i,"lat"]=test[2,]
  df_test[i,"type"]=df_osm_data$amenity[i]
  df_test[i,"name"]=df_osm_data$name[i]
}

df_test2 <- as.data.frame(df_test)
df_test2$lon <-as.numeric(as.character(df_test2$lon))
df_test2$lat <-as.numeric(as.character(df_test2$lat))
df_test2<-df_test2[-which (is.na(df_test2),arr.ind = TRUE)[,1],] #on supprime les lignes avec NA
df_amenities <- df_test2

rm(out,df_features,lat,lon,type,name,test,df_test,df_test2,i)

require("dplyr")

#restaurant
target <- c("restaurant", "fast_food", "bar", "cafe","pub","ice_cream","food_court")
df_test3 <- filter(df_amenities, type%in%target)
restaurant <- subset(df_test3, lon >= -74.1172 & lon < -73.9126 & lat >= 40.6533 & lat < 40.8141)
restaurant$category <- "restaurant"

#entertainment
target2 <- c("nightclub","theatre","cinema")
df_test4 <- filter(df_amenities, type%in%target2)
entertainment <- subset(df_test4, lon >= -74.1172 & lon < -73.9126 & lat >= 40.6533 & lat < 40.8141)
entertainment$category <- "entertainment"

rm(target,target2,df_test3,df_test4)

#shops
lat=numeric(length(df_osm_data$osm_id))
lon = numeric(length(df_osm_data$osm_id))
type = numeric(length(df_osm_data$osm_id))
name = numeric(length(df_osm_data$osm_id))
df_test5=cbind(type,name,lat,lon)

for (i in 1:dim(df_osm_data)[1]){
  test2 <- as.data.frame(df_osm_data$coordinates[i])
  df_test5[i,"lon"]=test2[1,]
  df_test5[i,"lat"]=test2[2,]
  df_test5[i,"type"]=df_osm_data$shop[i]
  df_test5[i,"name"]=df_osm_data$name[i]
}

df_test6 <- as.data.frame(df_test5)
df_test6$lon <-as.numeric(as.character(df_test6$lon))
df_test6$lat <-as.numeric(as.character(df_test6$lat))
df_test6<-df_test6[-which (is.na(df_test6),arr.ind = TRUE)[,1],] #on supprime les lignes avec NA
shop <- subset(df_test6, lon >= -74.1172 & lon < -73.9126 & lat >= 40.6533 & lat < 40.8141)
shop$category <- "shop"

rm(df_test5,test2,df_test6,i,lat,lon,name,type)


##  Add NTA to clients ##


library(raster)
library(leaflet)
library(geojsonio)
library(rgdal)

nyc <- shapefile("nta/geo_export_0954f06e-1e23-4e30-90e2-0267a442ce03.shp")

#station
xy <- station[,c(4,3)]
spdf <- SpatialPointsDataFrame(coords = xy, data = station[,c(1,2,5)],proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

inside.zone <- !is.na(over(spdf, as(nyc, "SpatialPolygons")))
spdf$ntaname <- over(spdf, nyc)$ntaname
spdf$ntacode <- over(spdf, nyc)$ntacode

df_tmp <- as.data.frame(spdf)

df_tmp2 <- merge(x = df_tmp, y = nta, by = "ntacode", all.x = TRUE)
station <- df_tmp2[,c("category","type.x","name","lon","lat","ntacode","ntaname.x","boro_code","boro_name")]
names(station)[names(station)=="type.x"] <- "type"
names(station)[names(station)=="ntaname.x"] <- "nta_name"
names(station)[names(station)=="ntacode"] <- "nta_code"


#shop
xy <- shop[,c(4,3)]
spdf <- SpatialPointsDataFrame(coords = xy, data = shop[,c(1,2,5)],proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

inside.zone <- !is.na(over(spdf, as(nyc, "SpatialPolygons")))
spdf$ntaname <- over(spdf, nyc)$ntaname
spdf$ntacode <- over(spdf, nyc)$ntacode

df_tmp <- as.data.frame(spdf)

df_tmp2 <- merge(x = df_tmp, y = nta, by = "ntacode", all.x = TRUE)
shop <- df_tmp2[,c("category","type.x","name","lon","lat","ntacode","ntaname.x","boro_code","boro_name")]
names(shop)[names(shop)=="type.x"] <- "type"
names(shop)[names(shop)=="ntaname.x"] <- "nta_name"
names(shop)[names(shop)=="ntacode"] <- "nta_code"



#restaurant
xy <- restaurant[,c(4,3)]
spdf <- SpatialPointsDataFrame(coords = xy, data = restaurant[,c(1,2,5)],proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

inside.zone <- !is.na(over(spdf, as(nyc, "SpatialPolygons")))
spdf$ntaname <- over(spdf, nyc)$ntaname
spdf$ntacode <- over(spdf, nyc)$ntacode

df_tmp <- as.data.frame(spdf)

df_tmp2 <- merge(x = df_tmp, y = nta, by = "ntacode", all.x = TRUE)
restaurant <- df_tmp2[,c("category","type.x","name","lon","lat","ntacode","ntaname.x","boro_code","boro_name")]
names(restaurant)[names(restaurant)=="type.x"] <- "type"
names(restaurant)[names(restaurant)=="ntaname.x"] <- "nta_name"
names(restaurant)[names(restaurant)=="ntacode"] <- "nta_code"


#entertainment
xy <- entertainment[,c(4,3)]
spdf <- SpatialPointsDataFrame(coords = xy, data = entertainment[,c(1,2,5)],proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

inside.zone <- !is.na(over(spdf, as(nyc, "SpatialPolygons")))
spdf$ntaname <- over(spdf, nyc)$ntaname
spdf$ntacode <- over(spdf, nyc)$ntacode

df_tmp <- as.data.frame(spdf)

df_tmp2 <- merge(x = df_tmp, y = nta, by = "ntacode", all.x = TRUE)
entertainment <- df_tmp2[,c("category","type.x","name","lon","lat","ntacode","ntaname.x","boro_code","boro_name")]
names(entertainment)[names(entertainment)=="type.x"] <- "type"
names(entertainment)[names(entertainment)=="ntaname.x"] <- "nta_name"
names(entertainment)[names(entertainment)=="ntacode"] <- "nta_code"


rm(xy,df_tmp,df_tmp2,inside.zone,spdf)


######################## Station la plus proche ####################

library(sp)
library(SearchTrees)
library(leaflet)

## Entertainment
A <- SpatialPoints(cbind(x=station$lat, y=station$lon))
B <- SpatialPoints(cbind(x=entertainment$lat, y=entertainment$lon))

## Find indices of the nearest point in A to each of the points in B
tree <- createTree(coordinates(A))
inds <- knnLookup(tree, newdat=coordinates(B), k=1)


for (i in 1:dim(entertainment)[1]){
  
  entertainment[i,"station_name"] <- station[inds[i,],]$name
  entertainment[i,"station_lat"] <- station[inds[i,],]$lat
  entertainment[i,"station_lon"] <- station[inds[i,],]$lon
}


## Shop
A <- SpatialPoints(cbind(x=station$lat, y=station$lon))
B <- SpatialPoints(cbind(x=shop$lat, y=shop$lon))

## Find indices of the nearest point in A to each of the points in B
tree <- createTree(coordinates(A))
inds <- knnLookup(tree, newdat=coordinates(B), k=1)


for (i in 1:dim(shop)[1]){
  
  shop[i,"station_name"] <- station[inds[i,],]$name
  shop[i,"station_lat"] <- station[inds[i,],]$lat
  shop[i,"station_lon"] <- station[inds[i,],]$lon
}


## Restaurant
A <- SpatialPoints(cbind(x=station$lat, y=station$lon))
B <- SpatialPoints(cbind(x=restaurant$lat, y=restaurant$lon))

## Find indices of the nearest point in A to each of the points in B
tree <- createTree(coordinates(A))
inds <- knnLookup(tree, newdat=coordinates(B), k=1)


for (i in 1:dim(restaurant)[1]){
  
  restaurant[i,"station_name"] <- station[inds[i,],]$name
  restaurant[i,"station_lat"] <- station[inds[i,],]$lat
  restaurant[i,"station_lon"] <- station[inds[i,],]$lon
}

rm(A,B,tree,inds,i)











