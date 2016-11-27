# library(jsonlite)
# jsonFile <- "./data/census block 2010.json"
# jsonFile <-"./data/query.txt"
# geojson <- jsonlite::fromJSON(jsonFile)
# library("leaflet")
# leaflet() %>% 
#   addTiles() %>%
#   setView(lng = -98.583, lat = 39.833, zoom = 3) %>% 
#   addGeoJSON(geojson)
# ? addGeoJSON(geojson)
# 
# 
# 
# df_block = data.frame(fromJSON(jsonFile))
# str(df_block)
# 
# #install.packages("rgdal", type = "source")
# library(rgdal)
# "GeoJSON" %in% ogrDrivers()$name
# map = readOGR(jsonFile, "OGRGeoJSON")
# plot (map)
# 
# 
# library(rgdal)
# data.shape<-readOGR(dsn="./data/nybb_16d")
# plot(data.shape)
# 
# library(rgdal)
# data.shape<-readOGR(dsn="./data/nybbwi_16d")
# plot(data.shape)
# 
# library(rgdal)
# data.shape<-readOGR(dsn="./data/nycc_16d")
# plot(data.shape)
# 
# library(rgdal)
# data.shape<-readOGR(dsn="./data/nycd_16d")
# plot(data.shape)
# 
# # Convertir un shp en json pour utiliser dans le leaflet
# # http://www.weblog.eliaz.fr/article120.html
# # sudo apt install gdal-bin
# # ogrinfo -al -so ./data/nycd_16d/nycd.shp 
# # ogr2ogr -t_srs EPSG:4326 -f geoJSON -lco COORDINATE_PRECISION=12  ./data/nycd_16d/nycd.json ./data/nycd_16d/nycd.shp 
# library("leaflet")
# jsonFile <- "./data/nycd_16d/nycd.json"
# leaflet() %>% 
#   addTiles() %>%
#   setView(lng = -98.583, lat = 39.833, zoom = 3) %>% 
#   addGeoJSON(jsonFile)
# 
# 
# leaflet() %>% 
#   addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png", attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
#   setView(-73.97125, 40.78306, zoom=10) 
# 
# library(rgdal)
# "GeoJSON" %in% ogrDrivers()$name
# map = readOGR(jsonFile, "OGRGeoJSON")
# plot (map)
# head(map)
# str(map)
# 
# 
# 
# library(leafletR)
# ?leaflet
# # store data in GeoJSON file (just a subset here)
# q.dat <- jsonFile
# # make style based on quake magnitude
# #q.style <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5),     style.val=rev(heat.colors(5)), leg="Richter Magnitude", fill.alpha=0.7, rad=8)
# # create map
# q.map <- leaflet(data=q.dat, dest="./data/nycd_16d", title="NY Boundaries",base.map="osm" , popup="*")
# q.map <- leaflet(data=q.dat, dest=tempdir(), title="NY Boundaries",base.map="positron" ,  popup="*")
# # view map in browser
# browseURL(q.map)
# q.dat

##############################
## now with the census file
##############################
jsonFile <- "./data/census block 2010.json"
q.dat <- jsonFile
q.map <- leaflet(data=q.dat, dest=tempdir(), title="NY Census",base.map="positron" ,  popup="*")
# view map in browser
browseURL(q.map)
#=> not intersting - staten is. only
##############################
## now with the census file
##############################
jsonFile <-"./data/query.txt"
q.dat <- jsonFile
q.map <- leaflet(data=q.dat, dest=tempdir(), title="NY Census",base.map="positron" ,  popup="*")
# view map in browser
browseURL(q.map)
#=> not intersting - staten is. only




##############################
## now with the nybb_16d files
##############################
direct<-"./data/nybb_16d"
filshp <- "nybb.shp"
filjson <- "nybb.json"
## 1 - convert shp into json
# sudo apt install gdal-bin
cmd <- paste(direct,"/",filshp,sep="" )
#cmd
system(command= paste ("ogrinfo -al -so " , cmd))
system (command = paste ("ogr2ogr -t_srs EPSG:4326 -f geoJSON -lco COORDINATE_PRECISION=12 ", direct, "/",filjson," ", cmd, sep=""))

## 2 - plot json on a leaflet
library(leafletR)
# store data in GeoJSON file (just a subset here)
q.dat <- paste(direct,"/",filjson, sep = "")
# make style based on quake magnitude
#q.style <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5),     style.val=rev(heat.colors(5)), leg="Richter Magnitude", fill.alpha=0.7, rad=8)
# create map
q.map <- leaflet(data=q.dat, dest=tempdir(), title=paste (direct, "- NY Boundaries"),base.map="positron" ,  popup="*")
# view map in browser
browseURL(q.map)

#=> ok , but only the boroughs of NYC (QU / MA / SI /  BX / BK)

##############
##############################
## now with the data/nybbwi_16d files
##############################
direct<-"./data/nybbwi_16d"
filshp <- "nybbwi.shp"
filjson <- "nybbwi.json"
## 1 - convert shp into json
# sudo apt install gdal-bin
cmd <- paste(direct,"/",filshp,sep="" )
#cmd
system(command= paste ("ogrinfo -al -so " , cmd))
system (command = paste ("ogr2ogr -t_srs EPSG:4326 -f geoJSON -lco COORDINATE_PRECISION=12 ", direct, "/",filjson," ", cmd, sep=""))

## 2 - plot json on a leaflet
library(leafletR)
# store data in GeoJSON file (just a subset here)
q.dat <- paste(direct,"/",filjson, sep = "")
# make style based on quake magnitude
#q.style <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5),     style.val=rev(heat.colors(5)), leg="Richter Magnitude", fill.alpha=0.7, rad=8)
# create map
q.map <- leaflet(data=q.dat, dest=tempdir(), title=paste (direct, "- NY Boundaries"),base.map="positron" ,  popup="*")
# view map in browser
browseURL(q.map)

#=> ok , but only the boroughs + Water of NYC (QU / MA / SI /  BX / BK)



##############################
## now with the ./data/nycc_16d files
##############################
direct<-"./data/nycc_16d"
filshp <- "nycc.shp"
filjson <- "nycc.json"
## 1 - convert shp into json
# sudo apt install gdal-bin
cmd <- paste(direct,"/",filshp,sep="" )
#cmd
system(command= paste ("ogrinfo -al -so " , cmd))
system (command = paste ("ogr2ogr -t_srs EPSG:4326 -f geoJSON -lco COORDINATE_PRECISION=12 ", direct, "/",filjson," ", cmd, sep=""))

## 2 - plot json on a leaflet
library(leafletR)
# store data in GeoJSON file (just a subset here)
q.dat <- paste(direct,"/",filjson, sep = "")
# make style based on quake magnitude
#q.style <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5),     style.val=rev(heat.colors(5)), leg="Richter Magnitude", fill.alpha=0.7, rad=8)
# create map
q.map <- leaflet(data=q.dat, dest=tempdir(), title=paste (direct, "- NY Boundaries"),base.map="positron" ,  popup="*")
# view map in browser
browseURL(q.map)

#=> ok , but only the districts (around 50)


#############################
## now with the ./data/nycd_16d files
##############################
direct<-"./data/nycd_16d"
filshp <- "nycd.shp"
filjson <- "nycd.json"
## 1 - convert shp into json
# sudo apt install gdal-bin
cmd <- paste(direct,"/",filshp,sep="" )
#cmd
system(command= paste ("ogrinfo -al -so " , cmd))
system (command = paste ("ogr2ogr -t_srs EPSG:4326 -f geoJSON -lco COORDINATE_PRECISION=12 ", direct, "/",filjson," ", cmd, sep=""))

## 2 - plot json on a leaflet
library(leafletR)
# store data in GeoJSON file (just a subset here)
q.dat <- paste(direct,"/",filjson, sep = "")
# make style based on quake magnitude
#q.style <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5),     style.val=rev(heat.colors(5)), leg="Richter Magnitude", fill.alpha=0.7, rad=8)
# create map
q.map <- leaflet(data=q.dat, dest=tempdir(), title=paste (direct, "- NY Boundaries"),base.map="positron" ,  popup="*")
# view map in browser
browseURL(q.map)

#=> ok , with the districts

###########################


##############################
## now with the neighboorhoods.json file
##############################
jsonFile <- "./data/neighboorhoods.json"
q.dat <- jsonFile
q.map <- leaflet(data=q.dat, dest=tempdir(), title="NY Neighboor",base.map="positron" ,  popup="*")
# view map in browser
browseURL(q.map)
#=> not intersting - points only


##############################
## now with the sidewalkcafe.json file
##############################
jsonFile <- "./data/sidewalkcafe.json"
q.dat <- jsonFile
q.map <- leaflet(data=q.dat, dest=tempdir(), title="NY Neighboor",base.map="positron" ,  popup="*")
# view map in browser
browseURL(q.map)
#=> not intersting - very partial
