require(ggmap)

# Lower Manhattan Retailers
df_retailers <- read.csv("Lower_Manhattan_Retailers.csv", sep = ",", header = TRUE)

df_retailers <- df_retailers[-1,] #suppression de la seconde ligne (répétition du nom des champs)
summary(df_retailers)
head(df_retailers)
str(df_retailers)

df_retailers$address <- paste (as.character(df_retailers$CnAdrPrf_Addrline1),as.character(df_retailers$CnAdrPrf_City), as.character(df_retailers$CnAdrPrf_State), sep = ",", collapse = NULL)
  
df_retailers$coodirnates <- geocode(df_retailers$address)

map <- get_map(location = c(lon=-74.011288,lat=40.706929), zoom = 15 ,source="osm", maptype="roadtype")
ggmap(map) + geom_point(data = df_retailers, aes(x = coodirnates$lon , y = coodirnates$lat, colour = Primary))



# Violations NYC Restaurant Health Violations - package(mdsr)
require(mdsr)
data(Violations)
str(Violations)
df_restaurants <- aggregate(x = Violations$camis, 
                                 by = list(camis = Violations$camis,dba = Violations$dba, boro = Violations$boro, building = Violations$building, street = Violations$street, zipcode = Violations$zipcode, phone = Violations$phone), 
                                 FUN = length)
df_restaurants$address <- paste (as.character(df_restaurants$building),df_restaurants$street, "NY", sep = ",", collapse = NULL)


if (!require("devtools")) install.packages("devtools")
library(devtools)
library(httr)
set_config(use_proxy(url="proxy-sgt.si.socgen", port=8080, username="user",password="password"))
devtools::install_github("hrbrmstr/nominatim")

library(nominatim)
df_restaurants$coordinates <- osm_geocode(df_restaurants$address,key='kUHeoed2NVX83ICyejLstM2AF7Wh0GS8')





