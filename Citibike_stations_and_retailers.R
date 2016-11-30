library(jsonlite)
library(ggmap)
library(devtools)
library(ggplot2)


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


# Lower Manhattan Retailers
df_retailers <- read.csv("Lower_Manhattan_Retailers.csv", sep = ",", header = TRUE)

df_retailers <- df_retailers[-1,] #suppression de la seconde ligne (répétition du nom des champs)
summary(df_retailers)
head(df_retailers)
str(df_retailers)

df_retailers$address <- paste (as.character(df_retailers$CnAdrPrf_Addrline1),as.character(df_retailers$CnAdrPrf_City), as.character(df_retailers$CnAdrPrf_State), sep = ",", collapse = NULL)
df_retailers$coodirnates <- geocode(df_retailers$address)
df_retailers2 <- cbind(df_retailers,df_retailers$coodirnates)
df_retailers2 <- df_retailers2[,c(-11)]

df_tmp2<-df_retailers2[,c(1,8,11,12)]
names(df_tmp2)[names(df_tmp2)=="CnBio_Org_Name"] <- "name"
names(df_tmp2)[names(df_tmp2)=="coordinates.lat"] <- "lat"
names(df_tmp2)[names(df_tmp2)=="coordinates.lon"] <- "lon"
names(df_tmp2)[names(df_tmp2)=="Primary"] <- "type"

df_map <- rbind(df_tmp1,df_tmp2)

map <- get_map(location = c(lon=-74.011288,lat=40.706929), zoom = 15 ,source="google", maptype="roadmap")
ggmap(map) + geom_point(data = df_map, aes(x = df_map$lon , y = df_map$lat, colour = type ))