library(leaflet)

#Stations, shops, restaurants,entertainment places & NTA
map1 <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = nyc, stroke = TRUE, color = "#03F",weight = 1,opacity = 0.5, fill = TRUE,fillColor=rainbow(12,s=0.5,alpha=NULL),group = "Neighborhood Tabulation Area", popup = ~as.character(paste(boro_name," - ",ntacode))) %>%
  addCircleMarkers(data = shop,~lon, ~lat,color = "blue", popup = ~as.character(paste(name," - ",nta_code)), group = "Shops",clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  addCircleMarkers(data = station,~lon, ~lat,color = "red", popup = ~as.character(paste(name," - ",nta_code)), group = "CitiBike stations",clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  addCircleMarkers(data = restaurant,~lon, ~lat,color = "green", popup = ~as.character(paste(name," - ",nta_code)), group = "Restaurants",clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  addCircleMarkers(data = entertainment,~lon, ~lat,color = "yellow", popup = ~as.character(paste(name," - ",nta_code)), group = "Entertainment places",clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  addLayersControl(
    overlayGroups = c("Shops","CitiBike stations", "Restaurants","Entertainment places","Neighborhood Tabulation Area"),
    position = 'bottomright',
    options = layersControlOptions(collapsed = FALSE)
  )
map1


library(leaflet)
#Stations, shops, restaurants,entertainment places & NTA

map2 <- leaflet() %>%
  addTiles() %>%
  setView(-73.985130,40.758896,zoom = 9)%>%
  addPolygons(data = nyc, stroke = TRUE, color = "#03F",weight = 1,opacity = 0.5, fill = TRUE,fillColor=rainbow(12,s=0.5,alpha=NULL),group = "Neighborhood Tabulation Area", popup = ~as.character(paste(boro_name," - ",ntacode))) %>%
  addCircleMarkers(data = station,~lon, ~lat,color = "red", popup = ~as.character(paste(name," - ",nta_code)), group = "CitiBike stations",clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  addCircleMarkers(data = shop,~lon, ~lat,color = "blue", popup = ~as.character(paste(name," - ",nta_code)), group = "Shops",clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  addCircleMarkers(data = restaurant,~lon, ~lat,color = "green", popup = ~as.character(paste(name," - ",nta_code)), group = "Restaurants",clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  addCircleMarkers(data = entertainment,~lon, ~lat,color = "yellow", popup = ~as.character(paste(name," - ",nta_code)), group = "Entertainment places",clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  addLayersControl(
    overlayGroups = c("Neighborhood Tabulation Area","CitiBike stations","Shops", "Restaurants","Entertainment places","Shop/Station","Restaurant/Station","Entertainment places/Station"),
    position = 'bottomright',
    options = layersControlOptions(collapsed = TRUE))%>%
    hideGroup(c("Neighborhood Tabulation Area","Shops", "Restaurants","Entertainment places","Shop/Station","Restaurant/Station","Entertainment places/Station"))

for(i in 1:nrow(shop)){
  map2 <- addPolylines(map2, lat = as.numeric(shop[i, c(5,11)]),
                       lng = as.numeric(shop[i, c(4, 12)]),color = "blue", weight = 2, opacity = 1,group = "Shop/Station")
}

for(i in 1:nrow(restaurant)){
  map2 <- addPolylines(map2, lat = as.numeric(restaurant[i, c(5,11)]),
                       lng = as.numeric(restaurant[i, c(4, 12)]),color = "green", weight = 2, opacity = 1,group = "Restaurant/Station")
}

for(i in 1:nrow(entertainment)){
  map2 <- addPolylines(map2, lat = as.numeric(entertainment[i, c(5,11)]),
                       lng = as.numeric(entertainment[i, c(4, 12)]),color = "yellow", weight = 2, opacity = 1,group = "Entertainment places/Station")
}
map2
rm(i)

