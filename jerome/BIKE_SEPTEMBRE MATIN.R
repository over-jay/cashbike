#Chargement des données

BIKE <- read.csv('./jerome/20160915-matin.csv', sep=";", head=TRUE, as.is=TRUE)

head(BIKE,5)
dim(BIKE)
summary(BIKE)
str(BIKE)
names(BIKE)

########################################################################
#Transformer les variables starttime et stoptime en format date et heure
########################################################################


# 
# vecteur <- do.call('rbind',strsplit(BIKE$starttime,' ',fixed=TRUE))
# vecteur2 <- do.call('rbind',strsplit(BIKE$stoptime,' ',fixed=TRUE))
# 
# BIKE$DATE_DEP <- as.Date(vecteur[,1], format = "%m/%d/%Y")
# BIKE$DATE_DEP
# BIKE$DATE_ARR <- as.Date(vecteur2[,1], format = "%m/%d/%Y")
# BIKE$DATE_ARR
# strptime("9/15/2016 07:50:38", "%m/%d/%Y %H:%M:%S")
# remplacé par les 2 ligne suivantes  

# datetime arrivée 
BIKE$DTTM_DEP<-as.POSIXct(strptime(BIKE$starttime,  "%m/%d/%Y %H:%M:%S"))
# date time depart
BIKE$DTTM_ARR<-as.POSIXct(strptime(BIKE$stoptime,  "%m/%d/%Y %H:%M:%S"))
#durée en minutes
BIKE$DURATION<-BIKE$DTTM_ARR-BIKE$DTTM_DEP



# # La classe POSIXlt d´ecrit les temps comme une liste dont les champs sont 
# # les secondes, minutes, heures, jour, mois ,annee (depuis 1900)....
# ?as.POSIXlt
# str(vecteur[,2])
# 
# BIKE$HEURE_DEP <- as.POSIXlt(vecteur[,2], format = "%H:%M:%S")
#   #Separe chaque composant en une valeur
# str(unclass(BIKE$HEURE_DEP))
#   #Garde les heures, minutes et secondes
# BIKE$HEURE_DEP_hour <-BIKE$HEURE_DEP$hour
# BIKE$HEURE_DEP_min <-BIKE$HEURE_DEP$min
# BIKE$HEURE_DEP_sec <-BIKE$HEURE_DEP$sec
# 
# str(BIKE)


########################################################################
# Compter le nb de départs par station
########################################################################

install.packages("data.table")
library("data.table")
install.packages('dplyr')
library('dplyr')

tableau<-table(BIKE$start.station.id,BIKE$end.station.id) 
#  tableau
#  dim(tableau)
#  print(tableau)
#  tail(tableau)

vecteur<-tableau
dim(vecteur)<- c( 577*571,1)
 # vecteur
 # str(vecteur)
vecteur<-as.numeric(vecteur)
 # summary(vecteur)

# lister les valeurs sup à 10
head(sort(vecteur, decreasing=TRUE),34)


trowsum<-rowSums(tableau)
tcolsum<-colSums(tableau)

# top 34 des stations d'arrivée
head(sort(tcolsum, decreasing=TRUE),34)
# top 34 des stations de départ
head(sort(trowsum, decreasing=TRUE),34)

# top 10 des stations d'arrivée - DPLYR
  BIKE %>% group_by (start.station.id) %>% summarize(n=n()) %>% arrange (desc(n))
# top 10 des stations de départ- DPLYR
  BIKE %>% group_by (end.station.id) %>% summarize(n=n()) %>% arrange (desc(n))
  
# top X des couple depart-arrivées - DPLYR
  trajet<-  BIKE %>%group_by (start.station.id, end.station.id) %>% summarize(n=n())   %>% arrange (desc(n))
trajet

dim(trajet)
# renverse les trajets pour que l'aller et le retour compte autant
trajet2<-trajet[,c(2,1,3)]
trajetall <- rbind(trajet,trajet2)

# quelles station on le plus de deport + arrivée
  trajetall<-trajetall[,-2,]
summary(trajetall)

trajetsum<- aggregate(x= trajetall$n, by= list((trajetall$start.station.id))
                                             , FUN = sum)  

# pair de trajets classés par nombre de transport A ou R
trajetsum<- trajetsum [rev(order(trajetsum$x)),]
# plus que 200 par exemple (il y en a 50)
trajetsum[trajetsum$x> 200,1]
# donc 50 stations qui on plus de 200 depart et arrivées
tops<-trajetsum[trajetsum$x> 200,1]



