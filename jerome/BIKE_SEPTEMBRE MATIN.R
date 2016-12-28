#Chargement des données

BIKE <- read.csv('20160915-matin.csv', sep=";", head=TRUE, as.is=TRUE)

head(BIKE,5)
dim(BIKE)
summary(BIKE)
str(BIKE)
names(BIKE)

########################################################################
#Transformer les variables starttime et stoptime en format date et heure
########################################################################

vecteur <- do.call('rbind',strsplit(BIKE$starttime,' ',fixed=TRUE))
BIKE$DATE_DEP <- as.Date(vecteur[,1], format = "%m/%d/%Y")
BIKE$DATE_DEP

# La classe POSIXlt d´ecrit les temps comme une liste dont les champs sont 
# les secondes, minutes, heures, jour, mois ,annee (depuis 1900)....
?as.POSIXlt
str(vecteur[,2])

BIKE$HEURE_DEP <- as.POSIXlt(vecteur[,2], format = "%H:%M:%S")
  #Separe chaque composant en une valeur
str(unclass(BIKE$HEURE_DEP))
  #Garde les heures, minutes et secondes
BIKE$HEURE_DEP_hour <-BIKE$HEURE_DEP$hour
BIKE$HEURE_DEP_min <-BIKE$HEURE_DEP$min
BIKE$HEURE_DEP_sec <-BIKE$HEURE_DEP$sec

str(BIKE)


########################################################################
# Compter le nb de départs par station
########################################################################

install.packages("data.table")
library("data.table")
install.packages('dplyr')
library('dplyr')

tableau<-table(BIKE$start.station.id,BIKE$end.station.id) 
print(tableau)

filter(BIKE %>% count(start.station.id))
filter(BIKE %>% count(end.station.id))
trajet<-filter(BIKE %>% count(start.station.id, end.station.id))
