#Chargement des données
# fichier des trajets
# on va prendre le 20/9/2016 and plus du 15/9/2016
# source de la donnée : https://s3.amazonaws.com/tripdata/201609-citibike-tripdata.zip

BIKE <- read.csv('./jerome/201609-citibike-tripdata.csv', sep=",", head=TRUE, as.is=TRUE)

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
if (!("data.table" %in% rownames(installed.packages()))) install.packages("data.table")
library("data.table")
if (!("dplyr" %in% rownames(installed.packages()))) install.packages("dplyr")
library('dplyr')

tableau<-table(BIKE$start.station.id,BIKE$end.station.id) 
#  tableau
#  dim(tableau)
#  print(tableau)
#  tail(tableau)

vecteur<-tableau
dim(tableau)[1] *   dim(tableau)[2]
dim(vecteur)<- c( dim(tableau)[1] *   dim(tableau)[2],1)
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
# summary(trajetall)

trajetsum<- aggregate(x= trajetall$n, by= list((trajetall$start.station.id))
                                             , FUN = sum)  

# pair de trajets classés par nombre de transport A ou R
trajetsum<- trajetsum [rev(order(trajetsum$x)),]
# plus que 10.000 / 200 par exemple (il y en a 107 / 50)
trajetsum[trajetsum$x> 10000,1]
# donc 107 / 50 stations qui on plus de 10.000  200 depart et arrivées
tops<-trajetsum[trajetsum$x> 10000,1]

#CJ.table.2 <- function(X,Y) {
#  eval(parse(text=paste0("setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],list(",paste0(unique(c(names(X),names(Y))),collapse=","),")][,k:=NULL]")))
#}

BK1<-data.table(BIKE)
BK2<-data.table(BIKE)
#setkey 
#res<-setkey(BK1[,c(k=1,.SD)],k)[BK2[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]
#BK1[BK2, on=c("birth.year","usertype","gender")]


rm(BK1)
rm(BK2)
rm(BKtemp1)
rm(BKtemp2)
rm(BKtemp)
rm(BU)
rm(BKsymo)
rm(BKsymoT)
rm(ssi)

#du lundi 19 au 23 septembre
BK1<-BIKE[  (strftime(BIKE$DTTM_DEP, format = "%e") == 20)  ,]
ind<- ( is.na(BK1$start.station.id)  |   BK1$start.station.id == "NA"  )
BK1 <-  BK1[!(ind),]
BK2 <-  BK1
BU<-BK1[,c("start.station.id","end.station.id")]
ind<- ( is.na(BU$start.station.id)  |   BU$start.station.id == "NA"  )
BU <-  BU[!(ind),]
BU<-unique(BU)



BKsym<-NULL
BKlen<-NULL

for ( i in 1:length(BU[,1]) )
{
  ssi<-BU[i,1]
  esi<-BU[i,2]
  if (ssi>esi)
    #on va ramasser les trajets de ssi à esi et de esi à ssi => pas besoin de prendre esi>ssi
  {
    #print (ssi, esi)
    #print (paste(i," - ", ssi, esi))
    symuser<-intersect(
      BK1[BK1$start.station.id==ssi & BK1$end.station.id==esi, 
          c("gender","birth.year","usertype") ] 
      ,
      BK2[BK2$start.station.id==esi & BK2$end.station.id==ssi, 
          c("gender","birth.year","usertype") ])
    if (length(symuser[,1]) !=0 & esi!=ssi ) {
      # ici on a ssi = start et esi = end avec une personne similaire (yob/gender/type)
      # pour chacun, il faudrait identifier le gender/yob/type
      # print (i)
      for (j in 1:length(symuser[,1]))
      {gender<-symuser[j,1]
      birth.year<-symuser[j,2]
      usertype<-symuser[j,3]
      # trajet de ssi à esi
      BKtemp1<- BK1[BK1$start.station.id==ssi 
                     & BK1$end.station.id == esi
                     & BK1$gender == gender
                     & BK1$usertype == usertype
                     & BK1$birth.year == birth.year,]
      # trajet de esi à ssi
      BKtemp2<- BK1[BK1$start.station.id==esi 
                     & BK1$end.station.id == ssi
                     & BK1$gender == gender
                     & BK1$usertype == usertype
                     & BK1$birth.year == birth.year,]
      names(BKtemp2)<-paste("2.",names(BKtemp2), sep="")
      
      
      # traiter le cas où on a 2 trajet retour ou 1 aller
     BKtemp<-merge(BKtemp1, BKtemp2, all = TRUE)
     # BKtemp<-cbind(BKtemp1[1,], BKtemp2[1,])
      BKtlen<-cbind(ssi,esi, usertype, gender, birth.year,length(BKtemp[,1]), length(BKtemp[,2]))
      if (length(BKtemp[,1])* length(BKtemp[,2])>1) BKlen <- rbind(BKlen, BKtlen)
      BKsym <- rbind(BKsym,(BKtemp))
      }
    }
  }
}

#?order
BKsymo<-BKsym[order(BKsym$gender, BKsym$birth.year, BKsym$usertype),]
  
#il y a des lignes N/A a la fin ... a étudier
BKsymo[,c("start.station.id", "end.station.id", "birth.year", "usertype", "gender", "DURATION") ]
ind<- ( is.na(BKsymo$start.station.id)  |   BKsymo$start.station.id == "NA"  )
BKsymo <-  BKsymo[!(ind),]

BKsymo[,c("start.station.id", "end.station.id", "birth.year", "usertype", "gender", "DURATION") ]

# 632*2 trajets symétriques - ou plus ... 

# avoir une durée minimale entre aller et retour
ind <- ( is.na(BKsymo[,"DTTM_DEP"]) | is.na(BKsymo[,"2.DTTM_DEP"]) ) 
sum(ind)
BKsymo <-  BKsymo[!(ind),]
worktime<- 3*60 #minutes
worktraj<-BKsymo[(BKsymo[,"DTTM_DEP"]- BKsymo[,"2.DTTM_DEP"]) > worktime,]

worktraj[,c("start.station.id", "end.station.id", "birth.year", "usertype", "gender", "DURATION", "2.DURATION") ]


# quel est le top 10 des stations in/out ?
# top 10 des stations de départ- DPLYR
worktraj %>% group_by (start.station.id) %>% summarize(n=n()) %>% arrange (desc(n)) %>% print
# top 10 des stations d'arrivée - DPLYR
worktraj %>% group_by (end.station.id) %>% summarize(n=n()) %>% arrange (desc(n)) %>% print
plot(  worktraj$start.station.id  ,   worktraj$end.station.id , col = worktraj$gender)
#  interessant = 0-500 parle à 0-500 puis 2000 puis 3000-3500

names(worktraj)[c(34,36)]<-c('DTTM_DEP2', 'DURATION2')
worktrajcount<-worktraj %>% group_by (start.station.id, start.station.name,start.station.latitude,start.station.longitude, end.station.id,
                       end.station.name,end.station.latitude,end.station.longitude,usertype,birth.year,gender,DTTM_DEP,
                       DURATION,DTTM_DEP2,DURATION2)%>% summarize(n=n()) %>% arrange (desc(n)) %>% print

worktrajcount[, c("start.station.id", "end.station.id", "birth.year", "usertype", "gender", "DURATION", "DURATION2","n")]

worktrajcount$start.station.id<-as.factor(x=worktrajcount$start.station.id)
worktrajcount$end.station.id<-as.factor(x=worktrajcount$end.station.id)
worktrajcount$usertype<-as.factor(x=worktrajcount$usertype)
worktrajcount$gender<-as.factor(x=worktrajcount$gender)
worktrajcount$DURATION<-as.numeric(x=worktrajcount$DURATION)
worktrajcount$DURATION2<-as.numeric(x=worktrajcount$DURATION2)

summary (worktrajcount)
plot(worktrajcount$birth.year, worktrajcount$DURATION)
points(worktrajcount$birth.year, worktrajcount$DURATION2, pch = 16, col = 2)
points(worktrajcount$birth.year, worktrajcount$DURATION, pch = 17, col = 1)

plot(worktrajcount$gender, worktrajcount$DURATION)
plot(worktrajcount$gender, worktrajcount$DURATION2)

plot(worktrajcount$start.station.id, worktrajcount$DURATION)
plot(worktrajcount$end.station.id, worktrajcount$DURATION2)

measure <-function (lat1, lon1, lat2, lon2)
{  # generally used geo measurement function
  R <- 6378.137; # Radius of earth in KM
  dLat <- lat2 * pi / 180 - lat1 * pi / 180;
  dLon <- lon2 * pi / 180 - lon1 * pi / 180;
  a <- sin(dLat/2) * sin(dLat/2) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dLon/2) * sin(dLon/2);
  c <- 2 * atan2(sqrt(a), sqrt(1-a));
  d <- R * c;
  return (d * 1000) # meters
}

# source : http://stackoverflow.com/questions/639695/how-to-convert-latitude-or-longitude-to-meters

worktrajcount$measure <- measure(worktrajcount$start.station.latitude, 
                                 worktrajcount$start.station.longitude,
                                 worktrajcount$end.station.latitude,
                                 worktrajcount$end.station.longitude)
worktrajcount$distL1<-abs (worktrajcount$start.station.latitude-worktrajcount$end.station.latitude) + abs(worktrajcount$start.station.longitude-worktrajcount$end.station.longitude)
worktrajcount$distL2<-sqrt((worktrajcount$start.station.latitude-worktrajcount$end.station.latitude)^2 + (worktrajcount$start.station.longitude-worktrajcount$end.station.longitude)^2)

# travail sur les vitesses comparées par genre en L1
plot (worktrajcount$distL1 , worktrajcount$DURATION, pch = 20, cex = 0.3, col = worktrajcount$gender)
mod<-lm(worktrajcount$DURATION ~worktrajcount$distL1 )
abline(mod)
mod1 <- lm(    (worktrajcount$DURATION[worktrajcount$gender==1])   ~ (worktrajcount$distL1[worktrajcount$gender ==1])  )
abline(mod1, col = 2)
mod2 <- lm(    (worktrajcount$DURATION[worktrajcount$gender==2])   ~ (worktrajcount$distL1[worktrajcount$gender ==2])  )
abline(mod2, col = 3)
mod1
mod2

summary(worktrajcount)
# travail sur les vitesses comparées par genre en L2
plot (worktrajcount$distL2 , worktrajcount$DURATION, pch = 20, cex = 0.3, col = worktrajcount$gender)
mod<-lm(worktrajcount$DURATION ~worktrajcount$distL2 )
abline(mod)
mod1 <- lm(    (worktrajcount$DURATION[worktrajcount$gender==1])   ~ (worktrajcount$distL2[worktrajcount$gender ==1])  )
abline(mod1, col = 2)
mod2 <- lm(    (worktrajcount$DURATION[worktrajcount$gender==2])   ~ (worktrajcount$distL2[worktrajcount$gender ==2])  )
abline(mod2, col = 3)
mod1
mod2
summary(worktrajcount)


# travail sur les vitesses comparées par genre en MEASURE
plot (worktrajcount$measure , worktrajcount$DURATION, pch = 20, cex = 0.3, col = worktrajcount$gender)
mod<-lm(worktrajcount$DURATION ~worktrajcount$measure )
abline(mod)
mod1 <- lm(    (worktrajcount$DURATION[worktrajcount$gender==1])   ~ (worktrajcount$measure[worktrajcount$gender ==1])  )
abline(mod1, col = 2)
mod2 <- lm(    (worktrajcount$DURATION[worktrajcount$gender==2])   ~ (worktrajcount$measure[worktrajcount$gender ==2])  )
abline(mod2, col = 3)
#vistesse moyenne homme  en metre / minute
1/mod1[[1]][2]
#vistesse moyenne femme en metre / minute
1/mod2[[1]][2]



# L1 et L2 sont proche, prennons measure à la place (qui a la bon gout de donner des metres)
worktrajcount$deltadist <- worktrajcount$distL1 / worktrajcount$distL2
library(rgl)
plot3d (worktrajcount$distL1 , worktrajcount$distL2, worktrajcount$measure, col = worktrajcount$deltadist/5000)

plot(worktrajcount$d,worktrajcount$distL2  )
summary(worktrajcount)

# calcul de la vitesse "speed"
worktrajcount$speed <- worktrajcount$measure/worktrajcount$DURATION
worktrajcount$speed2 <- worktrajcount$measure/worktrajcount$DURATION2
                                   
plot(worktrajcount$speed ~ worktrajcount$speed2, col = worktrajcount$gender)                                   
mod<-lm( worktrajcount$speed2 ~ worktrajcount$speed  )                                   
abline(mod)
plot3d(worktrajcount$speed , worktrajcount$DURATION, worktrajcount$birth.year, col = as.numeric(worktrajcount$gender)+1)
plot(worktrajcount$speed ~ worktrajcount$birth.year, col = worktrajcount$gender)                                   
mod<-lm( worktrajcount$speed ~ worktrajcount$birth.year   )                                   
abline(mod)


