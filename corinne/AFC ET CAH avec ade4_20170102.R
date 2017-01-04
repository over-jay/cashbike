#Chargement des donnees
DATA_NY <- read.csv('DONNEES DEMOGRAPHIQUES NY_ENS 20170102.csv', sep=";", head=TRUE, row.names=1)
dim(DATA_NY)
########################################
#PACKAGES UTILISES
########################################

install.packages("FactoMineR") # calcul AFC
library ("FactoMineR")
install.packages("ca")
library ("ca")
install.packages("ade4")
library ("ade4")
install.packages("FactoClass")
library ("FactoClass")
install.packages("MASS")
library ("MASS")

#Pour installer le package factoextra afin qu'il fonctionne avec ade4
install.packages("devtools")
devtools::install_github("kassambara/factoextra", force = TRUE) 
library ("factoextra")


#########################################################
#AFC : ANALYSE FACTORIELLE DES CORRESPONDANCES
#########################################################

##############   Variables ACTIVES  ##################
#INCOME_1	Less than $10,000
#INCOME_2	$10,000 to $14,999
#INCOME_3	$15,000 to $24,999
#INCOME_4	$25,000 to $34,999
#INCOME_5	$35,000 to $49,999
#INCOME_6	$50,000 to $74,999
#INCOME_7	$75,000 to $99,999
#INCOME_8	$100,000 to $149,999
#INCOME_9	$150,000 to $199,999
#INCOME_10	$200,000 or more
#INCOME_11	With earnings
#INCOME_12	With Social Security
#INCOME_13	With retirement income
#INCOME_14	With Supplemental Security Income
#INCOME_15	With cash public assistance income
#INCOME_16	With Food Stamp/SNAP bnfts in the past 12 mo
#FAMILIES_1	Less than $10,000
#FAMILIES_2	$10,000 to $14,999
#FAMILIES_3	$15,000 to $24,999
#FAMILIES_4	$25,000 to $34,999
#FAMILIES_5	$35,000 to $49,999
#FAMILIES_6	$50,000 to $74,999
#FAMILIES_7	$75,000 to $99,999
#FAMILIES_8	$100,000 to $149,999
#FAMILIES_9	$150,000 to $199,999
#FAMILIES_10	$200,000 or more

#Les lignes ou des donnees sont manquantes sont mises en lignes supplementaires (AFC ne traite pas les valeurs manquantes)
#Les variables autres que INCOME et FAMILIES sont mises en colonnes supplementaires
#Les coordonnes des lignes et des colonnes supplementaires sont calcules sur la base de l'AFC avec les donnees actives
DATA_ACTIVES <- DATA_NY[1:184,1:26]
dim(DATA_ACTIVES)
head(DATA_ACTIVES,5)

DATA_ILLUSTRATIVES <- DATA_NY[1:184,27:100]
head(DATA_ILLUSTRATIVES,5)



############################################
#AFC (reduction)  et CAH (regroupement)
############################################

#AFC puis classification ascendante hierarchique des quartiers, a partir de leur coordonnees
#selon les axes factoriels

#Utilisation des packages ade4 et FactoClass

#AFC avec la fonction dudi.coa pour pouvoir calculer les distances euclidiennes entre les points
#Resultats de l'AFC stockes dans res.CA2
res.CA2 <- dudi.coa(DATA_ACTIVES, scannf=FALSE, nf=5)
res.CA2
plot.dudi(res.CA2, cex=0.5)
plot.dudi(res.CA2, Tcol = TRUE, Trow = TRUE, cex=0.5)
plot.dudi(res.CA2, Tcol = FALSE, Trow = TRUE, cex=0.5)
fviz_ca(res.CA2)
fviz_ca_row(res.CA2, alpha.col = "contrib", labelsize=3, pointsize=1)
fviz_ca_col(res.CA2, alpha.col = "contrib", labelsize=3, pointsize=1)


#Remarque : La commande dudi.coa du package ade4 ne permet pas d'obtenir facilement les contributions à l'inertie 
# ni les qualités de représentation des individus. En revanche, elle donne également les taux de liaison.

#Representation du plan factoriel : biplot de variables de ligne et de colonne
scatter(res.CA2, method=2, sub = "REVENU", clab.row = 0.5, clab.col = 1)
scatter(res.CA2, method=2, sub = "REVENU", clab.row = 0.5, clab.col = 1, posieig="none") #enleve la screeplot


#Autre Representation graphique
plot(res.CA2$li[,1],res.CA2$li[,2],type="n",xlab="Axe 1",ylab="Axe 2",xlim=c(-0.7,1.4), ylim=c(-0.5,0.7))
text(res.CA2$li[,1], res.CA2$li[,2], label=row.names(DATA_ACTIVES), labelsize=2, cex=0.6)
text(res.CA2$co[,1], res.CA2$co[,2], label= colnames(DATA_ACTIVES), col="red", cex=0.6)
title("Repartition par INCOME and FAMILIES")
abline(h=0,v=0)

#Contributions des colonnes a la construction des axes
inertia.dudi(res.CA2,col.inertia = T)$col.abs

#Contributions des lignes a la construction des axes
inertia.dudi(res.CA2,row.inertia = T)$row.abs

########################Lignes supplementaires

row.sup <- DATA_NY[185:195, 1:26, drop = FALSE]
head(row.sup)
#Prevision des coordonnees
row.sup.ca <- suprow(res.CA2, row.sup)
names(row.sup.ca)
#Les coordonnes
row.sup.coord <- row.sup.ca$lisup
head(row.sup.coord)
#Visualisation
p <- fviz_ca_row(res.CA2, labelsize=3, pointsize=1)
fviz_add(p, row.sup.coord, color ="darkgreen", labelsize=3, pointsize=1)

########################Colonnes supplementaires

###LABOUR
#POP_IN_LABOUR_FORCE_CIVIL_EMP	: Employed
#POP_IN_LABOUR_FORCE_CIVIL_UNEMP	: Unemployed
#POP_IN_LABOUR_FORCE_ARME	: Armed Forces
#POP_OUT_LABOUR_FORCE	: Not in labor force

col.sup.labour <- DATA_NY[1:184, 27:30, drop = FALSE]
head(col.sup.labour)
#Prevision des coordonnees
col.sup.ca.labour <- supcol(res.CA2, col.sup.labour)
names(col.sup.ca.labour)
#Les coordonnes
col.sup.coord.labour <- col.sup.ca.labour$cosup
head(col.sup.coord.labour)
#Visualisation
p.labour <- fviz_ca_col(res.CA2, labelsize=3, pointsize=1)
fviz_add(p.labour, col.sup.coord.labour , color ="darkgreen", labelsize=3, pointsize=1)

######EMPLOI

#OCC_1	Management, business, science, and arts occupations
#OCC_2	Service occupations
#OCC_3	Sales and office occupations
#OCC_4	Natural resources, construction, and maintenance occupations
#OCC_5	Production, transportation, and material moving occupations
#IND_1	Agriculture, forestry, fishing, hunting, and mining
#IND_2	Construction
#IND_3	Manufacturing
#IND_4	Wholesale trade
#IND_5	Retail trade
#IND_6	Transportation and warehousing, and utilities
#IND_7	Information
#IND_8	Finance and insurance, and real estate and rental and leasing
#IND_9	Professional, scientific, and management, and administrative and waste management services
#IND_10	Educational services, and health care and social assistance
#IND_11	Arts, entertainment, and recreation, and accommodation and food services
#IND_12	Other services, except public administration
#IND_13	Public administration
#WORKER_1	Private wage and salary workers
#WORKER_2	Government workers
#WORKER_3	Self-employed in own not incorporated business workers
#WORKER_4	Unpaid family workers
#0_VEHICLE	
#1_VEHICULE	
#2_VEHICULE	
#3_MORE_VEHICULE

col.sup.emploi <- DATA_NY[1:184, 31:56, drop = FALSE]
head(col.sup.emploi)
#Prevision des coordonnees
col.sup.ca.emploi <- supcol(res.CA2, col.sup.emploi)
names(col.sup.ca.emploi)
#Les coordonnes
col.sup.coord.emploi <- col.sup.ca.emploi$cosup
head(col.sup.coord.emploi)
#Visualisation
p.emploi <- fviz_ca_col(res.CA2, labelsize=3, pointsize=1)
fviz_add(p.emploi, col.sup.coord.emploi , color ="darkgreen", labelsize=3, pointsize=1)

  #Separe OCC
col.sup.occ <- DATA_NY[1:184, 31:35, drop = FALSE]
#Prevision des coordonnees
col.sup.ca.occ <- supcol(res.CA2, col.sup.occ)
#Les coordonnes
col.sup.coord.occ <- col.sup.ca.occ$cosup
#Visualisation
p.occ <- fviz_ca_col(res.CA2, labelsize=3, pointsize=1)
fviz_add(p.occ, col.sup.coord.occ , color ="darkgreen", labelsize=3, pointsize=1)

  #Separe IND
col.sup.ind <- DATA_NY[1:184, 36:48, drop = FALSE]
#Prevision des coordonnees
col.sup.ca.ind <- supcol(res.CA2, col.sup.ind)
#Les coordonnes
col.sup.coord.ind <- col.sup.ca.ind$cosup
#Visualisation
p.ind <- fviz_ca_col(res.CA2, labelsize=3, pointsize=1)
fviz_add(p.ind, col.sup.coord.ind , color ="darkgreen", labelsize=3, pointsize=1)

  #Separe WORKER
col.sup.wor <- DATA_NY[1:184, 49:52, drop = FALSE]
#Prevision des coordonnees
col.sup.ca.wor <- supcol(res.CA2, col.sup.wor)
#Les coordonnes
col.sup.coord.wor <- col.sup.ca.wor$cosup
#Visualisation
p.wor <- fviz_ca_col(res.CA2, labelsize=3, pointsize=1)
fviz_add(p.wor, col.sup.coord.wor , color ="darkgreen", labelsize=3, pointsize=1)

  #Separe VEHICULE
col.sup.veh <- DATA_NY[1:184, 53:56, drop = FALSE]
#Prevision des coordonnees
col.sup.ca.veh <- supcol(res.CA2, col.sup.veh)
#Les coordonnes
col.sup.coord.veh <- col.sup.ca.veh$cosup
#Visualisation
p.veh <- fviz_ca_col(res.CA2, labelsize=3, pointsize=1)
fviz_add(p.veh, col.sup.coord.veh , color ="darkgreen", labelsize=3, pointsize=1)

######SEXE, AGE ET SITUATION

#SEXE_M	Male
#SEXE_F	Female
#TR1_AGE 	Under 5 years
#TR2_AGE 	5 to 9 years
#TR3_AGE 	10 to 14 years
#TR4_AGE 	15 to 19 years
#TR5_AGE 	20 to 24 years
#TR6_AGE 	25 to 34 years
#TR7_AGE 	35 to 44 years
#TR8_AGE 	45 to 54 years
#TR9_AGE 	55 to 59 years
#TR10_AGE 	60 to 64 years
#TR11_AGE 	65 to 74 years
#TR12_AGE 	75 to 84 years
#TR13_AGE 	85 years and over
#RELATION_HOUSEHOLDER	Householder
#RELATION_SPOSE	Spouse
#RELATION_CHILD	Child
#RELATION_OTHER	Other relatives
#RELATION_NORELATIVE	Nonrelatives
#RELATION_NORELATIVE_UNMARRIED	Nonrelatives - Unmarried partner

col.sup.sas <- DATA_NY[1:184, 57:77, drop = FALSE]
#Prevision des coordonnees
col.sup.ca.sas <- supcol(res.CA2, col.sup.sas)
#Les coordonnes
col.sup.coord.sas <- col.sup.ca.sas$cosup
#Visualisation
p.sas <- fviz_ca_col(res.CA2, labelsize=3, pointsize=1)
fviz_add(p.sas, col.sup.coord.sas , color ="darkgreen", labelsize=3, pointsize=1)

  #Separe SEXE
col.sup.sex <- DATA_NY[1:184, 57:58, drop = FALSE]
#Prevision des coordonnees
col.sup.ca.sex <- supcol(res.CA2, col.sup.sex)
#Les coordonnes
col.sup.coord.sex <- col.sup.ca.sex$cosup
#Visualisation
p.sex <- fviz_ca_col(res.CA2, labelsize=3, pointsize=1)
fviz_add(p.sex, col.sup.coord.sex , color ="darkgreen", labelsize=3, pointsize=1)

  #Separe AGE
col.sup.age <- DATA_NY[1:184, 59:71, drop = FALSE]
#Prevision des coordonnees
col.sup.ca.age <- supcol(res.CA2, col.sup.age)
#Les coordonnes
col.sup.coord.age <- col.sup.ca.age$cosup
#Visualisation
p.age <- fviz_ca_col(res.CA2, labelsize=3, pointsize=1)
fviz_add(p.age, col.sup.coord.age , color ="darkgreen", labelsize=3, pointsize=1)

  #Separe RELATION
col.sup.rel <- DATA_NY[1:184, 72:77, drop = FALSE]
#Prevision des coordonnees
col.sup.ca.rel <- supcol(res.CA2, col.sup.rel)
#Les coordonnes
col.sup.coord.rel <- col.sup.ca.rel$cosup
#Visualisation
p.rel <- fviz_ca_col(res.CA2, labelsize=3, pointsize=1)
fviz_add(p.rel, col.sup.coord.rel , color ="darkgreen", labelsize=3, pointsize=1)


######EDUCATION

#SCHOOL_ENROLLMENT	Population 3 years and over enrolled in school
#SCHOOL_BEFORE	Nursery school, preschool
#SCHOOL_KINDERGARDEN	Kindergarten
#SCHOOL_ELEMENTARY	Elementary school (grades 1-8)
#SCHOOL_HIGH	High school (grades 9-12)
#SCHOOL_COLLEGE	College or graduate school
#EDUC_1	Less than 9th grade
#EDUC_2	9th to 12th grade, no diploma
#EDUC_3	High school graduate (includes equivalency)
#EDUC_4	Some college, no degree
#EDUC_5	Associate's degree
#EDUC_6	Bachelor's degree
#EDUC_7	Graduate or professional degree

col.sup.edu <- DATA_NY[1:184, 78:89, drop = FALSE]
#Prevision des coordonnees
col.sup.ca.edu <- supcol(res.CA2, col.sup.edu)
#Les coordonnes
col.sup.coord.edu <- col.sup.ca.edu$cosup
#Visualisation
p.edu <- fviz_ca_col(res.CA2, labelsize=3, pointsize=1)
fviz_add(p.edu, col.sup.coord.edu , color ="darkgreen", labelsize=3, pointsize=1)

######HOUSING

#OWNER_OCC	Owner-occupied
#RENTER_OCC	Renter-occupied
#1_UNIT_DET	1-unit, detached
#1_UNIT_ATT	1-unit, attached
#2_UNITS	2 units
#3_4_UNITS	3 or 4 units
#5_9_UNITS	5 to 9 units
#10_19_UNITS	10 to 19 units
#20_MORE_UNITS	20 or more units
#MOBILE_HOME	Mobile home
#BOAT_RV_VAN	Boat, RV, van, etc.

col.sup.hou <- DATA_NY[1:184, 90:100, drop = FALSE]
#Prevision des coordonnees
col.sup.ca.hou <- supcol(res.CA2, col.sup.hou)
#Les coordonnes
col.sup.coord.hou <- col.sup.ca.hou$cosup
#Visualisation
p.hou <- fviz_ca_col(res.CA2, labelsize=3, pointsize=1)
fviz_add(p.hou, col.sup.coord.hou , color ="darkgreen", labelsize=3, pointsize=1)



##################CAH : Calcul de la distance euclidienne entre les lignes
res.CA2$li
distMat <- dist(res.CA2$li)
distMat
#Classification
res.CAH <- hclust(distMat, method = "ward.D2")

#Dendogramme
plot(res.CAH)
names(res.CAH)
plot(sort(res.CAH$height, dec=T)[1:100],type="h")

#Dendogramme avec distinction des groupes
plot(res.CAH, labels = FALSE, main = "Partition 6 classes", xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(res.CAH, 2, border = "red3")
rect.hclust(res.CAH, 3, border = "yellow3")
rect.hclust(res.CAH, 4, border = "blue3")
rect.hclust(res.CAH, 5, border = "green3")
rect.hclust(res.CAH, 6, border = "orange3")


#Decoupage en 6 groupes et liste des groupes
groupe.res.CAH <- cutree(res.CAH, k=6)
print(sort(groupe.res.CAH))

#Exporte les quartiers et les groupes dans un fichier csv
write.table(groupe.res.CAH, file='CAH_20170102.csv', col.names=TRUE, na="", sep=";")

plot.dudi(res.CA2, Tcol = TRUE, Trow = FALSE, cex=0.5)

s.class(cstar=1,addaxes=TRUE, grid=TRUE, axesell=TRUE,
        dfxy=res.CA2$li, fac=as.factor(groupe.res.CAH), col=1:6,
        label=c(1:6), csub=0.2, possub="bottomright")


#Fonction de calcul de stats pour comparer les moyennes des variables selon le groupe.
#Calcul des ecarts avec la proportion de variance expliquee.
#L'idee est de comparer les moyennes des variables conditionnellement aux groupes.

stat.comp <- function(x,y){
  #nombre de groupes
  K <- length(unique(y))
  #nb. d'observations
  n <- length(x)
  #moyenne globale
  m <- mean(x)
  #variabilite totale
  TSS <- sum((x-m)^2)
  #effectifs conditionnels
  nk <- table(y)
  #moyennes conditionnelles
  mk <- tapply(x,y,mean)
  #variabilite expliquee
  BSS <- sum(nk * (mk - m)^2)
  #moyennes + prop. variance expliquee
  result <- c(mk,100.0*BSS/TSS)
  #nommer les elements du vecteur
  names(result) <- c(paste("G",1:K),"% epl.")
  #renvoyer le vecteur resultat
  return(result)
}
#Appliquer stat.comp aux variables de la base d'origine DATA_ACTIVES
sapply(DATA_ACTIVES,stat.comp,y=groupe.res.CAH)

