# autoformation R data.table()
dt<-data.table(mtcars)
# pas de i , j  = .(cyl,gear), pas de by
# toutes les lignes , extrait uniquement les colonne cyl et gear
dt2<-dt[,.(cyl,gear)]
dt2
# i = "" toutes les lignes, j=unique(gear)  count(distinct (gear)), by = par cyl, qui reste
dt3<-dt2[,unique(gear), by=cyl]

# pas de i , j  = .(cyl,gear), pas de by
# toutes les lignes , extrait uniquement les colonnes gear et  cyl e
dt4 <- data.table(mtcars)[,.(gear, cyl)]
dt4
#pas de i :; toutes les lignes
# j : nouvelle col gearL : list de liste des gear unique 
# ajout de cette colonne à dt
# by par cyl

#t4[,gearsL:=list(unique(gear)), by=cyl]
#dt4
dt4[,gearsLL:=list(list(unique(gear))), by=cyl]
dt4

#pas de i :; toutes les lignes
# j: ajout de cette colonne à dt
# j : nouvelle col gearL : funciton prendre le 2eme element de la liste gearsL
# marche pareil sapply ou lapply
# pas de by : pour tous
dt[,gearL1:=lapply(gearsL, function(x) x[2])]
dt
dt[,gearS1:=sapply(gearsL, function(x) x[2])] 
dt
str((dt[,gearL1]))
str((dt[,gearS1]))
str(dt)
#gearS1 est un vecteur
sum(  ((dt[,gearS1]))==rep(3,32) ) 
sum(dt[,gearS1])
sum(  ((dt[,gearL1]))==rep(3,32) ) 
sum(dt[,gearL1]) # marche pas !
sum(as.numeric(dt[,gearL1]))

# i = all lines
# j : ajout col qui calcule les autres gear de la liste qui ne sont pas celui de la ligne 
# pas de by
dt[,other_gear:=mapply(function(x, y) setdiff(x, y), x=gearsL, y=gear)]
head(dt)


dt <- data.table(mtcars)
# i : all lines
# j renvoi les écart a la moyenne en abs , aroundi à 2 dec
# par cyl
dt[,{tmp1=mean(mpg)}, by=cyl]
dt[,{tmp1=mean(mpg); tmp2=mean(abs(mpg-tmp1))}, by=cyl]
dt[,{tmp1=mean(mpg); tmp2=mean(abs(mpg-tmp1)); tmp3=round(tmp2, 2)}, by=cyl]
# ou comme cela
dt[,{tmp1=mean(mpg)
    tmp2=mean(abs(mpg-tmp1))
    tmp3=round(tmp2, 2)
    list(tmp1 = tmp1, tmp2=tmp2, tmp3=tmp3)},
    by=cyl]
# on fait in DT uniquement avec les col cyp mpg
dt <- data.table(mtcars)[,.(cyl, mpg)]
dt
# moyenne des mgp par cyl, puis moy des abs de mgp-mmpg et réinit de tmp&
dt[,tmp1:=mean(mpg), by=cyl][,tmp2:=mean(abs(mpg-tmp1)), by=cyl][,tmp1:=NULL]
dt
?data.table
#doc :  https://github.com/Rdatatable/data.table/wiki

example(data.table)  # to run these examples at the prompt

DF = data.frame(x=rep(c("a","b","c"),each=3), y=c(1,3,6), v=1:9)
DT = data.table(x=rep(c("a","b","c"),each=3), y=c(1,3,6), v=1:9)
DF
DT
identical(dim(DT),dim(DF)) # TRUE
identical(DF$a, DT$a)      # TRUE
is.list(DF)                # TRUE
is.list(DT)                # TRUE

is.data.frame(DT)          # TRUE
is.data.frame(DF)          # TRUE

tables()

DT[2]                      # 2nd row
DT[,v]                     # v column (as vector)
DT[,list(v)]               # v column (as data.table)
DT[2:3,sum(v)]             # sum(v) over rows 2 and 3
DT[2:5,cat(v,"")]        # just for j's side effect
DT[c(FALSE,TRUE)]          # even rows (usual recycling)

DT[,2,with=FALSE]          # 2nd column
DT[,2,with=TRUE]          # 2nd column
colNum = 2
DT[,colNum,with=FALSE]     # same

setkey(DT,x)               # set a 1-column key. No quotes, for convenience.
tables()
setkeyv(DT,"x")            # same (v in setkeyv stands for vector)
tables()
v="x"
setkeyv(DT,v)              # same
# key(DT)<-"x"             # copies whole table, please use set* functions instead
key(DT)
DT["a"]                    # binary search (fast)
DT[x=="a"]                 # vector scan (slow)

DT[,sum(v),by=x]           # keyed by
DT[,sum(v),by=key(DT)]     # same
DT[,sum(v),by=y]           # ad hoc by

DT["a",sum(v)]             # j for one group
DT[c("a","b"),sum(v)]      # j for two groups

X = data.table(c("b","c"),foo=c(4,2))
X
DT

DT[X]                      # join on 1st col (no outer join)
DT[X,sum(v)]               # join and eval j for each row in i
DT[X,mult="first"]         # first row of each group of 1st col join
DT[X,mult="last"]          # last row of each group of 1st col join
DT[X,sum(v)*foo]           # join inherited scope
X$foo

setkey(DT,x,y)             # 2-column key
key(DT)
tables()
setkeyv(DT,c("x","y"))     # same

DT["a"]                    # join to 1st column of key
DT[J("a")]                 # same. J() stands for Join, an alias for list()
DT[list("a")]              # same
DT[.("a")]                 # same. In the style of package plyr.
DT[J("a",3)]               # join to 2 columns
DT[.("a",3)]               # same
DT[J("a",3:6)]             # join 4 rows (2 missing) - outerjoin
DT[J("a",3:6),nomatch=0]   # remove missing
DT[J("a",3:6),roll=TRUE]   # rolling join (locf)
DT
DT$y%%2
DT[,sum(v),by=list(y%%2)]  # by expression here "parity of y"
DT[,.SD[2],by=x]           # 2nd row of each group
DT[,tail(.SD,2),by=x]      # last 2 rows of each group
DT[,lapply(.SD,sum),by=x]  # apply through columns by group  
# (SD = Subset of Data)

DT[,list(MySum=sum(v),
         MyMin=min(v),
         MyMax=max(v)),
   by=list(x,y%%2)]       # by 2 expressions

DT[,sum(v),x][V1<20]       # compound query
DT[,sum(v),x][order(-V1)]  # ordering results

print(DT[,z:=42L])         # add new column by reference
print(DT[,z:=NULL])        # remove column by reference
print(DT["a",v:=42L])      # subassign to existing v column by reference
DT
print(DT["b",v2:=84L])     # subassign to new column by reference (NA padded)

DT[,m:=mean(v),by=x][]     # add new column by reference by group
# NB: postfix [] is shortcut to print()
DT

DT[,.SD[which.min(v)],by=x][]  # nested query by group

DT[!J("a")]                # not join
DT[!"a"]                   # same
DT[!2:4]                   # all rows other than 2:4
DT[2:4]                   # all rows other than 2:4
DT[x!="b" | y!=3]          # multiple vector scanning approach, slow
DT[!J("b",3)]              # same result but much faster


# Follow r-help posting guide, support is here (*not* r-help) :
# datatable-help@lists.r-forge.r-project.org
# or
# http://stackoverflow.com/questions/tagged/data.table

vignette("datatable-intro")
vignette("datatable-faq")
vignette("datatable-timings")

test.data.table()          # over 700 low level tests

update.packages()          # keep up to date
