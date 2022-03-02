library(caret)
load("saheartbis.Rda")
listconti=c("age", "tobacco", "ldl",
  "adiposity",  "typea", "famhist.Absent")

paste(listconti,collapse="+")


# Control de la semilla para reproducibilidad en caret si se usa validación cruzada simple 

#  Una semilla general (set.seed) 

# ¿PARA QUÉ SE USAN LAS SEMILLAS EN BAGGING?
#  
# a) PARA SORTEAR QUE OBSERVACIONES CAEN EN CADA FOLD DE VALIDACIÓN CRUZADA SI SE USA METHOD="CV"
# b) PARA SORTEAR LAS OBSERVACIONES QUE SE USAN PARA CONSTRUIR UN ÁRBOL EN CADA ITERACION DE BAGGING (sampsize=200)
# (EN CADA EJECUCIÓN DE VALIDACIÓN CRUZADA SE
#  DEJA FUERA UN FOLD DE TAMAÑO (462/4)=115 Y SE UTILIZAN (3/4)*462=346 OBSERVACIONES TRAINING PARA CONSTRUIR 
#  EL MODELO CON LO CUAL SAMPSIZE MÁXIMO HA DE SER 345 APROX).


# EJEMPLO PROBANDO UN SOLO PARÁMETRO MTRY

rfgrid<-expand.grid(mtry=c(6))

set.seed(1234)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
 classProbs=TRUE) 

rf<- train(data=saheartbis,
 factor(chd)~age+tobacco+ldl+adiposity+typea+famhist.Absent,
 method="rf",trControl=control,tuneGrid=rfgrid,
 linout = FALSE,ntree=5000,nodesize=10,replace=TRUE)

rf

# PARA PLOTEAR EL ERROR OOB A MEDIDA QUE AVANZAN LAS ITERACIONES
# SE USA DIRECTAMENTE EL PAQUETE randomForest

library(randomForest)
set.seed(12345)

rfbis<-randomForest(factor(chd)~age+tobacco+ldl+adiposity+typea+famhist.Absent,
 data=saheartbis,
 mtry=6,ntree=5000,sampsize=300,nodesize=10,replace=TRUE)

plot(rfbis$err.rate[,1])

# La función cruzadarfbin permite plantear bagging 
# (para bagging hay que poner mtry=numero de variables independientes)

load ("saheartbis.Rda")
source ("cruzadas avnnet y log binaria.R")
source ("cruzada arbolbin.R")
source ("cruzada rf binaria.R")

medias1<-cruzadalogistica(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco", "ldl",
  "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""), grupos=4,sinicio=1234,repe=5)

 medias1$modelo="Logística"


medias2<-cruzadaavnnetbin(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),grupos=4,sinicio=1234,repe=5,
  size=c(5),decay=c(0.1),repeticiones=5,itera=200)

  medias2$modelo="avnnet"


  medias3<-cruzadaarbolbin(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),grupos=4,sinicio=1234,repe=5,
  cp=c(0),minbucket =5)

  medias3$modelo="arbol"


  medias4<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=5,nodesize=10,
  mtry=6,ntree=1000,replace=TRUE)

  medias4$modelo="bagging"
  

    
union1<-rbind(medias1,medias2,medias3,medias4)

par(cex.axis=1.5)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union1,auc~modelo,main="AUC",col="pink")

# Probamos variaciones sobre el tamaño muestral en bagging
# con 10 grupos de CV, maximo 415 sampsize

   medias1<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=10,sinicio=1234,repe=20,nodesize=10,
  mtry=6,ntree=3000,replace=TRUE,sampsize=50)

      medias1$modelo="bagging50"
    

    medias2<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=10,sinicio=1234,repe=20,nodesize=10,
  mtry=6,ntree=3000,replace=TRUE,sampsize=100)

      medias2$modelo="bagging100"
    
      medias3<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=10,sinicio=1234,repe=20,nodesize=10,
  mtry=6,ntree=3000,replace=TRUE,sampsize=150)


      medias3$modelo="bagging150"

    medias4<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=10,sinicio=1234,repe=20,nodesize=10,
  mtry=6,ntree=3000,replace=TRUE,sampsize=200)

      medias4$modelo="bagging200"
  
medias5<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=10,sinicio=1234,repe=20,nodesize=10,
  mtry=6,ntree=3000,replace=TRUE,sampsize=300)


  medias5$modelo="bagging300"
  
    medias6<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=10,sinicio=1234,repe=20,nodesize=10,
  mtry=6,ntree=3000,replace=TRUE)

    # En este modelo al no usar sampsize usa el que tiene por defecto 
# caret, que son todas las observaciones con reemplazamiento (415) 

  medias6$modelo="baggingBASE"

  
union1<-rbind(medias1,medias2,medias3,medias4,medias5,
 medias6)

par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")

uni<-union1
uni$modelo <- with(uni,
 reorder(modelo,tasa, mean))
par(cex.axis=0.8,las=2)
boxplot(data=uni,tasa~modelo,col="pink")

# PROBAMOS CON SAMPSIZE=150 CON 10 GRUPOS DE CV

medias1<-cruzadalogistica(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco", "ldl",
  "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""), grupos=10,sinicio=1234,repe=20)

 medias1$modelo="Logística"


medias2<-cruzadaavnnetbin(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),grupos=10,sinicio=1234,repe=20,
  size=c(5),decay=c(0.1),repeticiones=5,itera=200)

  medias2$modelo="avnnet"


  medias3<-cruzadaarbolbin(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),grupos=10,sinicio=1234,repe=20,
  cp=c(0),minbucket =5)

  medias3$modelo="arbol"


  medias4<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=20,nodesize=10,
  mtry=6,ntree=3000,replace=TRUE)

  medias4$modelo="baggingBASE"

    medias5<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=20,nodesize=10,
  mtry=6,ntree=3000,replace=TRUE,sampsize=50)

  medias5$modelo="bagging50"
    
union1<-rbind(medias1,medias2,medias3,medias4,medias5)

par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union1,auc~modelo,main="AUC",col="pink")


# EJEMPLO CON VARIABLE CONTINUA
# La función cruzadarf permite plantear bagging PARA VDEP CONTINUA
# (para bagging hay que poner mtry=numero de variables independientes)
# No se puede plotear oob

load("compressbien.Rda")

library(randomForest)

rfbis<-randomForest(cstrength~age+water+cement+blast,
 data=compressbien,
 mtry=4,ntree=1000,sampsize=300,nodesize=10,replace=TRUE)

# En continuas No estoy seguro de que 
# este gráfico esté realizado sobre oob
# pero puede valer para decidir iteraciones

plot(rfbis$mse)

source("cruzada arbol continua.R")
source("cruzadas avnnet y lin.R")
source("cruzada rf continua.R")
load("compressbien.Rda")

data<-compressbien

medias1<-cruzadaavnnet(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),grupos=4,sinicio=1234,repe=5,
size=c(15),decay=c(0.01),repeticiones=5,itera=100)

medias1$modelo="avnnet"

medias2<-cruzadalin(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),grupos=4,sinicio=1234,repe=5)

medias2$modelo="lineal"

medias3<-cruzadaarbol(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
 grupos=4,sinicio=1234,repe=5,cp=0,minbucket=5)

medias3$modelo="arbol"

medias4<-cruzadarf(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
  nodesize=20,replace=TRUE,ntree=200,mtry=4)

medias4$modelo="bagging"

union1<-rbind(medias1,medias2,medias3,medias4)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo,col="pink")

union1<-rbind(medias1,medias3,medias4)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo,col="pink")

union1<-rbind(medias1,medias4)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo,col="pink")


