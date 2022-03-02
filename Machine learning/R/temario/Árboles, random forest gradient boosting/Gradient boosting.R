
# EJEMPLOS GRADIENT BOOSTING

# TUNEADO DE GRADIENT BOOSTING CON CARET

# Caret permite tunear estos parámetros básicos:
#  
# 	shrinkage (parámetro v de regularización, mide la velocidad de ajuste, a menor v, más lento y necesita más iteraciones, pero es más fino en el ajuste)
# 	n.minobsinnode: tamaño máximo de nodos finales (el principal parámetro que mide la complejidad)
# 	n.trees=el número de iteraciones (árboles)
# 	interaction.depth (2 para árboles binarios)


library(caret)
load("saheartbis.Rda")

set.seed(12345)

gbmgrid<-expand.grid(shrinkage=c(0.1,0.05,0.03,0.01,0.001),
 n.minobsinnode=c(5,10,20),
 n.trees=c(100,500,1000,5000),
 interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
 classProbs=TRUE) 

gbm<- train(factor(chd)~age+tobacco+ldl+adiposity+typea+sbp+
              famhist.Absent,data=saheartbis,
 method="gbm",trControl=control,tuneGrid=gbmgrid,
 distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm
 
plot(gbm)

# ESTUDIO DE EARLY STOPPING
# Probamos a fijar algunos parámetros para ver como evoluciona
# en función de las iteraciones

gbmgrid<-expand.grid(shrinkage=c(0.1),
 n.minobsinnode=c(10),
 n.trees=c(50,100,300,500,800,1000,1200),
 interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
 classProbs=TRUE) 

gbm<- train(factor(chd)~.,data=saheartbis,
 method="gbm",trControl=control,tuneGrid=gbmgrid,
 distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm
plot(gbm)

# IMPORTANCIA DE VARIABLES
par(cex=1.3)
summary(gbm)

tabla<-summary(gbm)
par(cex=1.5,las=2)
barplot(tabla$rel.inf,names.arg=row.names(tabla))


# La función cruzadagbmbin permite plantear gradient boosting para binarias 

load ("saheartbis.Rda")
source ("cruzadas avnnet y log binaria.R")
source ("cruzada arbolbin.R")
source ("cruzada rf binaria.R")
source ("cruzada gbm binaria.R")

medias1<-cruzadalogistica(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco", "ldl",
  "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""), grupos=4,sinicio=1234,repe=10)

 medias1$modelo="Logística"

medias2<-cruzadaavnnetbin(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),grupos=4,sinicio=1234,repe=10,
  size=c(5),decay=c(0.1),repeticiones=5,itera=200)

  medias2$modelo="avnnet"


  medias3<-cruzadaarbolbin(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),grupos=4,sinicio=1234,repe=10,
  cp=c(0),minbucket =5)

  medias3$modelo="arbol"

  medias4<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=10,nodesize=10,
  mtry=6,ntree=3000,replace=TRUE,sampsize=150)

  medias4$modelo="bagging"

    medias5<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=10,nodesize=10,
  mtry=5,ntree=3000,replace=TRUE,sampsize=150)

  medias5$modelo="rf"

medias6<-cruzadagbmbin(data=saheartbis, vardep="chd",
   listconti=c("age", "tobacco",
  "ldl", "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=10,
n.minobsinnode=10,shrinkage=0.1,n.trees=50,interaction.depth=2)

medias6$modelo="gbm"


union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6)

par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union1,auc~modelo,main="AUC",col="pink")



# EJEMPLO CON VARIABLE CONTINUA

load("compressbien.Rda")

gbmgrid<-expand.grid(shrinkage=c(0.1,0.05,0.03,0.01,0.001),
 n.minobsinnode=c(5,10,20),
 n.trees=c(100,500,1000,5000),
 interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

gbm<- train(cstrength~age+water+cement+blast,data=compressbien,
 method="gbm",trControl=control,tuneGrid=gbmgrid,
 distribution="gaussian", bag.fraction=1,verbose=FALSE)

gbm
 
plot(gbm)


# ESTUDIO DE EARLY STOPPING
# Probamos a fijar algunos parámetros para ver como evoluciona
# en función de las iteraciones

gbmgrid<-expand.grid(shrinkage=c(0.1),
 n.minobsinnode=c(10),
 n.trees=c(100,300,500,800,1000,1200,2000,5000,7000,8000,20000),
 interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

gbm<- train(cstrength~age+water+cement+blast,data=compressbien,
 method="gbm",trControl=control,tuneGrid=gbmgrid,
 distribution="gaussian", bag.fraction=1,verbose=FALSE)

gbm

plot(gbm,ylim=c(3,18))

# IMPORTANCIA DE VARIABLES

summary(gbm)

source("cruzada arbol continua.R")
source("cruzadas avnnet y lin.R")
source("cruzada rf continua.R")
source("cruzada gbm continua.R")

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
  nodesize=10,replace=TRUE,ntree=200,mtry=4)

medias4$modelo="bagging"

medias5<-cruzadarf(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
  nodesize=10,replace=TRUE,ntree=600,mtry=3)

medias5$modelo="rf"

medias6<-cruzadagbm(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
n.minobsinnode=10,shrinkage=0.10,n.trees=20000,interaction.depth=2)

medias6$modelo="gbm"


union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo,col="pink")

union1<-rbind(medias1,medias3,medias4,medias5,medias6)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo,col="pink")

