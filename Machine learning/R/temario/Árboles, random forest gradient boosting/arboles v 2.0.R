
library(rpart)
library(rpart.plot)
library(rattle)
library(pROC)
library(caret)

# **********************************
# EJEMPLO VARIABLE DEPENDIENTE BINARIA
# En este caso hay que nombrarla como factor, y/o pasarla antes a factor,
# y poner method="class" 
# **********************************

load("saheart.Rda")
dput(names(saheart))

saheart$chd<-ifelse(saheart$chd==1,"Yes","No")

arbol1 <- rpart(factor(chd) ~ tobacco, data = saheart,
 minbucket =30,method = "class",parms=list(split="gini"))

summary(arbol1)
# **********************************
# GRÁFICOS
# **********************************

# extra=101: 
# En cada nodo, Número de observaciones de cada clase 
# y % de observaciones sobre el total
rpart.plot(arbol1,extra=101)
# extra=105: 
# En cada nodo, % de observaciones de cada clase 
# y % de observaciones sobre el total
rpart.plot(arbol1,extra=105)# extra=101: 
# extra=5: 
#  En cada nodo, solo % de observaciones de cada clase 
rpart.plot(arbol1,extra=5)
# extra=5: 
#  En cada nodo, solo Número de observaciones de cada clase 
rpart.plot(arbol1,extra=1)

# SI QUIERO EL NÚMERO DE NODO: nn=TRUE
rpart.plot(arbol1,extra=1,nn=TRUE) 

# EL MÁS COMPLETO
rpart.plot(arbol1,extra=105,nn=TRUE) 

# Tamaño de letra

# The default tweak is 1, meaning no adjustment.
# Use say tweak=1.2 to make the text 20% larger.

rpart.plot(arbol1,extra=1,tweak=0.7)

# Grabar en archivo gráfico

tiff(file="arbol1.tiff")
rpart.plot(arbol1,extra=1,tweak=1.2)
dev.off()

# **********************************
# REGLAS DE DECISIÓN DEL ÁRBOL
# Obtener las reglas en texto: función asRules del paquete rattle
# **********************************

asRules(arbol1)

# **********************************
# IMPORTANCIA DE VARIABLES 
# **********************************
arbol2 <- rpart(factor(chd) ~ ., data = saheart,minbucket =25,
method = "class")

# la función par(cex=) antes de los gráficos
# cambia el tamaño del texto

par(cex=1.2)
arbol2$variable.importance
barplot(arbol2$variable.importance)

par(cex=1.2)
rpart.plot(arbol2,extra=105,tweak=0.7,type=1,nn=TRUE)
asRules(arbol2)

arbol2 <- rpart(factor(chd) ~ ., data = saheart,minbucket =25,
method = "class",maxsurrogate=0)

par(cex=1.2)
arbol2$variable.importance
barplot(arbol2$variable.importance)


# **********************************
# TUNEANDO LA COMPLEJIDAD DEL ÁRBOL
# **********************************
#  Número de observaciones máximo en nodo final: minbucket

load("spam.Rda")

# Cambiando minbucket
arbol2 <- rpart(factor(spam) ~ .,data = spam,minbucket =5,cp=0)
rpart.plot(arbol2,extra=1)
arbol2 <- rpart(factor(spam) ~ .,data = spam,minbucket =30,cp=0) 
rpart.plot(arbol2,extra=1)
arbol2 <- rpart(factor(spam) ~ .,data = spam,minbucket =100,cp=0)
rpart.plot(arbol2,extra=1)


# **********************************
# EJEMPLO VARIABLE DEPENDIENTE CONTINUA
# PONER method="anova" en rpart
# **********************************

load("compress.Rda")

arbol3 <- rpart(cstrength~ .,
data = compress,minbucket =40,method = "anova",maxsurrogate=0,cp=0)

summary(arbol3)

# **********************************
# GRÁFICOS
# **********************************

rpart.plot(arbol3,extra=1,tweak=1.1,nn=TRUE)

asRules(arbol3)

arbol3$variable.importance 
par(cex=1.2)
barplot(arbol3$variable.importance,col="orange")


# **********************************
# TUNEANDO LA COMPLEJIDAD DEL ÁRBOL
# **********************************
# Cambiando minbucket

arbol3 <- rpart(cstrength~ .,data = compress,minbucket =10,cp=0)
rpart.plot(arbol3,extra=1)

arbol3 <- rpart(cstrength~ .,data = compress,minbucket =40,cp=0)
rpart.plot(arbol3,extra=1)

arbol3 <- rpart(cstrength~ .,data = compress,minbucket =100,cp=0)
rpart.plot(arbol3,extra=1)


# *****************************************************
# TUNEADO Y EVALUACIÓN DE LA EFICACIA PREDICTIVA CON CARET
# *****************************************************

# EJEMPLO VARIABLE DEPENDIENTE BINARIA

load("saheartbis.Rda")

# Validación cruzada simple

control<-trainControl(method = "cv",number=4,
classProbs=TRUE,savePredictions = "all") 

# CON LOGÍSTICA

logi<- train(factor(chd)~ldl+tobacco+famhist.Absent,data=saheartbis,
method="glm",trControl=control)

summary(logi)
logi
sal<-logi$pred

salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu

curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
auc<-curvaroc$auc
auc
plot(roc(response=sal$obs,predictor=sal$Yes))

# CON  RED

nnetgrid <-  expand.grid(size=c(5),decay=c(0.1),bag=FALSE)

red1<- train(chd~ldl+tobacco+famhist.Absent,data=saheartbis,
method="avNNet",linout = FALSE,maxit=100,repeats=5,
 trControl=control,tuneGrid=nnetgrid)

summary(red1)
sal<-red1$pred

salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu

curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
auc<-curvaroc$auc
auc
plot(roc(response=sal$obs,predictor=sal$Yes))


# CON ARBOL: con rpart se puede tunear el cp, pero no lo hacemos por ´no tener extensión a otros paquetes y modelos de árboles
# En su lugar hacemos pruebas con diferentes valores de minbucket

# UNA SOLA PRUEBA CON MINBUCKET=30

arbolgrid <-  expand.grid(cp=c(0))

arbolcaret<- train(factor(chd)~ldl+tobacco+famhist.Absent,data=saheartbis,
method="rpart",minbucket=30,trControl=control,tuneGrid=arbolgrid)

arbolcaret

sal<-arbolcaret$pred

salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu

curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
auc<-curvaroc$auc
auc
plot(roc(response=sal$obs,predictor=sal$Yes))

# TUNEADO VARIANDO EL VALOR DE MINBUCKET EN UN BUCLE (ya que en caret no se puede tunear minbucket en el grid)
for (minbu in seq(from=5, to=60, by=5))
{
print(minbu)
cat("/n")

arbolgrid <-  expand.grid(cp=c(0))

arbolcaret<- train(factor(chd)~ldl+tobacco+famhist.Absent,data=saheartbis,
method="rpart",minbucket=minbu,trControl=control,tuneGrid=arbolgrid)

# arbolcaret

sal<-arbolcaret$pred

salconfu<-confusionMatrix(sal$pred,sal$obs)
print(salconfu)

curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
auc<-curvaroc$auc
print(auc)
# plot(roc(response=sal$obs,predictor=sal$Yes))
}

# PRUEBAS VALIDACIÓN CRUZADA REPETIDA

source ("cruzadas avnnet y log binaria.R")
source ("cruzada arbolbin.R")

medias1<-cruzadalogistica(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco",  "ldl",
  "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""), grupos=4,sinicio=1234,repe=5)

 medias1$modelo="Logística"


medias2<-cruzadaavnnetbin(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco",  "ldl",
  "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),grupos=4,sinicio=1234,repe=5,
  size=c(5),decay=c(0.1),repeticiones=5,itera=200)

  medias2$modelo="avnnet"

  
  medias3<-cruzadaarbolbin(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco",  "ldl",
  "adiposity",  "typea", "famhist.Absent"),
 listclass=c(""),grupos=4,sinicio=1234,repe=5,
  cp=c(0),minbucket =5)

  medias3$modelo="arbol"

  
  union1<-rbind(medias1,medias2,medias3)

par(cex.axis=0.5)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
boxplot(data=union1,auc~modelo,main="AUC")


# EJEMPLO VARIABLE DEPENDIENTE CONTINUA

load("compressbien.Rda")

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

# Regresión
reg1<- train(cstrength~cement+plasti+age+blast+water+ash,
 data=compressbien, method="lm",trControl=control)

reg1

# Red

nnetgrid <-  expand.grid(size=c(15),decay=c(0.01),bag=F)

rednnet<- train(cstrength~cement+plasti+age+blast+water+ash,
 data=compressbien, method="avNNet",linout = TRUE,maxit=100,
 trControl=control,repeats=5,tuneGrid=nnetgrid)

rednnet

# Árbol con rpart: parece que minbucket=45 está bien

arbolgrid <-  expand.grid(cp=c(0))

for (minbu in seq(from=5, to=60, by=5))
{

arbolcaret<- train(cstrength~cement+plasti+age+blast+water+ash,
 data=compressbien,method="rpart",minbucket=minbu,
 trControl=control,tuneGrid=arbolgrid)
print(minbu)
print(arbolcaret)
}

source("cruzada arbol continua.R")
source("cruzadas avnnet y lin.R")

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
listclass=c(""),grupos=4,sinicio=1234,repe=5,cp=0,minbucket=45)
 
medias3$modelo="arbol"

union1<-rbind(medias1,medias2,medias3)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo)

