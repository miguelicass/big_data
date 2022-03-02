
library(nnet)
library(dummies)
library(MASS)
library(reshape)
library(caret)
library(pROC)

# Lectura y esquema de variables

load("saheart.Rda")
dput(names(saheart))


# c("sbp", "tobacco", "ldl", "adiposity", "famhist", "typea", "obesity", 
# "alcohol", "age", "chd")

listconti<-c("sbp", "tobacco", "ldl", "adiposity",  "obesity", 
 "alcohol", "age", "typea")
listclass<-c("famhist")
vardep<-c("chd")

# a)Eliminar las observaciones con missing en alguna variable

saheart2<-na.omit(saheart,(!is.na(saheart)))

# b)pasar las categóricas a dummies

saheart3<- dummy.data.frame(saheart2, listclass, sep = ".")

# c)estandarizar las variables continuas

# Calculo medias y dtipica de datos y estandarizo (solo las continuas)

means <-apply(saheart3[,listconti],2,mean) 
sds<-sapply(saheart3[,listconti],sd) 

# Estandarizo solo las continuas y uno con las categoricas

saheartbis<-scale(saheart3[,listconti], center = means, scale = sds)
numerocont<-which(colnames(saheart3)%in%listconti)
saheartbis<-cbind(saheartbis,saheart3[,-numerocont])

# El archivo saheartbis ya está preparado:no hay missing, las continuas salvo la dependiente 
# están estandarizadas y las categoricas pasadas a dummy

dput(names(saheartbis))

# NOTA: En los modelos pondremos solo k-1 dummies por cada categórica

c("sbp", "tobacco", "ldl", "adiposity", "obesity", "alcohol", 
"age", "typea", "famhist.Absent", "famhist.Present", "chd")


# PARA EVITAR PROBLEMAS, MEJOR DEFINIR LA VARIABLE OUTPUT
# con valores alfanuméricos Yes, No

saheartbis$chd<-ifelse(saheartbis$chd==1,"Yes","No")

# EJEMPLO BÁSICO CON CARET TRAIN TEST 

# CON  LOGÍSTICA

# Training test una sola vez
control<-trainControl(method = "LGOCV",p=0.8,number=1,
 classProbs=TRUE,savePredictions = "all") 

logi<- train(chd~ldl+tobacco+famhist.Absent,data=saheartbis,
method="glm",trControl=control)

summary(logi)
logi
sal<-logi$pred

# CON  RED

nnetgrid <-  expand.grid(size=c(5),decay=c(0.1),bag=FALSE)

red1<- train(chd~ldl+tobacco+famhist.Absent,data=saheartbis,
method="avNNet",linout = FALSE,maxit=100,repeats=5,
 trControl=control,tuneGrid=nnetgrid)

summary(red1)
sal<-red1$pred


# La función confusionMatrix de caret calcula la matriz de confusión

salconfu<-confusionMatrix(sal$pred,sal$obs)
salconfu

# Para dibujar la curva roc y calcular el auc se usa el paquete pROC

curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
auc<-curvaroc$auc
auc
plot(roc(response=sal$obs,predictor=sal$Yes))
  

# Si se quiere obtener el conjunto test elegido por caret con todas las variables:
# filas<-logi$pred$rowIndex
# 
# saheartbis$index <- as.numeric(row.names(saheartbis))
# cosa<-saheartbis[saheartbis$index %in% filas,]
# 
# cosa<-cbind(cosa,logi$pred) 
 
# ***************************
# TUNING Y EVALUACIÓN CON CARET
# ***************************

set.seed(12346)

# # Validación cruzada una sola vez
# control<-trainControl(method = "cv",number=4,savePredictions = "all") 
# 
# # Validación cruzada repetida
# control<-trainControl(method = "repeatedcv",number=4,repeats=5,savePredictions = "all") 
# 
# # Training test una sola vez
# control<-trainControl(method = "LGOCV",p=0.8,number=1,savePredictions = "all") 
# 
# # Training test repetido
# control<-trainControl(method = "LGOCV",p=0.8,number=5,savePredictions = "all") 
# 
# # EN LO SUCESIVO APLICAMOS VALIDACIÓN CRUZADA REPETIDA
# 
# # Training test una sola vez
# control<-trainControl(method = "LGOCV",p=0.8,number=1,savePredictions = "all") 

# EN LO SUCESIVO APLICAMOS VALIDACIÓN CRUZADA REPETIDA

# Importante classProbs=TRUE para guardar las probabilidades
# y definir la variable de salida con valores alfanuméricos Yes, No

set.seed(12346)

# Validación cruzada repetida
control<-trainControl(method = "repeatedcv",number=4,repeats=5,
 savePredictions = "all",classProbs=TRUE) 

# ***************************************************************
# avNNet: parámetros
    # Number of Hidden Units (size, numeric)
    # Weight Decay (decay, numeric)
    # Bagging (bag, logical)
# ***************************************************************
avnnetgrid <-expand.grid(size=c(5,10,15,20),
 decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(chd~ldl+tobacco+famhist.Absent,data=saheartbis,
method="avNNet",linout = FALSE,maxit=100,
 trControl=control,tuneGrid=avnnetgrid,
 repeats=5)

redavnnet


# SELECCIÓN DE VARIABLES EN CLASIFICACIÓN BINARIA LOGÍSTICA

full<-glm(factor(chd)~.,data=saheartbis,family = binomial(link="logit"))
null<-glm(factor(chd)~1,data=saheartbis,family = binomial(link="logit"))

library(MASS)

seleccion<-stepAIC(null,scope=list(upper=full),direction="both")

# Para ver los efectos escogidos
dput(names(seleccion$coefficients))

# Esto si se quiere en versión formula
formula(seleccion)

dput(names(saheartbis))


# ************************************
# APLICANDO steprepetidobinaria 
# ************************************
source("funcion steprepetido binaria.R")


listconti<-c("sbp", "tobacco", "ldl", "adiposity",
 "obesity", "alcohol","age", "typea",
 "famhist.Absent", "famhist.Present")
vardep<-c("chd")

data<-saheartbis

lista<-steprepetidobinaria(data=data,
 vardep=vardep,listconti=listconti,sinicio=12345,
 sfinal=12355,porcen=0.8,criterio="AIC")

tabla<-lista[[1]]
dput(lista[[2]][[1]])
dput(lista[[2]][[2]])

lista<-steprepetidobinaria(data=data,
 vardep=vardep,listconti=listconti,sinicio=12345,
 sfinal=12355,porcen=0.8,criterio="BIC")

tabla<-lista[[1]]
dput(lista[[2]][[1]])
dput(lista[[2]][[2]])


# ************************************
# APLICANDO cruzadalogistica a los modelos candidatos
# ************************************

source("cruzadas avnnet y log binaria.R")

medias1<-cruzadalogistica(data=saheartbis,
 vardep="chd",listconti=c("age", "famhist.Absent",
  "typea", "ldl", "tobacco"),
 listclass=c(""), grupos=4,sinicio=1234,repe=5)

 medias1$modelo="Logística1"
 
 medias2<-cruzadalogistica(data=saheartbis,
 vardep="chd",listconti=c("age", "ldl", "famhist.Absent"),
 listclass=c(""), grupos=4,sinicio=1234,repe=5)

medias2$modelo="Logística2"

union1<-rbind(medias1,medias2)

par(cex.axis=0.5)
boxplot(data=union1,tasa~modelo,col="pink",main="TASA FALLOS")
boxplot(data=union1,auc~modelo,col="pink",main="AUC")

# ************************************
# TUNEANDO LA RED CON LOS DOS MODELOS CANDIDATOS
# ***********************************

avnnetgrid <-expand.grid(size=c(5,10,15,20),
 decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(chd~age+tobacco+famhist.Absent+typea+ldl,
 data=saheartbis,
method="avNNet",linout = FALSE,maxit=100,
 trControl=control,tuneGrid=avnnetgrid,
 repeats=5)

redavnnet

avnnetgrid <-expand.grid(size=c(5,10,15,20),
 decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(chd~age+famhist.Absent+ldl,
 data=saheartbis,
method="avNNet",linout = FALSE,maxit=100,
 trControl=control,tuneGrid=avnnetgrid,
 repeats=5)

redavnnet

# ************************************
# COMPARANDO LOS MODELOS FINALES
# ***********************************
 
medias3<-cruzadaavnnetbin(data=saheartbis,
 vardep="chd",listconti=c("age", "famhist.Absent",
  "typea", "ldl", "tobacco"),
 listclass=c(""),grupos=4,sinicio=1234,repe=5,
  size=c(5),decay=c(0.1),repeticiones=5,itera=200)

medias3$modelo="avnnet1"

medias4<-cruzadaavnnetbin(data=saheartbis,
 vardep="chd",listconti=c("age", "ldl", "famhist.Absent"),
 listclass=c(""),grupos=4,sinicio=1234,repe=5,
  size=c(10),decay=c(0.1),repeticiones=5,itera=200)

medias4$modelo="avnnet2"

union1<-rbind(medias1,medias2,medias3,medias4)

par(cex.axis=0.5)
boxplot(data=union1,tasa~modelo,col="pink",main="TASA FALLOS")
boxplot(data=union1,auc~modelo,col="pink",main="AUC")






