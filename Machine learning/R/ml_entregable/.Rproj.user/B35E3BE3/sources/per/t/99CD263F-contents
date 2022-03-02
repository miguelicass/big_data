# LIBRERIES

# install
# install.packages("reshape")
# install.packages('doParallel')

# load
library(ggplot2)
library(inspectdf) # EDAs automaticos
library(plotly)
library(dummies)
library(MASS)
library(caret)
library(plyr)
library(reshape)
library(randomForest)
library(tinytex)
library(doParallel)
library(evaluate)


# ENUNCIADO

# a) Se deben realizar pruebas suficientes para obtener una buena selección de variables, obteniendo uno o varios conjuntos de variables tentativos

# b) Se requiere la comparación entre los mejores algoritmos y regresión logística ;

# c) Se comprobará el efecto de la variación de los parámetros básicos de cada
# algoritmo (tuneado) (número de nodos en redes, shrink en gradient boosting, etc.).

# d) Los algoritmos a utilizar son obligatoriamente y como mínimo:
#    Redes Neuronales
#    Regresión Logística
#    Bagging
#    Random Forest
#    Gradient Boosting
#    Support Vector Machines
#    También si se quiere y para comprender los datos se puede probar con un simple árbol pero no es obligatorio.

# e) Es necesario utilizar validación cruzada, validación cruzada repetida o como mínimo training/test repetido.

# f) Es necesario hacer alguna prueba de ensamblado.

# TRABAJO
getwd()
# data <- read.csv("~/Desktop/ml_entregable/healthcare-dataset-stroke-data.csv")
data <- read.csv("./healthcare-dataset-stroke-data.csv")

# EDA 

#compruebo los tipos
str(data) 

# transfromamos en yes or no la variable obj
data$stroke<-ifelse(data$stroke==1,"Yes","No")

# convert to factor
data[,c(2,4,5,6,7,8,11,12)] <- lapply(data[,c(2,4,5,6,7,8,11,12)], factor)

# convert to nummeric
data$bmi <- as.numeric(data$bmi)

# comprobar tipos
str(data) 

# Comprobar observaciones de la var objetivo
length(filter(data, stroke == "Yes")[,1])
length(filter(data, stroke == "No")[,1])

# Horizontal bar plot for categorical column composition
x <- inspect_cat(data)
show_plot(x)

# Correlation betwee numeric columns + confidence intervals
x <- inspect_cor(data)
show_plot(x)

# Occurence of NAs in each column ranked in descending order
x <- inspect_na(data)
show_plot(x)


# FEATURE INGENIERING

# lista de variables
# dput(names(data))
list_int <- c("id")
list_cont <- c("avg_glucose_level", "bmi", "age")
list_cat <- c("gender", "hypertension", "heart_disease", "ever_married", "work_type", "Residence_type", "smoking_status")
var_dep <- c("stroke")
col_var_dep<-data[,var_dep]

# ESTANDARIZACIÓN DE TODAS LAS VARIABLES CONTINUAS

# calc estandarizacion
means <-apply(data[,list_cont],2,mean,na.rm=TRUE)
sds<-sapply(data[,list_cont],sd,na.rm=TRUE)

# var continuas estandarizadas
stroke_data <- scale(data[,list_cont], center = means, scale = sds)
# stroke_data <- data.frame(cbind(stroke_data,col_var_dep))

# union continuas y categoricas
index_cont<-which(colnames(data)%in%list_cont) # index cont
stroke_data<-cbind(stroke_data,data[,-index_cont]) # join


# ELIMINAR MISSING
stroke_data<-na.omit(stroke_data,(!is.na(stroke_data)))
# length(filter(stroke_data, stroke == "Yes")[,1])
# length(filter(stroke_data, stroke == "No")[,1])


# DUMMIES 
stroke_data<- dummy.data.frame(stroke_data, list_cat, sep = ".")
#eliminamos los dummies pocos representados
stroke_data$work_type.Never_worked <- NULL

# # Make Valid Column Names 
colnames(stroke_data) <- make.names(colnames(stroke_data))


## SELECCION DE VARIABLES - LOGISTICA

# Selección de variables por el metodo stepAIC
full<-glm(factor(stroke)~.,data=stroke_data,family = binomial(link="logit"))
null<-glm(factor(stroke)~1,data=stroke_data,family = binomial(link="logit"))

# aplicamos stepAIC
seleccionAIC<-stepAIC(null,scope=list(upper=full),direction="both")
seleccionBIC<-stepAIC(null,scope=list(upper=full),direction="both")

# variables escogidas AIC
dput(names(seleccionAIC$coefficients))
# variables escogidas BIC
dput(names(seleccionBIC$coefficients))

# versión formula AIC
formula(seleccionAIC)
# versión formula BIC
formula(seleccionBIC)

# AIC
seleccionAIC
# BIC
seleccionBIC

# c("age", "avg_glucose_level", "hypertension.0", "work_type.Self.employed", "smoking_status.smokes", "heart_disease.0")
# factor(stroke) ~ age + avg_glucose_level + hypertension.0 + work_type.Self.employed + smoking_status.smokes + heart_disease.0

# ************************************
# APLICANDO steprepetidobinaria
# ************************************
source("funcion steprepetido binaria.R")

list_var <- names(stroke_data[,-24]) # dput

# AIC
listaAIC<-steprepetidobinaria(data=stroke_data,
                           vardep=var_dep,listconti=list_var,
                           sinicio=12340,
                           sfinal=12390,porcen=0.8,criterio="AIC")

tablaAIC <- listaAIC[[1]]

# BIC
listaBIC <- steprepetidobinaria(data=stroke_data,
                              vardep=var_dep,listconti=list_var,
                              sinicio=12340,
                              sfinal=12390,porcen=0.8,criterio="BIC")

tablaBIC <- listaBIC[[1]]

# table freq
head(tablaAIC,5)
head(tablaBIC,5)



# MODELOS

# TUNEO CON CARET

# REGRESION LOGISTICA
set.seed(1234967)

control<-trainControl(method = "cv", 
                      number=4,savePredictions = "all") # repeats=5

reg_log <- train(factor(stroke) ~ age + avg_glucose_level + hypertension.0 
                 + work_type.Self.employed + smoking_status.smokes 
                 + heart_disease.0,
                 data=stroke_data,method="glm",trControl=control)

reg_log



# RED 
set.seed(1234967)

control<-trainControl(method = "cv", 
                      number=4,savePredictions = "all")

nnetgrid <- expand.grid(size=c(5,10,15,20),decay=c(0.01,0.1,0.001),bag=F)

avnnet <- train(factor(stroke) ~ age + avg_glucose_level + hypertension.0 
                + work_type.Self.employed + smoking_status.smokes 
                + heart_disease.0,
                data=stroke_data, method="avNNet",linout = TRUE,maxit=100,
                trControl=control,repeats=5, tuneGrid=nnetgrid)

avnnet



# ARBOL
# caret
set.seed(1234967)

control<-trainControl(method = "cv",number=4,savePredictions = "all")

arbolgrid <-  expand.grid(cp=c(0,0.001,0.01,0.1))

arbol <- train(factor(stroke) ~ age + avg_glucose_level + hypertension.0 
               + work_type.Self.employed + smoking_status.smokes 
               + heart_disease.0,
               data=stroke_data, method="rpart",minbucket=30,
               trControl=control,tuneGrid=arbolgrid)


arbol

# sal<-arbolcaret$pred
# salconfu<-confusionMatrix(sal$pred,sal$obs)
# salconfu

# curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
# auc<-curvaroc$auc
# auc
# plot(roc(response=sal$obs,predictor=sal$Yes))



# BANGGIN
set.seed(1234967)

rfgrid<-expand.grid(mtry=c(6))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE)

bangging <-  train(factor(stroke) ~ age + avg_glucose_level + hypertension.0 
                  + work_type.Self.employed + smoking_status.smokes 
                  + heart_disease.0, data=stroke_data,method="rf",
                  trControl=control,tuneGrid=rfgrid,linout = FALSE,
                  ntree=1000,nodesize=10,replace=TRUE) # sampsize=200

bangging


# # IMPORTANCIA DE VARIABLES
# summary(gbm)
# tabla<-summary(gbm)
# par(cex=1.5,las=2) 
# barplot(tabla$rel.inf,names.arg=row.names(tabla))



# RANDOM FORES
set.seed(1234967)

rfgrid<-expand.grid(mtry=c(3,4,5,6))

control<-trainControl(method = "cv",number=4,savePredictions = "all", 
                      classProbs=TRUE)

rf <- train(factor(stroke) ~ age + avg_glucose_level + hypertension.0 
            + work_type.Self.employed + smoking_status.smokes 
            + heart_disease.0, data=stroke_data,method="rf",
            trControl=control,tuneGrid=rfgrid,linout = FALSE,ntree=500,
            nodesize=12,replace=TRUE,importance=TRUE)

rf


# GRADIENT BOOSTING
set.seed(1234967)

gbmgrid<-expand.grid(shrinkage=c(0.1,0.05,0.03,0.01,0.001),
                     n.minobsinnode=c(5,10,20),
                     n.trees=c(100,500,1000,5000),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                     classProbs=TRUE)

gbm <- train(factor(stroke) ~ age + avg_glucose_level + hypertension.0 + work_type.Self.employed + smoking_status.smokes + heart_disease.0,
      data=stroke_data,
      method="gbm",trControl=control,tuneGrid=gbmgrid,
      distribution="bernoulli", bag.fraction=1,verbose=FALSE)
gbm

plot(gbm)


# XGBOOST
set.seed(1234967)

xgbmgrid <- expand.grid( min_child_weight=c(5,10,20), 
                         eta=c(0.1,0.05,0.03,0.01,0.001), 
                         nrounds=c(100,500,1000,5000),
                       max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control <- trainControl(method = "cv",number=4,savePredictions = "all", 
                        classProbs=TRUE)

xgbm <- train(factor(stroke) ~ age + avg_glucose_level + hypertension.0 + work_type.Self.employed + smoking_status.smokes + heart_disease.0
              ,data=stroke_data, method="xgbTree",trControl=control,
              tuneGrid=xgbmgrid,verbose=FALSE) 

xgbm
plot(xgbm)



# SVM

# SVM linear
SVMgrid<-expand.grid(C=c(0.0001,0.001,0.01,0.05,0.1,0.2,0.5,1,2,5,10))

control<-trainControl(method = "cv",number=4,savePredictions = "all")

n_cores <- detectCores() - 1  
cl <- makeCluster(n_cores, type="PSOCK")  
registerDoParallel(cl) 

start.time <- proc.time()
SVM_line <- train(data=stroke_data, factor(stroke) ~ age + avg_glucose_level 
                  + hypertension.0 + work_type.Self.employed 
                  + smoking_status.smokes + heart_disease.0,
                  method="svmLinear",trControl=control,
                  tuneGrid=SVMgrid,verbose=FALSE,allowParallel= TRUE)

getDefaultCluster()

stop.time <- proc.time()
run.time <- stop.time- start.time
run.time

SVM_line
# plot
plot(SVM_line$results$C,SVM_line$results$Accuracy)



# SVMPoly

SVMgrid<-expand.grid(C=c(0.0001,0.1,1,10),
                     degree=c(2,3,4),scale=c(0.1,1,5))

control<-trainControl(method = "cv",
                      number=2,savePredictions = "all")


SVM_Poly <- train(data=stroke_data,factor(stroke) ~ age + avg_glucose_level 
                  + hypertension.0 + work_type.Self.employed 
                  + smoking_status.smokes + heart_disease.0,
                  method="svmPoly",trControl=control,
                  tuneGrid=SVMgrid,verbose=FALSE)


SVM_Poly
# plot
dat<-as.data.frame(SVM_Poly$results)
ggplot(dat, aes(x=factor(C), y=Accuracy,
                color=factor(degree),pch=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)


# SVMRBF
SVMgrid<-expand.grid(C=c(0.0001,0.001,0.01,0.05,0.1,0.2,0.5,1,2,5,10,30),
                     sigma=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,30))

control<-trainControl(method = "cv",
                      number=2,savePredictions = "all")

SVM_Radial <- train(data=stroke_data, factor(stroke) ~ age + avg_glucose_level 
                    + hypertension.0 + work_type.Self.employed 
                    + smoking_status.smokes + heart_disease.0, 
                    method="svmRadial",trControl=control,
                    tuneGrid=SVMgrid,verbose=FALSE)

SVM_Radial
#plot
dat<-as.data.frame(SVM_Radial$results)
ggplot(dat, aes(x=factor(C), y=Accuracy,
                color=factor(sigma))) +
  geom_point(position=position_dodge(width=0.5),size=3)


########################################################

# MODELO GANADOR Y ANALISIS SESGO-VARIANZA

#LOGISTICA
source("cruzadas avnnet y log binaria.R")

medias_model_1 <- cruzadalogistica(data=stroke_data,
                    vardep="stroke",listconti= c("age", "avg_glucose_level",
                      "hypertension.0","work_type.Self.employed", 
                      "smoking_status.smokes", "heart_disease.0"),
                    listclass=c(""), grupos=4,sinicio=1234967,repe=5)

medias_model_1$modelo="Logística"


# RED 
medias_model_2 <- cruzadaavnnetbin(data=stroke_data,
                     vardep="stroke",listconti= c("age", "avg_glucose_level",
                        "hypertension.0","work_type.Self.employed", 
                        "smoking_status.smokes", "heart_disease.0"),
                     listclass=c(""),grupos=4,sinicio=1234967,repe=5,
                     size=c(5),decay=c(0.1),repeticiones=5,itera=100,
                     trace = TRUE)

medias_model_2$modelo="avnnet"


# ARBOL
source ("cruzada arbolbin.R")

medias_model_3 <- cruzadaarbolbin(data=stroke_data,
                    vardep="stroke",listconti= c("age", "avg_glucose_level",
                       "hypertension.0","work_type.Self.employed", 
                       "smoking_status.smokes", "heart_disease.0"),
                    listclass=c(""),grupos=4,sinicio=1234967,repe=5,
                    cp=c(0.1),minbucket =30)

medias_model_3$modelo="arbol"


# BANGGIN
source ("cruzada rf binaria.R")

medias_model_4 <- cruzadarfbin(data=stroke_data,
                   vardep="stroke",listconti= c("age", "avg_glucose_level",
                      "hypertension.0","work_type.Self.employed", 
                      "smoking_status.smokes", "heart_disease.0"),
                   listclass=c(""),grupos=4,sinicio=1234967,repe=5,
                   nodesize=10,mtry=6,ntree=1000,replace=TRUE)

medias_model_4$modelo="bagging"


# RANDOM FORES
medias_model_5 <- cruzadarfbin(data=stroke_data,
                     vardep="stroke",listconti= c("age", "avg_glucose_level",
                        "hypertension.0","work_type.Self.employed", 
                        "smoking_status.smokes", "heart_disease.0"),
                     listclass=c(""),grupos=4,sinicio=1234967,repe=5,
                     nodesize=12,mtry=3,ntree=500,replace=TRUE)

medias_model_5$modelo="rf"


# GRADIENT BOOSTING
source ("cruzada gbm binaria.R")

medias_model_6 <- cruzadagbmbin(data=stroke_data,
                    vardep="stroke",listconti= c("age", "avg_glucose_level",
                       "hypertension.0","work_type.Self.employed", 
                       "smoking_status.smokes", "heart_disease.0"),
                    listclass=c(""),grupos=4,sinicio=1234967,repe=5,
                    n.minobsinnode=5,shrinkage=0.015,n.trees=100,
                    interaction.depth=2)

medias_model_6$modelo="gbm"


# XGBOOST
source ("cruzada xgboost binaria.R")

medias_model_7 <-cruzadaxgbmbin(data=stroke_data,
                    vardep="stroke",listconti= c("age", "avg_glucose_level",
                       "hypertension.0","work_type.Self.employed", 
                       "smoking_status.smokes", "heart_disease.0"),
                    listclass=c(""),grupos=4,sinicio=1234967,repe=5,
                    min_child_weight=5,eta=0.015,nrounds=100,max_depth=6,
                    gamma=0,colsample_bytree=1,subsample=1,
                    alpha=0,lambda=0,lambda_bias=0)

medias_model_7$modelo="xgbm"


# SVM

# SVM linear
source ("cruzada SVM binaria lineal.R")

medias_model_8 <- cruzadaSVMbin(data=stroke_data,
                    vardep="stroke",listconti= c("age", "avg_glucose_level",
                       "hypertension.0","work_type.Self.employed", 
                       "smoking_status.smokes", "heart_disease.0"),
                    listclass=c(""),grupos=4,sinicio=1234967,repe=5,
                    C=0.0001)

medias_model_8$modelo="SVMLin"


# SVMPoly
source ("cruzada SVM binaria polinomial.R")

medias_model_9 <- cruzadaSVMbinPoly(data=stroke_data,
                    vardep="stroke",listconti= c("age", "avg_glucose_level",
                     "hypertension.0","work_type.Self.employed", 
                     "smoking_status.smokes", "heart_disease.0"),
                    listclass=c(""),grupos=4,sinicio=1234967,repe=5,
                    C=0.0001,degree=2,scale=0.01)

medias_model_9$modelo="SVMPoly"


# SVMRBF
source ("cruzada SVM binaria RBF.R")

medias_model_10 <- cruzadaSVMbinRBF(data=stroke_data,
                      vardep="stroke",listconti= c("age", "avg_glucose_level",
                       "hypertension.0","work_type.Self.employed", 
                       "smoking_status.smokes", "heart_disease.0"),
                      listclass=c(""),grupos=4,sinicio=1234967,repe=5,
                      C=2,sigma=0.2)

medias_model_10$modelo="SVMRBF"


## PINTAR DATOS SESGO-VARIANZA
union_models<-rbind(medias_model_1,medias_model_2,medias_model_3,
                    medias_model_4,medias_model_5,medias_model_6,
                    medias_model_7,medias_model_8,medias_model_9,
                    medias_model_10)
# tasa fallos 
boxplot(data=union_models,tasa~modelo,col="pink",main="TASA FALLOS")
# auc
boxplot(data=union_models,auc~modelo,col="pink",main="AUC")



## ZOOM
union_models_zoom<-rbind(medias_model_1,medias_model_2,
                    medias_model_6,medias_model_7)
# tasa fallos 
boxplot(data=union_models_zoom,tasa~modelo,col="pink",main="TASA FALLOS")
# auc
boxplot(data=union_models_zoom,auc~modelo,col="pink",main="AUC")


# ENSAMBLADO
source("cruzadas ensamblado binaria fuente.R")

vardep<-"stroke"
listconti<-c("age", "avg_glucose_level",
             "hypertension.0","work_type.Self.employed", 
             "smoking_status.smokes", "heart_disease.0")
listclass<-c("")
grupos<-4
sinicio<-1234967
repe<-5

# REGRESION LOGISTICA
medias_model_1_en <- cruzadalogistica(data=stroke_data,
                      vardep=vardep,listconti=listconti,
                      listclass=listclass,grupos=grupos,
                      sinicio=sinicio,repe=repe)

medias_model_1_bis<-as.data.frame(medias_model_1_en[1])
medias_model_1_bis$modelo <- "logistica_en"
predi_model_1 <- as.data.frame(medias_model_1_en[2])
predi_model_1$logistica_en <- predi_model_1$Yes

# RED
medias_model_2_en <- cruzadaavnnetbin(data=stroke_data,
                      vardep=vardep,listconti=listconti,
                      listclass=listclass,grupos=grupos,
                      sinicio=sinicio,repe=repe, 
                      size=c(5),decay=c(0.1),repeticiones=5,itera=100,)

medias_model_2_bis<-as.data.frame(medias_model_2_en[1])
medias_model_2_bis$modelo <- "avnnet_en"
predi_model_2 <- as.data.frame(medias_model_2_en[2])
predi_model_2$avnnet_en <- predi_model_2$Yes

# GRADIENT BOOSTING
medias_model_6_en <- cruzadagbmbin(data=stroke_data,
                        vardep=vardep,listconti=listconti,
                        listclass=listclass,grupos=grupos,
                        sinicio=sinicio,repe=repe,
                        n.minobsinnode=5,shrinkage=0.015,n.trees=100,
                        interaction.depth=2)

medias_model_6_bis<-as.data.frame(medias_model_6_en[1])
medias_model_6_bis$modelo <- "gbm_en"
predi_model_6 <- as.data.frame(medias_model_6_en[2])
predi_model_6$gbm_en <- predi_model_6$Yes

# XGBOOST
medias_model_7_en <- cruzadaxgbmbin(data=stroke_data,
                      vardep=vardep,listconti=listconti,
                      listclass=listclass,grupos=grupos,
                      sinicio=sinicio,repe=repe,
                      min_child_weight=5,eta=0.015,nrounds=100,max_depth=6,
                      gamma=0,colsample_bytree=1,subsample=1,
                      alpha=0,lambda=0,lambda_bias=0)

medias_model_7_bis<-as.data.frame(medias_model_7_en[1])
medias_model_7_bis$modelo <- "xgbm_en"
predi_model_7 <- as.data.frame(medias_model_7_en[2])
predi_model_7$xgbm_en <- predi_model_7$Yes

union_models_bis<-rbind(medias_model_1_bis,medias_model_2_bis,
                        medias_model_6_bis,medias_model_7_bis)

# PLOT
par(cex.axis=0.5)
boxplot(data=union_models_bis,tasa~modelo)
boxplot(data=union_models_bis,auc~modelo)

# Empezamos con el ensamblado
uni_predi<-cbind(predi_model_1,predi_model_2,predi_model_6,predi_model_7)
uni_predi<- uni_predi[, !duplicated(colnames(uni_predi))]

# promedios
uni_predi$predi_11 <- (uni_predi$logistica_en+uni_predi$avnnet_en)/2
uni_predi$predi_12 <- (uni_predi$avnnet_en+uni_predi$gbm_en
                       +uni_predi$xgbm_en)/3
uni_predi$predi_13 <- (uni_predi$logistica_en+uni_predi$avnnet_en
                        +uni_predi$gbm_en+uni_predi$xgbm_en)/4

# PROCESADO DE ENSAMBLADOS
listado<-c("logistica_en", "avnnet_en",
           "gbm_en", "xgbm_en", "predi_11", 
           "predi_12",  "predi_13")

# Defino funcion tasafallos
tasafallos<-function(x,y) {
  confu<-confusionMatrix(x,y)
  tasa<-confu[[3]][1]
  return(tasa)
}

auc<-function(x,y) {
  curvaroc<-roc(response=x,predictor=y)
  auc<-curvaroc$auc
  return(auc)
}

# Se obtiene el numero de repeticiones CV y se calculan las medias por repe en
# el data frame medias0
repeticiones<-nlevels(factor(uni_predi$Rep))
uni_predi$Rep<-as.factor(uni_predi$Rep)
uni_predi$Rep<-as.numeric(uni_predi$Rep)


medias0<-data.frame(c())
for (prediccion in listado){
  uni_predi$proba<-uni_predi[,prediccion]
  uni_predi[,prediccion]<-ifelse(uni_predi[,prediccion]>0.5,"Yes","No")
  for (repe in 1:repeticiones)
  {
    paso <- uni_predi[(uni_predi$Rep==repe),]
    pre<-factor(paso[,prediccion])
    archi<-paso[,c("proba","obs")]
    archi<-archi[order(archi$proba),]
    obs<-paso[,c("obs")]
    tasa=1-tasafallos(pre,obs)
    t<-as.data.frame(tasa)
    t$modelo<-prediccion
    auc<-suppressMessages(auc(archi$obs,archi$proba))
    t$auc<-auc
    medias0<-rbind(medias0,t)
  }
}

# PLOT
par(cex.axis=0.5,las=2)
# FALLOS
boxplot(data=medias0,tasa~modelo,col="pink",main="TASA FALLOS")
# AUC
boxplot(data=medias0,auc~modelo,col="pink",main="AUC")


# ZOOM
listado_zoom<-c("logistica_en", "avnnet_en",
               "predi_11", "predi_12", "predi_13")
medias0$modelo<-as.character(medias0$modelo)
mediasver<-medias0[medias0$modelo %in% listado_zoom,]
mediasver$modelo <- with(mediasver,
                         reorder(modelo,auc, median))

# PLOT
par(cex.axis=0.5,las=2)
# FALLOS
boxplot(data=mediasver,tasa~modelo,col="pink",main="TASA FALLOS")
# AUC
boxplot(data=mediasver,auc~modelo,col="pink",main='AUC')


