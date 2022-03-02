

load("saheartbis.Rda")

# ******************************************
# MUESTREO ALEATORIO SIMPLE Y ESTRATIFICADO
# ******************************************

# Muestreo aleatorio simple
# Para extraer muestra aleatoria simple de 100 observaciones

library(dplyr)
 
muestra<-sample_n(saheartbis,size=100)


# Muestreo aleatorio estratificado por la variable dependiente

# table(saheartbis$chd)
# prop.table(table(saheartbis$chd))
# Se ve que hay un 65% de No y un 35% de Yes

# Al estratificar mantenemos esos % en la muestra obtenida

# Tomamos sample_frac(0.25) (1/4 de los 462 para tener aproximadamente 100 observaciones finales)

muestra2<- saheartbis %>%
  group_by(chd) %>%
  sample_frac(0.25)


table(muestra2$chd)
prop.table(table(muestra2$chd))

# ******************************************
# EJEMPLOS CONTROL DEL TIEMPO EN R
# ******************************************

# start_time <- Sys.time()
# 
# end_time <- Sys.time()
# 
# tiempo_total<-end_time - start_time



# Ejemplo spam CV de 10 grupos con 30% observaciones training y tuneado de una gran rejilla

# Después comparamos con hacerlo con todas las observaciones

# ******************************************
# DIVISIÓN SIMPLE DE UN DATA FRAME EN TRAIN TEST
# ******************************************

# Primero obtenemos una muestra de 30% de observaciones, dividiendo en train test el archivo

# Se calcula cuantas observaciones son el 30% en el archivo spam
sample_size = floor(0.3*nrow(spam))

# sample_size=1380

# Se crean los índices para train test
set.seed(12345)
indices = sample(seq_len(nrow(spam)),size = sample_size)

# Se crean los archivos train test
train =spam[indices,]
test =spam[-indices,]


# Como hemos dicho vamos a trabajar solo con los train

# Control tiempo inicio
start_time <- Sys.time()

gbmgrid<-expand.grid(shrinkage=c(0.1,0.01,0.001,0.0001),
 n.minobsinnode=c(10,20),
 n.trees=c(100,200,500,1000,2000,5000),
 interaction.depth=c(2))

set.seed(12345)
control<-trainControl(method = "CV",number=10,savePredictions = "all",classProbs=TRUE) 

gbm<- train(factor(spam)~.,data=train,tuneGrid=gbmgrid,
 method="gbm",trControl=control,distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm

# Control tiempo final

end_time <- Sys.time()
 
tiempo_total<-end_time - start_time

tiempo_total


# Time difference of 12.04829 mins

#   shrinkage  n.minobsinnode  n.trees  Accuracy   Kappa    
#   1e-04      10               100     0.6144939  0.0000000
#   1e-04      10               200     0.6144939  0.0000000
#   1e-04      10               500     0.6144939  0.0000000
#   1e-04      10              1000     0.6144939  0.0000000
#   1e-04      10              2000     0.6144939  0.0000000
#   1e-04      10              5000     0.8274924  0.6060646
#   1e-04      20               100     0.6144939  0.0000000
#   1e-04      20               200     0.6144939  0.0000000
#   1e-04      20               500     0.6144939  0.0000000
#   1e-04      20              1000     0.6144939  0.0000000
#   1e-04      20              2000     0.6144939  0.0000000
#   1e-04      20              5000     0.8274924  0.6060646
#   1e-03      10               100     0.6144939  0.0000000
#   1e-03      10               200     0.6144939  0.0000000
#   1e-03      10               500     0.8274924  0.6060646
#   1e-03      10              1000     0.8695323  0.7107994
#   1e-03      10              2000     0.9014325  0.7866719
#   1e-03      10              5000     0.9173695  0.8220123
#   1e-03      20               100     0.6144939  0.0000000
#   1e-03      20               200     0.6144939  0.0000000
#   1e-03      20               500     0.8274924  0.6060646
#   1e-03      20              1000     0.8695323  0.7107994
#   1e-03      20              2000     0.9014325  0.7866719
#   1e-03      20              5000     0.9180942  0.8236282
#   1e-02      10               100     0.8666337  0.7040989
#   1e-02      10               200     0.9014325  0.7866719
#   1e-02      10               500     0.9173695  0.8220123
#   1e-02      10              1000     0.9318573  0.8540395
#   1e-02      10              2000     0.9333171  0.8575229
#   1e-02      10              5000     0.9354909  0.8629139
#   1e-02      20               100     0.8666337  0.7040989
#   1e-02      20               200     0.9014325  0.7866719
#   1e-02      20               500     0.9180942  0.8236282
#   1e-02      20              1000     0.9289692  0.8479959
#   1e-02      20              2000     0.9340521  0.8595193
#   1e-02      20              5000     0.9304290  0.8526953
#   1e-01      10               100     0.9318573  0.8539168
#   1e-01      10               200     0.9354858  0.8623332
#   1e-01      10               500     0.9340469  0.8600427
#   1e-01      10              1000     0.9347716  0.8615995
#   1e-01      10              2000     0.9296990  0.8511634
#   1e-01      10              5000     0.9260863  0.8435132
#   1e-01      20               100     0.9311589  0.8530143
#   1e-01      20               200     0.9347821  0.8610303
#   1e-01      20               500     0.9297043  0.8509079
#   1e-01      20              1000     0.9275355  0.8461920
#   1e-01      20              2000     0.9275356  0.8464272
#   1e-01      20              5000     0.9224684  0.8359014
# 
# Tuning parameter 'interaction.depth' was held constant at a value of 2
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were n.trees = 5000, interaction.depth = 2, shrinkage = 0.01 and n.minobsinnode = 10.

# Si hacemos el tuneado con todos los datos:

# gbm<- train(factor(spam)~.,data=spam,tuneGrid=gbmgrid,
#  method="gbm",trControl=control,
#  distribution="bernoulli", bag.fraction=1,verbose=FALSE)



# Time difference of 38.96596 mins

#   shrinkage  n.minobsinnode  n.trees  Accuracy   Kappa    
#   1e-04      10               100     0.6059554  0.0000000
#   1e-04      10               200     0.6059554  0.0000000
#   1e-04      10               500     0.6059554  0.0000000
#   1e-04      10              1000     0.6059554  0.0000000
#   1e-04      10              2000     0.6059554  0.0000000
#   1e-04      10              5000     0.8219975  0.5991490
#   1e-04      20               100     0.6059554  0.0000000
#   1e-04      20               200     0.6059554  0.0000000
#   1e-04      20               500     0.6059554  0.0000000
#   1e-04      20              1000     0.6059554  0.0000000
#   1e-04      20              2000     0.6059554  0.0000000
#   1e-04      20              5000     0.8219975  0.5991490
#   1e-03      10               100     0.6059554  0.0000000
#   1e-03      10               200     0.6059554  0.0000000
#   1e-03      10               500     0.8222149  0.5995952
#   1e-03      10              1000     0.8750307  0.7273842
#   1e-03      10              2000     0.9072008  0.8013130
#   1e-03      10              5000     0.9315439  0.8551018
#   1e-03      20               100     0.6059554  0.0000000
#   1e-03      20               200     0.6059554  0.0000000
#   1e-03      20               500     0.8222149  0.5995952
#   1e-03      20              1000     0.8750307  0.7273842
#   1e-03      20              2000     0.9072008  0.8013130
#   1e-03      20              5000     0.9315439  0.8551018
#   1e-02      10               100     0.8745959  0.7264963
#   1e-02      10               200     0.9080704  0.8032530
#   1e-02      10               500     0.9315439  0.8551236
#   1e-02      10              1000     0.9415420  0.8766334
#   1e-02      10              2000     0.9469744  0.8883513
#   1e-02      10              5000     0.9537103  0.9028800
#   1e-02      20               100     0.8745959  0.7264963
#   1e-02      20               200     0.9080704  0.8032530
#   1e-02      20               500     0.9315439  0.8551236
#   1e-02      20              1000     0.9417594  0.8771052
#   1e-02      20              2000     0.9482774  0.8911627
#   1e-02      20              5000     0.9530576  0.9014714
#   1e-01      10               100     0.9419773  0.8775577
#   1e-01      10               200     0.9482802  0.8911360
#   1e-01      10               500     0.9539281  0.9033567
#   1e-01      10              1000     0.9541446  0.9039796
#   1e-01      10              2000     0.9569693  0.9099731
#   1e-01      10              5000     0.9517504  0.8991881
#   1e-01      20               100     0.9419773  0.8776058
#   1e-01      20               200     0.9495813  0.8939154
#   1e-01      20               500     0.9530572  0.9015502
#   1e-01      20              1000     0.9534915  0.9025473
#   1e-01      20              2000     0.9528374  0.9013120
#   1e-01      20              5000     0.9495770  0.8944796
# 
# Tuning parameter 'interaction.depth' was held constant at a value of 2
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were n.trees = 2000, interaction.depth = 2, shrinkage = 0.1 and n.minobsinnode = 10.

# Time difference of 38.96596 mins

# *******************************************************************************
# Vemos que la solución con un 30% de la muestra era suficientemente buena, con
# n.trees = 5000, interaction.depth = 2, shrinkage = 0.01 and n.minobsinnode = 10.
# y un Accuracy medio al usarlo sobre todos los datos de 0.9537103
# *******************************************************************************

# ******************************************
# EJEMPLO PARALLEL
# ******************************************


library(parallel)
library(doParallel)

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing

gbmgrid<-expand.grid(shrinkage=c(0.1,0.01,0.001,0.0001),
 n.minobsinnode=c(10,20),
 n.trees=c(100,200,500,1000,2000,5000),
 interaction.depth=c(2))

set.seed(12345)
control<-trainControl(method = "CV",number=10,savePredictions = "all",classProbs=TRUE) 

gbm<- train(factor(spam)~.,data=spam,tuneGrid=gbmgrid,
 method="gbm",trControl=control,distribution="bernoulli", bag.fraction=1,verbose=FALSE)


stopCluster(cluster) # shut down the cluster 
registerDoSEQ(); #  force R to return to single threaded processing
GS_T1 <- Sys.time()
GS_T1-GS_T0

gbm


# ******************************************
# LA FUNCIÓN PREDICT EN R
# ******************************************

# ******************************************
# Ejemplo 1: variable dependiente binaria
# ******************************************

# Para el ejemplo, dividimos el archivo en una muestra train para 
# construir el modelo y una test para aplicarlo sobre ella.
# 
# En cualquer aplicación práctica, los datos test son otro data frame 
# diferente del utilizado para construir el modelo.

load("spam.Rda")
sample_size = floor(0.3*nrow(spam))

# sample_size=1380

# Se crean los índices para train test
set.seed(12345)
indices = sample(seq_len(nrow(spam)),size = sample_size)

# Se crean los archivos train test
train =spam[indices,]
test =spam[-indices,]

# 1) Creamos un objeto modelo con caret, method = "none" en trainControl
# Los parámetros se fijan con los mejores obtenidos con el proceso
# de tuneado.

gbmgrid<-expand.grid(shrinkage=c(0.01),
 n.minobsinnode=c(10),
 n.trees=c(5000),
 interaction.depth=c(2))

set.seed(12345)
control<-trainControl(method = "none",savePredictions = "all",classProbs=TRUE) 

gbm<- train(factor(spam)~.,data=train,tuneGrid=gbmgrid,
 method="gbm",trControl=control,
 distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm

# 2) Aplicamos el objeto modelo creado sobre datos test

# a) Con probabilidades es predicciones1 y con corte 0.5 predicciones2
# pero lo pone en un factor y es difícil de tratar

predicciones1<-predict(gbm,test,type = "prob")
predicciones2<-as.data.frame(predict(gbm,test,type="raw"))

# Lo mejor es crear un data frame con las probabilidades y la clase 
# más probable según corte 0.5

library(dplyr)

prediccionestodas<-predict(gbm,test,type = "prob")%>%
mutate('pred'=names(.)[apply(., 1, which.max)]) 

prediccionestodas

# Se obtienen tres columnas, las dos primeras son las probabilidades predichas
# y la tercera la predicción con punto de corte 0.5
#       email         spam  pred
# 1    1.164679e-01 8.835321e-01  spam
# 2    2.292862e-04 9.997707e-01  spam
# 3    7.174668e-03 9.928253e-01  spam
# 4    7.174668e-03 9.928253e-01  spam

# 3) Medidas de performance sobre datos test
# 
# Se utilizará la función confusionMatrix de caret, para lo que hay 
# que pasar a factor las columnas predichas y original,
# y la función roc del paquete pORC para calcular el auc

prediccionestodas$pred<-as.factor(prediccionestodas$pred)
test$spam<-as.factor(test$spam)

salconfu<-confusionMatrix(prediccionestodas$pred,test$spam)
salconfu

library(pROC)

curvaroc<-roc(response=test$spam,predictor=prediccionestodas$spam)
auc<-curvaroc$auc

plot(roc(response=test$spam,predictor=prediccionestodas$spam))

# ******************************************
# Ejemplo 2: variable dependiente continua
# En este caso no se pone classProbs=TRUE en control
# ******************************************
load("compress.Rda")

sample_size = floor(0.3*nrow(compress))

# sample_size=1380

# Se crean los índices para train test
set.seed(12345)
indices = sample(seq_len(nrow(compress)),size = sample_size)

# Se crean los archivos train test
train =compress[indices,]
test =compress[-indices,]

# 1) Creamos un objeto modelo con caret, method = "none" en trainControl
# Los parámetros se fijan con los mejores obtenidos con el proceso
# de tuneado. En este caso no se pone classProbs=TRUE en control al ser la 
# variable dependiente continua

gbmgrid<-expand.grid(shrinkage=c(0.1),
 n.minobsinnode=c(10),
 n.trees=c(20000),
 interaction.depth=c(2))

set.seed(12345)
control<-trainControl(method = "none",savePredictions = "all") 

gbm<-train(cstrength~age+water+cement+blast,data=train,tuneGrid=gbmgrid,
 method="gbm",trControl=control)

# 2) Se obtienen las predicciones con predict, se pasa a data frame y 
# se renombra la columna de predicciones

predicciones<-as.data.frame(predict(gbm,test))

colnames(predicciones)<-"predi"

# 3) Se calcula medida de performance

errorMSE<-mean((predicciones$predi-test$cstrength)^2)




# *******************************************************************************
# USO DE H2o
# *******************************************************************************

# *******************************************************************************
# INSTALACIÓN 
# *******************************************************************************
# The following two commands remove any previously installed H2O packages for R.

# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# 
# # Next, we download packages that H2O depends on.
# pkgs <- c("RCurl","jsonlite")
# for (pkg in pkgs) {
# if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
# 
# # Now we download, install and initialize the H2O package for R.
# install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-yates/2/R")


# INICIALIZACIÓN

# h2o.init(nthreads=1) permite control de reproducibilidad

# h2o.init(nthreads=8) es mucho más rápido aunque los resultados al dividirlo en cores
# no son reproducibles

# h2o.shutdown() permite cerrar la aplicación que come mucha memoria (ver administrador de tareas en windows
# y observar como h2o usa mucho la CPU y los recursos de la máquina a tope)

library(h2o)

# EJEMPLO DE VELOCIDAD
# COMPARAMOS CON gbm del caret

# Primero calculamos el tiempo de hacer CV de 10 grupos con todas las observaciones con caret,
# con parámetros más o menos óptimos, accuracy 0.9528374

load("spam.rda")

library(caret)
start_time <- Sys.time()

gbmgrid<-expand.grid(shrinkage=c(0.1),
 n.minobsinnode=c(10),
 n.trees=c(2000),
 interaction.depth=c(2))

set.seed(12345)
control<-trainControl(method = "CV",number=10,savePredictions = "all",classProbs=TRUE) 

gbm<- train(factor(spam)~.,data=spam,tuneGrid=gbmgrid,
 method="gbm",trControl=control,distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm

# Control tiempo final

end_time <- Sys.time()
tiempo_total<-end_time - start_time

tiempo_total


# Time difference of 2.185471 mins


library(h2o)

# Pongo un solo cluster para que los resultados sean reproducibles.
# Para más velocidad se puede poner 8, es mejor, y lo haremos después

h2o.init(nthreads=1) 

# Para h2o Lo primero es traducir el archivo a h2o

spam$spam<-as.factor(spam$spam)

train<- as.h2o(spam)

# Después hay que verificar los números de variables en  las columnas
# y si es necesario reordenarlas. En spamm y=58, x=1:57

start_time <- Sys.time()

gbm <- h2o.gbm(x = 1:57,   y = 58,
training_frame = train,ntrees = 2000,learn_rate=0.1,min_rows = 10,nfolds=10)

gbm
end_time <- Sys.time()
tiempo_total<-end_time - start_time

tiempo_total

# Time difference of 6.228595 mins
# accuracy                 0.96082205

# Vemos que se tarda mucho, pero si cambiamos nthread a 8:

h2o.shutdown()
h2o.init(nthreads=8) 

train<- as.h2o(spam)

start_time <- Sys.time()

gbm<- h2o.gbm(x = 1:57,   y = 58,  training_frame = train,
 ntrees = 2000,learn_rate=0.1,min_rows = 10,nfolds=10)

gbm
end_time <- Sys.time()
tiempo_total<-end_time - start_time

tiempo_total


# Time difference of 1.597189 mins

# accuracy                 0.96208805


# ************************************
# Pruebas con AutoML
# ************************************

start_time <- Sys.time()

# Nota: en lugar de poner max_models=m, 
# se puede limitar el tiempo de proceso con max_runtime_secs = 60 por ejemplo

# Los modelos que prueba con AutoML por defecto son: 

# Redes (h20.deeplearning)
# Gbm (h20.gbm)
# Random forest (h20.drf)
# Logística o regresión, ambos LASSO (h20.glm)
# Ensamblado (stacking) de varios modelos


  aml <- h2o.automl(x = 1:57,y=58,
  training_frame = train,max_runtime_secs = 60, seed = 1,
   keep_cross_validation_predictions=TRUE,nfolds=4)
  

lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)


aml@leader


end_time <- Sys.time()

end_time - start_time


aml@leader

# "GBM_grid_1_AutoML_20191108_122504_model_5"

modelo2 <- h2o.getModel("StackedEnsemble_BestOfFamily_AutoML_20200321_101152")

aml@leader@parameters


gbm1<- h2o.gbm(x = 1:57,   y = 58,  training_frame = train,
 ntrees = 156,learn_rate=0.05,min_rows = 5,nfolds=10,sample_rate=0.9,
 col_sample_rate=0.4,col_sample_rate_per_tree=0.4)

gbm1

gbm2<- h2o.gbm(x = 1:57,   y = 58,  training_frame = train,
 ntrees = 200,learn_rate=0.05,min_rows = 5,nfolds=10,sample_rate=0.9,
 col_sample_rate=1,col_sample_rate_per_tree=1)


gbm2




