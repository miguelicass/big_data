
load("compressbien.Rda")

library(caret)

# Training test una sola vez
control<-trainControl(method = "LGOCV",
 p=0.8,number=1,savePredictions = "all") 

# Los parámetros a probar-tunear se ponen en una rejilla(grid)
# Cada paquete-función permite tunear una serie limitada de parámetros
# Desgraciadamente no se puede manipular la semilla de la red
 # así que la  semilla de aleatorización train es la misma
 # que la de inicialización de pesos

nnetgrid <-  expand.grid(size=c(5,10,15,20),decay=c(0.01,0.1,0.001))

set.seed(123)

rednnet<- train(cstrength~age+water,
 data=compressbien,
 method="nnet",linout = TRUE,maxit=100,
 trControl=control,tuneGrid=nnetgrid)

rednnet


# Training test repetido
control<-trainControl(method = "LGOCV",
 p=0.8,number=5,savePredictions = "all")

set.seed(123)
nnetgrid <-  expand.grid(size=c(5,10,15,20),decay=c(0.01,0.1,0.001))

rednnet<- train(cstrength~age+water,
 data=compressbien,
 method="nnet",linout = TRUE,maxit=100,
 trControl=control,tuneGrid=nnetgrid)

rednnet


# Validación cruzada una sola vez
control<-trainControl(method = "cv",
 number=4,savePredictions = "all") 

set.seed(123)
nnetgrid <-  expand.grid(size=c(5,10,15,20),decay=c(0.01,0.1,0.001))

rednnet<- train(cstrength~age+water,
 data=compressbien,
 method="nnet",linout = TRUE,maxit=100,
 trControl=control,tuneGrid=nnetgrid)

rednnet

# Validación cruzada repetida
control<-trainControl(method = "repeatedcv",
 number=4,repeats=5,savePredictions = "all") 

set.seed(123)
nnetgrid <-  expand.grid(size=c(5,10,15,20),decay=c(0.01,0.1,0.001))

rednnet<- train(cstrength~age+water,
 data=compressbien,
 method="nnet",linout = TRUE,maxit=100,
 trControl=control,tuneGrid=nnetgrid)

rednnet

# UTILIZANDO avNNet

# Validación cruzada repetida
control<-trainControl(method = "repeatedcv",
 number=4,repeats=5,savePredictions = "all") 

set.seed(123)
nnetgrid <-  expand.grid(size=c(15),decay=c(0.01),bag=F)

rednnet<- train(cstrength~age+water,
 data=compressbien,
 method="avNNet",linout = TRUE,maxit=100,
 trControl=control,repeats=5,tuneGrid=nnetgrid)

rednnet


# Mismo ejemplo con regresión lineal
# Se utiliza el paquete lm (linear model). No hay parámetros.

control<-trainControl(method = "repeatedcv",
 number=4,repeats=5,savePredictions = "all") 

set.seed(123)

reg1<- train(cstrength~age+water,
 data=compressbien,
 method="lm",trControl=control)

reg1

