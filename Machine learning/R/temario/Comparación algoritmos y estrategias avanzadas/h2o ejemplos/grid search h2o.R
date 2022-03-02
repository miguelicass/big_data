
load("fevbis.Rda")

# Reordeno las columnas
fevtris<-fevbis[,c("FEV","Age", "Height", "Sex.Female", "Smoker.Current")]

train<- as.h2o(fevtris)

library(h2o)
h2o.shutdown()
h2o.init(nthreads=8) 

train<- as.h2o(fevtris)

start_time <- Sys.time()

redpar <- list(hidden = c(3,4,5,6,7,10,15))

grid1 <- h2o.grid("deeplearning",x = 2:4,y=1,training_frame = train,nfolds=30,seed=12345,
epochs =100,activation = "Tanh",hyper_params =redpar)

print(grid1)

end_time <- Sys.time()

cat("H2O")
end_time - start_time


nnetgrid <-  expand.grid(size=c(3,4,5,6,7,10,15),decay=c(0.1))
control<-trainControl(method = "cv",number=30,savePredictions = "all") 

start_time <- Sys.time()

set.seed(12345)
rednnet<- train(FEV~Age+Height+Sex.Female+Smoker.Current,data=fevbis,
method="nnet",linout = TRUE,maxit=100,trControl=control,tuneGrid=nnetgrid,trace=FALSE)

rednnet

end_time <- Sys.time()

cat("CARET")
end_time - start_time





start_time <- Sys.time()

redpar <- list(hidden = seq(3,15,2))

grid1 <- h2o.grid("deeplearning",x = 2:5,y=1,training_frame = train,nfolds=30,seed=12345,
epochs =100,activation = "Tanh",hyper_params =redpar)

print(grid1)

end_time <- Sys.time()

cat("H2O")
end_time - start_time

# ***************************************
             # AutoML
# ***************************************

load("fevbis.Rda")
# Reordeno las columnas
fevtris<-fevbis[,c("FEV","Age", "Height", "Sex.Female", "Smoker.Current")]
train<- as.h2o(fevtris)


aml <- h2o.automl(x = 2:5,y=1,training_frame = train,max_models = 20,seed = 1)

# Lista de modelos ordenada de mejor a peor, en validación cruzada de 5 hrupos por defecto
lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)

# Ahora, si se quiere más información sobre un modelo concreto:
# 1) Se busca su nombre
# 2) Con getModel se obtiene el objeto del modelo
# 3) Se miran sus parámetros

# En nuestro caso el mejor modelo es un ensamblado (Stacked). 
# Mejor miramos primero otros modelos para omprender la estructura.
# Copiamos de la lista lb el nombre del segundo modelo de deep learning

modelo2 <- h2o.getModel("DeepLearning_grid_1_AutoML_20190426_115940_model_2")

# Vemos la estructura; como se ve es compleja
# En parameters está la lista de parámetros básica (los demás se dejan por defecto como los tiene h2o)
# En All parameters está la lista completa, incluidos los valores por defecto

str(modelo2)

modelo2@parameters

"GBM_4_AutoML_20190426_115940"

str(aml@leader)

aml@leader@parameters

aml@leader@parameters$base_models[[2]]$`__meta`




