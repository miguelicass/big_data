
load("pima.Rda")

listclass<- c("")
listconti<-c("embarazos", "glucosa", "presion", "triceps", "insulina", "bodymass",
"diabfun", "edad")
vardep<-"diabetes"



library(caret)

set.seed(12345)

control<-trainControl(method = "cv",number=10,savePredictions = "all") 

start_time <- Sys.time()

nnetgrid <-  expand.grid(size=c(10),decay=c(0.01),bag=FALSE)

rednnet<- train(factor(diabetes)~.,data=pima,
method="avNNet",repeats=5,linout = FALSE,maxit=100,trControl=control,tuneGrid=nnetgrid,trace=FALSE)

rednnet

end_time <- Sys.time()

end_time - start_time


library(h2o)

h2o.init()
h2o.shutdown()
h2o.init(nthreads=8)

# Pongo un solo cluster para que los resultados sean reproducibles.
# Para más velocidad se puede poner 8


# Reordeno las columnas
pima2<-pima[,c("diabetes","embarazos", "glucosa",
 "presion", "triceps", "insulina", "bodymass","diabfun", "edad")]


# Para clasificación hay que definir como factor la dependiente
pima2$diabetes<-as.factor(pima2$diabetes)

train<- as.h2o(pima2)

start_time <- Sys.time()

red1<-h2o.deeplearning(x = 2:9,y=1,training_frame = train,seed=12345,
hidden = c(10),epochs =100,activation = "Tanh",rate=0.01,nfolds=10,adaptive_rate=FALSE)

red1
 
end_time <- Sys.time()

end_time - start_time


# ************************************
# Pruebas con AutoML
# ************************************

start_time <- Sys.time()

aml <- h2o.automl(x = 2:9,y=1,training_frame = train,max_models = 20,seed = 1,keep_cross_validation_predictions=TRUE)

lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)


aml@leader


end_time <- Sys.time()

end_time - start_time


modelo2 <- h2o.getModel("DeepLearning_grid_1_AutoML_20190426_192859_model_2")

# Vemos la estructura; como se ve es compleja
# En parameters está la lista de parámetros básica (los demás se dejan por defecto como los tiene h2o)
# En All parameters está la lista completa, incluidos los valores por defecto

str(modelo2)

modelo2@parameters


# Intento replicar aml@leader con sus parametros 

# H2OBinomialModel: deeplearning
# Model ID:  DeepLearning_grid_1_AutoML_20190425_200847_model_2 
# Status of Neuron Layers: predicting diabetes, 2-class classification, bernoulli distribution, CrossEntropy loss, 2.202 weights/biases, 31,6 KB, 1.912.320 training samples, mini-batch size 1
#   layer units             type dropout       l1       l2 mean_rate rate_rms momentum mean_weight weight_rms
# 1     1     8            Input 10.00 %       NA       NA        NA       NA       NA          NA         NA
# 2     2   200 RectifierDropout 20.00 % 0.000000 0.000000  0.002425 0.010279 0.000000   -0.029609   0.237066
# 3     3     2          Softmax      NA 0.000000 0.000000  0.000771 0.000353 0.000000   -0.005419   0.376135


red1<-h2o.deeplearning(x = 2:9,y=1,training_frame = train,seed=23,
hidden = c(200),input_dropout_ratio=0.1,
activation ="RectifierWithDropout",hidden_dropout_ratios=0.20, nfolds=5,mini_batch_size=1)

red1

# Modelo 4 GLM=LOGISTICA

modelo4 <- h2o.getModel("GLM_grid_1_AutoML_20190426_192859_model_1")
str(modelo4)
modelo4@allparameters

# Es regresión logística donde los parámetros se estiman con con regularizacion lambda y alpha
# Al final quedan los parámetros:

modelo4@model$coefficients_table


# Modelo 5 GBM

modelo5 <- h2o.getModel("GBM_5_AutoML_20190426_192859")

str(modelo5)
modelo5@parameters
modelo5@allparameters

# $ntrees
# [1] 31
# 
# $max_depth
# [1] 15
# 
# $min_rows
# [1] 100
# 
# $stopping_metric
# [1] "logloss"
# 
# $stopping_tolerance
# [1] 0.03608439
# 
# $seed
# [1] 1
# 
# $distribution
# [1] "bernoulli"
# 
# $sample_rate
# [1] 0.8
# 
# $col_sample_rate
# [1] 0.8
# 
# $col_sample_rate_per_tree
# [1] 0.8

# Lo replico:

gbm1<-h2o.gbm(x = 2:9,y=1,training_frame = train,seed=23,
ntrees=31,max_depth=15,min_rows = 100,sample_rate = 0.8,col_sample_rate = 0.8,col_sample_rate_per_tree = 0.8,nfolds=5)

gbm1


# Cambio algunas cosas para probar y funciona mejor (yo por defecto no hago sample nrow ni col, los pongo a 1):

gbm1<-h2o.gbm(x = 2:9,y=1,training_frame = train,seed=23,
ntrees=40,max_depth=15,min_rows =100,sample_rate = 1,col_sample_rate = 1,col_sample_rate_per_tree = 1,nfolds=5)

gbm1



# Veamos el modelo ganador, hay que ver los modelos que componen el ensamblado

modelo1 <- h2o.getModel("StackedEnsemble_BestOfFamily_AutoML_20190426_192859")

str(modelo1)
modelo1@parameters
modelo1@allparameters

modelo1@parameters$base_models

# Copio los nombres aquí:

# "DeepLearning_grid_1_AutoML_20190426_192859_model_2"
# "GLM_grid_1_AutoML_20190426_192859_model_1"
# "GBM_5_AutoML_20190426_192859"
# "XRT_1_AutoML_20190426_192859"
# "DRF_1_AutoML_20190426_192859"

modelo1 <- h2o.getModel("DeepLearning_grid_1_AutoML_20190426_192859_model_2")
modelo2 <- h2o.getModel("GLM_grid_1_AutoML_20190426_192859_model_1")
modelo3 <- h2o.getModel("GBM_5_AutoML_20190426_192859")
modelo4 <- h2o.getModel("XRT_1_AutoML_20190426_192859")
modelo5 <- h2o.getModel("DRF_1_AutoML_20190426_192859")

# ASI NO FUNCIONA

ensemble1 <- h2o.stackedEnsemble(x = 2:9,y=1,training_frame = train,seed=23,model_id = "amdsso",
base_models = list(modelo1,modelo3))


# HAY QUE RECONSTRUIR CADA MODELO Y ENSAMBLAR

gbm1 <- h2o.gbm(x = 2:9,y=1,training_frame = train,seed=234,
ntrees=40,max_depth=15,min_rows =100,sample_rate = 1,col_sample_rate = 1,col_sample_rate_per_tree = 1,
                                    distribution = "bernoulli",
                  nfolds = 5,
                  keep_cross_validation_predictions = TRUE)

red1<-h2o.deeplearning(x = 2:9,y=1,training_frame = train,seed=234,
hidden = c(200),input_dropout_ratio=0.1,
activation ="RectifierWithDropout",hidden_dropout_ratios=0.20, nfolds=5,mini_batch_size=1,keep_cross_validation_predictions = TRUE)


ensemble2 <- h2o.stackedEnsemble(x = 2:9,y=1,training_frame = train,seed=234,model_id = "en1",base_models = list(gbm1,red1),
 metalearner_algorithm = "glm")


gbm1
red1
ensemble2



# EJEMPLO GERMAN REVISITADO

load(file="c:/germanbis.Rda")
a<-dput(names(germanbis))


 # PONEMOS EN LA PRIMERA COLUMNA A LA VARIABLKE DEPENDIENTE

 germanbis<-germanbis[,
c("bad","checking", "duration", "amount", "savings", "employed", "installp", 
"coapp", "resident", "age", "existcr", "job", "depends", "history.0", 
"history.1", "history.2", "history.3", "history.4", "purpose.0", 
"purpose.1", "purpose.2", "purpose.3", "purpose.4", "purpose.5", 
"purpose.6", "purpose.8", "purpose.9", "purpose.X", "marital.1", 
"marital.2", "marital.3", "marital.4", "property.1", "property.2", 
"property.3", "property.4", "other.1", "other.2", "other.3", 
"housing.1", "housing.2", "housing.3", "telephon.1", "telephon.2", 
"foreign.1", "foreign.2")] 
 
 vari<-c("bad","checking", "duration", "amount", "savings", "employed", "installp", 
"coapp", "resident", "age", "existcr", "job", "depends", "history.0", 
"history.1", "history.2", "history.3", "history.4", "purpose.0", 
"purpose.1", "purpose.2", "purpose.3", "purpose.4", "purpose.5", 
"purpose.6", "purpose.8", "purpose.9", "purpose.X", "marital.1", 
"marital.2", "marital.3", "marital.4", "property.1", "property.2", 
"property.3", "property.4", "other.1", "other.2", "other.3", 
"housing.1", "housing.2", "housing.3", "telephon.1", "telephon.2", 
"foreign.1", "foreign.2")

 length(vari)
 
  
# aml = H2OAutoML(max_runtime_secs = 120)    
train<- as.h2o(germanbis)

  
aml <- h2o.automl(x = 2:45,y=1,training_frame = train,max_models = 20,seed = 1)

lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)



modelo4 <- h2o.getModel("GLM_grid_1_AutoML_20190427_182650_model_1")
str(modelo4)
modelo4@allparameters

# Es regresión logística donde los parámetros se estiman con con regularizacion lambda y alpha
# Al final quedan los parámetros:

tabla<-modelo4@model$coefficients_table

