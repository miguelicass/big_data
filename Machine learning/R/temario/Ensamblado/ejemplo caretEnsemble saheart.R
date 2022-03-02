load ("saheartbis.Rda")
library(caret)
library(caretEnsemble)

# PARÁMETROS EXTRAÍDOS DE TUNING CON CARET de ejemplos anteriores
# A veces hay que cambiar el nombre de los parámetros
# Los de grid hay que ponerlos en un grid

# avNNet recomienda en este caso size=5 nodos, 
# decay=0.1 , iteraciones=200

# gbm 
# n.minobsinnode=5,shrinkage=0.001,n.trees=3000,interaction.depth=2

# rforest 
# nodesize=10, mtry=6,n.trees=200,replace=TRUE,sampsize=150

# SVM RBF C=5,sigma=0.01

# SVM LINEAL  C=0.03
# SVM Poly  C=0.02,degree=2,scale=2

# ************************************************************
# Estas tres líneas son importantes, sustituir por el nombre
# de variable y archivo
# ************************************************************

formula1<-as.formula(paste("factor(","chd",")","~."))
saheartbis$chd<-as.factor(saheartbis$chd)
levels(saheartbis$chd) <- make.names(levels(factor(saheartbis$chd)))
  
# Aquí se fijan el número de repeticiones de validación cruzada 
# y la semilla
set.seed(3005)
repeticiones=10


# Manera de evaluar los modelos
stackControl <- trainControl(method="repeatedcv", 
 number=4, repeats=repeticiones, savePredictions=TRUE, classProbs=TRUE)

# Parámetros para caret, tunear antes

# Muy importante considerar esto:

# Cada method (algoritmo) tiene parámetros a tunear con Grid
# y parámetros específicos que no se pueden tunear.
# --Los que se pueden tunear hay que ponerlos en un Grid aunque 
# solo se les de un valor (ver por ejemplo gbmGrid).
# --Los que no se pueden tunear hay que nombrarlos directamente
# en train (ver por ejemplo rf)


gbmGrid <- expand.grid(n.trees = c(3000),
  interaction.depth = c(2), shrinkage =c(0.001), n.minobsinnode = c(5))

rfGrid <- expand.grid(mtry=c(6))

svmRadialGrid <- expand.grid(sigma=c(0.01),C=c(5))

svmlinGrid <- expand.grid(C=c(0.03))

svmPolyGrid <- expand.grid(C=c(0.02),degree=c(2),scale=c(2))

set.seed(3005)

models <- caretList(chd~., data=saheartbis, trControl=stackControl,
tuneList=list(
 parrf=caretModelSpec(method="rf",maxnodes=30,
 n.trees = 200,nodesize=10,sampsize=150,tuneGrid=rfGrid), 
 glm=caretModelSpec(method="glm"),
 gbm=caretModelSpec(method="gbm",tuneGrid=gbmGrid),
 svmlinear=caretModelSpec(method="svmLinear",tuneGrid=svmlinGrid), 
 svmPoly=caretModelSpec(method="svmPoly",tuneGrid=svmPolyGrid),
 svmradial=caretModelSpec(method="svmRadial",tuneGrid=svmRadialGrid)
 ))

results <- resamples(models)
summary(results)
dotplot(results)

modelCor(results)
splom(results)
results[[2]]

ense <- caretEnsemble(models)

# Aquí se recomiendan los pesos para el ensamblado 
# de todos los modelos y se ve la tasa de aciertos
# de cada modelo y ensamblado
summary(ense)

