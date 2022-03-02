
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-yates/2/R")



# Finally, let's load H2O and start up an H2O cluster
library(h2o)

h2o.init()
h2o.shutdown()
h2o.init(nthreads=1) 
# Pongo un solo cluster para que los resultados sean reproducibles.
# Para más velocidad se puede poner 8, es mejor, y lo haremos después

load("fevbis.Rda")

# Reordeno las columnas
fevtris<-fevbis[,c("FEV","Age", "Height", "Sex.Female", "Smoker.Current")]

train<- as.h2o(fevtris)

red1<-h2o.deeplearning(x = 2:4,y=1,training_frame = train,seed=12345,
hidden = c(10),epochs =100,activation = "Tanh",nfolds=4)

red1
 
red2<-h2o.deeplearning(x = 2:4,y=1,training_frame = train,
hidden = c(10),epochs =100,activation = "Maxout",nfolds=4,seed=12345)

red2
 
red3<-h2o.deeplearning(x = 2:4,y=1,training_frame = train,
hidden = c(10),epochs =100,activation = "Rectifier",nfolds=4,seed=12345)

red3
 
red4<-h2o.deeplearning(x = 2:4,y=1,training_frame = train,
hidden = c(10),epochs =100,activation = "RectifierWithDropout",nfolds=4,seed=12345)

red4

# COMPARACIÓN CON CARET


library(caret)
set.seed(12345)
# Validación cruzada repetida
control<-trainControl(method = "cv",number=10,savePredictions = "all") 

# ***************************************************************
# nnet: parámetros
#     Number of Hidden Units (size, numeric)
#     Weight Decay (decay, numeric)
# ***************************************************************

nnetgrid <-  expand.grid(size=c(20),decay=c(0.1))
control<-trainControl(method = "cv",number=20,savePredictions = "all") 

start_time <- Sys.time()

set.seed(12345)
rednnet<- train(FEV~Age+Height+Sex.Female+Smoker.Current,data=fevbis,
method="nnet",linout = TRUE,maxit=200,trControl=control,tuneGrid=nnetgrid,trace=FALSE)

rednnet

end_time <- Sys.time()

end_time - start_time


# Time difference of 9.062638 secs

h2o.shutdown()
h2o.init(nthreads=8) 
# Con 8 cores no es reproducible pero es muy rápido

start_time <- Sys.time()

red4<-h2o.deeplearning(x = 2:4,y=1,training_frame = train,
hidden = c(20),epochs =200,activation = "Tanh",nfolds=20,seed=12345)

red4

end_time <- Sys.time()

end_time - start_time

# Time difference of 0.2838008 secs



