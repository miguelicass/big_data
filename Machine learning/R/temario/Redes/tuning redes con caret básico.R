library(sas7bdat)
library(nnet)
library(h2o)
library(dummies)
library(MASS)
library(reshape)
library(caret)

# Lectura y esquema de variables

fev<-read.sas7bdat("c:/fev.sas7bdat")
dput(names(fev))

# c("ID", "Age", "FEV", "Height", "Sex", "Smoker")

continuas<-c("Age", "Height")
categoricas<-c("Sex", "Smoker")

# a)Eliminar las observaciones con missing en alguna variable

fev2<-na.omit(fev,(!is.na(fev)))

# b)pasar las categóricas a dummies

fev3<- dummy.data.frame(fev2, categoricas, sep = ".")

# c)estandarizar las variables continuas

# Calculo medias y dtipica de datos y estandarizo (solo las continuas)

means <-apply(fev3[,continuas],2,mean) 
sds<-sapply(fev3[,continuas],sd) 

# Estandarizo solo las continuas y uno con las categoricas

fevbis<-scale(fev3[,continuas], center = means, scale = sds)
numerocont<-which(colnames(fev3)%in%continuas)
fevbis<-cbind(fevbis,fev3[,-numerocont])

# El archivo fevbis ya está preparado:no hay missing, las continuas salvo la dependiente 
# están estandarizadas y las categoricas pasadas a dummy

dput(names(fevbis))

# NOTA: En los modelos pondremos solo k-1 dummies por cada categórica

c("Age", "Height", "ID", "FEV", "Sex.Female", "Sex.Male", "Smoker.Current", 
"Smoker.Non")

# ***************************
# TUNING CON CARET
# ***************************

set.seed(12346)

# Validación cruzada una sola vez
control<-trainControl(method = "cv",number=4,,savePredictions = "all") 

# Validación cruzada repetida
control<-trainControl(method = "repeatedcv",number=4,repeats=5,savePredictions = "all") 

# Training test una sola vez
control<-trainControl(method = "LGOCV",p=0.8,number=1,savePredictions = "all") 

# Training test repetido
control<-trainControl(method = "LGOCV",p=0.8,number=5,savePredictions = "all") 


# EN LO SUCESIVO APLICAMOS VALIDACIÓN CRUZADA REPETIDA

set.seed(12346)
# Validación cruzada repetida
control<-trainControl(method = "repeatedcv",number=4,repeats=5,savePredictions = "all") 

# ***************************************************************
# nnet: parámetros
#     Number of Hidden Units (size, numeric)
#     Weight Decay (decay, numeric)
# ***************************************************************

nnetgrid <-  expand.grid(size=c(5,10,15,20),decay=c(0.01,0.1,0.001))

rednnet<- train(FEV~Age+Height+Sex.Female+Smoker.Current,data=fevbis,
method="nnet",linout = TRUE,maxit=100,trControl=control,tuneGrid=nnetgrid)

rednnet


# ***************************************************************
# avNNet: parámetros
    # Number of Hidden Units (size, numeric)
    # Weight Decay (decay, numeric)
    # Bagging (bag, logical)
# ***************************************************************
avnnetgrid <-expand.grid(size=c(5,10,15,20),decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(FEV~Age+Height+Sex.Female+Smoker.Current,data=fevbis,
method="avNNet",linout = TRUE,maxit=100,trControl=control,repeats=5,tuneGrid=avnnetgrid)

redavnnet

# ***************************************************************
# Ejemplo validación cruzada h2o:
# Podría repetirlo para varios valores y/o hacer validacion cruzada repetida
# *************************************************************

library(h2o)
h2o.init()

# Reordeno las columnas
fevtris<-fevbis[,c("FEV","Age", "Height", "Sex.Female", "Smoker.Current")]

train.hex <- as.h2o(fevtris, destination_frame = "train.hex")

red5<-h2o.deeplearning(x = 2:4,y=1,training_frame = train.hex,
hidden = c(5),epochs =100,activation = "Tanh",nfolds=4,seed=12345)

red5
 # 0.40350476
red10<-h2o.deeplearning(x = 2:4,y=1,training_frame = train.hex,
hidden = c(10),epochs =100,activation = "Tanh",nfolds=4,seed=12345)

red10
# 0.39948225


red5<-h2o.deeplearning(x = 2:4,y=1,training_frame = train.hex,
hidden = c(5),epochs =100,activation = "Tanh",nfolds=4,seed=12346)

red5
 # 0.3929712
red10<-h2o.deeplearning(x = 2:4,y=1,training_frame = train.hex,
hidden = c(10),epochs =100,activation = "Tanh",nfolds=4,seed=12346)

red10
 # 0.38932687




