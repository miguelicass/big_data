
load("fitness.Rda")

listclass<-c("TEACHER","SEX")
listconti<-c("AGE","HEART","EXER")
vardep<-c("AERO")

data<-fitness

library(ggplot2)


# Gráficos básicos con puntos coloreados por sexo
ggplot(data, aes(AGE, AERO,color=SEX))+geom_point() 
ggplot(data, aes(HEART,AERO,color=SEX))+geom_point() 


# Preparación del archivo
# Esto sería para eliminar las observaciones con algún missing 
# en alguna variable 

# data<-na.omit(data)

# IMPUTACIÓN POR LA MEDIA EN CONTINUAS

for (vari in listconti)
{
data[,vari]<-ifelse(is.na(data[,vari]),
 mean(data[,vari],na.rm=TRUE),data[,vari]) 
}

  # a)pasar las categóricas a dummies en todo el archivo

library(dummies)
  
if (any(listclass==c(""))==FALSE)
  {
   databis<-data[,c(vardep,listconti,listclass)]
   databis<- dummy.data.frame(databis, listclass, sep = ".")
   }  else   {
   databis<-data[,c(vardep,listconti)]
  }

# databis es el nuevo archivo con dummies, sin missing

# ESTANDARIZACIÓN 

means <-apply(databis[,listconti],2,mean,na.rm=TRUE)
sds<-sapply(databis[,listconti],sd,na.rm=TRUE)

databis2<-scale(databis[,listconti], center = means, scale = sds)
numerocont<-which(colnames(databis)%in%listconti)
databis<-cbind(databis2,databis[,-numerocont,drop=FALSE ])


# En el modelo se ponen todas las dummies menos una referentes 
# a lascategorías de las variables categóricas

library(caret)

set.seed(12346)
# Training test una sola vez
control<-trainControl(method = "LGOCV",p=0.8,number=1,savePredictions = "all") 

nnetgrid <-expand.grid(size=c(3),decay=c(0.1))

rednnet<- train(AERO~HEART+AGE+SEX.F,
 data=databis, method="nnet",linout = TRUE,maxit=100,
 trControl=control,tuneGrid=nnetgrid)

# EL ERROR PRESENTADO EL ERROR SOBRE DATOS TEST 
rednnet

# Comparo con regresión lineal, method=lm. Aquí no hay grid.

reg1<- train(AERO~HEART+AGE+SEX.F,
 data=databis, method="lm",trControl=control)

reg1

# COMPROBANDO BAJO DIFERENTES SEMILLAS TRAIN-TEST

for (semilla in 120:125)
{
set.seed(semilla) 
control<-trainControl(method = "LGOCV",p=0.8,number=5,savePredictions = "all") 

nnetgrid <-expand.grid(size=c(3),decay=c(0.1))

rednnet<- train(AERO~HEART+AGE+SEX.F,
 data=databis, method="nnet",linout = TRUE,
 trControl=control,tuneGrid=nnetgrid,trace=FALSE)

cat("\n")
print(semilla)
cat("\n")
print(rednnet$results$RMSE)

# Comparo con regresión lineal, method=lm. Aquí no hay grid.

reg1<- train(AERO~HEART+AGE+SEX.F,
 data=databis, method="lm",trControl=control)

print(reg1$results$RMSE)
 
 }


