library(ggplot2)
load("compress.Rda")
listconti<-c("cement", "blast", "ash", "water", "plasti", "aggreg", 
"fineagg", "age")
vardep<-c("cstrength")
cstrength<-compress[,vardep]

repito<-function(nodos)
 {
 # Aquí se cambia la semilla de la partición
set.seed(12347)
sample <- sample.int(n = nrow(compress),
 size = floor(0.75*nrow(compress)), replace = F)

train <- compress[sample, ]
test  <- compress[-sample, ]

# Estandarizo train y test
# Los datos de test se estandarizan con las medias y d.típica de train

means <-apply(train[,listconti],2,mean,na.rm=TRUE)
sds<-sapply(train[,listconti],sd,na.rm=TRUE)

train2<-scale(train[,listconti], center = means, scale = sds)
numerocont<-which(colnames(train)%in%listconti)
train2<-cbind(train2,train[,-numerocont,drop=FALSE ])

test2<-scale(test[,listconti], center = means, scale = sds)
numerocont<-which(colnames(test)%in%listconti)
test2<-cbind(test2,test[,-numerocont,drop=FALSE ])


library(nnet)
set.seed(22342)

# Se construye la red con los datos train

red1<-nnet(data=train2,
 cstrength~age+water,linout = TRUE,size=nodos,maxit=100,trace=FALSE)

summary(red1)

# Se calculan las predicciones sobre datos test

predi<-predict(red1,newdata=test2,type="raw")

# Se calcula el error de predicción sobre datos test
MSEtestred <- sum((test2$cstrength - predi)^2)/nrow(test2)

MSEtestred
 
# Error cometido por el modelo de regresión

reg1<-lm(data=train2,cstrength~age+water)

summary(reg1)
 
# Para obtener el MSE y el RMSE

predi<-predict(reg1,newdata=test2,type="response")

# Se calcula el error de predicción sobre datos test
MSEtestreg <- sum((test2$cstrength - predi)^2)/nrow(test2)

MSEtestreg
 return(list(nodos,MSEtestred,MSEtestreg))
}

# Aquí creo un data.frame para ir guardando los datos para un gráfico

resul<-data.frame(c())
resulfin<-data.frame(c())

for (nodos in 3:35)
{
 repe<-repito(nodos)
 cat(repe[[1]],"\n")
 print(repe[[2]])
 print(repe[[3]])

 nodos<-repe[[1]]
  
 resul<-as.data.frame(nodos)
 resul$red<-repe[[2]]
 resul$reg<-repe[[3]]
 resulfin<-rbind(resulfin,resul)
  }

library(reshape2)

meltdf <- melt(resulfin,id="nodos")

ggplot(meltdf,aes(x=nodos,
 y=value,colour=variable,group=variable)) + geom_line()+
 scale_x_continuous(breaks =seq(3,35, by=1))+
 theme(axis.text.x=element_text(size=7))
 
 
    

