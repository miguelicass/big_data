# *********************************
# CRUZADAS PARA ENSAMBLADO DEPENDIENTE CONTINUA
# *********************************
# VALIDACIÓN CRUZADA REPETIDA Y BOXPLOT para
# 
# LOGISTICA
# AVNNET
# RF
# GBM
# XGBM
# SVM
# *********************************


library(dummies)
library(MASS)
library(reshape)
library(caret)



cruzadaavnnet<-
 function(data=data,vardep="vardep",
  listconti="listconti",listclass="listclass",
  grupos=4,sinicio=1234,repe=5,
  size=c(5),decay=c(0.01),repeticiones=5,itera=100,trace=FALSE)
  
 { 

  # Preparación del archivo
  
  # b)pasar las categóricas a dummies
  
if (any(listclass==c(""))==FALSE)
  {
   databis<-data[,c(vardep,listconti,listclass)]
   databis<- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
   databis<-data[,c(vardep,listconti)]
  }
  
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center = means, scale = sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  formu<-formula(paste(vardep,"~.",sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",
   number=grupos,repeats=repe,
   savePredictions = "all") 
  
  # Aplico caret y construyo modelo
  
  avnnetgrid <-  expand.grid(size=size,decay=decay,bag=FALSE)
  
  avnnet<- train(formu,data=databis,
   method="avNNet",linout = TRUE,maxit=itera,repeats=repeticiones,
   trControl=control,tuneGrid=avnnetgrid,trace=trace)
  
  print(avnnet$results)
  
  preditest<-avnnet$pred[,c("pred","obs","Resample")]
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL

  preditest$error<-(preditest$pred-preditest$obs)^2
    

  
  
  tabla<-table(preditest$Rep)
  listarep<-c(names(tabla))
  medias<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    error=mean(paso1$error)  
    medias<-rbind(medias,error)
  }
  names(medias)<-"error"
  
  
  
  
return(list(medias,preditest))
  
 }

cruzadalin<-
 function(data=data,vardep="vardep",
  listconti="listconti",listclass="listclass",
  grupos=4,sinicio=1234,repe=5)
  
 { 
  library(caret)
  
  library(dummies)
  
  # Preparación del archivo
  
  # b)pasar las categóricas a dummies
  
if (any(listclass==c(""))==FALSE)
  {
   databis<-data[,c(vardep,listconti,listclass)]
   databis<- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
   databis<-data[,c(vardep,listconti)]
  }
  
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center = means, scale = sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  formu<-formula(paste(vardep,"~.",sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",
   number=grupos,repeats=repe,
   savePredictions = "all") 
  
  # Aplico caret y construyo modelo
  
   lineal<- train(formu,data=databis,
   method="lm",trControl=control)
  
  print(lineal$results)
  
  preditest<-lineal$pred[,c("pred","obs","Resample")]
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL

  preditest$error<-(preditest$pred-preditest$obs)^2
    

  
  
  tabla<-table(preditest$Rep)
  listarep<-c(names(tabla))
  medias<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    error=mean(paso1$error)  
    medias<-rbind(medias,error)
  }
  names(medias)<-"error"
  
  
  
  
return(list(medias,preditest))
  
 }


cruzadarf<-
 function(data=data,vardep="vardep",
  listconti="listconti",listclass="listclass",
  grupos=4,sinicio=1234,repe=5,
  nodesize=20,replace=TRUE,ntree=100,mtry=2,sampsize=1)
  
 { 

  if  (sampsize==1)
  {
   sampsize=floor(nrow(data)/(grupos-1))
  }  
  # Preparación del archivo
  
  # b)pasar las categóricas a dummies
  
if (any(listclass==c(""))==FALSE)
  {
   databis<-data[,c(vardep,listconti,listclass)]
   databis<- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
   databis<-data[,c(vardep,listconti)]
  }
  
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center = means, scale = sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  formu<-formula(paste(vardep,"~.",sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",
   number=grupos,repeats=repe,
   savePredictions = "all") 
  
  # Aplico caret y construyo modelo

   rfgrid <-expand.grid(mtry=mtry)
  
  rf<- train(formu,data=databis,
   method="rf",trControl=control,
   tuneGrid=rfgrid,nodesize=nodesize,replace=replace,
   ntree=ntree)
  
  print(rf$results)
  
  preditest<-rf$pred[,c("pred","obs","Resample")]


    preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL

  preditest$error<-(preditest$pred-preditest$obs)^2
    

  
  
  tabla<-table(preditest$Rep)
  listarep<-c(names(tabla))
  medias<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    error=mean(paso1$error)  
    medias<-rbind(medias,error)
  }
  names(medias)<-"error"
  
  
  
  
return(list(medias,preditest))
  
}

cruzadagbm<-
 function(data=data,vardep="vardep",
  listconti="listconti",listclass="listclass",
  grupos=4,sinicio=1234,repe=5,
  n.minobsinnode=20,shrinkage=0.1,n.trees=100,interaction.depth=2)
  
 { 
  # Preparación del archivo
  
  # b)pasar las categóricas a dummies
  
if (any(listclass==c(""))==FALSE)
  {
   databis<-data[,c(vardep,listconti,listclass)]
   databis<- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
   databis<-data[,c(vardep,listconti)]
  }
  
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center = means, scale = sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  formu<-formula(paste(vardep,"~.",sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",
   number=grupos,repeats=repe,
   savePredictions = "all") 
  
  # Aplico caret y construyo modelo

  # n.minobsinnode=20,shrinkage=0.1,n.trees=100,interaction.depth=2
  
   gbmgrid <-expand.grid(n.minobsinnode=n.minobsinnode,
    shrinkage=shrinkage,n.trees=n.trees,
    interaction.depth=interaction.depth)
  
  gbm<- train(formu,data=databis,
   method="gbm",trControl=control,
   tuneGrid=gbmgrid,distribution="gaussian",verbose=FALSE)
  
  print(gbm$results)
  
  preditest<-gbm$pred[,c("pred","obs","Resample")]


    preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL

  preditest$error<-(preditest$pred-preditest$obs)^2
    

  
  
  tabla<-table(preditest$Rep)
  listarep<-c(names(tabla))
  medias<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    error=mean(paso1$error)  
    medias<-rbind(medias,error)
  }
  names(medias)<-"error"
  
  
  
  
return(list(medias,preditest))
  
}


 cruzadaxgbm<-
 function(data=data,vardep="vardep",
  listconti="listconti",listclass="listclass",
  grupos=4,sinicio=1234,repe=5,
  min_child_weight=20,eta=0.1,nrounds=100,max_depth=2,
  gamma=0,colsample_bytree=1,subsample=1,alpha=0,lambda=0,lambda_bias=0)  
 { 

  # Preparación del archivo
  
  # b)pasar las categóricas a dummies
  
if (any(listclass==c(""))==FALSE)
  {
   databis<-data[,c(vardep,listconti,listclass)]
   databis<- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
   databis<-data[,c(vardep,listconti)]
  }
  
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center = means, scale = sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  formu<-formula(paste(vardep,"~.",sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",
   number=grupos,repeats=repe,
   savePredictions = "all") 
  
  # Aplico caret y construyo modelo

  xgbmgrid <-expand.grid( min_child_weight=min_child_weight,
  eta=eta,nrounds=nrounds,max_depth=max_depth,
  gamma=gamma,colsample_bytree=colsample_bytree,subsample=subsample)
  
  xgbm<- train(formu,data=databis,
   method="xgbTree",trControl=control,
   tuneGrid=xgbmgrid,verbose=FALSE,
   alpha=alpha,lambda=lambda,lambda_bias=lambda_bias)
  
  print(xgbm$results)
  
  preditest<-xgbm$pred[,c("pred","obs","Resample")]
  
  
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL

  preditest$error<-(preditest$pred-preditest$obs)^2
    

  
  
  tabla<-table(preditest$Rep)
  listarep<-c(names(tabla))
  medias<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    error=mean(paso1$error)  
    medias<-rbind(medias,error)
  }
  names(medias)<-"error"
  
  
  
  
return(list(medias,preditest))
  
}

 
 cruzadaSVM<-
 function(data=data,vardep="vardep",
  listconti="listconti",listclass="listclass",
  grupos=4,sinicio=1234,repe=5, C=1)  
 { 

  # Preparación del archivo
  
  # b)pasar las categóricas a dummies
  
if (any(listclass==c(""))==FALSE)
  {
   databis<-data[,c(vardep,listconti,listclass)]
   databis<- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
   databis<-data[,c(vardep,listconti)]
  }
  
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center = means, scale = sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  formu<-formula(paste(vardep,"~.",sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",
   number=grupos,repeats=repe,
   savePredictions = "all") 
  
  # Aplico caret y construyo modelo

  SVMgrid <-expand.grid(C=C)
  
  SVM<- train(formu,data=databis,
   method="svmLinear",trControl=control,
   tuneGrid=SVMgrid,verbose=FALSE)
  
  print(SVM$results)
  
  preditest<-SVM$pred[,c("pred","obs","Resample")]
  
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL

  preditest$error<-(preditest$pred-preditest$obs)^2
    

  
  
  tabla<-table(preditest$Rep)
  listarep<-c(names(tabla))
  medias<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    error=mean(paso1$error)  
    medias<-rbind(medias,error)
  }
  names(medias)<-"error"
  
  
  
  
return(list(medias,preditest))
  
}


 cruzadaSVMpoly<-
 function(data=data,vardep="vardep",
  listconti="listconti",listclass="listclass",
  grupos=4,sinicio=1234,repe=5, C=1,degree=2,scale=1)  
 { 

  # Preparación del archivo
  
  # b)pasar las categóricas a dummies
  
if (any(listclass==c(""))==FALSE)
  {
   databis<-data[,c(vardep,listconti,listclass)]
   databis<- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
   databis<-data[,c(vardep,listconti)]
  }
  
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center = means, scale = sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  formu<-formula(paste(vardep,"~.",sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",
   number=grupos,repeats=repe,
   savePredictions = "all") 
  
  # Aplico caret y construyo modelo

  SVMgrid <-expand.grid(C=C,degree=degree,scale=scale)
  
  SVM<- train(formu,data=databis,
   method="svmPoly",trControl=control,
   tuneGrid=SVMgrid,verbose=FALSE)
  
  print(SVM$results)
  
  preditest<-SVM$pred[,c("pred","obs","Resample")]
  
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL

  preditest$error<-(preditest$pred-preditest$obs)^2
    
  
  
  tabla<-table(preditest$Rep)
  listarep<-c(names(tabla))
  medias<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    error=mean(paso1$error)  
    medias<-rbind(medias,error)
  }
  names(medias)<-"error"
  
  
  
  
return(list(medias,preditest))
  
}


 cruzadaSVMRBF<-
 function(data=data,vardep="vardep",
  listconti="listconti",listclass="listclass",
  grupos=4,sinicio=1234,repe=5, C=1,sigma=1)  
 { 

  # Preparación del archivo
  
  # b)pasar las categóricas a dummies
  
if (any(listclass==c(""))==FALSE)
  {
   databis<-data[,c(vardep,listconti,listclass)]
   databis<- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
   databis<-data[,c(vardep,listconti)]
  }
  
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center = means, scale = sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  formu<-formula(paste(vardep,"~.",sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",
   number=grupos,repeats=repe,
   savePredictions = "all") 
  
  # Aplico caret y construyo modelo

  SVMgrid <-expand.grid(C=C,sigma=sigma)
  
  SVM<- train(formu,data=databis,
   method="svmRadial",trControl=control,
   tuneGrid=SVMgrid,verbose=FALSE)
  
  print(SVM$results)
  
  preditest<-SVM$pred[,c("pred","obs","Resample")]
  
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL

  preditest$error<-(preditest$pred-preditest$obs)^2
    

  
  
  tabla<-table(preditest$Rep)
  listarep<-c(names(tabla))
  medias<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    error=mean(paso1$error)  
    medias<-rbind(medias,error)
  }
  names(medias)<-"error"
  
  
  
  
return(list(medias,preditest))
  
}

 