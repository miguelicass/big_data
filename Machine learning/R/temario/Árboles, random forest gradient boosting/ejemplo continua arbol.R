load("compressbien.Rda")

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

# Regresión
reg1<- train(cstrength~cement+plasti+age+blast+water+ash,
 data=compressbien, method="lm",trControl=control)

reg1

# Red

nnetgrid <-  expand.grid(size=c(15),decay=c(0.01),bag=F)

rednnet<- train(cstrength~cement+plasti+age+blast+water+ash,
 data=compressbien, method="avNNet",linout = TRUE,maxit=100,
 trControl=control,repeats=5,tuneGrid=nnetgrid)

rednnet

# Árbol con rpart

arbolgrid <-  expand.grid(cp=c(0,0.001,0.01,0.05,0.1))

arbolcaret<- train(cstrength~cement+plasti+age+blast+water+ash,
 data=compressbien,method="rpart",minbucket=30,
 trControl=control,tuneGrid=arbolgrid)

arbolcaret


source("cruzada arbol continua.R")
source("cruzadas avnnet y lin.R")

data<-compressbien

medias1<-cruzadaavnnet(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),grupos=4,sinicio=1234,repe=5,
size=c(15),decay=c(0.01),repeticiones=5,itera=100)
 
medias1$modelo="avnnet"

medias2<-cruzadalin(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),grupos=4,sinicio=1234,repe=5)
 
medias2$modelo="lineal"

medias3<-cruzadaarbol(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),grupos=4,sinicio=1234,repe=5,cp=0,minbucket=5)
 
medias3$modelo="arbol"


union1<-rbind(medias1,medias2,medias3)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo)

