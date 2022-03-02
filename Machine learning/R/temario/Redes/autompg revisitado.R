
load("autompg.Rda")

dput(names(autompg))

# c("mpg", "cylinders", "displacement", "horsepower", "weight", 
# "acceleration", "modelyear", "origin", "carname")
 
listconti<-c("cylinders", "displacement", "horsepower", "weight", 
"acceleration")
listclass<-c("modelyear")

vardep<-c("mpg")


# Primero me quedo con solo las variables de interés
# (listconti, listclass y vardep)

auto<-autompg[,c(listconti,listclass,vardep)]

# Borro observaciones con algún missing 
# (son pocas, también se puede imputar)

auto<-na.omit(auto)
 
# Copio  y pego de otros ejemplos estandarización continuas

means <-apply(auto[,listconti],2,mean,na.rm=TRUE)
sds<-sapply(auto[,listconti],sd,na.rm=TRUE)

auto2<-scale(auto[,listconti], center = means, scale = sds)

auto<-data.frame(cbind(auto2,auto[,c(listclass,vardep)]))

# Hago dummies

# Antes Observo si hay categorías poco representadas en modelyear 
# para eliminarlas tras hacer dummies

table(auto$modelyear)

# Todo ok

library(dummies)

auto3<- dummy.data.frame(auto, listclass, sep = ".")

# SELECCIÓN DE VARIABLES

# Utilizamos 
# a) stepwise sobre todos los datos
# b) Con remuestreo (función steprepetido)

data<-auto3

full<-lm(mpg~.,data=data)
null<-lm(mpg~1,data=data)

selec1<-stepAIC(null,scope=list(upper=full),direction="both",trace=FALSE)

summary(selec1)

formula(selec1)
dput(names(selec1$coefficients))


# c("(Intercept)", "weight", "modelyear.80", "modelyear.82", "modelyear.81", 
# "modelyear.79", "modelyear.73", "modelyear.78", "modelyear.77", 
# "horsepower", "modelyear.76", "modelyear.74")

# La función steprepetido permite realizar el proceso training test
# varias veces obteniendo el modelo por stepwise sobre datos train
# y la tabla de frecuencias de los modelos escogidos.

dput(names(auto3))

# c("cylinders", "displacement", "horsepower", "weight", "acceleration", 
# "modelyear.70", "modelyear.71", "modelyear.72", "modelyear.73", 
# "modelyear.74", "modelyear.75", "modelyear.76", "modelyear.77", 
# "modelyear.78", "modelyear.79", "modelyear.80", "modelyear.81", 
# "modelyear.82", "mpg")

source("funcion steprepetido.R")

lista<-steprepetido(data=data,vardep=c("mpg"),
listconti=c("cylinders", "displacement", "horsepower", "weight", "acceleration", 
"modelyear.70", "modelyear.71", "modelyear.72", "modelyear.73", 
"modelyear.74", "modelyear.75", "modelyear.76", "modelyear.77", 
"modelyear.78", "modelyear.79", "modelyear.80", "modelyear.81", "modelyear.82"),
sinicio=12345,sfinal=12385,porcen=0.8,criterio="AIC")

tabla<-lista[[1]]

dput(lista[[2]][[1]])
dput(lista[[2]][[2]])

lista<-steprepetido(data=data,vardep=c("mpg"),
listconti=c("cylinders", "displacement", "horsepower", "weight", "acceleration", 
"modelyear.70", "modelyear.71", "modelyear.72", "modelyear.73", 
"modelyear.74", "modelyear.75", "modelyear.76", "modelyear.77", 
"modelyear.78", "modelyear.79", "modelyear.80", "modelyear.81", "modelyear.82"),
sinicio=12345,sfinal=12385,porcen=0.8,criterio="BIC")

tabla<-lista[[1]]

dput(lista[[2]][[1]])
dput(lista[[2]][[2]])

# Obtenemos 3 modelos:
 
# El de stepAIC  todos los datos

# c("(Intercept)", "weight", "modelyear.80", "modelyear.82", "modelyear.81", 
# "modelyear.79", "modelyear.73", "modelyear.78", "modelyear.77", 
# "horsepower", "modelyear.76", "modelyear.74")

# El que sale mas veces  de stepAIC  repetido con AIC

# c("weight", "modelyear.80", "modelyear.82", "modelyear.81", "modelyear.79", 
# "horsepower", "modelyear.77", "modelyear.78", "modelyear.73")

# El que sale mas veces  de stepAIC  repetido con BIC

# c("weight", "modelyear.80", "modelyear.82", "modelyear.81", "modelyear.79", 
# "modelyear.77", "modelyear.78")
  
# Probamos también con modelyear continua 
  # para obtener más modelos .
  # El archivo auto es antes de pasar modelyear a dummy
  
data<-auto

full<-lm(mpg~.,data=data)
null<-lm(mpg~1,data=data)

selec1<-stepAIC(null,scope=list(upper=full),direction="both",trace=FALSE)

summary(selec1)

formula(selec1)
dput(names(selec1$coefficients))

# mpg ~ weight + modelyear

# c("(Intercept)", "weight", "modelyear")

dput(names(auto))

# c("cylinders", "displacement", "horsepower", "weight", "acceleration", 
# "modelyear", "mpg")

source("c:/funcion steprepetido.R")

lista<-steprepetido(data=data,vardep=c("mpg"),
listconti=
c("cylinders", "displacement", "horsepower", "weight", "acceleration", 
"modelyear"),
sinicio=12345,sfinal=12385,porcen=0.8,criterio="AIC")

tabla<-lista[[1]]

dput(lista[[2]][[1]])

lista<-steprepetido(data=data,vardep=c("mpg"),
listconti=
c("cylinders", "displacement", "horsepower", "weight", "acceleration", 
"modelyear"),
sinicio=12345,sfinal=12385,porcen=0.8,criterio="BIC")

tabla<-lista[[1]]

dput(lista[[2]][[1]])

# SALE SIEMPRE EL MISMO MODELO

# COMPARAMOS TODOS LOS MODELOS CON VALIDACIÓN CRUZADA REPETIDA
# PARA LOS DE MODELYEAR CATEGORICA USAMOS 
# EL ARCHIVO auto3
# PARA LOS DE MODELYEAR CATEGORICA USAMOS 
# EL ARCHIVO auto
# COMO SON LAS MISMAS OBSERVACIONES POR EL MISMO ORDEN NO HAY PROBLEMA

source("cruzadas avnnet y lin.R")

medias1<-cruzadalin(data=auto3,
vardep="mpg",listconti=c("horsepower", "weight",
"modelyear.74","modelyear.76", "modelyear.77", 
"modelyear.78", "modelyear.79", "modelyear.80", "modelyear.81", "modelyear.82"),
listclass=c(""),grupos=4,sinicio=1234,repe=30)
 
medias1$modelo=1

medias2<-cruzadalin(data=auto3,
vardep="mpg",listconti=c("horsepower", "weight",
"modelyear.73","modelyear.77","modelyear.78",
 "modelyear.79", "modelyear.80",
 "modelyear.81", "modelyear.82"),
listclass=c(""),grupos=4,sinicio=1234,repe=30)
 
medias2$modelo=2

medias3<-cruzadalin(data=auto3,
vardep="mpg",listconti=c("weight",
"modelyear.73","modelyear.77","modelyear.78",
 "modelyear.79", "modelyear.80",
 "modelyear.81", "modelyear.82"),
listclass=c(""),grupos=4,sinicio=1234,repe=30)
 
medias3$modelo=3

medias4<-cruzadalin(data=auto,
vardep="mpg",listconti=c("weight","modelyear"),
listclass=c(""),grupos=4,sinicio=1234,repe=30)
 
medias4$modelo=4

union1<-rbind(medias1,medias2,medias3,medias4)

par(cex.axis=0.5)
boxplot(data=union1,col="pink",error~modelo)

# TOMAMOS EL MODELO 1 COMO REFERENCIA Y HACEMOS REDES
# EL PROBLEMA ES EL NÚMERO DE VARIABLES (10)

# PRIMERO TUNEAMOS CON CARET PARA VER EL NÚMERO DE NODOS
# LO MEJOR: UTILIZAR AVNNET Y CONTROL CON CV REPETIDA

# NOTA: PARA PEGAR LA FORMULA CON EL SIGNO MAS UTILIZAMOS EL TRUCO SIGUIENTE

modelo1<-c("horsepower", "weight",
"modelyear.74","modelyear.76", "modelyear.77",
"modelyear.78", "modelyear.79", "modelyear.80", "modelyear.81", "modelyear.82")

cosa<-paste(modelo1,sep="",collapse='+')
cat(cosa)

library(caret)

control<-trainControl(method = "repeatedcv",
 number=4,repeats=5,savePredictions = "all") 

set.seed(123)
nnetgrid <-  expand.grid(size=c(5,8,10,12,15,20),
 decay=c(0.01,0.1),bag=F)

rednnet<- train(mpg ~ 
horsepower+weight+modelyear.74+modelyear.76+modelyear.77+modelyear.78+
  modelyear.79+modelyear.80+modelyear.81+modelyear.82,
 data=auto3,
 method="avNNet",linout = TRUE,maxit=100,
 trControl=control,repeats=5,tuneGrid=nnetgrid)

rednnet

# size = 5, decay = 0.01

# PROBAMOS LA RED TAMBIÉN CON EL MODELO 4 MAS SENCILLO
# A LAS REDES LES GUSTAN LAS VARIABLES CONTINUAS

library(caret)

control<-trainControl(method = "repeatedcv",
 number=4,repeats=5,savePredictions = "all") 

set.seed(123)
nnetgrid <-  expand.grid(size=c(5,8,10,12,15,20),
 decay=c(0.01,0.1),bag=F)

rednnet<- train(mpg ~ weight+modelyear,
 data=auto,
 method="avNNet",linout = TRUE,maxit=100,
 trControl=control,repeats=5,tuneGrid=nnetgrid)

rednnet

# size = 20, decay = 0.1
# Mejor size=5,decay=0.1

# FINALMENTE COMPARAMOS LOS DOS MODELOS DE REDES
# CON LOS DE REGRESIÓN ANTERIORES CON CV REPETIDA

medias5<-cruzadaavnnet(data=auto3,
vardep="mpg",listconti=c("horsepower", "weight",
"modelyear.74","modelyear.76", "modelyear.77", 
"modelyear.78", "modelyear.79", "modelyear.80", "modelyear.81", "modelyear.82"),
listclass=c(""),grupos=4,sinicio=1234,repe=30,
   size=c(5),decay=c(0.1),repeticiones=5,itera=100)
 
medias5$modelo=5

medias6<-cruzadaavnnet(data=auto,
vardep="mpg",listconti=c("weight","modelyear"),
listclass=c(""),grupos=4,sinicio=1234,repe=30,
   size=c(5),decay=c(0.1),repeticiones=5,itera=100)
 
medias6$modelo=6


union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6)

par(cex.axis=0.5)
boxplot(data=union1,col="pink",error~modelo)

