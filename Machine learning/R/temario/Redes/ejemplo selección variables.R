
library(MASS)

load("compressbien.Rda")

dput(names(compressbien))

listconti<-c("cement", "blast", "ash", "water",
 "plasti", "aggreg", "fineagg", "age")
vardep<-c("cstrength")
 
# Del archivo, solo me quedo con las variables que me interesan,
# así es más facil poner ~. en el modelo

# Si alguna vez tenemos que pegar la formula del modelo a partir de una lista de 
# variables listconti , para no tener que eliminar comillas y escribir se usa este truco:
#  
# cosa<-paste(listconti,sep="",collapse='+')
# cat(cosa)

# Este ejemplo es para AIC, si se quiere BIC  se calcula log(n) y se pone k=log(n) 
# en el stepAIC. Como truco para reducir las variables del modelo se puede poner 
# k grande

data<-compressbien[,c(listconti,vardep)]

full<-lm(cstrength~.,data=data)
null<-lm(cstrength~1,data=data)

selec1<-stepAIC(null,scope=list(upper=full),direction="both",trace=FALSE)

summary(selec1)

# Esto es para obtener la lista de variables que entran en el modelo y pegarla 
# en la función de validación cruzada. HAY QUE BORRAR LA CONSTANTE (Intercept) 
# DEL MODELO

dput(names(selec1$coefficients))

# Esto si se quiere en versión formula
formula(selec1)

# La función steprepetido permite realizar el proceso training test
# varias veces obteniendo el modelo por stepwise sobre datos train
# y la tabla de frecuencias de los modelos escogidos.
# 
# Previamente hay que pre procesar y meter en dummies las variables
# categóricas y meterlas en la lista listconti.

source("funcion steprepetido.R")

lista<-steprepetido(data=compressbien,vardep=c("cstrength"),
listconti=c("cement", "blast", "ash", "water",
"plasti", "aggreg", "fineagg", "age"),
sinicio=12345,sfinal=12355,porcen=0.8,criterio="AIC")

tabla<-lista[[1]]

dput(lista[[2]][[1]])
dput(lista[[2]][[2]])

lista<-steprepetido(data=compressbien,vardep=c("cstrength"),
listconti=c("cement", "blast", "ash", "water",
"plasti", "aggreg", "fineagg", "age"),
sinicio=12345,sfinal=12355,porcen=0.8,criterio="BIC")

tabla<-lista[[1]]

dput(lista[[2]][[1]])
dput(lista[[2]][[2]])


load("fitnessbien.Rda")

# La función dput es muy importante para copiar y pegar 
# modelos y listas de variables

dput(names(fitnessbien))

# En la consola quedan los nombres de las variables para copiar y pegar

# c("AERO", "AGE", "HEART", "EXER", "TEACHER.Amund", "TEACHER.Czika", 
# "TEACHER.Reed", "TEACHER.Yang", "SEX.F", "SEX.M")

listconti<-c("AGE", "HEART", "EXER", "SEX.F", "SEX.M")
vardep<-c("AERO")

lista<-steprepetido(data=fitnessbien,vardep=vardep,
listconti=listconti,sinicio=12345,sfinal=12385,porcen=0.8,criterio="AIC")

tabla<-lista[[1]]

dput(lista[[2]][[1]])
dput(lista[[2]][[2]])

lista<-steprepetido(data=fitnessbien,vardep=vardep,
listconti=listconti,sinicio=12345,sfinal=12385,porcen=0.8,criterio="BIC")

tabla<-lista[[1]]

dput(lista[[2]][[1]])
dput(lista[[2]][[2]])

# En este último ejemplo, hay varios modelos plausibles, 
# uno muy simple. Podemos probar validación cruzada repetida para comparar
# vía sesgo-varianza

source("cruzadas avnnet y lin.R")

medias1<-cruzadalin(data=fitnessbien,
vardep=vardep,listconti=c("SEX.F","EXER"),
listclass=c(""),grupos=4,sinicio=1234,repe=30)
 
medias1$modelo="SEX.F,EXER"

medias2<-cruzadalin(data=fitnessbien,
vardep=vardep,listconti=c("SEX.F","AGE","HEART"),
listclass=c(""),grupos=4,sinicio=1234,repe=30)
 
medias2$modelo="SEX.F,AGE,HEART"

union1<-rbind(medias1,medias2)

par(cex.axis=0.5)
boxplot(data=union1,col="pink",error~modelo)

