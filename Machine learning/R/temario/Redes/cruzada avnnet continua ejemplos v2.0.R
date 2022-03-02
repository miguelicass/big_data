
## Ejemplo de utilización cruzada AVNNET
source("cruzadas avnnet y lin.R")
load("compressbien.Rda") 
data<-compressbien 

medias1<-cruzadaavnnet(data=data,
vardep="cstrength",listconti=c("age","water"),
listclass=c(""),grupos=4,sinicio=1234,repe=5,
size=c(15),decay=c(0.01),repeticiones=5,itera=100)
 
medias1$modelo="avnnet"

## Ejemplo de utilización cruzada regresión lineal

medias2<-cruzadalin(data=data,
vardep="cstrength",listconti=c("age","water"),
listclass=c(""),grupos=4,sinicio=1234,repe=5)
 
medias2$modelo="lineal"

## Unión de las medias y presentación del boxplot

union1<-rbind(medias1,medias2)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo)


# Ejemplo comparación tres redes recomendadas por nuestro
# trabajo previo con tuning con caret

medias1<-cruzadaavnnet(data=data,
vardep="cstrength",listconti=c("age","water"),
listclass=c(""),grupos=4,sinicio=1234,repe=5,
size=c(15),decay=c(0.01),repeticiones=5,itera=100)
 
medias1$modelo="avnnet1"

medias2<-cruzadaavnnet(data=data,
vardep="cstrength",listconti=c("age","water"),
listclass=c(""),grupos=4,sinicio=1234,repe=5,
size=c(20),decay=c(0.001),repeticiones=5,itera=100)
 
medias2$modelo="avnnet2"

medias3<-cruzadaavnnet(data=data,
vardep="cstrength",listconti=c("age","water"),
listclass=c(""),grupos=4,sinicio=1234,repe=5,
size=c(10),decay=c(0.01),repeticiones=5,itera=100)
 
medias3$modelo="avnnet3"


union1<-rbind(medias1,medias2,medias3)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo)



# Ejemplo comparación diferentes modelos
# Las variables input no están estandarizadas
# pero esta operación se realiza internamente
# dentro de la función cruzada


load("compress.Rda")
data<-compress

medias1<-cruzadaavnnet(data=data,
vardep="cstrength",listconti=c("age","water"),
listclass=c(""),grupos=4,sinicio=1234,repe=5,
size=c(15),decay=c(0.01),repeticiones=5,itera=100)
 
medias1$modelo="avnnet1"

medias2<-cruzadaavnnet(data=data,
vardep="cstrength",listconti=c("age","water","cement"),
listclass=c(""),grupos=4,sinicio=1234,repe=5,
size=c(15),decay=c(0.01),repeticiones=5,itera=100)
 
medias2$modelo="avnnet2"

medias3<-cruzadaavnnet(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),grupos=4,sinicio=1234,repe=5,
size=c(15),decay=c(0.01),repeticiones=5,itera=100)
 
medias3$modelo="avnnet3"

medias4<-cruzadalin(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),grupos=4,sinicio=1234,repe=5)
 
medias4$modelo="lineal"


union1<-rbind(medias1,medias2,medias3,medias4)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo)

# Nota: en el archivo de union se pueden seleccionar 
# los modelos para observar mejor ciertas comparaciones
 
union1<-rbind(medias2,medias3)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo)


