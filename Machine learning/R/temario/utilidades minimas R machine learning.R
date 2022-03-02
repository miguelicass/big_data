
# *****************************************************************
# ALGUNAS UTILIDADES BÁSICAS PARA MACHINE LEARNING Y DATA MINING
# *****************************************************************

load("ameshousing.Rda")  

# Obtener listado de variables en la consola para copiar y pegar 
dput(names(ames))

# Observar variables y su tipo:
# 
# General
str(ames)

# Guardar numericas, caracter, factores en diferentes listados 
# para copiar y pegar

numericas<-names(Filter(is.numeric, ames))
caracter<-names(Filter(is.character, ames))
factores<-names(Filter(is.factor, ames))

dput(numericas)
dput(caracter)

# Observar gráficamente missing y estructura
library(naniar)
gg_miss_var(ames)

# ************************************
# TRATAMIENTO DE MISSING BÁSICO
# ************************************

# ************************************
# MISSING POR VARIABLES
# ************************************

# 1)OBSERVAR LOS MISSING EN VARIABLES CONTINUAS O NUMERICAS
numericas<-names(Filter(is.numeric, ames))
caracter<-names(Filter(is.character, ames))
factores<-names(Filter(is.factor, ames))

tablamis1<-as.data.frame(sapply(ames[,numericas],function(x) sum(is.na(x))))
names(tablamis1)[1]<-"nmiss"

# 2)OBSERVAR LOS MISSING EN VARIABLES DE CARACTER O FACTORES

caracter<-names(Filter(is.character, ames))
tablamis2<-as.data.frame(sapply(ames[,caracter],function(x) sum(is.na(x))))

names(tablamis2)[1]<-"nmiss"

# 3) CREAR LISTA DE VARIABLES CON MÁS DE K MISSING PARA ELIMINAR

# a) uno las tablas  tablamis1 y 2
tablatotal<-as.data.frame(rbind(tablamis1,tablamis2))

# b) lista variables con más de k=300 missing
# Primero guardo los nombres
tablatotal$nombrevar<-row.names(tablatotal)

listamis<-tablatotal[which(tablatotal$nmiss>300),]

listaborrar<-dput(listamis$nombrevar)

# 4) ELIMINAR VARIABLES DE LA LISTA CON MUCHOS MISSING 

ames2<-ames[, !(colnames(ames) %in% listaborrar)]


# *******************************************************************
# ANTES DE IMPUTAR, BUSCAMOS OBSERVACIONES CON MÁS DE K missing, sabiendo 
# en nuestro ejemplo hay 82 variables, eliminaremos observaciones con más de 
# 10 missing (esto es arbitrario)

ames2$contarmis<- apply(ames2, 1, function(x) sum(is.na(x)))

ames2<-ames2[ames2$contarmis<=10,]

# *******************************************************************

# 5) GUARDAR VARIABLES NUMÉRICAS CON ALGÚN MISSING PARA IMPUTAR

lista<-tablatotal[which(tablatotal$nmiss<=300&tablatotal$nmiss>0),]
comple<-tablatotal[which((tablatotal$nmiss==0)),]
listacomple<-dput(comple$nombrevar)

listamisimp<-dput(lista$nombrevar)


# 6) IMPUTAR POR LA MEDIANA O MEDIA EN CONTINUAS

lista1<-intersect(listamisimp, numericas)

ames3<-as.data.frame(sapply(ames2[,lista1],function(x) ifelse(is.na(x),median(x,na.rm=TRUE),x)))

# 7) IMPUTAR POR LA MODA EN CARACTER

lista2<-intersect(listamisimp, caracter)

Mode <- function(x){
a = table(x) # x is a vector
return(names(a[which.max(a)]))
}

ames4<-as.data.frame(sapply(ames2[,lista2],function(x) ifelse(is.na(x),Mode(x),x)))

# 8) UNIR a) imputadas continuas b) imputadas caracter c) columnas complementarias

amesfin<-as.data.frame(cbind(ames3,ames4,ames2[,listacomple]))

# *************************************
# TRATAMIENTO CATEGÓRICAS
# *************************************

# Buscar variables numéricas con k o menos 
# valores diferentes, para ver si pueden ser categóricas

# (Igualmente habría que investigar en el listado de numéricas 
# si se ha colado alguna categórica)

numericas<-names(Filter(is.numeric, amesfin))
caracter<-names(Filter(is.character, amesfin))
factores<-names(Filter(is.factor, amesfin))

lista<-names(amesfin[, sapply(amesfin[,numericas],
function(col) length(table(col))) < 4])

dput((lista))

# Lista de Frecuencias de las categóricas, útil pues algunos niveles
# con pocas observaciones no deben ser tenidos en cuenta 
# en modelos de machine learning para evitar sobreajuste

library(plyr)
frecu<-ldply(amesfin[,caracter],function(x) t(rbind(names(table(x)),table(x))))
names(frecu)<-c("variable","nivel","frecuencia")
frecu$frecuencia<-as.numeric(frecu$frecuencia)

frecu

# Obtener dummies (en el ejemplo solo con caracter, 
# hay que tener en cuenta que muchas variables numéricas
# pueden ser categóricas). Las dummies sustituyen a las variables originales,
# con lo que es mejor crear un archivo nuevo por si queremos utilizar las 
# originales en algún momento

library(dummies)
amesbis<-dummy.data.frame(amesfin, caracter, sep = ".")

# Para borrar las dummies con menos de k observaciones se utiliza el 
# listado de frecuencias frecu obtenido anteriormente

# 1) Obtengo filas de frecu con frecuencia menor que k=20 como ejemplo
frecu20<-frecu[frecu$frecuencia<20,]

# 2) Obtengo listado de los niveles en el mismo formato que las dummies,
# con separador .
frecu20$dum<-paste(frecu20$variable,frecu20$nivel,sep=".")
listamal<-dput(frecu20$dum)

# 3) Borro las dummies de amesbis que coinciden con la listamal
amesbis[,listamal]<-NULL

# save(amesbis,file="amesbis.Rda")
