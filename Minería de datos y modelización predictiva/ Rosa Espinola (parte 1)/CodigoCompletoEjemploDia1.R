# Cargo las funciones que voy a utilizar despu�s
source("FuncionesRosa.R")

# Cargo las librerias 

# Cargo los datos con el bot�n "Import y le pongo de nombre "datos"
str(datos) 
#Comprobamos el tipo de variable asignado a cada una
#No todas las categ�ricas est�n como factores

# Indico los factores
datos[,c(3,12:15)] <- lapply(datos[,c(3,12:15)], factor)
summary(datos)

# Cuento el n�mero de valores diferentes para las num�ricas
sapply(Filter(is.numeric, datos),function(x) length(unique(x))) #Si detecto alguna variable adicional, uso el c�digo de antes

# Ver el reparto de las categor�as de las variables cualitativas
freq(datos$CalifProductor)
freq(datos$Etiqueta)

# Para otros estad�sticos
describe(Filter(is.numeric, datos)) #hay otro describe en otra libreria

# Missings no declarados variables cualitativas (NSNC, ?)
datos$Clasificacion<-recode.na(datos$Clasificacion,"?")

# Missings no declarados variables cuantitativas (-1, 99999)
datos$Azucar<-replace(datos$Azucar,which(datos$Azucar==99999),NA)

# Valores fuera de rango
datos$Alcohol<-replace(datos$Alcohol, which((datos$Alcohol < 0)|(datos$Alcohol>100)), NA)

#Errores de escritura en variables cualitativas. En este caso, se puede usar tambi�n "toupper()"
datos$Etiqueta<-recode(datos$Etiqueta, "'b'='B';'m'='M';'mb'='MB';'mm'='MM';'r'='R'")

#Variables cualitativas con categor�as poco representadas
datos$CalifProductor<-recode(datos$CalifProductor, "c(0,1)='0-1';c(5,6,7,8,9,10,11,12)='5-12'")

#Indico la variableObj, el ID y las Input (los at�picos y los missings se gestionan s�lo de las input)
varObjCont<-datos$Beneficio
varObjBin<-datos$Compra
input<-as.data.frame(datos[,-(1:3)])
row.names(input)<-datos$ID #Se considera la variable identificadora

##At�picos
# Cuento el porcentaje de at�picos de cada variable. Si son muchos, elimino esas variables
sapply(Filter(is.numeric, input),function(x) atipicosAmissing(x)[[2]])/nrow(input)
# Modifico los at�picos como missings
input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(Filter(is.numeric, input),function(x) atipicosAmissing(x)[[1]])

## MISSINGS
#Busco si existe alg�n patr�n en los missings, que me pueda ayudar a entenderlos
corrplot(cor(is.na(input[colnames(input)[colSums(is.na(input))>0]])),method = "ellipse",type = "upper") #No se aprecia ning�n patr�n

#Proporci�n de missings por variable y observaci�n
input$prop_missings<-apply(is.na(input),1,mean)
summary(input$prop_missings)
(prop_missingsVars<-apply(is.na(input),2,mean))

#elimino las observaciones y las variables con m�s de la mitad de datos missings (No hay ninguna, no ejecuto este c�digo)
input <- subset(input, prop_missings< 0.5, select=names(prop_missingsVars)[prop_missingsVars<0.5])
varObjBin<-varObjBin[input$prop_missings<0.5] #Actualizar las observaciones de las variables objetivo
varObjCont<-varObjCont[input$prop_missings<0.5]

#Recategorizo categ�ricas con "suficientes" observaciones missings
#Solo la variable Clasificaci�n que es la que tiene un 26% missing
#Se considera una categor�a m�s los missing.
input$Clasificacion<-recode(input$Clasificacion,"NA='Desconocido'",as.factor = T)

## Imputaciones
# Imputo todas las cuantitativas, seleccionar el tipo de imputaci�n: media, mediana o aleatorio
input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(Filter(is.numeric, input),function(x) ImputacionCuant(x,"aleatorio"))

# Imputo todas las cualitativas, seleccionar el tipo de imputaci�n: moda o aleatorio
# Si solo se quiere imputar una, variable<-ImputacionCuali(variable,"moda")
input[,as.vector(which(sapply(input, class)=="factor"))]<-sapply(Filter(is.factor, input),function(x) ImputacionCuali(x,"aleatorio"))
# A veces se cambia el tipo de factor a character al imputar, as� que hay que indicarle que es factor
input[,as.vector(which(sapply(input, class)=="character"))] <- lapply(input[,as.vector(which(sapply(input, class)=="character"))] , factor)

# Reviso que no queden datos missings
summary(input)

# Una vez finalizado este proceso, se puede considerar que los datos est�n depurados. Los guardamos
saveRDS(cbind(varObjBin,varObjCont,input),"datosVinoDep")


# Ver el directorio de trabajo y cambiarlo
getwd()
setwd("C:\otro_directorio")
