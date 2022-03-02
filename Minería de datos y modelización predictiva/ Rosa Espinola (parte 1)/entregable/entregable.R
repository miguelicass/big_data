# install.packages("questionr")
# install.packages("psych")
# install.packages("car")
# install.packages("corrplot")
# install.packages("caret")
# install.packages("ggplot2")
# install.packages("lmSupport")
# install.packages("unmarked")
# install.packages("VGAM")
# 
# install.packages("pROC")
# install.packages("glmnet")

# install.packages ("readxl")
# install.packages ("tidyr")

library(questionr)
library(psych)
library(car)
library(corrplot)
library(caret)
library(ggplot2)
library(lmSupport)
library(unmarked)
library(VGAM)

library(pROC)
library(glmnet)

library(readxl)
library(tidyr)

#######################
#### LIMPIAR DATOS ####
#######################

getwd()
setwd("/Users/macasib/Desktop/entregable_temp/entregable")
#setwd("/Users/macasib/Desktop/entregable_temp")

## CARGAMOS EL PAQUETE DE FUNCIONES
source("FuncionesRosa.R")

## CARGA DE DATOS
datos <- read_excel("DatosEleccionesEspaña.xlsx")

## TIPOS
#compruebo los tipos
str(datos) 
#factores
datos[,c(2,3,7,11,12,34,38)] <- lapply(datos[,c(2,3,7,11,12,34,38)], factor)
#compruebo observaciones
summary(datos)
#numero de valores diferentes para las variables numericas
sapply(Filter(is.numeric, datos),function(x) length(unique(x))) #Si detecto alguna variable adicional, uso el c?digo de antes

## CORRECCION DE ERRORES
## Observaciones poco representadas
# freq(datos$CCAA)
# freq(datos$CodigoProvincia)
freq(datos[,2])
freq(datos[,3])
freq(datos[,7])
freq(datos[,11])
freq(datos[,12])
freq(datos[,34])
freq(datos[,38])

#Recode de las variables poco representadas
#ActividadPpal
freq(datos$ActividadPpal)
datos$ActividadPpal<-recode(datos$ActividadPpal, "c('Industria','Servicios', 'Construccion')='Serv-Constr-Industr'")
freq(datos$ActividadPpal)

#CodigoProvincia
freq(datos$CodigoProvincia)
cadizCode <- subset(datos, datos$Name == "Cádiz") #11
melillaCode <- subset(datos, datos$Name == "Melilla") #52
ceutaCode <- subset(datos, datos$Name == "Ceuta") #51
datos$CodigoProvincia<-recode(datos$CodigoProvincia, "c('51','52')='11'") #"c('ceutaCode$CodigoProvincia','melillaCode$CodigoProvincia')='cadizCode$CodigoProvincia'")
freq(datos$CodigoProvincia)

#CCAA
freq(datos$CCAA)
datos$CCAA<-recode(datos$CCAA, "c('Melilla','Ceuta')='Andalucía'")
freq(datos$CCAA)

## Comprobar >30% en abtencion
##### NO CASE #####
#datos$AbstencionAux<-datos$AbstentionPtge
#datos$AbstencionAux<-replace(datos$AbstencionAux,which(datos$AbstencionAux<=30),0)
#datos$AbstencionAux<-replace(datos$AbstencionAux,which(datos$AbstencionAux>30),1)
#datos[,c(-1)] <- lapply(datos[,c(-1)], factor)
#datos$AbstencionAlta<-replace(datos$AbstencionAlta,which(datos$AbstencionAlta!=datos$AbstencionAux),NA)

## Missings no declarados variables cualitativas (NSNC, ?)
datos$Densidad<-recode.na(datos$Densidad,"?")

## Compruebo observaciones (para variables cuantitativas)
summary(datos)
#otros estadissticos
describe(Filter(is.numeric, datos)) #numericos

## Missings no declarados variables cuantitativas (99999)
datos$Explotaciones<-replace(datos$Explotaciones,which(datos$Explotaciones==99999),NA)

## Valores fuera de rango
datos$ForeignersPtge<-replace(datos$ForeignersPtge, which((datos$ForeignersPtge < 0)), NA)
datos$PobChange_pct<-replace(datos$PobChange_pct, which((datos$PobChange_pct > 100)), NA)
datos$SameComAutonPtge<-replace(datos$SameComAutonPtge, which((datos$SameComAutonPtge > 100)), NA)

## Comprobar el porcentaje de votos total != 100%
##### NO CASE #####
# datos$totalPercentVotes<-datos$Izda_Pct+datos$Dcha_Pct+datos$Otros_Pct+datos$AbstentionPtge
# datos$totalPercentVotes<-replace(datos$totalPercentVotes, which((datos$totalPercentVotes > 100)), NA)

## Errores de escritura en variables cualitativas. 
##### NO CASE #####
#EJEMPLO VINO => En este caso, se puede usar tambien "toupper()"
#datos$Etiqueta<-recode(datos$Etiqueta, "'b'='B';'m'='M';'mb'='MB';'mm'='MM';'r'='R'")

## INPUTS & OBJETIVOS
#Indico la variableObj, el ID y las Input (los atipicos y los missings se gestionan en las input)
varObjCont<-datos$AbstentionPtge
varObjBin<-datos$AbstencionAlta
input <- as.data.frame(datos[,c(-(1),-(6:12))])
#para obtener identificador único
#no se puede identificar por Name 
#porque hay repeticiones en diferentes CCAA
datos$auxID <-unite(datos, auxID, c(1:2),  sep = " ", remove = FALSE)$auxID
row.names(input)<-datos$auxID

## ATIPICOS
# Cuento el porcentaje de atipicos en variables (numericas). Si son muchos, elimino esas variables
sapply(Filter(is.numeric, input),function(x) atipicosAmissing(x)[[2]])/nrow(input)
# Modifico los atipicos como missings
input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(Filter(is.numeric, input),function(x) atipicosAmissing(x)[[1]])

## MISSINGS
##Busco si existe algun patron en los missings, que me pueda ayudar a entenderlos
corrplot(cor(is.na(input[colnames(input)[colSums(is.na(input))>0]])),method = "ellipse",type = "upper",  tl.cex = 0.75)

## Proporcion de missings por OBSEVACION
input$prop_missings<-apply(is.na(input),1,mean) #missing por observacion
summary(input$prop_missings) #missing por observacion
## Proporcion de missings por VARIABLE
(prop_missingsVars<-apply(is.na(input),2,mean)) #missing por variables
#no presenta muchos missing ninguna

## Eliminaciones
##### NO CASE #####
# generalmente a partir del 50%
# varObjBin<-varObjBin[input$prop_missings<0.5] #Actualizar las observaciones de las variables objetivo
# varObjCont<-varObjCont[input$prop_missings<0.5]
# input <- subset(input, prop_missings< 0.5, select=names(prop_missingsVars)[prop_missingsVars<0.333])

## Recategorizo categoricas con "suficientes" observaciones missings
##### NO CASE #####
# #EJEMPLO VINO
# #Solo la variable Clasificacion que es la que tiene un 26% missing
# #Se considera una categoraa mas los missing.
# input$Clasificacion<-recode(input$Clasificacion,"NA='Desconocido'",as.factor = T)

## Imputaciones
##Repetir hasta que no queden NA's (solo imputacion con valores aleartorios)
# Imputo todas las CUANTITATIVAS, seleccionar el tipo de imputacion: mediana //no used: media, aleatorio
input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(Filter(is.numeric, input),function(x) ImputacionCuant(x,"mediana"))
# Imputo todas las CUALITATIVAS, seleccionar el tipo de imputacion: moda // no used: moda, aleatorio
# Si solo se quiere imputar una, variable<-ImputacionCuali(variable,"moda")
input[,as.vector(which(sapply(input, class)=="factor"))]<-sapply(Filter(is.factor, input),function(x) ImputacionCuali(x,"moda"))
# A veces se cambia el tipo de factor a character al imputar, as? que hay que indicarle que es factor
input[,as.vector(which(sapply(input, class)=="character"))] <- lapply(input[,as.vector(which(sapply(input, class)=="character"))] , factor)

# Reviso que no queden datos missings
summary(input)
str(input) 
str(datos) 
# cuantitativas
freq(input$CodigoProvincia)
freq(input$CCAA)
freq(input$ActividadPpal)
freq(input$Densidad)

# Una vez finalizado este proceso, se puede considerar que los datos estan depurados. Los guardamos
saveRDS(cbind(varObjBin,varObjCont,input),"datosEleccionesDep")

#comprobar el directorio de trabajo
getwd()
# setwd("/Users/macasib/Desktop/entregable_temp/entregable")




#TRANSFORMACIONES Y RELACIONES VARIABLES OBJETIVOS

 

# Para ejecutar este codigo vuelvo
# a limpiar el historial de datos

#diectorio de trabajo
getwd()
setwd("/Users/macasib/Desktop/entregable_temp/entregable")

## CARGAMOS EL PAQUETE DE FUNCIONES
source("FuncionesRosa.R")

# Parto de los datos sin atipicos ni ausentes guardados
datos<-readRDS("datosEleccionesDep")
varObjCont<-datos$varObjCont
varObjBin<-datos$varObjBin
input<-datos[,-(1:2)]

#Obtengo la importancia de las variables. Falla si hay alguna variable cuantitativa con menos de 6 valores diferentes
graficoVcramer(input,varObjBin)
graficoVcramer(input,varObjCont) #regresion lineal ObjContinua # tl.cex = 0.75, cl.cex = 0.75

# Descarto CodProvincia debido a la multicolinealidad 
input<-input[,-(1)]

#Obtengo la importancia de las variables. Falla si hay alguna variable cuantitativa con menos de 6 valores diferentes
graficoVcramer(input,varObjBin)
graficoVcramer(input,varObjCont) #regresion lineal ObjContinua

#SOBRE LA BINARIA
#Veo graficamente el efecto de dos variables cualitativas sobre la binaria
#Tendria que hacerse para todas las CUALITATIVAS
mosaico_targetbinaria(input$Densidad,varObjBin,"Densidad") #esta si influye
mosaico_targetbinaria(input$CCAA,varObjBin,"CCAA") #esta si influye
barras_targetbinaria(input$Densidad,varObjBin,"Densidad")
barras_targetbinaria(input$CCAA,varObjBin,"CCAA")

#Veo graficamente el efecto de dos variables cuantitativas sobre la binaria
#Hacerse para todas las CUANTITATIVAS
boxplot_targetbinaria(input$UnemployLess25_Ptge,varObjBin,"UnemployLess25_Ptge")
boxplot_targetbinaria(input$PersonasInmueble,varObjBin,"PersonasInmueble")
hist_targetbinaria(input$UnemployLess25_Ptge,varObjBin,"UnemployLess25_Ptge")
hist_targetbinaria(input$PersonasInmueble,varObjBin,"PersonasInmueble")

#SOBRE LA CONTINUA
#Todas las variables numuricas frente a la objetivo continua
graficoCorrelacion(varObjCont,input[,1:10]) #[,1:10] #Nos fijamos en la forma de las l?neas rojas (si hay muchas variables numuricas, tarda un poco)
#correlacion entre las variables numericas
corrplot(cor(cbind(varObjCont,Filter(is.numeric, input)), use="pairwise", method="pearson"), method = "ellipse",type = "upper", tl.cex = 0.75)

## TRANSFORMACIONES VARIABLES NUMERICAS
#Busco las mejores transformaciones para las variables numuricas con respesto a los dos tipos de variables
input_cont<-cbind(input,Transf_Auto(Filter(is.numeric, input),varObjCont))
input_bin<-cbind(input,Transf_Auto(Filter(is.numeric, input),varObjBin))
names(input_cont[,-(1:33)])
names(input_bin[,-(1:33)])

saveRDS(data.frame(input_bin,varObjBin),"todo_bin")
saveRDS(data.frame(input_cont,varObjCont),"todo_cont")


##########################
#### REGRESION LINEAL ####
##########################

## ObjContinua

# Para ejecutar este codigo vuelvo 
# a limpiar el historial de datos

#Cargo el paquete de FuncionesRosa.R 
source("FuncionesRosa.R")

# no utilizo estos comandos, tomo los siguientes recuprando los guardados
# todo<-data.frame(input,varObjCont)

#Cargo los datos depurados
todo<-readRDS("todo_cont")

#eliminar variaables NA
names(todo)
todo<-todo[,c(-(37:38),-(40),-(59),-(61),-(63))]

#Obtengo la particion
set.seed(123456)
trainIndex <- createDataPartition(todo$varObjCont, p=0.8, list=FALSE)
data_train <- todo[trainIndex,]
data_test <- todo[-trainIndex,]

## MODELOS MANUALES
#Construyo un modelo preliminar con todas las variables
modelo1<-lm(varObjCont~.,data=data_train)
summary(modelo1)
Rsq(modelo1,"varObjCont",data_train)
Rsq(modelo1,"varObjCont",data_test) 
modelo1$rank
AIC(modelo1)
BIC(modelo1)

# Nos fijamos en la importancia de las variables. Podemos sacar un grafico que muestra lo que se pierde en R2 en train al quitarlas del modelo
modelEffectSizes(modelo1)
barplot(sort(modelEffectSizes(modelo1)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")

#Construyo un modelo con las variables mas significativas (0.2 V de Cramer)
modelo2<-lm(varObjCont~CCAA+ActividadPpal+prop_missings+ComercTTEHosteleria+Construccion+Servicios+Industria+Population+totalEmpresas+TotalCensus+Pob2010,data=data_train)
summary(modelo2)
Rsq(modelo2,"varObjCont",data_train)
Rsq(modelo2,"varObjCont",data_test) 
modelo2$rank

# #Construyo un modelo con las variables mas significativas (modelo 2 y transformada)
# # modelo3<-lm(varObjCont~CCAA+ActividadPpal+prop_missings+ComercTTEHosteleria+raiz4ComercTTEHosteleria+Construccion+sqrtxConstruccion+Servicios+sqrtxServicios+Industria+raiz4Industria+Population+logxPopulation+totalEmpresas+logxtotalEmpresas+TotalCensus+logxTotalCensus+Pob2010+logxPob2010,data=data_train)
# modelo3<-lm(varObjCont~CCAA+ActividadPpal+prop_missings+ComercTTEHosteleria+raiz4ComercTTEHosteleria+Construccion+sqrtxConstruccion+raiz4Industria+Population+logxPopulation+totalEmpresas+TotalCensus+logxTotalCensus+logxPob2010,data=data_train)
# summary(modelo3)
# Rsq(modelo3,"varObjCont",data_train)
# Rsq(modelo3,"varObjCont",data_test) 
# modelo3$rank
# # AIC(modelo3)

## ELEGIR MODELOS DE FORMA AUTOMATICA
# Seleccion de variables "clasica"
null<-lm(varObjCont~1, data=data_train) #Modelo minimo
full<-lm(varObjCont~., data=data_train[,c(1:33,58)]) #Modelo maximo, le quitamos las transformaciones
# aux <- data_train[,c(1:33,58)]

#MODELOS
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")
summary(modeloStepAIC)
Rsq(modeloStepAIC,"varObjCont",data_train)
Rsq(modeloStepAIC,"varObjCont",data_test)
modeloStepAIC$rank
# coef(modeloStepAIC)

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward")
summary(modeloBackAIC)
Rsq(modeloBackAIC,"varObjCont",data_train)
Rsq(modeloBackAIC,"varObjCont",data_test)
modeloBackAIC$rank

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC)
Rsq(modeloStepBIC,"varObjCont",data_train)
Rsq(modeloStepBIC,"varObjCont",data_test) 
modeloStepBIC$rank

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",k=log(nrow(data_train)))
summary(modeloBackBIC)
Rsq(modeloBackBIC,"varObjCont",data_train)
Rsq(modeloBackBIC,"varObjCont",data_test) 
modeloBackBIC$rank

#Genero todas la iteraciones de las variables
formInt<-formulaInteracciones(todo[,c(1:33,58)],34)#en el subconjunto de las vbles. originales, la objetivo columna 58
fullInt<-lm(formInt, data=data_train) #Modelo con todas las variables y todas las interacciones

modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
summary(modeloStepAIC_int)
Rsq(modeloStepAIC_int,"varObjCont",data_train) 
Rsq(modeloStepAIC_int,"varObjCont",data_test) 
modeloStepAIC_int$rank

modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_int)
Rsq(modeloStepAIC_int,"varObjCont",data_train) 
Rsq(modeloStepBIC_int,"varObjCont",data_test)
modeloStepBIC_int$rank

#Trans e interacciones (modelo completo todas la variables, transformadas)
formIntT<-formulaInteracciones(todo,58)
fullIntT<-lm(formIntT, data=data_train)

modeloStepAIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both")
summary(modeloStepAIC_transInt)
Rsq(modeloStepAIC_transInt,"varObjCont",data_train) 
Rsq(modeloStepAIC_transInt,"varObjCont",data_test)
modeloStepAIC_transInt$rank

modeloStepBIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_transInt)
Rsq(modeloStepBIC_transInt,"varObjCont",data_train) 
Rsq(modeloStepBIC_transInt,"varObjCont",data_test)
modeloStepBIC_transInt$rank
#Por el principio de parsimonia, es preferible el modeloStepBIC_transInt

## Metodo de seleccion aleatoria (submuestra de los datos de entrenamiento)
rep<-100
prop<-0.7 # se realiza con el 70% de los datos de entrenamiento por velocidad. El resultado es el mismo.
modelosGenerados<-c()
for (i in 1:rep){
  set.seed(12345+i)
  subsample<-data_train[sample(1:nrow(data_train),prop*nrow(data_train),replace = T),]
  full<-lm(formIntT,data=subsample)
  null<-lm(varObjCont~1,data=subsample)
  modeloAux<-step(null,scope=list(lower=null,upper=full),direction="both",trace=0,k=log(nrow(subsample)))
  modelosGenerados<-c(modelosGenerados,paste(sort(unlist(strsplit(as.character(formula(modeloAux))[3]," [+] "))),collapse = "+"))
}
freq(modelosGenerados,sort="dec")

## MODELO "HIBRIDO"
# contruyo un modelo con ayuda del metodo clasico y aleatorio(si los hubiere)
modeloHibrido<-lm(varObjCont~raiz4ComercTTEHosteleria+ComercTTEHosteleria+CCAA+prop_missings+logxTotalCensus+WomanPopulationPtge+SameComAutonPtge+Age_over65_pct+TotalCensus+ActividadPpal+sqrtxAge_over65_pct+sqrtxSameComAutonDiffProvPtge+SUPERFICIE+raiz4Industria+PersonasInmueble+prop_missings:ActividadPpal+ActividadPpal:PersonasInmueble,data=data_train)
summary(modeloHibrido)
Rsq(modeloHibrido,"varObjCont",data_train)
Rsq(modeloHibrido,"varObjCont",data_test) 
modeloHibrido$rank

## Comparacion final de los modelos
total<-c()
# modelos seleccionados
modelos<-c("varObjCont ~CCAA+ActividadPpal+SameComAutonPtge+prop_missings+Explotaciones+totalEmpresas+ConstructionUnemploymentPtge+ServicesUnemploymentPtge+IndustryUnemploymentPtge+SUPERFICIE+DifComAutonPtge+Age_0.4_Ptge+WomanPopulationPtge+TotalCensus+SameComAutonDiffProvPtge+ComercTTEHosteleria+AgricultureUnemploymentPtge+Age_over65_pct+Age_under19_Ptge+UnemployLess25_Ptge+inmuebles",
           "varObjCont ~CCAA+ActividadPpal+SameComAutonPtge+ConstructionUnemploymentPtge+SUPERFICIE+Densidad+IndustryUnemploymentPtge+ServicesUnemploymentPtge+Age_19_65_pct+SameComAutonDiffProvPtge+Age_0.4_Ptge+TotalCensus+WomanPopulationPtge+Age_over65_pct+UnemployLess25_Ptge+PersonasInmueble+CCAA:SameComAutonPtge+ActividadPpal:SameComAutonPtge+CCAA:ServicesUnemploymentPtge+CCAA:Age_19_65_pct+Densidad:ServicesUnemploymentPtge+ActividadPpal:Age_19_65_pct+ActividadPpal:IndustryUnemploymentPtge+CCAA:SameComAutonDiffProvPtge+SUPERFICIE:Densidad+Densidad:IndustryUnemploymentPtge+ActividadPpal:TotalCensus+CCAA:WomanPopulationPtge+CCAA:ConstructionUnemploymentPtge+CCAA:Age_over65_pct+Densidad:TotalCensus+ActividadPpal:Age_over65_pct+Densidad:PersonasInmueble+Densidad:WomanPopulationPtge+Densidad:Age_over65_pct",
           "varObjCont ~CCAA+ActividadPpal+SameComAutonPtge+Explotaciones+Age_0.4_Ptge+ConstructionUnemploymentPtge+SUPERFICIE+IndustryUnemploymentPtge+ServicesUnemploymentPtge+Age_19_65_pct+ActividadPpal:SameComAutonPtge+ActividadPpal:Age_19_65_pct",
           "varObjCont ~CCAA+logxinmuebles+prop_missings+sqrtxSameComAutonDiffProvPtge+logxTotalCensus+SameComAutonPtge+Age_over65_pct+WomanPopulationPtge+ActividadPpal+raiz4Industria+SUPERFICIE+totalEmpresas+PersonasInmueble+Age_0.4_Ptge+Age_19_65_pct+ConstructionUnemploymentPtge+sqrtxAge_0.4_Ptge+SameComAutonDiffProvPtge+AgricultureUnemploymentPtge+logxForeignersPtge+CCAA:logxinmuebles+CCAA:sqrtxSameComAutonDiffProvPtge+CCAA:SameComAutonPtge+CCAA:Age_over65_pct+prop_missings:ActividadPpal+WomanPopulationPtge:ActividadPpal+logxTotalCensus:ActividadPpal+Age_over65_pct:ActividadPpal+CCAA:WomanPopulationPtge+ActividadPpal:PersonasInmueble+CCAA:Age_19_65_pct+ActividadPpal:Age_19_65_pct+ActividadPpal:ConstructionUnemploymentPtge+ActividadPpal:totalEmpresas+CCAA:SameComAutonDiffProvPtge+CCAA:AgricultureUnemploymentPtge",
           "varObjCont ~CCAA+prop_missings+logxTotalCensus+WomanPopulationPtge+SameComAutonPtge+Age_over65_pct+TotalCensus+ActividadPpal+sqrtxAge_over65_pct+sqrtxSameComAutonDiffProvPtge+SUPERFICIE+raiz4Industria+PersonasInmueble+CCAA:sqrtxSameComAutonDiffProvPtge+prop_missings:ActividadPpal+ActividadPpal:PersonasInmueble"
           )
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = data_train,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                           nrow(vcr$resample))))
}

boxplot(Rsquared~modelo,data=total,main="R-Square") 
aggregate(Rsquared~modelo, data = total, mean) 
aggregate(Rsquared~modelo, data = total, sd) 

# Modelo ganador
ModeloGanador<-lm(varObjCont~CCAA+prop_missings+logxTotalCensus+WomanPopulationPtge+SameComAutonPtge+Age_over65_pct+TotalCensus+ActividadPpal+sqrtxAge_over65_pct+sqrtxSameComAutonDiffProvPtge+SUPERFICIE+raiz4Industria+PersonasInmueble+CCAA:sqrtxSameComAutonDiffProvPtge+prop_missings:ActividadPpal+ActividadPpal:PersonasInmueble,data=data_train)
# Vemos los coeficientes del modelo ganador
coef(ModeloGanador)
# Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
Rsq(ModeloGanador,"varObjCont",data_train)
Rsq(ModeloGanador,"varObjCont",data_test) 


#############################
#### REGRESION LOGISTICA ####
#############################

## ObjBinaria

getwd()
setwd("/Users/macasib/Desktop/entregable_temp/entregable")

# Cargo las funciones que voy a utilizar despues
source("FuncionesRosa.R")

#Cargo los datos depurados (incluidas las mejores transformaciones de las 
# variables numuricas respecto a la binaria)
todo<-readRDS("todo_bin")

#veo el reparto, la variable ObjBinaria 0 y 1
freq(todo$varObjBin)

#eliminar variaables NA
names(todo)
todo<-todo[,c(-(37:38),-(40),-(59),-(61),-(63))]

#Hago la particiion
set.seed(123456)
trainIndex <- createDataPartition(todo$varObjBin, p=0.8, list=FALSE)
data_train <- todo[trainIndex,] #c(1:33,58)] #quito las variables transformadas 
data_test <- todo[-trainIndex,] #c(1:33,58)] #quito las variables transformadas

## MODELOS MANUAL
#Construyo un modelo preliminar con todas las variables sin las transformadas
modelo1<-glm(varObjBin~.,data=data_train[,c(1:33,58)],family=binomial)
summary(modelo1)
pseudoR2(modelo1,data_train,"varObjBin")
pseudoR2(modelo1,data_test,"varObjBin")
modelo1$rank #numero de parametros

#fijandome en la significacion de las variables,el modelo con las variables mas significativas 
modelo2<-glm(varObjBin~CCAA+totalEmpresas+PersonasInmueble+Age_under19_Ptge+Population+Age_0.4_Ptge+ActividadPpal+Pob2010+TotalCensus+Age_over65_pct+Densidad+inmuebles,data=data_train, family=binomial)
summary(modelo2)
pseudoR2(modelo2,data_train,"varObjBin")
pseudoR2(modelo2,data_test,"varObjBin")
modelo2$rank

## ELEGIR MODELOS DE FORMA AUTOMATICA
# Seleccion de variables "clasica"
null<-glm(varObjBin~1, data=data_train,family=binomial) #Modelo minimo
full<-glm(varObjBin~., data=data_train[,c(1:33,58)],family=binomial) #Modelo maximo, le quitamos las transformaciones

# modeloStepAIC<-glm(varObjBin~CCAA+prop_missings+ActividadPpal+SameComAutonPtge+Age_19_65_pct+TotalCensus+ConstructionUnemploymentPtge+WomanPopulationPtge+Explotaciones+ServicesUnemploymentPtge+AgricultureUnemploymentPtge+totalEmpresas+Construccion+DifComAutonPtge+SameComAutonDiffProvPtge+Age_over65_pct+PersonasInmueble+Industria, family = binomial, data = data_train)
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")
summary(modeloStepAIC)
pseudoR2(modeloStepAIC,data_train,"varObjBin")
pseudoR2(modeloStepAIC,data_test,"varObjBin")
modeloStepAIC$rank

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward")
summary(modeloBackAIC)
pseudoR2(modeloBackAIC,data_train,"varObjBin")
pseudoR2(modeloBackAIC,data_test,"varObjBin")
modeloBackAIC$rank

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC)
pseudoR2(modeloStepBIC,data_train,"varObjBin")
pseudoR2(modeloStepBIC,data_test,"varObjBin")
modeloStepBIC$rank

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",k=log(nrow(data_train)))
summary(modeloBackBIC)
pseudoR2(modeloBackBIC,data_train,"varObjBin")
pseudoR2(modeloBackBIC,data_test,"varObjBin")
modeloBackBIC$rank

#Genero interacciones
formInt<-formulaInteracciones(todo[,c(1:33,58)],34)#en el subconjunto de las vbles. originales, la objetivo est? en la columna 25
fullInt<-glm(formInt, data=data_train,family=binomial) #Modelo con todas las variables y todas las interacciones

modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
summary(modeloStepAIC_int)
pseudoR2(modeloStepAIC_int,data_train,"varObjBin")
pseudoR2(modeloStepAIC_int,data_test,"varObjBin")
modeloStepAIC_int$rank

modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_int)
pseudoR2(modeloStepBIC_int,data_train,"varObjBin")
pseudoR2(modeloStepBIC_int,data_test,"varObjBin") 
modeloStepBIC_int$rank
#Por el principio de parsimonia, es preferible el modeloStepBIC_int

# # Pruebo con todas las transf y las variables originales
# fullT<-glm(varObjCont~., data=data_train,family=binomial)
# 
# modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both")
# summary(modeloStepAIC_trans)
# pseudoR2(modeloStepAIC_trans,data_train,"varObjBin")
# pseudoR2(modeloStepAIC_trans,data_test,"varObjBin")
# modeloStepAIC_trans$rank
# 
# modeloStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",k=log(nrow(data_train)))
# summary(modeloStepBIC_trans)
# pseudoR2(modeloStepBIC_trans,data_train,"varObjBin")
# pseudoR2(modeloStepBIC_trans,data_test,"varObjBin")
# modeloStepBIC_trans$rank

#Trans e interacciones
formIntT<-formulaInteracciones(todo,58)
fullIntT<-glm(formIntT, data=data_train,family=binomial)

modeloStepAIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both")
summary(modeloStepAIC_transInt)
pseudoR2(modeloStepAIC_transInt,data_train,"varObjBin")
pseudoR2(modeloStepAIC_transInt,data_test,"varObjBin")
modeloStepAIC_transInt$rank

modeloStepBIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_transInt)
pseudoR2(modeloStepBIC_transInt,data_train,"varObjBin")
pseudoR2(modeloStepBIC_transInt,data_test,"varObjBin") 
modeloStepBIC_transInt$rank

## Metodo de seleccion aleatoria (submuestra de los datos de entrenamiento)
rep<-100
prop<-0.7 # se realiza con el 70% de los datos de entrenamiento por velocidad. El resultado es el mismo.
modelosGenerados<-c()
for (i in 1:rep){
  set.seed(12345+i)
  subsample<-data_train[sample(1:nrow(data_train),prop*nrow(data_train),replace = T),]
  full<-glm(formIntT,data=subsample,family=binomial)
  null<-glm(varObjBin~1,data=subsample,family=binomial)
  modeloAux<-step(null,scope=list(lower=null,upper=full),direction="both",trace=0,k=log(nrow(subsample)))
  modelosGenerados<-c(modelosGenerados,paste(sort(unlist(strsplit(as.character(formula(modeloAux))[3]," [+] "))),collapse = "+"))
}
freq(modelosGenerados,sort="dec")

# ## MODELO "HIBRIDO"
# # contruyo un modelo con ayuda del metodo clasico y aleatorio(si los hubiere)
# modeloHibrido<-glm(varObjBin~ActividadPpal+ActividadPpal:SameComAutonDiffProvPtge+ActividadPpal:SameComAutonPtge+ActividadPpal:TotalCensus+Age_over65_pct+CCAA+ConstructionUnemploymentPtge+prop_missings+SameComAutonDiffProvPtge+SameComAutonPtge+ServicesUnemploymentPtge+TotalCensus+WomanPopulationPtge, data=data_train, family=binomial)
# summary(modeloHibrido)
# pseudoR2(modeloHibrido,data_train,"varObjBin")
# pseudoR2(modeloHibrido,data_test,"varObjBin")
# modeloHibrido$rank

#Validacion cruzada repetida para elegir entre todos
auxVarObj<-todo$varObjBin
todo$varObjBin<-make.names(todo$varObjBin) #formateo la variable objetivo para que funcione el codigo
total<-c()
modelos<-sapply(list(modeloStepAIC,modeloBackAIC,modeloStepBIC,modeloBackBIC,modeloStepAIC_int,modeloStepBIC_int,modeloStepAIC_transInt,modeloStepBIC_transInt),formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = todo,
             method = "glm", family="binomial",metric = "ROC",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      summaryFunction=twoClassSummary,
                                      classProbs=TRUE,returnResamp="all")
  )
  total<-rbind(total,data.frame(roc=vcr$resample[,1],modelo=rep(paste("Modelo",i),
                                                                nrow(vcr$resample))))
}
boxplot(roc~modelo,data=total,main="area bajo la curva ROC") 
aggregate(roc~modelo, data = total, mean)
aggregate(roc~modelo, data = total, sd) 

#recupero la variable objetivo en su formato
todo$varObjBin<-auxVarObj

## Modelo ganador
modeloGanador <- modeloStepBIC_transInt
summary(modeloGanador)
pseudoR2(modeloGanador,data_train,"varObjBin")
pseudoR2(modeloGanador,data_test,"varObjBin") 
modeloGanador$rank
# Vemos los coeficientes del modelo ganador
coef(modeloGanador)

## Buscamos el mejor punto de corte

#probamos dos
sensEspCorte(modeloGanador,data_test,"varObjBin",0.5,"1")
sensEspCorte(modeloGanador,data_test,"varObjBin",0.75,"1")

## generamos una rejilla de puntos de corte
posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,function(x) sensEspCorte(modeloGanador,data_test,"varObjBin",x,"1")))))
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1
plot(rejilla$posiblesCortes,rejilla$Youden)
plot(rejilla$posiblesCortes,rejilla$Accuracy)
rejilla$posiblesCortes[which.max(rejilla$Youden)]
rejilla$posiblesCortes[which.max(rejilla$Accuracy)]

#El resultado es 0.29 para youden y 0.45 para Accuracy
#Los comparamos
sensEspCorte(modeloGanador,data_test,"varObjBin",0.29,"1") # Youden
sensEspCorte(modeloGanador,data_test,"varObjBin",0.45,"1") # Accuracy

# Vemos las variables mas importantes del modelo ganador
impVariablesLog(modeloGanador,"varObjBin")

# Vemos los coeficientes del modelo ganador
coef(modeloGanador)

#Evaluamos la estabilidad del modelo en test:
pseudoR2(modeloGanador,data_train,"varObjBin")
pseudoR2(modeloGanador,data_test,"varObjBin")
#hay una poca diferencia, el modelo tiene un poco de sesgo
roc(data_train$varObjBin, predict(modeloGanador,data_train,type = "response"))
roc(data_test$varObjBin, predict(modeloGanador,data_test,type = "response"))

