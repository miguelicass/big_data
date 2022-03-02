# install.packages("readr")
# install.packages("forecast")
# install.packages("ggplot2")
# install.packages("fUnitRoots") #fUnitsRoots
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("lmtest")

library(readr)
library(forecast)
library(ggplot2)
library(fUnitRoots) #fUnitsRoots
library(readxl)
library(dplyr)
library(lmtest)


# entorno
getwd()
setwd("/Users/macasib/Desktop/entregable_temp")

# cargamos datos
datos <- read_excel("rural.xlsx")

# serie temporal
viajeros <- ts(datos[,-1], start=c(2007,1), frequency=12)


#######################
##### EJERCICIO 2 #####
#######################

# ploteamos la serie temporal
autoplot(viajeros) +
  ggtitle("Turismo Rural mensual") +
  xlab("mes") +
  ylab("Numero de viajeros")

# descomposicion de la serie temporal y sus componentes
viajeros_Comp<- decompose(viajeros,type=c("multiplicative")) #multiplicativo

# representamos la serie y componentes: estacional, tendencia y el error estacional
autoplot(viajeros_Comp)

#coeficientes de estacionalidad
print(viajeros_Comp$seasonal)

# graficamente superponemos a la serie el ajuste estacional y la tendencia
autoplot(viajeros, series="Datos") +
  autolayer(trendcycle(viajeros_Comp), series="Tendencia") +
  autolayer(seasadj(viajeros_Comp), series="Estacionalmente ajustada") +
  xlab("Year") + ylab("Viajeros") +
  ggtitle("Viajeros") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Datos","Estacionalmente ajustada","Tendencia"))

# # graficos los coeficientes de estacionalidad y la irregularidad
# viajeros_season<- cbind(viajeros_Comp$seasonal,viajeros_Comp$random)
# autoplot(viajeros_season,facets=TRUE)

# serie cada año
ggseasonplot(viajeros, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Numero") +
  ggtitle("Seasonal plot: Viajeros")


#######################
##### EJERCICIO 3 #####
#######################

# eliminar del fichero las observaciones del ultimo periodo (12 meses)
viajeros_TR<-window(viajeros,start=c(2007,1), end=c(2018,12))


#######################
##### EJERCICIO 4 #####
#######################

# deterministicos
### modelos de suavizado exponencial


## metodo alisado simple (series: NO tendencia, NO estacionalidad)
# este modelo no nos sirve para nuestro conjunto de datos
viajeros_ses=ses(viajeros_TR, h=12) #predicción para 1 año (12 meses)

# valor de alpha, predicciones e intervalos de confianza 
summary(viajeros_ses) 

# # observaciones y predicciones
# plot(viajeros_ses) 

# grafica de los valores observados y los suavizados 
# con la predicción para los siguientes 12 meses
autoplot(viajeros_ses) +
  autolayer(fitted(viajeros_ses), series="Fitted") + 
  ylab("Viajeros") + 
  xlab("Años")


## metodo de alisado doble de Holt (series: SI tendencia, NO estacionalidad)
# este modelo no nos sirve para nuestro conjunto de datos
viajeros_holt <- holt(viajeros_TR, h=12)

# valor de alpha, predicciones e intervalos de confianza 
summary(viajeros_holt)

# # observaciones y predicciones
# plot(viajeros_holt) 

# grafica de los valores observados y los suavizados 
# con la predicción para los siguientes 12 meses
autoplot(viajeros_holt) + 
  autolayer(fitted(viajeros_holt), series="Fitted") +
  ylab("Viajeros") + 
  xlab("Años")


## metodo Holt-winters multiplicativo (series: SI tendencia , SI estacionalidad)
# METODO ELEGIDO
viajeros_hw <- hw(viajeros_TR, h=12, seasonal="multiplicative")

# valor de alpha, predicciones e intervalos de confianza 
summary(viajeros_hw) 

# # observaciones y predicciones
# plot(viajeros_hw)

# grafica de los valores observados y los suavizados 
# con la predicción para los siguientes 12 meses
autoplot(viajeros_hw) +
  autolayer(fitted(viajeros_hw), series="Fitted") +
  ylab("Viajeros") + 
  xlab("Años")


#######################
##### EJERCICIO 5 #####
#######################

# # NO USAMOS ESTE METODO YA QUE CON EL ESQUEMA MULTIPLICATIVO ES SUFICIENTE
# # Para estabilizar la varianza tomamos la transformacion logaritmica
# # y representamos la serie para observar el efecto de la transformación.
# autoplot(log(viajeros)) +
#   ggtitle("Logaritmo de viajeros mensuales") +
#   xlab("mes") + ylab("log(viajeros)")
# #########################################
# cbind("número de viajeros" = viajeros, "log(viajeros)" = log(viajeros),
#       "primera diferencia log(viajeros)" = diff(log(viajeros)),
#       "diferencia anual log(viajeros)" = diff(diff(log(viajeros)),12)) %>%
#   autoplot(facets=TRUE) + xlab("mes") + ylab("") + ggtitle("Número de viajeros")
# 


# representamos la serie temporal
autoplot(viajeros_TR) +
  ggtitle("Turismo Rural mensual") +
  xlab("mes") +
  ylab("Numero de viajeros")


# funciones de autocorrelacion simple y parcial para la serie original
# autocorrelacion simple
ggAcf(viajeros_TR, lag=48) #48 retardos, 4 años
# autocorrelacion parcial
ggPacf(viajeros_TR,lag=48) #48 retardos

# decrece lentamente en los multiplos de 12 (estacionalidad)
# una diferenciacion de orde estacional (12), porque decrece lentamente
# representamos la serie temporal diferenciada
autoplot(diff(viajeros_TR, 12)) +
  ggtitle("Turismo Rural mensual") +
  xlab("mes") +
  ylab("Numero de viajeros")

# la serie presenta estacionalidad, por lo que tomamos 
# diferenciacion de orden 12 para eliminarla:
# autocorrelacion simple
ggAcf(diff(viajeros_TR,12), lag=48)
# autocorrelacion parcial
ggPacf(diff(viajeros_TR,12), lag=48)


# es necesaria la diferenciacion 1 tambien
autoplot(diff(diff(viajeros_TR, 12))) +
  ggtitle("Turismo Rural mensual") +
  xlab("mes") +
  ylab("Numero de viajeros")

# autocorrelacion simple
ggAcf(diff(diff(viajeros_TR),12), lag=48)
# autocorrelacion parcial
ggPacf(diff(diff(viajeros_TR),12), lag=48)

#######################
##### EJERCICIO 6 #####
#######################

# # auto.arima
autoFit <- auto.arima(viajeros,seasonal=TRUE)
summary(autoFit)

# ajustamos el modelo adecuado. 
fitARIMA<-arima(viajeros_TR,order=c(0,1,1), seasonal=c(2,1,1))
summary(fitARIMA)

# comprobamos la idoneidad del modelo 
coeftest(fitARIMA) 

#######################
##### EJERCICIO 7 #####
#######################

# comprobamos la hipotesis con los residuos
resi<-residuals(fitARIMA)
ggtsdisplay(resi)
checkresiduals(fitARIMA)

# calculamos predicciones e intervalos de confianza.
# forecast(fitARIMA,h=12)
summary(forecast(fitARIMA,h=12))

#Representaci?n de los valores observados y ajustados
cbind("Viajeros" = viajeros, 
      "Valores ajustados" = fitted(fitARIMA)) %>% 
  autoplot() + xlab("Periodo") + ylab("") + 
  ggtitle("Numero de viajeros")

#Representar las predicciones obtenidas
autoplot(forecast(fitARIMA),h=24)

#######################
##### EJERCICIO 8 #####
#######################

#Comprobar el ajuste de los modelos
accuracy(fitARIMA) 
accuracy(viajeros_hw) 

###########################
# ARIMA(0,1,2)(1,2,3)[12]

# ajustamos el modelo adecuado. 
fitARIMA_second<-arima(viajeros_TR,order=c(0,1,2), seasonal=c(1,2,3))
summary(fitARIMA_second)

# comprobamos la idoneidad del modelo 
coeftest(fitARIMA_second) 

# comprobamos la hipotesis con los residuos
resi_second<-residuals(fitARIMA_second)
ggtsdisplay(resi_second)
checkresiduals(fitARIMA_second)

# calculamos predicciones e intervalos de confianza.
summary(forecast(fitARIMA_second,h=12))

#Representaci?n de los valores observados y ajustados
cbind("Viajeros" = viajeros, 
      "Valores ajustados" = fitted(fitARIMA_second)) %>% 
  autoplot() + xlab("Periodo") + ylab("") + 
  ggtitle("Numero de viajeros")

#Representar las predicciones obtenidas
autoplot(forecast(fitARIMA_second),h=24)

accuracy(fitARIMA_second) 




