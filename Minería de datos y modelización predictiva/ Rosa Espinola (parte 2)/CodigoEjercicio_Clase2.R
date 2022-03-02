#Crear un fichero que sea serie temporal:
 
precio <- ts(bitcoin_A[,2], start=c(2018,258), frequency=365)
  
#Coger los datos desde agosto de 2019:
    
precio_TR<-window(precio,start=c(2019,210))
  
#Representar gráficamente la serie

autoplot(precio_TR)+  ggtitle("Precio Bitcoin") +  xlab("dia") +  ylab("euro")


#Representar las funciones de autocorrelación simple y parcial

ggAcf(precio_TR, lag=48)

ggPacf(precio_TR,lag=48) 

#Para ver únicamente los valores de las funciones ACF y PACF se usa

acf(precio_TR, lag.max =20, plot =F)

pacf(precio_TR, lag.max =20, plot =F)


#Ajustar el modelo ARIMA adecuado

#Representar las ACF y PACF para la serie con las diferenciaciones para ver los parámetros del modelo ARIMA:

precio_dif<-diff(precio_TR)  

ggAcf(precio_dif, lag=48)

ggPacf(precio_dif, lag=48)
  
#Modelo Ajustado

fitARIMA<-arima(precio_TR,order=c(0,1,0))
summary(fitARIMA)

#¿Son significativamente diferentes de cero todos los estimadores calculados?  
  
ct<-coeftest(fitARIMA)  

#¿Los residuos están incorrelados?
  
resi<-residuals(fitARIMA)  

ggtsdisplay(resi)

#Cálculo predicciones para una semana

forecast(fitARIMA,h=7)




#EJERCICIO 2

vuelos <- ts(VUELOS[,-1], start=c(1995,1), frequency=12)

vuelos_TR<-window(vuelos,start=c(1995,1), end=c(2012,7))

#Representar la serie 

autoplot(vuelos_TR)+  ggtitle("VUELOS") +  xlab("Periodo") +  ylab("Vuelos")

#Representar la función de autocorrelación y autocorrelación parcial

ggAcf(vuelos_TR, lag=48)

ggPacf(vuelos_TR,lag=48) 

#La serie presenta estacionalidad, por lo que tomamos diferenciación de periodo 12:

ggAcf(diff(vuelos_TR,12), lag=48)

ggPacf(diff(vuelos_TR,12), lag=48)

#Es necesaria la diferenciación 1 también, por tanto:
  
ggAcf(diff(diff(vuelos_TR),12), lag=48)


ggPacf(diff(diff(vuelos_TR),12), lag=48)

#Ajustar el modelo adecuado. 

fitARIMA<-arima(vuelos_TR,order=c(0,1,0), seasonal=c(0,1,1))

summary(fitARIMA)

#Comprobar la idoneidad del modelo

coeftest(fitARIMA) 

#Comprobar hipótesis dadas a los residuos

resi<-residuals(fitARIMA)

ggtsdisplay(resi)

checkresiduals(fitARIMA)

#Cálculo de las predicciones y los intervalos de confianza.

forecast(fitARIMA,h=12)

summary(forecast(fitARIMA,h=12))

#Representar las predicciones obtenidas

autoplot(forecast(fitARIMA),h=12)

#Representación de los valores observados y ajustados

cbind("Vuelos" = vuelos_TR, "Valores ajustados" =fitted(fitARIMA)) %>%

autoplot() + xlab("Periodo") + ylab("") +   ggtitle("Número de Vuelos")

#Comprobar el ajuste de los modelos

accuracy(fitARIMA) 

accuracy(fitHW) 


