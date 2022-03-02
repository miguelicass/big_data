#A continuación, nos creamos un fichero que sea serie temporal
  
vuelos <- ts(VUELOS[,-1], start=c(1995,1), frequency=12)
  
autoplot(vuelos)+  ggtitle("Número de vuelos mensuales") +
  xlab("mes") +  ylab("Número de vuelos")
  
#Descomposición de la serie temporal en sus componentes.

vuelos_Comp<- decompose(vuelos,type=c("multiplicative"))

#Se muestran los coeficientes de estacionalidad
print(vuelos_Comp$seasonal)

#Representar gráficamente los coeficientes de estacionalidad y la irregularidad

vuelo_season<- cbind(vuelos_Comp$seasonal,vuelos_Comp$random)

autoplot(vuelo_season,facets=TRUE)

#Representar la serie, la componente estacional, la estimación de la tendencia y el error

autoplot(vuelos_Comp)

#Representar sobre la serie de vuelos original, la tendencia 
#calculada con la descomposición y la serie ajustada estacionalmente

autoplot(vuelos, series="Datos") +
  autolayer(trendcycle(vuelos_Comp), series="Tendencia") +
  autolayer(seasadj(vuelos_Comp), series="Estacionalmente ajustada") +
  xlab("Year") + ylab("Vuelos") +
  ggtitle("Vuelos") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Datos","Estacionalmente ajustada","Tendencia"))


#Representar las series de cada año
ggseasonplot(vuelos, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Número") +
  ggtitle("Seasonal plot: Vuelos")

#Eliminar del fichero Vuelos los últimos datos observados

vuelos_TR<-window(vuelos,start=c(1995,1), end=c(2012,7))


#Encontrar el modelo de suavizado exponencial más adecuado

fit1 <- hw(vuelos_TR,h=12, seasonal="multiplicative")

autoplot(fit1) +
autolayer(fitted(fit1), series="Fitted") +
ylab("Vuelos)") + xlab("Fecha")

summary(fit1) 


#EJERCICIO 2

#Representar la tasa de paro para hombres y mujeres conjuntamente

Paro_Fecha <- ts(Paro_S[,-1], start=c(2002,1), frequency=4)

autoplot(Paro_Fecha, facets=TRUE)

#Realizar el suavizado de la serie de hombres utilizando el modelo adecuado. 
#Realizar predicciones para el año siguiente al último observado.

Paro_Hombres <- ts(Paro_Fecha[,1], start=c(2002,1), frequency=4)

#Nos quedamos con cuatro trimestres menos:

Parohombres_TR<-window(Paro_Hombres,start=c(2002,1), end=c(2018,2))

#Suavizado Exponencial Simple

Parohombres_ses=ses(Parohombres_TR, h=4)

summary(Parohombres_ses)  

autoplot(Parohombres_ses) +
autolayer(fitted(Parohombres_ses), series="Fitted") +
ylab("Paro") + xlab("Fecha")


#Método Alisado Doble de Holt

Parohombres_sh <- holt(Parohombres_TR, h=4)    

summary(Parohombres_sh)

autoplot(Parohombres_sh) +
  autolayer(fitted(Parohombres_sh), series="Fitted") +
  ylab("Paro") + xlab("Fecha")



