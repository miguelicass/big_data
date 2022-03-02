# install all packages
install.packages('modeest')
install.packages('dplyr')
install.packages('PerformanceAnalytics')
install.packages('BSDA')

# directory
getwd()
setwd("/Users/macasib/Desktop/UCM/Estadistica/Tarea\ estadística")

## 1
## 1A
library(modeest)

dfHeight <- data.frame(c("Alumno 1", "Alumno 2", "Alumno 3", "Alumno 4", "Alumno 5", "Alumno 6", "Alumno 7", "Alumno 8", "Alumno 9", "Alumno 10", 
                        "Alumno 11", "Alumno 12", "Alumno 13", "Alumno 14", "Alumno 15", "Alumno 16", "Alumno 17", "Alumno 18", "Alumno 19", "Alumno 20", 
                        "Alumno 22", "Alumno 23", "Alumno 24", "Alumno 21", "Alumno 25", "Alumno 26", "Alumno 27", "Alumno 28", "Alumno 29", "Alumno 30"),
                      c(1.25, 1.28, 1.27, 1.21, 1.22, 1.29, 1.30, 1.24, 1.27, 1.29, 
                        1.23, 1.26, 1.30, 1.21, 1.28, 1.30, 1.22, 1.25, 1.20, 1.28, 
                        1.21, 1.29, 1.26, 1.22, 1.28, 1.27, 1.26, 1.23, 1.22, 1.21))
names(dfHeight) <- c("Alumno", "Estatura")

#############################
## medidas de centralizacion
media <- mean(dfHeight$Estatura)
mediana <- median(dfHeight$Estatura)
moda <- mfv(dfHeight$Estatura) 


## medidas de dispersion
# Rango
rangoAux <- range(dfHeight$Estatura)
rango <- max(rangoAux) - min(rangoAux)
# Varianza
varianza <- var(dfHeight$Estatura)
# Desviación típica
desviacionTipica <- sd(dfHeight$Estatura)
# Coeficiente de variación de Pearson
coeficienteVariacion <- sd(dfHeight$Estatura)/mean(dfHeight$Estatura) #desviacionTipica/media

## medidas de posicion
# Cuartiles
sum <- summary(dfHeight$Estatura) # 2nd Qu. = mediana
min <- sum[1]
firstQu <- sum[2]
secondQu <- sum[4] # mediana
thirdQu <- sum[5]
max <- sum[6]
print(sum)


#############################
# diagrama de caja y bigotes
boxplot(dfHeight$Estatura, main="Altura en una muestra de 30 niños")

## 1B
library(dplyr)
library(PerformanceAnalytics)
library(BSDA)

dfWeightHeight <- data.frame(c("Alumno 1", "Alumno 2", "Alumno 3", "Alumno 4", "Alumno 5", "Alumno 6", "Alumno 7", "Alumno 8", "Alumno 9", "Alumno 10",
                              "Alumno 11" , "Alumno 12" , "Alumno 13" , "Alumno 14" , "Alumno 15" , "Alumno 16" , "Alumno 17", "Alumno 18" , "Alumno 19" , "Alumno 20",
                              "Alumno 21" , "Alumno 22" , "Alumno 23" , "Alumno 24" , "Alumno 25" , "Alumno 26" , "Alumno 27", "Alumno 28" , "Alumno 29" , "Alumno 30"),
                            c(1.25, 1.28, 1.27, 1.21, 1.22, 1.29, 1.30, 1.24, 1.27, 1.29,
                              1.25, 1.28, 1.27, 1.21, 1.22, 1.29, 1.30, 1.24, 1.27, 1.29,
                              1.25, 1.28, 1.27, 1.21, 1.22, 1.29, 1.30, 1.24, 1.27, 1.29),
                            c(32, 33, 31, 34, 32, 31, 34, 32, 32, 35,
                              31, 35, 34, 33, 33, 31, 35, 32, 31, 33,
                              33 ,32 ,34 ,34 ,35 ,31 ,34, 33, 35, 34))

names(dfWeightHeight) <- c("Alumno", "Estatura", "Peso")

# # height
# height <- dfWeightHeight[!duplicated(dfWeightHeight$Estatura),]
# height <- sort(height$Estatura)
# # weight
# weight <- dfWeightHeight[!duplicated(dfWeightHeight$Peso),]
# weight <- sort(weight$Peso)

# tabla de correspondencias o contingencia
correspondenceTable <- table(dfWeightHeight$Peso, dfWeightHeight$Estatura)
dfCorrespondenceTable <- as.data.frame.matrix(correspondenceTable)
print(dfCorrespondenceTable)
# as.data.frame()

# intervalos de altura
dfItervalos <- dfCorrespondenceTable
dfItervalos[,1] <- dfItervalos$'1.21' + dfItervalos$'1.22'
dfItervalos[,2] <- dfItervalos$'1.24' + dfItervalos$'1.25'
dfItervalos[,3] <- dfItervalos$'1.27'
dfItervalos[,4] <- dfItervalos$'1.28' + dfItervalos$'1.29' + dfItervalos$'1.3'
dfItervalos <- dfItervalos[,1:4]
names(dfItervalos) <- c("1.21-1.225", "1.226-1.25", "1.251-1.275", "1.276-1.3")

pairs(dfWeightHeight$Estatura ~ dfWeightHeight$Peso) 
chart.Correlation(dfWeightHeight[,2:3]) # 0.0099 intervalo de correlacion irregular



## 2

# cargar dfWeightHeight del 1B de los 15 primeros
dfHeightFirst <- dfHeight[1:15,]
# cargar dfWeightHeight del 1B de los 15 ultimos
dfHeightSecond <- dfHeight[16:30,]

t.test(
  x           = dfHeightFirst$Estatura,
  y           = dfHeightSecond$Estatura,
  alternative = "two.sided",
  mu          = 0,
  var.equal   = FALSE,
  paired      = FALSE,
  conf.level  = 0.95
)

boxplot(dfHeightFirst$Estatura, dfHeightSecond$Estatura, 
        main="Altura en dos sub-muestras de 15 niños",
        names = c("15 primeros", "15 últimos")
        )


# # Intervalo confianza del 95% 
# # para el estatura de los 15 primeros
# heightFirst <-  t.test(x=dfHeightFirst$Estatura, conf.level=0.95)
# heightFirst$conf.int
# 
# # Intervalo confianza del 95% 
# # para el estatura de los 15 ultimos
# heightSecond <- t.test(x=dfHeightSecond$Estatura, conf.level=0.95)
# heightSecond$conf.int


#############################################
# Dado que p-value (0.2672) es mayor que alpha (0.05) , 
# no se dispone de evidencia suficiente para considerar 
# que existe una diferencia entre el peso promedio del
# primer subgrupo de 15 niños nacidos el del segundo. 

