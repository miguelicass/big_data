# install all packages
install.packages('gtools')
install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("sas7bdat")
install.packages("haven")

# 1
# load library
library(gtools) #1A to use permutations()

# 1A
# num combinaciones
combinations <- function(initNum, lastNum, rows) {
  nums <- c(initNum:lastNum)
  comb <- length(nums)**rows
}
# Genera la matriz de todas la combinaciones
generateNumbersMatrix <- function(initNum, lastNum, rows) {
  nums <- c(initNum:lastNum)
  #comb <- length(nums)**rows
  matrix <- permutations(n=length(nums), r=rows, v=nums, repeats.allowed=TRUE)
} 

comb <- combinations(0,9,4)
print(comb)
ONCE <- generateNumbersMatrix(0,9,4)
print(ONCE)


# 1B
# genera un vector con la suma de todas las combinaciones, la posicion = suma
vectorNumRep <- function(matrix) {
  sumMax <- matrix[nrow(matrix),ncol(matrix)]*ncol(matrix)
  sumRep <- rep(0, sumMax)
  for (i in 1:nrow(matrix)) {
    sumRep[sum(ONCE[i,])] = sumRep[sum(ONCE[i,])] + 1
  }
  sumRep
}
# recupera las veces de la suma que más se repite
maxNumRep <- function(vector) {
  max(vector)
}
# recupera la posicion de la suma que más se repite
sumRepeated <- function(times, vector) {
  sum = match(times, vector)
}

vectorRep <- vectorNumRep(ONCE)
maxRep <- maxNumRep(vectorRep)
sumRep <- sumRepeated(maxRep, vectorRep)
#Suma que mas se repite
print(sumRep)
#Veces que se repite
print(maxRep)

#########################################

# 2
# Libraries
library(dplyr) # 2B para usar la funcion filter
library(ggplot2)# 2C, 2D para potear las funciones 
library(reshape2)# 2D para moder hacer melt()

# 2A
# read.table(file.choose(z),header=T)
getwd()
setwd("/Users/macasib/Desktop/UCM/Programación\ R/TareaEvaluacion")
# read.csv(file, header=FALSE, sep=",",quote="\"'",dec=".", row.names, col.names, skip)
prov <- read.csv(file='./Datos/Cod_19/datos_provincias.csv', header=TRUE, sep=",")
codProv <- read.table(file='./Datos/Cod_19/CodProv.txt', header=TRUE, sep=",")
codCCAA <- read.csv(file='./Datos/Cod_19/CodCCAA.csv', header=TRUE, sep="\t")

# Limpiampos el codigo del pais  ES -
# Hasta 3 caracteres
# Prov
codProv$Código <- substr(codProv$Código, 4, 5) 
# CCAA
codCCAA$Código <- substr(codCCAA$Código, 4, 5) 
# merge con CCAA
provCCAA <- merge(prov, codProv, by.x = "provincia_iso", by.y = "Código",  all.x=TRUE)

# 2B
# calculode la CCAA
numCCAA <- 46081840 %% 17 
myCCAA <- filter(codCCAA, X == numCCAA) # Extremadura
# Filter just EX
dataEX <- filter(provCCAA, Comunidad.autónoma == myCCAA$Código) 
dataEX <- dataEX[order( dataEX$provincia_iso, dataEX$fecha),]
# group by: fecha
dataEX$fecha <- as.Date(dataEX$fecha)
dataAllEX <- aggregate(dataEX[,3:7], by=dataEX["fecha"], sum)

# 2C
# CCAA Extremadura
ggplot(data = dataAllEX, 
       aes(x=fecha, 
           y=num_casos))+
  geom_line() +
  ggtitle ("Numero de casos COVID en Extremadura") +
  xlab("Fecha") +
  ylab("Numero de casos") + 
  labs(color='Provincias') +
  theme(axis.text.x = element_blank())

# Provincias
ggplot(data = dataEX, 
       aes(x=fecha, 
           y=num_casos, 
           group = Nombre.de.la.subdivisión.en.la.ISO1, 
           colour = Nombre.de.la.subdivisión.en.la.ISO1))+
  geom_line() +
  ggtitle ("Numero de casos COVID en las provincias de Extremadura") +
  xlab("Fecha") +
  ylab("Numero de casos") + 
  labs(color='Provincias') +
  theme(axis.text.x = element_blank())
  #theme(axis.text.x = element_text(angle = 90, hjust=5))

# 2D
# CCAA Extremadura
dataAllEX <- melt(dataAllEX, id.vars="fecha")
ggplot(data = dataAllEX, 
       aes(x=fecha, y = value, 
           group = variable,
           colour = variable)) +
  ggtitle ("Numero de casos COVID Extremadura") +
  geom_line() +
  xlab("Fecha") +
  ylab("Numero de casos") + 
  labs(color='Casos') +
  scale_color_manual(labels = c("Numero de casos", "Numero de casos pruebas PCR", "Numero de casos test AC", "Numero de otras pruebas", "Numero de pruebas desconocidas"), 
                     values = c("blue", "red", "green", "brown", "yellow")) +
  theme(axis.text.x = element_blank())

# Badajoz
dataBA <- filter(provCCAA, provincia_iso == "BA") 
dataBA <- melt(dataBA[,2:7], id.vars="fecha")
ggplot(dataBA, 
       aes(x = fecha, y = value,
           group = variable,
           colour = variable)) + 
  geom_line() +
  ggtitle ("Numero de casos COVID Badajoz") +
  xlab("Fecha") +
  ylab("Numero de casos") + 
  labs(color='Casos') +
  scale_color_manual(labels = c("Numero de casos", "Numero de casos pruebas PCR", "Numero de casos test AC", "Numero de otras pruebas", "Numero de pruebas desconocidas"), 
                     values = c("blue", "red", "green", "brown", "yellow")) +
  theme(axis.text.x = element_blank())

# Caceres
dataCC <- filter(provCCAA, provincia_iso == "CC") 
dataCC <- melt(dataCC[,2:7], id.vars="fecha")
ggplot(dataCC, 
       aes(x= fecha, y = value,
           group = variable,
           colour = variable)) + 
  geom_line()+
  ggtitle ("Numero de casos COVID Cáceres") +
  xlab("Fecha") +
  ylab("Numero de casos") + 
  labs(color='Casos') +
  scale_color_manual(labels = c("Numero de casos", "Numero de casos pruebas PCR", "Numero de casos test AC", "Numero de otras pruebas", "Numero de pruebas desconocidas"), 
                     values = c("blue", "red", "green", "brown", "yellow")) +
  theme(axis.text.x = element_blank())


# 3
library(sas7bdat) # 3A para leer mas facilmente las extensiones .sas7bdat
library(dplyr) # 3D para extraer los caracteres literales
library(haven) # 3E para poder guardar el dataframen en un fichero .dta

#library(haven)

# 3A
getwd()
setwd("/Users/macasib/Desktop/UCM/Programación\ R/TareaEvaluacion")
punt <- read.sas7bdat('./Punt.sas7bdat')

# 3B
punt$overall <- (punt$TEST1*0.2 + punt$TEST2*0.2 + punt$TEST3*0.2 + punt$TEST4*0.4)

# 3C
# usamos un substring para coger el año, 
# pero creo que seria mas conveiente utilizar 
# una libreria que recupere el año, independientemente del formato
date <- Sys.Date()
year <- substr(date, 1, 4) 
punt$start <- paste(punt$ENROLLED,year,sep="-")
puntAux <- data.frame(punt[,'SEGSOC'], punt[,'COURSE'],punt[,'start'])
names(puntAux) <- c('SEGSOC', 'COURSE','start')
print(puntAux)

# 3D

substrLit <- function(x, n){
  substr(x, 1, nchar(x)-n)
}
# Funcion para extraer los numeros (desde la derecha)
substrNum <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#level500 <-  data.frame(substrLit(punt$COURSE, 3),substrNum(punt$COURSE, 3))
#names(level500) <- c('subject','level')

level500 <- punt
level500$subject <-substrLit(level500$COURSE, 3)
level500$level <- substrNum(level500$COURSE, 3)
level500 <- filter(level500, level == 500) 


# 3E
getwd()
setwd("/Users/macasib/Desktop/UCM/Programación\ R/TareaEvaluacion")
write_dta(level500, path = "level500.dta", version = 14L)

#########################################

# 4
matriz <- structure(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
            16, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 3, 
            2, 1, 1, 1, 1, 1, 1, 2, 1, 4, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 
            2, 3, 3, 1, 2, 1, 1, 1, 1, 1, 3, 4, 1, 1, 1, 2, 3, 4, 3, 1, 2, 
            1, 1, 4, 1, 1, 4, 4, 1, 1), .Dim = c(16L, 8L))

# 4.1
frequencyVector <- function(columnMatrix, maxBurning) {
  f <- tabulate(columnMatrix, maxBurning)
  sumF <- sum(f)
  f <- t(cbind(f))
  for(i in 1:length(f)){
    if (i==1){
      aux <- cbind(f[i],(sumF-f[i]))
    }else{
      aux <- cbind(aux,f[i],(sumF-f[i]))
    }
  }
  f <- as.vector(aux)
}

maxBurning <- max(matriz[,2:8])

freq <- frequencyVector(matriz[,8], maxBurning)
print(freq)

# 4.2
burningMatrix <- apply(matriz[,2:8],2,frequencyVector, maxBurning)

# 4.3
semanas <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7")

barplot(burningMatrix, 
        main="Ardor de hidrogel en 16 sujetos durante 7 semanas",
        axes = FALSE,
        xlab="Semana", 
        ylab ="Ardor total", 
        col=c("black","white"),
        names.arg = semanas,
        axisnames = TRUE,
        width = 1,
        space = 0.1,
        las=1
)
axis(side = 2, at = seq(8,16*4, by =16), labels = c(1:4), las=1, tick = FALSE)


# 4.4
calculateSpaceH <- function(width, space) {
  for(i in 1:7){
    if (i==1){
      aux <- (width/2 + space)
    }else{
      aux <- cbind(aux,aux[i-1] + width + space)
    }
  }
  posSem <- as.vector(aux)
}

posSem <- calculateSpaceH(1,0.1)

barplot(burningMatrix, 
        main="Ardor en 16 sujetos usando hidrogel\n\n",
        axes = FALSE,
        xlab="Semana", 
        ylab ="Ardor total", 
        col=c("red","white"),
        col.axis="blue", 
        # names.arg = c("1", "2", "3", "4", "5", "6", "7"),
        axisnames = TRUE,
        width = 1,
        space = 0.1,
        las=1
)
axis(side = 3, at = posSem, labels = semanas, tick = FALSE, col.axis = "blue") 
axis(side = 2, at = seq(8,16*4, by =16), labels = c(1:4), las=1, tick = FALSE, col.axis = "blue")

