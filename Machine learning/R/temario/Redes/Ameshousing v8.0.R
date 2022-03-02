load("ameshousing.Rda")

hist(ames$SalePrice)

library(plyr)
library(dummies)
library(ggplot2)
library(naniar)
library(MASS)

source("funcion steprepetido.R")
source("cruzadas avnnet y lin.R")


# Esto es un truco para observar de manera general los missings en el dataset, libreria ggplot2

gg_miss_var(ames)

dput(names(ames))


# c("Order", "PID", "MS.SubClass", "MS.Zoning", "Lot.Frontage", 
#   "Lot.Area", "Street", "Alley", "Lot.Shape", "Land.Contour", "Utilities", 
#   "Lot.Config", "Land.Slope", "Neighborhood", "Condition.1", "Condition.2", 
#   "Bldg.Type", "House.Style", "Overall.Qual", "Overall.Cond", "Year.Built", 
#   "Year.Remod.Add", "Roof.Style", "Roof.Matl", "Exterior.1st", 
#   "Exterior.2nd", "Mas.Vnr.Type", "Mas.Vnr.Area", "Exter.Qual", 
#   "Exter.Cond", "Foundation", "Bsmt.Qual", "Bsmt.Cond", "Bsmt.Exposure", 
#   "BsmtFin.Type.1", "BsmtFin.SF.1", "BsmtFin.Type.2", "BsmtFin.SF.2", 
#   "Bsmt.Unf.SF", "Total.Bsmt.SF", "Heating", "Heating.QC", "Central.Air", 
#   "Electrical", "X1st.Flr.SF", "X2nd.Flr.SF", "Low.Qual.Fin.SF", 
#   "Gr.Liv.Area", "Bsmt.Full.Bath", "Bsmt.Half.Bath", "Full.Bath", 
#   "Half.Bath", "Bedroom.AbvGr", "Kitchen.AbvGr", "Kitchen.Qual", 
#   "TotRms.AbvGrd", "Functional", "Fireplaces", "Fireplace.Qu", 
#   "Garage.Type", "Garage.Yr.Blt", "Garage.Finish", "Garage.Cars", 
#   "Garage.Area", "Garage.Qual", "Garage.Cond", "Paved.Drive", "Wood.Deck.SF", 
#   "Open.Porch.SF", "Enclosed.Porch", "X3Ssn.Porch", "Screen.Porch", 
#   "Pool.Area", "Pool.QC", "Fence", "Misc.Feature", "Misc.Val", 
#   "Mo.Sold", "Yr.Sold", "Sale.Type", "Sale.Condition", "SalePrice"
# )

# He realizado una preselección para no tardar demasiado

listclass=c("Bldg.Type","Neighborhood")
listconti=c("Lot.Area","Year.Built","Year.Remod.Add",
            "Total.Bsmt.SF","X1st.Flr.SF","X2nd.Flr.SF","Gr.Liv.Area","Full.Bath",
            "Half.Bath","Bedroom.AbvGr","Kitchen.AbvGr","TotRms.AbvGrd","Fireplaces",
            "Garage.Cars","Open.Porch.SF","Enclosed.Porch","Yr.Sold","Pool.Area", 
            "Overall.Qual", "Overall.Cond")
vardep<-"SalePrice"


# Primero me quedo con solo las variables de interés
# (listconti, listclass y vardep)

ames<-ames[,c(listconti,listclass,vardep)]

# Borro observaciones con algún missing 
# (son pocas, también se puede imputar)

ames<-na.omit(ames)

# Copio  y pego de otros ejemplos estandarización continuas

means <-apply(ames[,listconti],2,mean,na.rm=TRUE)
sds<-sapply(ames[,listconti],sd,na.rm=TRUE)

ames2<-scale(ames[,listconti], center = means, scale = sds)

ames<-data.frame(cbind(ames2,ames[,c(listclass,vardep)]))

# Lista de Frecuencias de las categóricas, útil pues algunos niveles
# con pocas observaciones no deben ser tenidos en cuenta 
# en modelos de machine learning para evitar sobreajuste


frecu<-ldply(ames[,listclass],function(x) t(rbind(names(table(x)),table(x))))
names(frecu)<-c("variable","nivel","frecuencia")
frecu$frecuencia<-as.numeric(frecu$frecuencia)

frecu

# Obtener dummies (en el ejemplo solo con caracter, 
# hay que tener en cuenta que muchas variables numéricas
# pueden ser categóricas). Las dummies sustituyen a las variables originales,
# con lo que es mejor crear un archivo nuevo por si queremos utilizar las 
# originales en algún momento


amesbis<-dummy.data.frame(ames, listclass, sep = ".")

# Para borrar las dummies con menos de k observaciones se utiliza el 
# listado de frecuencias frecu obtenido anteriormente

# 1) Obtengo filas de frecu con frecuencia menor que k=20 como ejemplo
frecu20<-frecu[frecu$frecuencia<20,]

# 2) Obtengo listado de los niveles en el mismo formato que las dummies,
# con separador .
frecu20$dum<-paste(frecu20$variable,frecu20$nivel,sep=".")
listamal<-dput(frecu20$dum)

# Borro las dummies de amesbis que coinciden con la lista
amesbis[,listamal]<-NULL


# save(amesbis,file="c:/amesbis.Rda")

data<-amesbis

full<-lm(SalePrice~.,data=data)
null<-lm(SalePrice~1,data=data)

selec1<-stepAIC(null,scope=list(upper=full),direction="both",trace=FALSE)

summary(selec1)

dput(names(selec1$coefficients))


# c("(Intercept)", "Overall.Qual", "Gr.Liv.Area", "Total.Bsmt.SF",
# "Garage.Cars", "Year.Built", "Bldg.Type.1Fam", "Bedroom.AbvGr",
# "Neighborhood.StoneBr", "Overall.Cond", "Neighborhood.NoRidge",
# "Lot.Area", "Fireplaces", "Year.Remod.Add", "X1st.Flr.SF", "Bldg.Type.2fmCon",
# "Neighborhood.CollgCr", "Neighborhood.Blmngtn", "Neighborhood.MeadowV",
# "TotRms.AbvGrd", "Kitchen.AbvGr", "X2nd.Flr.SF", "Pool.Area",
# "Yr.Sold")

# Esto si se quiere en versión formula
formula(selec1)

# La función steprepetido permite realizar el proceso training test
# varias veces obteniendo el modelo por stepwise sobre datos train
# y la tabla de frecuencias de los modelos escogidos.

source("funcion steprepetido.R")

dput(names(amesbis))


data<-amesbis
lista<-steprepetido(data=data,vardep=c("SalePrice"),
                    listconti=
                      c("Lot.Area", "Year.Built", "Year.Remod.Add", "Total.Bsmt.SF", 
                        "X1st.Flr.SF", "X2nd.Flr.SF", "Gr.Liv.Area", "Full.Bath", "Half.Bath", 
                        "Bedroom.AbvGr", "Kitchen.AbvGr", "TotRms.AbvGrd", "Fireplaces", 
                        "Garage.Cars", "Open.Porch.SF", "Enclosed.Porch", "Yr.Sold", 
                        "Pool.Area", "Overall.Qual", "Overall.Cond", "Bldg.Type.1Fam", 
                        "Bldg.Type.2fmCon", "Neighborhood.Blmngtn", "Neighborhood.BrDale", 
                        "Neighborhood.ClearCr", "Neighborhood.CollgCr", "Neighborhood.Greens", 
                        "Neighborhood.IDOTRR", "Neighborhood.MeadowV", "Neighborhood.NAmes", 
                        "Neighborhood.NoRidge", "Neighborhood.StoneBr", "Neighborhood.SWISU", 
                        "Neighborhood.Timber"),
                    sinicio=12345,sfinal=12385,porcen=0.8,criterio="BIC")


tabla<-lista[[1]]
dput(lista[[2]][[1]])

# c("Overall.Qual", "Gr.Liv.Area", "Total.Bsmt.SF", "Garage.Cars",
# "Year.Built", "Bldg.Type.1Fam", "Neighborhood.StoneBr", "Bedroom.AbvGr",
# "Overall.Cond", "Fireplaces", "Neighborhood.NoRidge", "Lot.Area",
# "Year.Remod.Add", "X1st.Flr.SF", "Bldg.Type.2fmCon", "Neighborhood.CollgCr")

dput(lista[[2]][[2]])

# c("Overall.Qual", "Gr.Liv.Area", "Total.Bsmt.SF", "Garage.Cars",
# "Year.Built", "Bedroom.AbvGr", "Bldg.Type.1Fam", "Overall.Cond",
# "Neighborhood.StoneBr", "Fireplaces", "Neighborhood.NoRidge",
# "Lot.Area", "X1st.Flr.SF", "Year.Remod.Add", "Bldg.Type.2fmCon"
# )

# Hacemos unas pruebas de cv repetida, sobre 
# a)El modelo elegido con todos los datos
# b)LOs  modelos más elegido con BIC en remuestreo (con AIC no merece la pena)

source("cruzadas avnnet y lin.R")

data<-amesbis

medias1<-cruzadalin(data=data,
                    vardep="SalePrice",listconti=
                      c("Overall.Qual", "Gr.Liv.Area", "Total.Bsmt.SF",
                        "Garage.Cars", "Year.Built", "Bldg.Type.1Fam", "Bedroom.AbvGr",
                        "Neighborhood.StoneBr", "Overall.Cond", "Neighborhood.NoRidge",
                        "Lot.Area", "Fireplaces", "Year.Remod.Add", "X1st.Flr.SF", "Bldg.Type.2fmCon",
                        "Neighborhood.CollgCr", "Neighborhood.Blmngtn", "Neighborhood.MeadowV",
                        "TotRms.AbvGrd", "Kitchen.AbvGr", "X2nd.Flr.SF", "Pool.Area",
                        "Yr.Sold"),listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias1$modelo=1

medias2<-cruzadalin(data=data,
                    vardep="SalePrice",listconti=
                      c("Overall.Qual", "Gr.Liv.Area", "Total.Bsmt.SF", "Garage.Cars",
                        "Year.Built", "Bldg.Type.1Fam", "Neighborhood.StoneBr", "Bedroom.AbvGr",
                        "Overall.Cond", "Fireplaces", "Neighborhood.NoRidge", "Lot.Area",
                        "Year.Remod.Add", "X1st.Flr.SF", "Bldg.Type.2fmCon", "Neighborhood.CollgCr"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias2$modelo=2

medias3<-cruzadalin(data=data,
                    vardep="SalePrice",listconti=c("Overall.Qual", "Gr.Liv.Area", "Total.Bsmt.SF", "Garage.Cars",
                                                   "Year.Built", "Bedroom.AbvGr", "Bldg.Type.1Fam", "Overall.Cond",
                                                   "Neighborhood.StoneBr", "Fireplaces", "Neighborhood.NoRidge",
                                                   "Lot.Area", "X1st.Flr.SF", "Year.Remod.Add", "Bldg.Type.2fmCon"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias3$modelo=3


union1<-rbind(medias1,medias2,medias3)

par(cex.axis=0.8)
boxplot(data=union1,col="pink",error~modelo)

# TUNEADO DE REDES SIMPLE CON CARET con el modelo 2

vardep<-"SalePrice"
listconti<-   c("Overall.Qual", "Gr.Liv.Area", "Total.Bsmt.SF", "Garage.Cars",
                "Year.Built", "Bldg.Type.1Fam", "Neighborhood.StoneBr", "Bedroom.AbvGr",
                "Overall.Cond", "Fireplaces", "Neighborhood.NoRidge", "Lot.Area",
                "Year.Remod.Add", "X1st.Flr.SF", "Bldg.Type.2fmCon", "Neighborhood.CollgCr")

paste(listconti,collapse = "+")

# Lot.Area+Year.Built+Year.Remod.Add+Total.Bsmt.SF+
# X1st.Flr.SF+X2nd.Flr.SF+Gr.Liv.Area+Full.Bath+
# Half.Bath+Bedroom.AbvGr+Kitchen.AbvGr+TotRms.AbvGrd+
# Fireplaces+Garage.Cars+Open.Porch.SF+Enclosed.Porch+
# Yr.Sold+Pool.Area+Overall.Qual+Overall.Cond

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

avnnetgrid <-expand.grid(size=c(5,10,15,20),
                         decay=c(0.01,0.1,0.001),bag=FALSE)


redavnnet<- train(SalePrice~ 
                    Overall.Qual+Gr.Liv.Area+Total.Bsmt.SF+Garage.Cars+Year.Built+Bldg.Type.1Fam+
                    Neighborhood.StoneBr+Bedroom.AbvGr+Overall.Cond+Fireplaces+
                    Neighborhood.NoRidge+Lot.Area+Year.Remod.Add+X1st.Flr.SF+Bldg.Type.2fmCon+Neighborhood.CollgCr,
                  data=data,method="avNNet",linout = TRUE,maxit=100,
                  trControl=control,tuneGrid=avnnetgrid, repeats=5)

redavnnet


medias4<-cruzadaavnnet(data=data,
                       vardep="SalePrice",listconti=c("Overall.Qual", "Gr.Liv.Area", "Total.Bsmt.SF", "Garage.Cars",
                                                      "Year.Built", "Bldg.Type.1Fam", "Neighborhood.StoneBr", "Bedroom.AbvGr",
                                                      "Overall.Cond", "Fireplaces", "Neighborhood.NoRidge", "Lot.Area",
                                                      "Year.Remod.Add", "X1st.Flr.SF", "Bldg.Type.2fmCon", "Neighborhood.CollgCr"),
                       listclass=c(""),grupos=4,sinicio=1234,repe=25,repeticiones=5,itera=100,
                       size=c(10),decay=c(0.01))

medias4$modelo=4


union1<-rbind(medias1,medias2,medias3,medias4)

par(cex.axis=0.5)
boxplot(data=union1,col="pink",error~modelo)

# Puede ser que necesitemos más iteraciones del algoritmo de optimización.
# Por defecto avnnet utiliza 100 pero en la función cruzadaavnnet podemos 
# utilizar itera=500 por ejemplo
medias5<-cruzadaavnnet(data=data,
                       vardep="SalePrice",listconti=c("Overall.Qual", "Gr.Liv.Area", "Total.Bsmt.SF", "Garage.Cars",
                                                      "Year.Built", "Bldg.Type.1Fam", "Neighborhood.StoneBr", "Bedroom.AbvGr",
                                                      "Overall.Cond", "Fireplaces", "Neighborhood.NoRidge", "Lot.Area",
                                                      "Year.Remod.Add", "X1st.Flr.SF", "Bldg.Type.2fmCon", "Neighborhood.CollgCr"),
                       listclass=c(""),grupos=4,sinicio=1234,repe=25,repeticiones=5,itera=500,
                       size=c(10),decay=c(0.01))

medias5$modelo=5



union1<-rbind(medias1,medias2,medias3,medias4,medias5)

par(cex.axis=0.5)
boxplot(data=union1,col="pink",error~modelo)

# Se mejora algo la red pero es insuficiente para batir a la regresión en estos datos

# Conclusiones

# 1) Se observa que con 10 nodos tenemos 181 parámetros, 16 obs. por parámetro, algo escaso.
# 2) La red no llega a mejorar la regresión, por lo cual la regresión es mejor modelo
# 3) Sea o no mejor la red, siempre es bueno presentar la tabla de parámetros y p-valores de la 
# regresión (en este caso regresión lineal,cuando trabajemos con variable dependiente binaria será regresión logística).
# Para obtenerla simplemente se usa la función lm (linear model)

regre<-lm(data=data,SalePrice~ 
            Overall.Qual+Gr.Liv.Area+Total.Bsmt.SF+Garage.Cars+Year.Built+Bldg.Type.1Fam+
            Neighborhood.StoneBr+Bedroom.AbvGr+Overall.Cond+Fireplaces+
            Neighborhood.NoRidge+Lot.Area+Year.Remod.Add+X1st.Flr.SF+Bldg.Type.2fmCon+Neighborhood.CollgCr)

summary(regre)


