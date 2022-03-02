
 # EJEMPLO AMESHOUSING REVISITADO (variable dependiente continua)

setwd("c:/")
load("amesbis.Rda")

source("cruzadas ensamblado continuas fuente.R")


# 1) lista con métodos stepwise

vardep<-"SalePrice"
listconti<-   c("Overall.Qual", "Gr.Liv.Area", "Total.Bsmt.SF", "Garage.Cars",
"Year.Built", "Bldg.Type.1Fam", "Neighborhood.StoneBr", "Bedroom.AbvGr",
"Overall.Cond", "Fireplaces", "Neighborhood.NoRidge", "Lot.Area",
"Year.Remod.Add", "X1st.Flr.SF", "Bldg.Type.2fmCon", "Neighborhood.CollgCr")

# 2) Obtengo otra lista con métodos importancia rf y aprovecho para el tuneado

library(caret)

set.seed(12345)
rfgrid<-expand.grid(mtry=c(10,20,25,30,35))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

rf<- train(SalePrice~.,data=amesbis,
 method="rf",trControl=control,tuneGrid=rfgrid,
 linout = TRUE,ntree=200,nodesize=20,replace=TRUE,
 importance=TRUE)

rf

# IMPORTANCIA DE VARIABLES

final<-rf$finalModel

tabla<-as.data.frame(final$importance)
names(tabla)
tabla<-tabla[order(-tabla[,c(2)]),]
tabla

barplot(tabla[,c(2)],names.arg=rownames(tabla))

# Podemos adoptar dos modelos: uno con 16 variables (las mismas que el de stepwise) 
# y otro con más, 20 por ejemplo.

lista1<-rownames(tabla[1:16,])
dput(lista1)

c("Overall.Qual", "Garage.Cars", "Gr.Liv.Area", "Year.Built", 
"Total.Bsmt.SF", "X1st.Flr.SF", "Year.Remod.Add", "Full.Bath", 
"X2nd.Flr.SF", "Lot.Area", "Fireplaces", "TotRms.AbvGrd", "Open.Porch.SF", 
"Overall.Cond", "Bedroom.AbvGr", "Half.Bath")

lista2<-rownames(tabla[1:20,])
dput(lista2)

c("Overall.Qual", "Garage.Cars", "Gr.Liv.Area", "Year.Built", 
"Total.Bsmt.SF", "X1st.Flr.SF", "Year.Remod.Add", "Full.Bath", 
"X2nd.Flr.SF", "Lot.Area", "Fireplaces", "TotRms.AbvGrd", "Open.Porch.SF", 
"Overall.Cond", "Bedroom.AbvGr", "Half.Bath", "Bldg.Type.1Fam", 
"Kitchen.AbvGr", "Yr.Sold", "Neighborhood.NoRidge")

# Por hacer una prueba rápida, comparo regresión con rf en los 3 sets de variables.
# Lo hago con el esquema de ensamblado, pero todavía sin ensamblar
# En cada modelo pongo las variables


archivo<-amesbis

vardep<-"SalePrice"
listclass<-c("")
grupos<-4
sinicio<-1234
repe<-10

# APLICACIÓN CRUZADAS PARA ENSAMBLAR

medias1<-cruzadalin(data=archivo,
 vardep=vardep,listconti=c("Overall.Qual", "Gr.Liv.Area", "Total.Bsmt.SF", "Garage.Cars",
"Year.Built", "Bldg.Type.1Fam", "Neighborhood.StoneBr", "Bedroom.AbvGr",
"Overall.Cond", "Fireplaces", "Neighborhood.NoRidge", "Lot.Area",
"Year.Remod.Add", "X1st.Flr.SF", "Bldg.Type.2fmCon", "Neighborhood.CollgCr"),
 listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe)

medias1bis<-as.data.frame(medias1[1])
medias1bis$modelo<-"regresion1"

medias2<-cruzadalin(data=archivo,
 vardep=vardep,listconti=c("Overall.Qual", "Garage.Cars", "Gr.Liv.Area", "Year.Built", 
"Total.Bsmt.SF", "X1st.Flr.SF", "Year.Remod.Add", "Full.Bath", 
"X2nd.Flr.SF", "Lot.Area", "Fireplaces", "TotRms.AbvGrd", "Open.Porch.SF", 
"Overall.Cond", "Bedroom.AbvGr", "Half.Bath"),
 listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe)

medias2bis<-as.data.frame(medias2[1])
medias2bis$modelo<-"regresion2"


medias3<-cruzadalin(data=archivo,
 vardep=vardep,listconti=c("Overall.Qual", "Garage.Cars", "Gr.Liv.Area", "Year.Built", 
"Total.Bsmt.SF", "X1st.Flr.SF", "Year.Remod.Add", "Full.Bath", 
"X2nd.Flr.SF", "Lot.Area", "Fireplaces", "TotRms.AbvGrd", "Open.Porch.SF", 
"Overall.Cond", "Bedroom.AbvGr", "Half.Bath", "Bldg.Type.1Fam", 
"Kitchen.AbvGr", "Yr.Sold", "Neighborhood.NoRidge"),
 listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe)

medias3bis<-as.data.frame(medias3[1])
medias3bis$modelo<-"regresion3"


medias4<-cruzadarf(data=archivo,
 vardep=vardep,listconti=c("Overall.Qual", "Gr.Liv.Area", "Total.Bsmt.SF", "Garage.Cars",
"Year.Built", "Bldg.Type.1Fam", "Neighborhood.StoneBr", "Bedroom.AbvGr",
"Overall.Cond", "Fireplaces", "Neighborhood.NoRidge", "Lot.Area",
"Year.Remod.Add", "X1st.Flr.SF", "Bldg.Type.2fmCon", "Neighborhood.CollgCr"),
 listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
 mtry=10,ntree=100,nodesize=20,replace=TRUE)

medias4bis<-as.data.frame(medias4[1])
medias4bis$modelo<-"rf1"

medias5<-cruzadarf(data=archivo,
 vardep=vardep,listconti=c("Overall.Qual", "Garage.Cars", "Gr.Liv.Area", "Year.Built", 
"Total.Bsmt.SF", "X1st.Flr.SF", "Year.Remod.Add", "Full.Bath", 
"X2nd.Flr.SF", "Lot.Area", "Fireplaces", "TotRms.AbvGrd", "Open.Porch.SF", 
"Overall.Cond", "Bedroom.AbvGr", "Half.Bath"),
 listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
 mtry=10,ntree=100,nodesize=20,replace=TRUE)

medias5bis<-as.data.frame(medias5[1])
medias5bis$modelo<-"rf2"


medias6<-cruzadarf(data=archivo,
 vardep=vardep,listconti=c("Overall.Qual", "Garage.Cars", "Gr.Liv.Area", "Year.Built", 
"Total.Bsmt.SF", "X1st.Flr.SF", "Year.Remod.Add", "Full.Bath", 
"X2nd.Flr.SF", "Lot.Area", "Fireplaces", "TotRms.AbvGrd", "Open.Porch.SF", 
"Overall.Cond", "Bedroom.AbvGr", "Half.Bath", "Bldg.Type.1Fam", 
"Kitchen.AbvGr", "Yr.Sold", "Neighborhood.NoRidge"),
 listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
 mtry=10,ntree=100,nodesize=20,replace=TRUE)

medias6bis<-as.data.frame(medias6[1])
medias6bis$modelo<-"rf3"


union1<-rbind(medias1bis,medias2bis,
 medias3bis,medias4bis,medias5bis,medias6bis)

par(cex.axis=0.9)
boxplot(data=union1,error~modelo)


# COMO SE VE, PARA LA REGRESIÓN FUNCIONA MEJOR EL PRIMER SET DE VARIABLES
# PERO PARA RF EL TERCERO. EN TODO CASO, CLARAMENTE NOS OLVIDAREMOS DE LA REGRESIÓN
# A PARTIR DE AHORA PUES ES MUCHO PEOR PARA ESTOS DATOS.
# 
# TUNEAMOS GBM A PARTIR DE AHORA Y DESPUÉS ENSAMBLADO. LAS REDES LAS DEJAMOS EN EL MODELO QUE TENÍAMOS
# CON 10 NODOS Y 0.01, PUES NO MERECE LA PENA TAMPOCO.
# SVM NO MERECE LA PENA TARDA DEMASIADO

listconti=c("Overall.Qual", "Garage.Cars", "Gr.Liv.Area", "Year.Built", 
"Total.Bsmt.SF", "X1st.Flr.SF", "Year.Remod.Add", "Full.Bath", 
"X2nd.Flr.SF", "Lot.Area", "Fireplaces", "TotRms.AbvGrd", "Open.Porch.SF", 
"Overall.Cond", "Bedroom.AbvGr", "Half.Bath", "Bldg.Type.1Fam", 
"Kitchen.AbvGr", "Yr.Sold", "Neighborhood.NoRidge")

paste(listconti,collapse="+")

# Overall.Qual+Garage.Cars+Gr.Liv.Area+Year.Built+Total.Bsmt.SF+
# X1st.Flr.SF+Year.Remod.Add+Full.Bath+X2nd.Flr.SF+Lot.Area+
# Fireplaces+TotRms.AbvGrd+Open.Porch.SF+Overall.Cond+Bedroom.AbvGr+
# Half.Bath+Bldg.Type.1Fam+Kitchen.AbvGr+Yr.Sold+Neighborhood.NoRidge

gbmgrid<-expand.grid(shrinkage=c(0.1,0.05,0.03,0.01,0.001),
 n.minobsinnode=c(20),
 n.trees=c(100,500,1000,5000),
 interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

gbm<- train(SalePrice~Overall.Qual+Garage.Cars+Gr.Liv.Area+Year.Built+Total.Bsmt.SF+
X1st.Flr.SF+Year.Remod.Add+Full.Bath+X2nd.Flr.SF+Lot.Area+
Fireplaces+TotRms.AbvGrd+Open.Porch.SF+Overall.Cond+Bedroom.AbvGr+
Half.Bath+Bldg.Type.1Fam+Kitchen.AbvGr+Yr.Sold+Neighborhood.NoRidge,data=amesbis,
 method="gbm",trControl=control,tuneGrid=gbmgrid,  bag.fraction=1,verbose=FALSE)

gbm

# n.trees=1000,shrink=0.1

# A PARTIR DE AHORA PEGAMOS ENSAMBLADO PARA CONTINUAS, QUITAMOS SVM Y XGBOOST
# PARA NO PERDER TIEMPO.

source("cruzadas ensamblado continuas fuente.R")

archivo<-amesbis
vardep="SalePrice"
listconti=c("Overall.Qual", "Garage.Cars", "Gr.Liv.Area", "Year.Built", 
"Total.Bsmt.SF", "X1st.Flr.SF", "Year.Remod.Add", "Full.Bath", 
"X2nd.Flr.SF", "Lot.Area", "Fireplaces", "TotRms.AbvGrd", "Open.Porch.SF", 
"Overall.Cond", "Bedroom.AbvGr", "Half.Bath", "Bldg.Type.1Fam", 
"Kitchen.AbvGr", "Yr.Sold", "Neighborhood.NoRidge")
listclass=c("")
grupos=4
sinicio=12345
repe=10


medias1<-cruzadalin(data=archivo,
 vardep=vardep,listconti=listconti,
 listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe)

medias1bis<-as.data.frame(medias1[1])
medias1bis$modelo<-"regresion"
predi1<-as.data.frame(medias1[2])
predi1$reg<-predi1$pred


medias2<-cruzadaavnnet(data=archivo,
 vardep=vardep,listconti=listconti,
 listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
    size=c(5),decay=c(0.1),repeticiones=5,itera=200,trace=FALSE)

medias2bis<-as.data.frame(medias2[1])
medias2bis$modelo<-"avnnet"
predi2<-as.data.frame(medias2[2])
predi2$avnnet<-predi2$pred


medias3<-cruzadarf(data=archivo,
  vardep=vardep,listconti=listconti,
  listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
 mtry=5,ntree=200,nodesize=10,replace=TRUE)


medias3bis<-as.data.frame(medias3[1])
medias3bis$modelo<-"rf"
predi3<-as.data.frame(medias3[2])
predi3$rf<-predi3$pred

medias4<-cruzadagbm(data=archivo,
  vardep=vardep,listconti=listconti,
  listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
n.minobsinnode=10,shrinkage=0.001,n.trees=5000,interaction.depth=2)

medias4bis<-as.data.frame(medias4[1])
medias4bis$modelo<-"gbm"
predi4<-as.data.frame(medias4[2])
predi4$gbm<-predi4$pred

union1<-rbind(medias1bis,medias2bis,
 medias3bis,medias4bis)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo)


# CONSTRUCCIÓN DE TODOS LOS ENSAMBLADOS
# SE UTILIZARÁN LOS ARCHIVOS SURGIDOS DE LAS FUNCIONES LLAMADOS predi1,...

unipredi<-cbind(predi1,predi2,predi3,predi4)

# Esto es para eliminar columnas duplicadas
unipredi<- unipredi[, !duplicated(colnames(unipredi))]

# Construccion de ensamblados, cambiar al gusto

unipredi$predi9<-(unipredi$reg+unipredi$avnnet)/2
unipredi$predi10<-(unipredi$reg+unipredi$rf)/2
unipredi$predi11<-(unipredi$reg+unipredi$gbm)/2
unipredi$predi16<-(unipredi$avnnet+unipredi$rf)/2
unipredi$predi17<-(unipredi$avnnet+unipredi$gbm)/2
unipredi$predi22<-(unipredi$rf+unipredi$gbm)/2

unipredi$predi31<-(unipredi$reg+unipredi$avnnet+unipredi$rf)/3
unipredi$predi32<-(unipredi$reg+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi37<-(unipredi$reg+unipredi$rf+unipredi$gbm)/3
unipredi$predi56<-(unipredi$rf+unipredi$avnnet+unipredi$gbm)/3

unipredi$predi64<-(unipredi$reg+unipredi$rf+unipredi$gbm+unipredi$avnnet)/4

dput(names(unipredi))


listado<-c("reg","avnnet", "rf", "gbm", "predi9", "predi10", "predi11", "predi16", "predi17", 
"predi22", "predi31", "predi32", "predi37", "predi56", "predi64")


repeticiones<-nlevels(factor(unipredi$Rep))
unipredi$Rep<-as.factor(unipredi$Rep)
unipredi$Rep<-as.numeric(unipredi$Rep)

# Calculo el MSE para cada repeticion de validación cruzada

medias0<-data.frame(c())

for (prediccion in listado)
{
paso <-unipredi[,c("obs",prediccion,"Rep")]
paso$error<-(paso[,c(prediccion)]-paso$obs)^2
paso<-paso %>%
  group_by(Rep) %>%
  summarize(error=mean(error))     
paso$modelo<-prediccion  
medias0<-rbind(medias0,paso) 
} 
# Finalmente boxplot

par(cex.axis=0.8,las=2)
boxplot(data=medias0,outcex=0.3,error~modelo)

# PRESENTACION TABLA MEDIAS

tablamedias<-medias0 %>%
    summarize(error=mean(error))     

tablamedias<-tablamedias[order(tablamedias$error),]

# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN ERROR
# PARA EL GRÁFICO

medias0$modelo <- with(medias0,
 reorder(modelo,error, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0,error~modelo,col="pink")




