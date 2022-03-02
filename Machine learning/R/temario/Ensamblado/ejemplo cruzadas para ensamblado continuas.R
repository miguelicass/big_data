
library(dplyr)
source("cruzadas ensamblado continuas fuente.R")

load ("compressbien.Rda")
dput(names(compressbien))
set.seed(12345)

archivo<-compressbien

vardep<-"cstrength"
listconti<-c("cement", "blast", "age", "water")
listclass<-c("")
grupos<-4
sinicio<-1234
repe<-15

# APLICACIÓN CRUZADAS PARA ENSAMBLAR

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
    size=c(15),decay=c(0.01),repeticiones=5,itera=100)

medias2bis<-as.data.frame(medias2[1])
medias2bis$modelo<-"avnnet"
predi2<-as.data.frame(medias2[2])
predi2$avnnet<-predi2$pred


medias3<-cruzadarf(data=archivo,
  vardep=vardep,listconti=listconti,
  listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
 mtry=3,ntree=600,nodesize=10,replace=TRUE)


medias3bis<-as.data.frame(medias3[1])
medias3bis$modelo<-"rf"
predi3<-as.data.frame(medias3[2])
predi3$rf<-predi3$pred

medias4<-cruzadagbm(data=archivo,
  vardep=vardep,listconti=listconti,
  listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
n.minobsinnode=20,shrinkage=0.10,n.trees=500,interaction.depth=2)

medias4bis<-as.data.frame(medias4[1])
medias4bis$modelo<-"gbm"
predi4<-as.data.frame(medias4[2])
predi4$gbm<-predi4$pred

medias5<-cruzadaxgbm(data=archivo,
  vardep=vardep,listconti=listconti,
  listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
   min_child_weight=10,eta=0.03,nrounds=5000,max_depth=6,
  gamma=0,colsample_bytree=1,subsample=1,
 alpha=0,lambda=0,lambda_bias=0)


medias5bis<-as.data.frame(medias5[1])
medias5bis$modelo<-"xgbm"
predi5<-as.data.frame(medias5[2])
predi5$xgbm<-predi5$pred


medias6<-cruzadaSVM(data=archivo,
   vardep=vardep,listconti=listconti,
   listclass=listclass,grupos=grupos,
 sinicio=sinicio,repe=repe,C=0.01)

medias6bis<-as.data.frame(medias6[1])
medias6bis$modelo<-"svmLinear"
predi6<-as.data.frame(medias6[2])
predi6$svmLinear<-predi6$pred


medias7<-cruzadaSVMpoly(data=archivo,
   vardep=vardep,listconti=listconti,
   listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
 C=0.05,degree=3,scale=0.5)

medias7bis<-as.data.frame(medias7[1])
medias7bis$modelo<-"svmPoly"
predi7<-as.data.frame(medias7[2])
predi7$svmPoly<-predi7$pred

medias8<-cruzadaSVMRBF(data=archivo,
   vardep=vardep,listconti=listconti,
   listclass=listclass,grupos=grupos,
 sinicio=sinicio,repe=repe,
 C=30,sigma=0.2)

medias8bis<-as.data.frame(medias8[1])
medias8bis$modelo<-"svmRadial"
predi8<-as.data.frame(medias8[2])
predi8$svmRadial<-predi8$pred

union1<-rbind(medias1bis,medias2bis,
 medias3bis,medias4bis,medias5bis,medias6bis,
 medias7bis,medias8bis)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo)





# CONSTRUCCIÓN DE TODOS LOS ENSAMBLADOS
# SE UTILIZARÁN LOS ARCHIVOS SURGIDOS DE LAS FUNCIONES LLAMADOS predi1,...

unipredi<-cbind(predi1,predi2,predi3,predi4,predi5,predi6,predi7,predi8)

# Esto es para eliminar columnas duplicadas
unipredi<- unipredi[, !duplicated(colnames(unipredi))]

# Construccion de ensamblados, cambiar al gusto

unipredi$predi9<-(unipredi$reg+unipredi$avnnet)/2
unipredi$predi10<-(unipredi$reg+unipredi$rf)/2
unipredi$predi11<-(unipredi$reg+unipredi$gbm)/2
unipredi$predi12<-(unipredi$reg+unipredi$xgbm)/2
unipredi$predi13<-(unipredi$reg+unipredi$svmLinear)/2
unipredi$predi14<-(unipredi$reg+unipredi$svmPoly)/2
unipredi$predi15<-(unipredi$reg+unipredi$svmRadial)/2
unipredi$predi16<-(unipredi$avnnet+unipredi$rf)/2
unipredi$predi17<-(unipredi$avnnet+unipredi$gbm)/2
unipredi$predi18<-(unipredi$avnnet+unipredi$xgbm)/2
unipredi$predi19<-(unipredi$avnnet+unipredi$svmLinear)/2
unipredi$predi20<-(unipredi$avnnet+unipredi$svmPoly)/2
unipredi$predi21<-(unipredi$avnnet+unipredi$svmRadial)/2
unipredi$predi22<-(unipredi$rf+unipredi$gbm)/2
unipredi$predi23<-(unipredi$rf+unipredi$xgbm)/2
unipredi$predi24<-(unipredi$rf+unipredi$svmLinear)/2
unipredi$predi25<-(unipredi$rf+unipredi$svmPoly)/2
unipredi$predi26<-(unipredi$rf+unipredi$svmRadial)/2
unipredi$predi27<-(unipredi$gbm+unipredi$xgbm)/2
unipredi$predi28<-(unipredi$gbm+unipredi$svmLinear)/2
unipredi$predi29<-(unipredi$gbm+unipredi$svmPoly)/2
unipredi$predi30<-(unipredi$gbm+unipredi$svmRadial)/2

unipredi$predi31<-(unipredi$reg+unipredi$avnnet+unipredi$rf)/3
unipredi$predi32<-(unipredi$reg+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi33<-(unipredi$reg+unipredi$avnnet+unipredi$xgbm)/3
unipredi$predi34<-(unipredi$reg+unipredi$avnnet+unipredi$svmLinear)/3
unipredi$predi35<-(unipredi$reg+unipredi$avnnet+unipredi$svmPoly)/3
unipredi$predi36<-(unipredi$reg+unipredi$avnnet+unipredi$svmRadial)/3
unipredi$predi37<-(unipredi$reg+unipredi$rf+unipredi$gbm)/3
unipredi$predi38<-(unipredi$reg+unipredi$rf+unipredi$xgbm)/3
unipredi$predi39<-(unipredi$reg+unipredi$rf+unipredi$svmLinear)/3
unipredi$predi40<-(unipredi$reg+unipredi$rf+unipredi$svmPoly)/3
unipredi$predi41<-(unipredi$reg+unipredi$rf+unipredi$svmRadial)/3
unipredi$predi42<-(unipredi$reg+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi43<-(unipredi$reg+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi44<-(unipredi$reg+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi45<-(unipredi$reg+unipredi$gbm+unipredi$svmPoly)/3
unipredi$predi46<-(unipredi$reg+unipredi$gbm+unipredi$svmRadial)/3
unipredi$predi47<-(unipredi$reg+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$predi48<-(unipredi$reg+unipredi$xgbm+unipredi$svmPoly)/3
unipredi$predi49<-(unipredi$reg+unipredi$xgbm+unipredi$svmRadial)/3

unipredi$predi50<-(unipredi$rf+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi51<-(unipredi$rf+unipredi$gbm+unipredi$svmPoly)/3
unipredi$predi52<-(unipredi$rf+unipredi$gbm+unipredi$svmRadial)/3

unipredi$predi53<-(unipredi$rf+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$predi54<-(unipredi$rf+unipredi$xgbm+unipredi$svmPoly)/3
unipredi$predi55<-(unipredi$rf+unipredi$xgbm+unipredi$svmRadial)/3

unipredi$predi56<-(unipredi$rf+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi57<-(unipredi$rf+unipredi$avnnet+unipredi$xgbm)/3
unipredi$predi58<-(unipredi$rf+unipredi$avnnet+unipredi$svmLinear)/3
unipredi$predi59<-(unipredi$rf+unipredi$avnnet+unipredi$svmPoly)/3
unipredi$predi60<-(unipredi$rf+unipredi$avnnet+unipredi$svmRadial)/3

unipredi$predi61<-(unipredi$avnnet+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi62<-(unipredi$avnnet+unipredi$gbm+unipredi$svmPoly)/3
unipredi$predi63<-(unipredi$avnnet+unipredi$gbm+unipredi$svmRadial)/3

unipredi$predi64<-(unipredi$reg+unipredi$rf+unipredi$gbm+unipredi$avnnet)/4
unipredi$predi65<-(unipredi$reg+unipredi$rf+unipredi$xgbm+unipredi$avnnet)/4
unipredi$predi66<-(unipredi$reg+unipredi$rf+unipredi$xgbm+unipredi$avnnet)/4

unipredi$predi67<-(unipredi$reg+unipredi$rf+unipredi$xgbm+unipredi$avnnet+unipredi$svmLinear)/5
unipredi$predi68<-(unipredi$reg+unipredi$rf+unipredi$xgbm+unipredi$avnnet+unipredi$svmPoly)/5
unipredi$predi69<-(unipredi$reg+unipredi$rf+unipredi$xgbm+unipredi$avnnet+unipredi$svmRadial)/5


dput(names(unipredi))

# Recorto los modelos de la lista de variables

listado<-c("reg", "avnnet", 
"rf", "gbm", "xgbm", "svmLinear", "svmPoly", "svmRadial", "predi9", 
"predi10", "predi11", "predi12", "predi13", "predi14", "predi15", 
"predi16", "predi17", "predi18", "predi19", "predi20", "predi21", 
"predi22", "predi23", "predi24", "predi25", "predi26", "predi27", 
"predi28", "predi29", "predi30", "predi31", "predi32", "predi33", 
"predi34", "predi35", "predi36", "predi37", "predi38", "predi39", 
"predi40", "predi41", "predi42", "predi43", "predi44", "predi45", 
"predi46", "predi47", "predi48", "predi49", "predi50", "predi51", 
"predi52", "predi53", "predi54", "predi55", "predi56", "predi57", 
"predi58", "predi59", "predi60", "predi61", "predi62", "predi63", 
"predi64", "predi65", "predi66", "predi67", "predi68", "predi69"
)

repeticiones<-nlevels(factor(unipredi$Rep))
unipredi$Rep<-as.factor(unipredi$Rep)
unipredi$Rep<-as.numeric(unipredi$Rep)

# Calculo el MSE para cada repeticion de validación cruzada

library(dplyr)
medias0<-data.frame(c())

for (prediccion in listado)
{
paso <-unipredi[,c("obs",prediccion,"Rep")]
paso$error<-(paso[,c(prediccion)]-paso$obs)^2
paso<-paso %>%
  
  summarize(error=mean(error))     
paso$modelo<-prediccion  
medias0<-rbind(medias0,paso) 
} 
# Finalmente boxplot 

par(cex.axis=0.5,las=2)
boxplot(data=medias0,outcex=0.3,error~modelo)

# PRESENTACION TABLA MEDIAS

tablamedias<-medias0 %>%
  group_by(modelo) %>%
  summarize(error=mean(error))     

tablamedias<-tablamedias[order(tablamedias$error),]

# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN ERROR
# PARA EL GRÁFICO

medias0$modelo <- with(medias0,
 reorder(modelo,error, mean))
par(cex.axis=0.5,las=2)
boxplot(data=medias0,error~modelo,col="pink")


# Se pueden escoger listas pero el factor hay que pasarlo a character
# para que no salgan en el boxplot todos los niveles del factor

listadobis<-c("regresion", "avnnet", 
"rf","gbm",  "xgbm", "svmLinear",  "svmPoly", 
"svmRadial","predi23","predi57", "predi18", "predi16") 

medias0$modelo<-as.character(medias0$modelo)

mediasver<-medias0[medias0$modelo %in% listadobis,]

mediasver$modelo <- with(mediasver,
 reorder(modelo,error, mean))

par(cex.axis=0.5,las=2)
boxplot(data=mediasver,error~modelo,col="pink")





