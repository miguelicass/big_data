# Cargo las funciones que voy a utilizar después
source("FuncionesRosa.R")

# Cargo las librerias que me van a hacer falta


#Cargo los datos depurados (incluidas las mejores transformaciones de las 
# variables numéricas respecto a la binaria)
todo<-readRDS("todo_bin")

#veo el reparto original. Compruebo que la variable objetivo tome valor 1 para el evento y 0 para el no evento
freq(todo$varObjBin) 

#Hago la partición
set.seed(123456)
trainIndex <- createDataPartition(todo$varObjBin, p=0.8, list=FALSE)
data_train <- todo[trainIndex,c(1:14,25)]#quito las variables transformadas por el momento
data_test <- todo[-trainIndex,c(1:14,25)]#quedo con las variables sin trasnformar y la objetivo

#pruebo un primer modelo sin las transformadas
modeloInicial<-glm(varObjBin~.,data=data_train,family=binomial)
summary(modeloInicial)
pseudoR2(modeloInicial,data_train,"varObjBin")
pseudoR2(modeloInicial,data_test,"varObjBin")
modeloInicial$rank #número de parámetros


#fijandome en la significación de las variables,el modelo con las variables más significativas queda
modelo2<-glm(varObjBin~Clasificacion+CalifProductor+Etiqueta+pH+Acidez+CloruroSodico,data=data_train,family=binomial)
summary(modelo2)
pseudoR2(modelo2,data_train,"varObjBin")#es un poquito peor que el anterior, pero el n. de parametros es casi la mitad
pseudoR2(modelo2,data_test,"varObjBin")
modelo2$rank

impVariablesLog(modelo2,"varObjBin") #quizás sobren las 3 últimas

#Miro el gráfico V de Cramer para ver las variables más importantes
graficoVcramer(todo[,c(1:14,25)],todo$varObjBin) 

modelo3<-glm(varObjBin~.-Region-Etiqueta-PrecioBotella-Alcohol-pH-Sulfatos-Azucar-AcidoCitrico,data=data_train,family=binomial)
summary(modelo3)
pseudoR2(modelo3,data_train,"varObjBin")#es bastante peor que el 2 y tiene más parámetros
pseudoR2(modelo3,data_test,"varObjBin")
modelo3$rank

#Eliminamos las variables que no son significativas

modelo3_bis<-glm(varObjBin~.-Region-Etiqueta-PrecioBotella-Alcohol-pH-Sulfatos-Azucar-AcidoCitrico-Densidad-prop_missings,data=data_train,family=binomial)
summary(modelo3_bis)
pseudoR2(modelo3_bis,data_train,"varObjBin")#es bastante peor que el 2 y tiene más parámetros
pseudoR2(modelo3_bis,data_test,"varObjBin")
modelo3_bis$rank

#Pruebo alguna interaccion sobre el 2
modelo4<-glm(varObjBin~Clasificacion+CalifProductor+Etiqueta+pH+Acidez+CloruroSodico+Clasificacion:Etiqueta,data=data_train,family=binomial)
summary(modelo4)
pseudoR2(modelo4,data_train,"varObjBin")
pseudoR2(modelo4,data_test,"varObjBin")
modelo4$rank


modelo4_1<-glm(varObjBin~Clasificacion+CalifProductor+Etiqueta+pH+Acidez+CloruroSodico+Clasificacion:CalifProductor,data=data_train,family=binomial)
summary(modelo4_1)
pseudoR2(modelo4_1,data_train,"varObjBin")
pseudoR2(modelo4_1,data_test,"varObjBin")
modelo4_1$rank

modelo4_2<-glm(varObjBin~Clasificacion+CalifProductor+Etiqueta+pH+Acidez+CloruroSodico+Clasificacion:CalifProductor,data=data_train,family=binomial)
# No es significativa esta interacción
summary(modelo4_2)
pseudoR2(modelo4_2,data_train,"varObjBin")
pseudoR2(modelo4_2,data_test,"varObjBin")
modelo4_2$rank

modelo4_3<-glm(varObjBin~Clasificacion+CalifProductor+Etiqueta+pH+Acidez+CloruroSodico+Etiqueta:CalifProductor,data=data_train,family=binomial)
# No es buena
summary(modelo4_3)
pseudoR2(modelo4_3,data_train,"varObjBin")
pseudoR2(modelo4_3,data_test,"varObjBin")
modelo4_3$rank

#pruebo uno con las variables más importantes del 2 
modelo5<-glm(varObjBin~Clasificacion+CalifProductor+Etiqueta,data=data_train,family=binomial)
summary(modelo5)
pseudoR2(modelo5,data_train,"varObjBin")
pseudoR2(modelo5,data_test,"varObjBin")
modelo5$rank

#pruebo uno con las variables más importantes del 2 y una interacción
modelo6<-glm(varObjBin~Clasificacion+CalifProductor+Etiqueta+Clasificacion:Etiqueta,data=data_train,family=binomial)
pseudoR2(modelo6,data_train,"varObjBin")#No parece muy buena idea
pseudoR2(modelo6,data_test,"varObjBin")
modelo6$rank

#Validacion cruzada repetida para elegir entre todos
auxVarObj<-todo$varObjBin
todo$varObjBin<-make.names(todo$varObjBin) #formateo la variable objetivo para que funcione el codigo
total<-c()
modelos<-sapply(list(modeloInicial,modelo2,modelo3,modelo4,modelo5,modelo6),formula)
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
boxplot(roc~modelo,data=total,main="Área bajo la curva ROC") #el 3 es peor, los otros parecidos
aggregate(roc~modelo, data = total, mean) 
aggregate(roc~modelo, data = total, sd) #muy similar

#recupero la variable objetivo en su formato
todo$varObjBin<-auxVarObj

#miro el numero de parametros
modeloInicial$rank
modelo2$rank 
modelo4$rank
modelo5$rank #Tiene menos parámetros y funciona prácticamente igual
modelo6$rank

## BUscamos el mejor punto de corte

#probamos dos
sensEspCorte(modelo5,data_test,"varObjBin",0.5,"1")
sensEspCorte(modelo5,data_test,"varObjBin",0.75,"1")

## generamos una rejilla de puntos de corte
posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,function(x) sensEspCorte(modelo5,data_test,"varObjBin",x,"1")))))
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1
plot(rejilla$posiblesCortes,rejilla$Youden)
plot(rejilla$posiblesCortes,rejilla$Accuracy)
rejilla$posiblesCortes[which.max(rejilla$Youden)]
rejilla$posiblesCortes[which.max(rejilla$Accuracy)]

#El resultado es 0.73 para youden y 0.48 para Accuracy
#Los comparamos
sensEspCorte(modelo5,data_test,"varObjBin",0.48,"1")
sensEspCorte(modelo5,data_test,"varObjBin",0.73,"1")

# Vemos las variables más importantes del modelo ganador
impVariablesLog(modelo5,"varObjBin") 

# Vemos los coeficientes del modelo ganador
coef(modelo5)

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
pseudoR2(modelo5,data_train,"varObjBin")
pseudoR2(modelo5,data_test,"varObjBin")
#es poca la diferencia, por lo que el modelo se puede considerar robusto
roc(data_train$varObjBin, predict(modelo5,data_train,type = "response"))
roc(data_test$varObjBin, predict(modelo5,data_test,type = "response"))
# también es poca la diferencia en el área bajo la curva roc y para el punto de corte
sensEspCorte(modelo5,data_train,"varObjBin",0.73,"1") # 1 indica el evento
sensEspCorte(modelo5,data_test,"varObjBin",0.73,"1")


