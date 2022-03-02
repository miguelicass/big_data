# Descripción: Cálculo de una scorecard usando el paquete de R "scorecard"

# https://cran.r-project.org/web/packages/scorecard/vignettes/demo.html


rm(list=ls())

# Carga de librarías ----

#install.packages("mfx")
#install.packages("stargazer")
#install.packages("scorecard")
#install.packages("devtools")
#devtools::install_github("shichenxie/scorecard")


#+ Traditional Credit Scoring Using Logistic Regression
library(stargazer)
library(mfx) # para efectos marginales
library(scorecard)
library(gmodels) #CrossTable()
#library(dplyr)
# Carga de Datos ----

#+ load germancredit data
data("germancredit")

#' #Visualización
#View(germancredit)
head(germancredit)


#+ descripción de las variables ----
# https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)
#?germancredit


# El conjunto de datos tiene 1000 observaciones y 21 variables:  
  
#  - **creditability** Es la variable objetivo que califica a los 1000 clientes como de buenos o malos en función de su riesgo de crédito.



# Las 20 restantes variables (7 numéricas y 13 categóricas) son los atributos o características observadas de esos clientes que se utilizarán para predecir la probabilidad de que los clientes cometan un impago de sis créditos, esto es, de que sean malos clientes. La descripción de estas 20 variables es la siguiente:
  
#  - Attribute 1: (qualitative) **Status of existing checking account o cuenta corriente** 
#   - A11 : ... < 0 DM 
#   - A12 : 0 <= ... < 200 DM 
#   - A13 : ... >= 200 DM / salary assignments for at least 1 year 
#   - A14 : no checking account 

#- Attribute 2: (numerical) **Duration in month** 
  
#  - Attribute 3: (qualitative) **Credit history**  
#     - A30 : no credits taken/ all credits paid back duly (devultos sin mora)
#     - A31 : all credits at this bank paid back duly 
#     - A32 : existing credits paid back duly till now 
#     - A33 : delay in paying off in the past 
#     - A34 : critical account/ other credits existing (not at this bank) 

# - Attribute 4: (qualitative) **Purpose** 
#     - A40 : car (new) 
#     - A41 : car (used)
#     - A42 : furniture/equipment
#     - A43 : radio/television
#     - A44 : domestic appliances
#     - A45 : repairs
#     - A46 : education
#     - A47 : (vacation - does not exist?)
#     - A48 : retraining
#     - A49 : business
#    - A410 : others 

# - Attribute 5: (numerical) **Credit amount** 
  
# - Attribute 6: (qualitative) **Savings account/bonds** 
#    - A61 : ... < 100 DM
#    - A62 : 100 <= ... < 500 DM
#    - A63 : 500 <= ... < 1000 DM
#    - A64 : .. >= 1000 DM 
#    - A65 : unknown/ no savings account 

# - Attribute 7: (qualitative) **Present employment since** 
#     - A71 : unemployed 
#     - A72 : ... < 1 year 
#     - A73 : 1 <= ... < 4 years 
#     - A74 : 4 <= ... < 7 years 
#     - A75 : .. >= 7 years 

# - Attribute 8: (numerical) **Installment rate in percentage of disposable income** 
  
#  - Attribute 9: (qualitative) **Personal status and sex** 
#     - A91 : male : divorced/separated
#     - A92 : female : divorced/separated/married
#     - A93 : male : single
#     - A94 : male : married/widowed
#     - A95 : female : single 

# - Attribute 10: (qualitative) **Other debtors / guarantors**
#    - A101 : none
#    - A102 : co-applicant
#    - A103 : guarantor 

# - Attribute 11: (numerical) **Present residence since**
  
# - Attribute 12: (qualitative) **Property**
#     - A121 : real estate
#     - A122 : if not A121 : building society savings agreement/ life insurance
#     - A123 : if not A121/A122 : car or other, not in attribute 6 
#     - A124 : unknown / no property 

# - Attribute 13: (numerical) **Age in years** 
  
# - Attribute 14: (qualitative) **Other installment plans** Otros pagos por plazos
#      - A141 : bank
#      - A142 : stores
#      - A143 : none 

# - Attribute 15: (qualitative) **Housing**
#      - A151 : rent 
#      - A152 : own
#      - A153 : for free 

# - Attribute 16: (numerical) **Number of existing credits at this bank**
  
# - Attribute 17: (qualitative) **Job**
#   - A171 : unemployed/ unskilled - non-resident
#   - A172 : unskilled - resident
#   - A173 : skilled employee / official
#   - A174 : management/ self-employed/highly qualified employee/ officer 

# - Attribute 18: (numerical) **Number of people being liable to provide maintenance for** 
  
# - Attribute 19: (qualitative) **Telephone**
#   - A191 : none
#   - A192 : yes, registered under the customers name 

# - Attribute 20: (qualitative) **foreign worker** 
#   - A201 : yes 
#   - A202 : no 




# Descriptivo de Datos ----
#+ summary
summary(germancredit$creditability)

# descriptivo de todas las variables
lapply(germancredit, summary) # la salid es un poco más ordenada que summary(germancredit)

# Renombro la base de datos (no es necesario pero me sirve para trabajar con cualquier base de datos)

dt<-germancredit
str(dt)
# summary(dt)

gmodels::CrossTable(dt$creditability)



# renombro creditability as y
dt$y<-as.numeric(dt$creditability)

# recodifico y= Default o Malo
dt$y<-ifelse(dt$y==2,0,1)
CrossTable(dt$y)

head(dt)
tail(dt)

# elimino el campo creditability
dt<-dt[,-21]


#+ bibariate tables ----
str(dt)

#' Comienzo con el análisis estadístico bivariante inicial
#' para tratar de detectar qu variables son buenas candidatas
#' a ser variables explicativas

#' Cómo tengo la variable y ('default') dicotómica, podré hacer dos
#' tipos de análisis, para las variabls x discretas, tablas de contingencia y 
#' test chi2 de independencia
#' Para las variables x continuas, test de diferencia de medias

chisq.test(x=dt$credit.history, y=dt$y)

CrossTable(x=dt$credit.history, y=dt$y, prop.r=TRUE,
           prop.c=FALSE, prop.t=FALSE, prop.chisq=TRUE,
           chisq = TRUE, format="SPSS",digits = 2)

CrossTable(x=dt$purpose, y=dt$y, prop.r=TRUE,
           prop.c=FALSE, prop.t=FALSE, prop.chisq=TRUE,
           chisq = TRUE, format="SPSS",digits = 2)

CrossTable(x=dt$personal.status.and.sex, y=dt$y, prop.r=TRUE,
           prop.c=FALSE, prop.t=FALSE, prop.chisq=TRUE,
           chisq = TRUE, format="SPSS",digits = 2)

# .... debería continuar con el resto de categóricas para comenzar a encontrar evidencia de variables
# que muestren una asociación significativa con y


# Variables continuas

# ¿Hay que transformar alguna variable continua (log??)?

# sobe todo me fijaré en las variables tipo renta  o tipo preci que suelen ser log-normales
hist(dt$credit.amount)

plot(density(dt$credit.amount), col="darkblue")

# Creo la variable en Logs

dt$logamount<-log(dt$credit.amount)

hist(dt$logamount)
plot(density(dt$logamount), col="darkblue")

#########OJO Borro Credit.amount
dt<-dt[, !names(dt) %in% "credit.amount"]

# o con dplyr
#dt <- select(dt, -credit.amount)






#' Test de diferencia de medias

library(onewaytests) #para el test welch


# duration

welch.out<-onewaytests::welch.test(duration.in.month ~ y, data=dt,na.rm=FALSE, verbose = F) 
welch.out$statistic
welch.out$p.value #H0: ausencia de asociación (medias iguales)

boxplot(duration.in.month ~ y , data=dt)

plot(density(dt$duration.in.month[dt$y==0]), col="darkgreen", main="duration.in.month")
lines(density(dt$duration.in.month[dt$y==1]), col="red")
legend(x="topright",legend=c("NO-impago", "IMPAGO"), col=c("darkgreen","red"),lty=1,lwd=1)

#12, 24 meses????



# esfuerzo financiero (En realidad no es numérica)

welch.out<-onewaytests::welch.test(installment.rate.in.percentage.of.disposable.income ~ y, data=dt,na.rm=FALSE, verbose = F) 
welch.out$statistic
welch.out$p.value #H0: ausencia de asociación (medias iguales)

boxplot(installment.rate.in.percentage.of.disposable.income ~ y , data=dt)

plot(density(dt$installment.rate.in.percentage.of.disposable.income[dt$y==0]), col="darkgreen", main="Esfuerzo financiero")
lines(density(dt$installment.rate.in.percentage.of.disposable.income[dt$y==1]), col="red")
legend(x="topleft",legend=c("NO-impago", "IMPAGO"), col=c("darkgreen","red"),lty=1,lwd=1)


# edad

welch.out<-onewaytests::welch.test(age.in.years ~ y, data=dt,na.rm=FALSE, verbose = F) 
welch.out$statistic
welch.out$p.value #H0: ausencia de asociación (medias iguales)

boxplot(age.in.years ~ y , data=dt)

plot(density(dt$age.in.years[dt$y==1]), col="red", main="Edad")
lines(density(dt$age.in.years[dt$y==0]), col="darkgreen")
legend(x="topright",legend=c("NO-impago", "IMPAGO"), col=c("darkgreen","red"),lty=1,lwd=1)


# Cantidad solicitada

welch.out<-onewaytests::welch.test(logamount ~ y, data=dt,na.rm=FALSE, verbose = F) 
welch.out$statistic
welch.out$p.value #H0: ausencia de asociación (medias iguales)

boxplot(logamount ~ y , data=dt)

plot(density(dt$logamount[dt$y==0]), col="darkgreen",main="logamount")
lines(density(dt$logamount[dt$y==1]), col="red")
legend(x="topright",legend=c("NO-impago", "IMPAGO"), col=c("darkgreen","red"),lty=1,lwd=1)






# selección inicial de Varaibles ----
#'  Selección inicial de variables
#'  Aquí se hacer una selección inicial
#'  por valores missing o por valores idénticos
#'  Hasta que no se agrupen y definan las variables WOE no debería
#'  filtrarse ningún otro criterio como IV o GINI

#+ ha creado una lista dt_s. En la que el dataframe dt_s$contiene sólo las variables no eliminadas
dt_s <- var_filter(dt, "y", iv_limit=0) #iv_limit=0 para que coja todas y sólo excluya a la missing
dt_s <- var_filter(dt,"y", iv_limit=0,return_rm_reason = TRUE)
# View(dt_s$rm)
# table(dt$foreign.worker)

#' dividimos la serie original entre entrenamiento (train) y validación (test), para trabajar por separado

dt_list <- split_df(dt_s$dt, y="y", ratio = 0.7, seed = 21)

train <- dt_list$train 
test <- dt_list$test

prop.table(table(train$y))
prop.table(table(test$y))


#MANUALMENTE
#set.seed(567)
#index_train<-sample(1:nrow(dt_s$dt),0.7 * nrow(dt_s$dt))
#train <- dt_s$dt[index_train, ]
#test<- dt_s$dt[-index_train, ]


# # woe binning con la muestra de entrenamiento (tramificación) ------
#+ generates optimal binning for numerical,
# factor and categorical variables using methods including tree-like segmentation or chi-square merge
bins <- woebin(train, "y", print_step = 5)
woebin_plot(bins)
# recordad, un Iv menor que 0.02 la variable es muy débil


#+ binning adjustment
# # adjust breaks interactively
#breaks_adj = woebin_adj(train, "y", bins) 
# # or specify breaks manually
#breaks_adj = list(
#  age.in.years=c(26, 35, 40),
#  other.debtors.or.guarantors=c("none", "co-applicant%,%guarantor"))
#bins_adj = woebin(train, y, breaks_list=breaks_adj)


# Variables woe ----
# Una vez que ya hemos determinado las tramificaciones 
# (con bins o bins_adj), calculo los WOE

train_woe <- woebin_ply(train, bins, print_step = 5)

# le aplico también la tramificacion bins calculada con la muestra
# de  entrenamiento a la muestra de validación
test_woe <- woebin_ply(test, bins, print_step = 5)


# Selección de variables -----
# Una vez que tenemos las variables transformadas en WOE se seleccionan
# las variables del modelo que tengan un iv>0.02

train_wf<- var_filter(train_woe,"y",return_rm_reason = TRUE)
#View(train_wf$rm)
train_woe<-train_wf$dt

# Para el conjunto test en realidad debería mantener las mismas que para l conjunto 
# train y quitar las que he quitado en train
train_kp<-train_wf$rm$variable[is.na(train_wf$rm$rm_reason)] 

train_rm<-train_wf$rm$variable[!is.na(train_wf$rm$rm_reason)]

test_wf  <- var_filter(test_woe,"y",var_rm=train_rm,
                        var_kp=train_kp,
                       return_rm_reason = TRUE)
# View(test_wf$rm)
test_woe<-test_wf$dt


# Ahora con 'train_woe' realizaré el entrenamiento del modelo y
# con 'test_woe' la validación


# Ojo que para hacer la score card deberé quitar del original las variable
# que haya decidido quitar


#' # ESTIMACION
#+ glm ------ Logistic regresion ----
m1 <- glm( y ~ ., family = binomial(link="logit"), data = train_woe)
summary(m1)

stargazer(m1,type = 'text')

# Interpretación de los coeficientes

logitor( y ~ ., data = train_woe)# mfx::This function estimates a binary logistic regression model and calculates the corresponding odds ratios.

logitmfx(y ~ ., data = train_woe, atmean = TRUE, robust = TRUE,) # mfx:: esta estima los efectos MArginales en la media



#+ ODDS Ratios MANUALMENTE
# cbind(
#   Estimate=round(coef(m1),4),OR=round(exp(coef(m1)),4)
#      )
# m1.or <-exp(coef(m1))
# stargazer(m1,type="text",coef=list(m1.or),p.auto=FALSE,out="m1or.txt")
# stargazer(m1,coef=list(m1.or),p.auto=FALSE, type='text')
#######################


#' # Select a formula-based model by AIC
m_step <- step(glm( y ~ ., family = binomial(link="logit"), data = train_woe), direction="backward", trace = TRUE) # modelo hacia atrás
m2 <- eval(m_step$call)
summary(m2)


m_step <- step(glm( y ~ 1, family = binomial(link="logit"), data = train_woe),scope= formula(m1), direction="forward", trace = TRUE) # modelo hacia adelante
m3 <- eval(m_step$call)


m_step <- step(glm( y ~ ., family = binomial(link="logit"), data = train_woe), direction="both", trace = TRUE) # modelo Stepwise
m4 <- eval(m_step$call)


stargazer(m1,m2,m3,m4, type="text")





# Diagnosis ----
#' # performance
#+ predicted proability
train_pred <- predict(m2, type='response', train_woe) # type='response', Para predecir probabilidad
test_pred <- predict(m2, type='response', test_woe)

mean(train_pred)


#' # ks & roc plot
perf_eva(label=train$y, pred=train_pred,title = "train")
perf_eva(label=train$y, pred=train_pred,title = "train", show_plot = c('ks','roc','f1', 'density'))

mean(train$y)
mean(train_pred)

# La validación siempre con la muestra test
perf_eva(label=test$y, pred=test_pred, title = "test")
mean(test_pred)

# The F1 score (also F-score or F-measure) is a measure 
# of a test's accuracy. It considers both the precision p 
# and the recall r of the test to compute the score: p is 
# the number of correct positive results divided by the 
# number of all positive results returned by the classifier,
# and r is the number of correct positive results divided by
# the number of all relevant samples (all samples that should
# have been identified as positive). 
# The F1 score is the harmonic mean of the precision and recall,
# where an F1 score reaches its best value at 1 (perfect precision
# and recall) and worst at 0


# se podría encontrar el máximo utilizando optimize
# library(MLmetrics)
# F1_Score(y_true=train$y, y_pred=train_predconf, positive = NULL)

# f1max<-function(y_true0, y_prob0, cuttoff0){
#   require(MLmetrics)
#   y_pred0<-ifelse(y_prob<=cuttoff,0,1)
#   F1value<-F1_Score(y_true=y_true0, y_pred=y_pred0, positive = "1")
#   return(F1value)  
# }
# f1obj<-optimize(f1max, y_true=train$y, y_prob=train_pred, interval=c(0,1), maximum =TRUE)
# cutoff<-1-f1obj$objective

cuttoff<-mean(train$y) # o máximo F1
cuttoff
#cuttoff<-0.5

# creo los pronósticos con el cuttoff
train_predconf<-ifelse(train_pred<cuttoff,0,1)

test_predconf<-ifelse(test_pred<cuttoff,0,1)

#' # Create confusion matrix
conf_matrix<-table(test$y,test_predconf)
conf_matrix
accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)
accuracy

#' # score

points0_0 <- 600
odds0_0 <- 1/50
pdo_0 <- 20

# Transformación lineal según apuntes
# score= Offset - Factor *ln(odds)
# Factor= (pdo_0/log(2))
# Offset = points0_0+(pdo_0/log(2))*log(odds0_0)

card <- scorecard(bins, m2, points0 = points0_0, odds0 = odds0_0, pdo = pdo_0)
card

card$logamount


#' ## credit score, only_total_score = TRUE
cuttoff_score<-points0_0+(pdo_0/log(2))*log(odds0_0) - (pdo_0/log(2)) *log((cuttoff)/(1-cuttoff))
cuttoff_score

train_scoreT <- scorecard_ply(train, card, print_step = 0, only_total_score = FALSE)
# para obtener la puntuaciónse suma los puntos al Offset-b0*Factor (ver apuntes)
# points0_0+(pdo_0/log(2))*log(odds0_0)-coefficients(m2)[1]*(pdo_0/log(2))

train_score <- scorecard_ply(train, card, print_step = 0)
test_score <- scorecard_ply(test, card, print_step = 0)





train$score<-train_score
train$prob<-train_pred
train_out<-cbind(train,train_scoreT)


View(data.frame(test_score,test_pred))


#' # psi (population stability index)
# Es una medida de diferencia en la distribución de dos muestras
# en nuestro caso entre la muestra test y entrenamiento (pero se aplica
# para ver cuando comienzan a verse diferencias con la predicciones de 
# nuestro modelo con la muestra -train- y los nuevos datos que vayan entrando,
# .... para detectar cuándo nustro modelo necesita una revisión)

perf_psi(
  score = list(train = train_score, test = test_score),
  label = list(train = train[,"y"], test = test[, "y"]),
  x_limits = c(250, 700),
  x_tick_break = 50
)
# PSI <que 0.1	No hay diferencias significativas entre las muestras de test y entrenamiento (resultado deseado, no se requiere más acciones)
# PSI entre 0.1 y  0.25	HAy cambio menores, valdría la pena revisar el modelo
# PSI mayor que 0.25 HAy cambios importantes entre las dos muestras HAY QUE CAMBIAR EL MODELO

