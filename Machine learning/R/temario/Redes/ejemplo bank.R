
# bank<-read.csv("bank-additional.csv",header=TRUE,sep = ";")
# save(bank,file="bank.Rda")

load("bank.Rda")

dput(names(bank))


todas<-c("age", "job", "marital", "education", "default", "housing", 
"loan", "contact", "month", "day_of_week", "duration", "campaign", 
"pdays", "previous", "poutcome", "emp.var.rate", "cons.price.idx", 
"cons.conf.idx", "euribor3m", "nr.employed", "y")

table(bank$pdays)

# pdays no la utilizamos

# Una rápida exploración para ver cuales tomamos como 
# cualitativas o continuas, y si hay missing

library(naniar)
gg_miss_var(bank)

for (vari in todas)
{cat(vari)
  print(table(bank[,vari],exclude=NULL))
}

listconti<-c("age","duration","campaign", "emp.var.rate", "cons.price.idx", 
"cons.conf.idx", "euribor3m", "nr.employed")

listclass<-c("job", "marital", "education", "default", "housing", 
"loan", "contact", "month", "day_of_week",
"previous", "poutcome")

vardep<-"y"

# NO HAY MISSING, pasamos directamente a crear dummies 
# y para la red, estandarizar


# b)pasar las categóricas a dummies
library(dummies)
bank2<- dummy.data.frame(bank, listclass, sep = ".")

# c)estandarizar las variables continuas

# Calculo medias y dtipica de datos y estandarizo (solo las continuas)

means <-apply(bank2[,listconti],2,mean) 
sds<-sapply(bank2[,listconti],sd) 

# Estandarizo solo las continuas y uno con las categoricas

bank3<-scale(bank2[,listconti], center = means, scale = sds)
# bank3 son solo las continuas de bank2
numerocont<-which(colnames(bank2)%in%listconti)
# numerocont son las columnas que corresponden a las continuas de bank2
bankfin<-cbind(bank3,bank2[,-numerocont])
# bankfin une bank3 con solo las dummies de bank2 y la dependiente

# Selección de variables 
full<-glm(factor(y)~.,data=bankfin,family = binomial(link="logit"))
null<-glm(factor(y)~1,data=bankfin,family = binomial(link="logit"))

library(MASS)

seleccion<-stepAIC(null,scope=list(upper=full),direction="both")

# Para ver los efectos escogidos
dput(names(seleccion$coefficients))

# Esto si se quiere en versión formula
formula(seleccion)



# c("(Intercept)", "duration", "nr.employed", "poutcome.success", 
# "month.may", "month.mar", "month.jun", "contact.cellular",
#  "cons.conf.idx", 
# "campaign", "month.nov", "previous.0", "`job.self-employed`", 
# "month.sep", "previous.4", "job.entrepreneur", "`job.blue-collar`"
# )
# 
# 
# factor(y) ~ duration + nr.employed + poutcome.success + month.may + 
#     month.mar + month.jun + contact.cellular + cons.conf.idx + 
#     campaign + month.nov + previous.0 + `job.self-employed` + 
#     month.sep + previous.4 + job.entrepreneur + `job.blue-collar`



# ************************************
# APLICANDO steprepetidobinaria 
# ************************************
source("funcion steprepetido binaria.R")

data<-bankfin

dput(names(bankfin))


listconti<-c("age", "duration", "campaign", "emp.var.rate", "cons.price.idx", 
"cons.conf.idx", "euribor3m", "nr.employed", "job.admin.", "job.blue-collar", 
"job.entrepreneur", "job.housemaid", "job.management", "job.retired", 
"job.self-employed", "job.services", "job.student", "job.technician", 
"job.unemployed", "job.unknown", "marital.divorced", "marital.married", 
"marital.single", "marital.unknown", "education.basic.4y", "education.basic.6y", 
"education.basic.9y", "education.high.school", "education.illiterate", 
"education.professional.course", "education.university.degree", 
"education.unknown", "default.no", "default.unknown", "default.yes", 
"housing.no", "housing.unknown", "housing.yes", "loan.no", "loan.unknown", 
"loan.yes", "contact.cellular", "contact.telephone", "month.apr", 
"month.aug", "month.dec", "month.jul", "month.jun", "month.mar", 
"month.may", "month.nov", "month.oct", "month.sep", "day_of_week.fri", 
"day_of_week.mon", "day_of_week.thu", "day_of_week.tue", "day_of_week.wed", 
"pdays", "previous.0", "previous.1", "previous.2", "previous.3", 
"previous.4", "previous.5", "previous.6", "poutcome.failure", 
"poutcome.nonexistent", "poutcome.success")

lista<-steprepetidobinaria(data=data,
 vardep=vardep,listconti=listconti,sinicio=12345,
 sfinal=12395,porcen=0.8,criterio="BIC")

tabla<-lista[[1]]
dput(lista[[2]][[1]])
dput(lista[[2]][[2]])
tabla

# Vamos a quedarnos con estos modelos

mod1<-c("duration", "nr.employed", "poutcome.success", "month.mar", 
"month.jun", "contact.cellular", "cons.conf.idx")

mod2<-c("duration", "nr.employed", "poutcome.success", "month.may", 
"month.mar", "month.jun", "cons.price.idx")

# Monitorizamos las redes con los dos básicamente y comparamos
# vía CV repetida con logística

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

avnnetgrid <-expand.grid(size=c(5,10,15,20),
 decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(y~ duration+nr.employed+poutcome.success+month.mar+month.jun+
  contact.cellular+cons.conf.idx,
 data=bankfin,
method="avNNet",linout = FALSE,maxit=100,
 trControl=control,tuneGrid=avnnetgrid,
 repeats=5)

redavnnet

# size=5, decay=0.1

avnnetgrid <-expand.grid(size=c(5,10,15,20),
 decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(y~ duration+nr.employed+poutcome.success+month.mar+month.jun+
  month.may+cons.price.idx,
 data=bankfin,
method="avNNet",linout = FALSE,maxit=100,
 trControl=control,tuneGrid=avnnetgrid,
 repeats=5)

redavnnet

# size=5, decay=0.1



source("cruzadas avnnet y log binaria.R")

# para esta función es necesario que la vardep 
# esté categorizada como Yes,No

bankfin$y<-ifelse(bankfin$y =='yes','Yes','No')
 

medias1<-cruzadalogistica(data=bankfin,
 vardep="y",listconti=c("duration", "nr.employed",
  "poutcome.success", "month.mar", 
"month.jun", "contact.cellular", "cons.conf.idx"),
 listclass=c(""), grupos=4,sinicio=1234,repe=25)

 medias1$modelo="Logística1"
 
 medias2<-cruzadalogistica(data=bankfin,
 vardep="y",listconti=c("duration", "nr.employed",
  "poutcome.success", "month.may", 
"month.mar", "month.jun", "cons.price.idx"),
 listclass=c(""), grupos=4,sinicio=1234,repe=25)

medias2$modelo="Logística2"

medias3<-cruzadaavnnetbin(data=bankfin,
 vardep="y",listconti=c("duration", "nr.employed", "poutcome.success", "month.mar", 
"month.jun", "contact.cellular", "cons.conf.idx"),
 listclass=c(""),grupos=4,sinicio=1234,repe=25,
  size=c(5),decay=c(0.1),repeticiones=5,itera=200)

medias3$modelo="avnnet1"

medias4<-cruzadaavnnetbin(data=bankfin,
 vardep="y",listconti=c("duration", "nr.employed",
  "poutcome.success", "month.may", 
"month.mar", "month.jun", "cons.price.idx"),
 listclass=c(""),grupos=4,sinicio=1234,repe=25,
  size=c(5),decay=c(0.1),repeticiones=5,itera=200)

medias4$modelo="avnnet2"

union1<-rbind(medias1,medias2,medias3,medias4)


par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union1,auc~modelo,main="AUC",col="pink")

