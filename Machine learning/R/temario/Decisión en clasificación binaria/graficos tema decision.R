
source("funcion resultadosglm.R")
source("funcion resultadosnnet.R")
source("funcion resultadosrf.R")
source("funcion resultadossvm.R")
source("funcion resultadosgbm.R")
source("funcion seleccionar 2.0.R")
source("toydata 2.0.R")

require(egg)
library(visualpred)
library(caret)

# grafico 1

v<-toydata(n=400,0.4,1,1,5,-5,0.0000)
v[[1]]+theme(legend.position = "none")+xlab("")+ylab("")

dataf<-v[[2]]
vardep<-"clase"
res<-resultadosglm(dataf=dataf,vardep=vardep)


# grafico 2

sp<-spiral(width=20,height=20,n1=400,n2=401,rbis=6,dtip=1.5)
sp[1]

dataf<-sp[[2]]
listconti<-c("x1","x2")
listclass<-c("")
vardep<-"clase"

result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="GLM",title2=" ",selec=0,modelo="glm",classvar=0)
result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")

res<-resultadosglm(dataf=dataf,vardep=vardep)

result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,title="SVM",
                    title2=" ",selec=0,modelo="svm",classvar=0)
result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")

res<-resultadossvm(dataf=dataf,vardep=vardep)

# grafico 3

v<-toydata(n=400,0.3,2,1,2,-5,1)
dataf<-v[[2]]
listconti<-c("x1","x2")
listclass<-c("")
vardep<-"clase"

result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="GLM",title2=" ",selec=0,modelo="glm",classvar=0)
g1<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="NNET",title2=" ",selec=0,modelo="nnet",classvar=0)
g2<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="RF",title2=" ",selec=0,modelo="rf",classvar=0)
g3<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="GBM",title2=" ",selec=0,modelo="gbm",classvar=0)
g4<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,title="SVM",
                    title2=" ",selec=0,modelo="svm",classvar=0)
g5<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")

ggarrange(g1,g2,g3,g4,g5,ncol =2,nrow=3)

# grafico 4

v<-toydata(n=5000,0.5,2,2,1,1,20,size=0.9)
v[[1]]

dataf<-v[[2]]
listconti<-c("x1","x2")
listclass<-c("")
vardep<-"clase"
res<-resultadosglm(dataf=dataf,vardep=vardep)

result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="GLM",title2=" ",selec=0,modelo="glm",classvar=0)
result[[2]]+xlab("")+ylab("")

# grafico 5

v<-toydata(n=5000,0.00004,1,1,1,-5,1)

dataf<-v[[2]]
listconti<-c("x1","x2")
listclass<-c("")
vardep<-"clase"


result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
title="RF",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="rf",classvar=0)

result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")

result[[2]]+xlab("")+ylab("")

res<-resultadosrf(dataf=dataf,vardep=vardep)

res<-resultadosrf(dataf=dataf,vardep=vardep,corte=0.4)

# grafico 6

v<-toydata(n=400,0.7,2,2,1,3,1)


dataf<-v[[2]]
listconti<-c("x1","x2")
listclass<-c("")
vardep<-"clase"


result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="NNET",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="nnet",classvar=0)

result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result[[2]]+xlab("")+ylab("")


res<-resultadosnnet(dataf=dataf,vardep=vardep)

# grafico 7

v<-toydata(n=400,0.0001,4,4,2,6,10,semilla=12345)


dataf<-v[[2]]
listconti<-c("x1","x2")
listclass<-c("")
vardep<-"clase"


result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="NNET",title2=" ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="nnet",classvar=0)

result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result[[2]]+xlab("")+ylab("")


res<-resultadosnnet(dataf=dataf,vardep=vardep,corte=0.5)

res<-resultadosnnet(dataf=dataf,vardep=vardep,corte=0.2)


# grafico 8

load("BEPS2.Rda")
archivo<-BEPS
listconti<-c("age", "economic.cond.national", "economic.cond.household",
             "Blair", "Hague", "Kennedy", "Europe", "political.knowledge")
listclass<-c("gender")
vardep<-"y"

archivo$class<-archivo$y
archivo$y<-NULL

dataf<-archivo
vardep<-"class"

result<-famdcontour(dataf=archivo,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="rf",title2=" ",Dime1="Dim.1",Dime2="Dim.2",selec=1,modelo="rf",classvar=0,mtry=5)
result[[2]]
result[[3]]
result[[4]]


archi<-dataf[,c(vardep,listconti)]
res<-resultadosrf(dataf=archi,vardep=vardep,mtry=5)
res<-resultadosrf(dataf=archi,vardep=vardep,mtry=5,corte=0.4)

# Esto es para ver el orden en que entran las variables en stepwise
seleccionar(archivo,listconti,listclass,vardep)


listconti<-c("Blair", "Hague","Europe")

result<-famdcontour(dataf=archivo,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="rf",title2=" ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="rf",classvar=0,mtry=3)
result[[2]]
result[[3]]
result[[4]]

archi<-dataf[,c(vardep,listconti)]
res<-resultadosrf(dataf=archi,vardep=vardep,mtry=3)
res<-resultadosrf(dataf=archi,vardep=vardep,mtry=3,corte=0.4)

# con GLM

listconti<-c("age", "economic.cond.national", "economic.cond.household",
             "Blair", "Hague", "Kennedy", "Europe", "political.knowledge")
listclass<-c("gender")

result<-famdcontour(dataf=archivo,listconti=listconti,listclass=listclass,vardep=vardep,
title="GLM",title2=" ",Dime1="Dim.1",Dime2="Dim.2",selec=1,modelo="glm",classvar=0)

result[[4]]


listconti<-c("Blair", "Hague","Europe","Kennedy","political.knowledge","economic.cond.national","age")
archi<-dataf[,c(vardep,listconti)]
res<-resultadosglm(dataf=archi,vardep=vardep)
res<-resultadosglm(dataf=archi,vardep=vardep,corte=0.4)



# Grafico 9


load("ozonodia.Rda")
archivo<-ozonodia
dataf<-archivo
listconti<-c("WSR0", "WSR1", "WSR2", "WSR3", "WSR4", "WSR5", "WSR6",
             "WSR7", "WSR8", "WSR9", "WSR10", "WSR11", "WSR12", "WSR13", "WSR14",
             "WSR15", "WSR16", "WSR17", "WSR18", "WSR19", "WSR20", "WSR21",
             "WSR22", "WSR23", "WSR_PK", "WSR_AV", "T0", "T1", "T2", "T3",
             "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13",
             "T14", "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22",
             "T23", "T_PK", "T_AV", "T85", "RH85", "U85", "V85", "HT85", "T70",
             "RH70", "U70", "V70", "HT70", "T50", "RH50", "U50", "V50", "HT50",
             "KI", "TT", "SLP", "SLP_", "Precp")

listclass<-c("")
vardep<-"diaozono"

result<-famdcontour(dataf=archivo,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="GLM",title2=" ",Dime1="Dim.1",Dime2="Dim.2",selec=1,modelo="glm",classvar=0)

result[[4]]

# Esto es para ver las variables en stepwise
seleccionar(archivo,listconti,listclass,vardep)

listconti<-c("WSR1","WSR5","WSR9","WSR12","WSR17","WSR_PK","T4",  "T13","T19","T22")

result<-famdcontour(dataf=archivo,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="GLM",title2=" ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="glm",classvar=0)

result[[4]]

archi<-dataf[,c(vardep,listconti)]
res<-resultadosglm(dataf=archi,vardep=vardep)


listconti<-c("WSR11","WSR9","T15","T4")

result<-famdcontour(dataf=archivo,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="GLM",title2=" ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="glm",classvar=0)

result[[4]]

archi<-dataf[,c(vardep,listconti)]
res<-resultadosglm(dataf=archi,vardep=vardep)

res<-resultadosglm(dataf=archi,vardep=vardep,corte=0.03)


# grafico 10

load("cerveza.Rda")

source("funcion seleccionar 2.0.R")

listac<-c("American IPA","Double IPA","English IPA","Imperial IPA","Specialty IPA: Belgian IPA",
          "Specialty IPA: Black IPA","Specialty IPA: Rye IPA","Specialty IPA: Brown IPA",
          "Specialty IPA: Red IPA","Specialty IPA: White IPA")

beer2$estilo<-ifelse(beer2[,"style"] %in% listac,"Yes","No")
beer2[,"style"]<-NULL


listconti<-c("size.l.", "og", "fg", "abv", "ibu", "color", "boilsize", "boiltime",
             "boilgravity", "efficiency")
listclass<-c("sugarscale", "brewmethod")

vardep<-"estilo"

archivo<-beer2
seleccionar(archivo,listconti,listclass,vardep)


listconti<-  c("size.l.","abv","ibu","color","boiltime","boilgravity")
listclass<-c("sugarscale","brewmethod")


