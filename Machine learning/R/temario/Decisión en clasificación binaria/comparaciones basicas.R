
# load("Gunnels.Rda")
# archivo<-Gunnels
# listconti<-c("Time", "Fromlow", "Slope", "Rw", "Amphiso")
# listclass<-c("Subst", "Pool", "Water", "Cobble")
# vardep<-c("Gunnel")
# 
# 
# clave<-"Gunnels"


# load("ozonodia.Rda")
# archivo<-ozonodia
# listconti<-c("WSR0", "WSR1", "WSR2", "WSR3", "WSR4", "WSR5", "WSR6",
#              "WSR7", "WSR8", "WSR9", "WSR10", "WSR11", "WSR12", "WSR13", "WSR14",
#              "WSR15", "WSR16", "WSR17", "WSR18", "WSR19", "WSR20", "WSR21",
#              "WSR22", "WSR23", "WSR_PK", "WSR_AV", "T0", "T1", "T2", "T3",
#              "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13",
#              "T14", "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22",
#              "T23", "T_PK", "T_AV", "T85", "RH85", "U85", "V85", "HT85", "T70",
#              "RH70", "U70", "V70", "HT70", "T50", "RH50", "U50", "V50", "HT50",
#              "KI", "TT", "SLP", "SLP_", "Precp")
# 
# listclass<-c("")
# vardep<-"diaozono"
# 
# clave<-"ozono"
# 



# load("Caravan.Rda")
# archivo<-Caravan
# listconti<-c("MOSTYPE", "MAANTHUI", "MGEMOMV", "MGEMLEEF", "MOSHOOFD", "MGODRK",
#              "MGODPR", "MGODOV", "MGODGE", "MRELGE", "MRELSA", "MRELOV", "MFALLEEN",
#              "MFGEKIND", "MFWEKIND", "MOPLHOOG", "MOPLMIDD", "MOPLLAAG", "MBERHOOG",
#              "MBERZELF", "MBERBOER", "MBERMIDD", "MBERARBG", "MBERARBO", "MSKA",
#              "MSKB1", "MSKB2", "MSKC", "MSKD", "MHHUUR", "MHKOOP", "MAUT1",
#              "MAUT2", "MAUT0", "MZFONDS", "MZPART", "MINKM30", "MINK3045",
#              "MINK4575", "MINK7512", "MINK123M", "MINKGEM", "MKOOPKLA", "PWAPART",
#              "PWABEDR", "PWALAND", "PPERSAUT", "PBESAUT", "PMOTSCO", "PVRAAUT",
#              "PAANHANG", "PTRACTOR", "PWERKT", "PBROM", "PLEVEN", "PPERSONG",
#              "PGEZONG", "PWAOREG", "PBRAND", "PZEILPL", "PPLEZIER", "PFIETS",
#              "PINBOED", "PBYSTAND", "AWAPART", "AWABEDR", "AWALAND", "APERSAUT",
#              "ABESAUT", "AMOTSCO", "AVRAAUT", "AAANHANG", "ATRACTOR", "AWERKT",
#              "ABROM", "ALEVEN", "APERSONG", "AGEZONG", "AWAOREG", "ABRAND",
#              "AZEILPL", "APLEZIER", "AFIETS", "AINBOED", "ABYSTAND")
# listclass<-c("")
# vardep<-c("Purchase")
# 
# clave<-"Caravan"



# load("acuteinf.Rda")
# archivo<-acuteinf
# listconti<-c("a1")
# listclass<-c("a2","a3","a4","a5","a6")
# vardep<-c("inf")
# 
# clave<-"acuteinf"

# load("heartclevbien.Rda")
# archivo<-heartsal
# listconti<-c("age", "trestbps", "chol", "thalach",
#              "oldpeak", "ca.2", "ca.3",
#              "ca.4", "ca.5", "thal.2", "thal.4", "sex.0", "sex.1", "cp.1",
#              "cp.2", "cp.3", "cp.4", "fbs.0", "fbs.1", "restecg.0", "restecg.2",
#              "exang.0", "exang.1", "slope.1", "slope.2", "slope.3")
# listclass<-c("")
# vardep<-"class2"
# 
# clave<-"heartclev"


# load("lympho.Rda")
# archivo<-lympho
# listconti<-c("nnodes")
# listclass<-c("ly","block","blc","bls","bypass","extra",
#              "reg","early","nodesdim","nodesen","changel","defect","changen",
#              "changestr","special","disloc","exclu")
# vardep<-"class2"
# 
# 
# clave<-"lympho"

# load("hepatitis.Rda")
# archivo<-hepatitis
# listconti<-c("BILIRUBIN", "SGOT", "ALBUMIN", "AGE", "STEROID.2", "STEROID.3", 
#              "FATIGUE.2", "FATIGUE.3", "MALAISE.2", "MALAISE.3", "ANOREXIA.2", 
#              "ANOREXIA.3", "LIVER_BIG.2", "LIVER_BIG.3", "LIVER_FIRM.2", "LIVER_FIRM.3", 
#              "SPLEEN_PALPABLE.2", "SPLEEN_PALPABLE.3", "SPIDERS.2", "SPIDERS.3", 
#              "ASCITES.2", "ASCITES.3", "VARICES.2", "SEX.1", "ANTIVIRALS.1", 
#              "ANTIVIRALS.2", "HISTOLOGY.1", "HISTOLOGY.2")
# listclass<-c("")
# vardep<-"Class"
# 
# clave<-"hepatitis"


# load("bridges.Rda")
# 
# archivo<-bridges
# listconti<-c("")
# listclass<-c("river","location","purpose","clear","tod","material","span","erected","length","lanes")
# vardep<-c("rellF")
# 
# clave<-"bridges-rellF"
#  
 
# load("breast.Rda")
# archivo<-breast
# listconti<-c("I0", "PA500", "HFS", "DA", "Area", "A.DA",
#              "Max.IP", "DR", "P")
# listclass<-c("")
# vardep<-c("Classfin")
# 
# clave<-"breast2"
 


# load("bankfin.Rda")
# 
# listconti<-c("duration","nr.employed","cons.conf.idx")
# 
# listclass<-c("poutcome.success", "month.mar",
#              "month.jun", "contact.cellular")
# 
# archivo<-bankfin
# archivo$clase<-archivo$y
# 
# archivo$y<-NULL
# 
# vardep<-"clase"
# 
# clave<-"bank"

# load("magic.Rda")
# #
# archivo<-magic
# # 
# listconti<-c("FLength","FSize","FConc1","FM3Long","FAlpha")
# listclass<-c("")
# vardep="class"
# 
# clave<-"magic"


# load("australian.Rda")
# 
# australian$nuevoA10<-ifelse(australian$A10==0,1,0)
# archivo<-australian
# 
# listclass<-c("A4", "A5","A8", "A9","A12","nuevoA10")
# listconti<-c("A14","A13")
# vardep<-"A15"
# 
# clave<-"australian"
# 
# load("breastwisconsin1.Rda")
# archivo<-breastwisconsin1
# 
# listconti=c( "clump_thickness","uniformity_of_cell_shape",
#              "marginal_adhesion","bare_nuclei",
#              "bland_chromatin", "normal_nucleoli", "mitosis")
# listclass=c("")
# vardep=c("classes")
# titulo="breast"
# 
# clave<-"breast"

# load("BEPS2.Rda")
# archivo<-BEPS
# listconti<-c("age", "economic.cond.national", "economic.cond.household",
#              "Blair", "Hague", "Kennedy", "Europe", "political.knowledge")
# listclass<-c("gender")
# vardep<-"y"
# 
# archivo$class<-archivo$y
# archivo$y<-NULL
# 
# vardep<-"class"
# 
# clave<-"BEPS"

#
# load("indian.Rda")
# archivo<-indian
# listconti<-c("Age","TB", "DB", "Alkphos", "Sgpt","Sgot", "TP", "ALB", "AGRatio")
# listclass<-c("Gender")
# vardep<-"Selector"
# archivo<-na.omit(archivo)
# 
# clave<-"Indian"
#
# load("nba.rda")
# 
# archivo<-nba
# archivo<-na.omit(archivo)
# 
# 
# listconti<-c("GP", "MIN", "PTS", "FGM", "FGA", "FG", "P3Made", "P3A",
#              "P3", "FTM", "FTA", "FT", "OREB", "DREB", "REB", "AST", "STL",
#              "BLK", "TOV")
# listclass<-c("")
# vardep<-"TARGET_5Yrs"
# 
# clave<-"NBA"



#
# load("fertility.Rda")
# archivo<-na.omit(fertility)
# listconti<-c("Age","alcohol","sedent")
# listclass<-c("Season","disease","accident","surgical", "fevers","smoking")
# vardep<-c("diagnosis")
# 
# clave<-"Fertility"


#
# load("Hmda.Rda")
# listconti<-c("dir", "hir", "lvr", "ccs", "mcs", "uria")
# listclass<-c("pbcr", "dmi", "self", "single", "condominium", "black")
# vardep<-c("deny")
#
# Hmda<-na.omit(Hmda)
#
# archivo<-Hmda
#
# clave<-"Hmda"

# load("plates.Rda")
# archivo<-plates
# listconti<-c("X_Minimum", "X_Maximum", "Y_Minimum", "Y_Maximum", "Pixels_Areas",
#              "X_Perimeter", "Y_Perimeter", "Sum_of_Luminosity", "Minimum_of_Luminosity",
#              "Maximum_of_Luminosity", "Length_of_Conveyer", "TypeOfSteel_A300",
#              "TypeOfSteel_A400", "Steel_Plate_Thickness", "Edges_Index", "Empty_Index",
#              "Square_Index", "Outside_X_Index", "Edges_X_Index", "Edges_Y_Index",
#              "Outside_Global_Index", "LogOfAreas", "Log_X_Index", "Log_Y_Index",
#              "Orientation_Index", "Luminosity_Index", "SigmoidOfAreas")
# listclass<-c("")
# vardep<-"Pastry"

# clave<-"plates"

# load("shuttle.Rda")
# archivo<-shuttle
# listconti<-c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")
# listclass<-c("")
# vardep<-"class"
# 
# clave<-"shuttle"


# load("spam.Rda")
# archivo<-spam
# listconti<-c("A.1", "A.2", "A.3", "A.4", "A.5", "A.6", "A.7", "A.8", "A.9",
#              "A.10", "A.11", "A.12", "A.13", "A.14", "A.15", "A.16", "A.17",
#              "A.18", "A.19", "A.20", "A.21", "A.22", "A.23", "A.24", "A.25",
#              "A.26", "A.27", "A.28", "A.29", "A.30", "A.31", "A.32", "A.33",
#              "A.34","A.35","A.36", "A.37", "A.38", "A.39", "A.40", "A.41",
#              "A.42", "A.43", "A.44", "A.45", "A.46", "A.47", "A.48", "A.49",
#              "A.50", "A.51", "A.52", "A.53", "A.54", "A.55", "A.56", "A.57" )
# listclass<-c("")
# vardep<-"spam"
# clave<-"spam"

# load("pima.Rda")
# 
# 
# names(pima)<-c("pregnant", "glucose", "pressure", "triceps", "insulin", "bodymass",
#                "pedigree", "age", "diabetes")
# 
# archivo<-pima
# listconti<-c("pregnant", "glucose", "pressure", "triceps", "insulin", "bodymass",
#              "pedigree", "age")
# listclass<-c("")
# vardep<-"diabetes"
# 
# clave<-"pima"

# load("yeast2.Rda")
# archivo<-yeast2
# listconti<-c("mcg", "gvh", "alm", "mit", "erl", "pox", "vac","nuc")
# listclass<-c("")
# vardep<-"class.NUC"
# 
# clave<-"yeast2"

library(parallel)
library(doParallel)

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing


source ("cruzadas avnnet y log binaria.R")
source ("cruzada rf binaria.R")
source ("cruzada gbm binaria.R")
source ("cruzada SVM binaria RBF.R")
source("funcion seleccionar 2.0.R")
require(egg)
library(visualpred)


return<-seleccionar(archivo,listconti,listclass,vardep)

dataf<-return[[1]]
listconti<-return[[2]]
listclass<-return[[3]]
if (length(listclass)==0)
{listclass<-c("")}

vardep<-return[[4]]


tabla1<-as.data.frame(table(dataf[,vardep]))
tabla1<-tabla1[order(tabla1$Freq),]
minoritaria<-as.character(tabla1[1,c("Var1")])
tabla1<-tabla1[order(-tabla1$Freq),]
mayoritaria<-as.character(tabla1[1,c("Var1")])

dataf[,c(vardep)]<-ifelse(dataf[,c(vardep)]==minoritaria,"Yes","No")

formu<-formula(paste(vardep,"~."))

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)


avnnetgrid <-expand.grid(size=c(5,10,15,20),
                         decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(formula(formu),data=dataf,method="avNNet",linout = FALSE,maxit=400,
                  trControl=control,tuneGrid=avnnetgrid,
                  repeats=5,trace=FALSE)

size=redavnnet$bestTune$size
decay=redavnnet$bestTune$decay


total<-length(listconti)+length(listclass)-1
set.seed(12345)
rfgrid<-expand.grid(mtry=seq(1,total,by=1))

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)

rf<- train(formula(formu),data=dataf,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=1000,nodesize=10,replace=TRUE,
           importance=TRUE)

mtry=rf$bestTune$mtry

gbmgrid<-expand.grid(shrinkage=c(0.1,0.05,0.03,0.01,0.001),
                     n.minobsinnode=c(10),
                     n.trees=c(100,500,1000,5000),
                     interaction.depth=c(2,4))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
classProbs=TRUE)

formu2<-paste("factor(",vardep,")~.")

gbm<-train(formula(formu2),data=dataf,method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

n.trees=gbm$bestTune$n.trees
shrink=gbm$bestTune$shrinkage
interaction.depth=gbm$bestTune$interaction.depth

SVMgrid<-expand.grid(C=c(0.0001,0.5,1,2,5,10,30),
                     sigma=c(0.0001,0.005,0.01,0.05,10))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all")

SVM<- train(formula(formu),data=dataf,
            method="svmRadial",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

gamma=SVM$bestTune$sigma
C=SVM$bestTune$C


archivo<-dataf
medias1<-cruzadalogistica(data=archivo,
                          vardep=vardep,listconti=listconti,
                          listclass=listclass, grupos=4,sinicio=1234,repe=5)

medias1$modelo="Logística"


medias2<-cruzadaavnnetbin(data=archivo,
                          vardep=vardep,listconti=listconti,
                          listclass=listclass,grupos=4,sinicio=1234,repe=5,
                          size=size,decay=decay,repeticiones=5,itera=200,trace=FALSE)

medias2$modelo="avnnet"


medias5<-cruzadarfbin(data=archivo, vardep=vardep,
                      listconti=listconti,
                      listclass=listclass,
                      grupos=4,sinicio=1234,repe=5,nodesize=10,
                      mtry=mtry,ntree=300,replace=TRUE)

medias5$modelo="rf"


medias6<-cruzadagbmbin(data=archivo, vardep=vardep,
                       listconti=listconti,
                       listclass=listclass,
                       grupos=4,sinicio=1234,repe=5,
                       n.minobsinnode=10,shrinkage=shrink,n.trees=n.trees,interaction.depth=2)

medias6$modelo="gbm"

medias10<-cruzadaSVMbinRBF(data=archivo, vardep=vardep,
                           listconti=listconti,
                           listclass=listclass,
                           grupos=4,sinicio=1234,repe=5,
                           C=C,sigma=gamma)

medias10$modelo="SVMRBF"


stopCluster(cluster) # shut down the cluster 
registerDoSEQ(); #  force R to return to single threaded processing
GS_T1 <- Sys.time()
GS_T1-GS_T0


union1<-rbind(medias1,medias2,medias5,
              medias6,medias10)

par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union1,auc~modelo,main="AUC",col="pink")

uni<-union1
uni$modelo <- with(uni,
                   reorder(modelo,tasa, median))
par(cex.axis=0.8,las=2)
boxplot(data=uni,tasa~modelo,col="pink",main="TASA FALLOS")

uni<-union1
uni$modelo <- with(uni,
                   reorder(modelo,auc, median))

par(cex.axis=1,las=1)
boxplot(data=uni,auc~modelo,col="pink",main="AUC")




dataf<-archivo
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                         title="GLM",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="glm",classvar=0)
g1<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                         title="NNET",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="nnet",
                         nodos=size,decay=decay,classvar=0)
g2<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                         title="RF",title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="rf",classvar=0,
                         mtry=mtry)
g3<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,
                         title="GBM",ntree=n.trees,shrink=shrink,title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,
                         modelo="gbm",classvar=0)
g4<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")
result<-famdcontour(dataf=dataf,listconti=listconti,listclass=listclass,vardep=vardep,title="SVM",
                         title2="  ",Dime1="Dim.1",Dime2="Dim.2",selec=0,modelo="svm",C=C,gamma=gamma,classvar=0)
g5<-result[[2]]+theme(legend.position = "none")+xlab("")+ylab("")


# Aquí empieza

titulo<-paste(clave,", AUC",sep="")

filetejp<-paste(clave,"z.jpg",sep="")
filetebox<-paste(clave,"box.jpg",sep="")

# Guardo el ggbox
jpeg(filename =filetebox, width = 500, height = 600, quality = 100)
par(cex.axis=1.2,las=1)
boxplot(data=uni,auc~modelo,col="pink",main=titulo)
dev.off()

# 28.4x19.6

# Guardo el ggarrange
jpeg(filename =filetejp, width =900, height =1200,quality = 500)
ggarrange(g1,g2,g3,g4,g5,ncol =2,nrow=3)
dev.off()


