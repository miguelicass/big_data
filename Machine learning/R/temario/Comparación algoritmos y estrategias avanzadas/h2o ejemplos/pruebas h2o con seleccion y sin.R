

 germanbis<-germanbis[,

c("bad","checking", "duration", "amount", "savings", "employed", "installp", 
"coapp", "resident", "age", "existcr", "job", "depends", "history.0", 
"history.1", "history.2", "history.3", "history.4", "purpose.0", 
"purpose.1", "purpose.2", "purpose.3", "purpose.4", "purpose.5", 
"purpose.6", "purpose.8", "purpose.9", "purpose.X", "marital.1", 
"marital.2", "marital.3", "marital.4", "property.1", "property.2", 
"property.3", "property.4", "other.1", "other.2", "other.3", 
"housing.1", "housing.2", "housing.3", "telephon.1", "telephon.2", 
"foreign.1", "foreign.2")] 


 var2<-c("bad","checking", "duration", "amount", "savings", "employed", "installp", 
"coapp", "resident", "age", "existcr", "job", "depends", "history.0", 
"history.1", "history.2", "history.3", "history.4", "purpose.0", 
"purpose.1", "purpose.2", "purpose.3", "purpose.4", "purpose.5", 
"purpose.6", "purpose.8", "purpose.9", "purpose.X", "marital.1", 
"marital.2", "marital.3", "marital.4", "property.1", "property.2", 
"property.3", "property.4", "other.1", "other.2", "other.3", 
"housing.1", "housing.2", "housing.3", "telephon.1", "telephon.2", 
"foreign.1", "foreign.2")
 
 length(var2)
 
library(MASS)
data<-germanbis

full<-glm(bad~.,data=data,family = binomial(link="logit"))
null<-glm(bad~1,data=data,family = binomial(link="logit"))

selec1<-stepAIC(null,scope=list(upper=full),direction="both",trace=FALSE,family = binomial(link="logit"))

summary(selec1)

formula(selec1)

 varsel<-c("bad","checking", "duration", "history.4","savings","purpose.0","purpose.6","other.3","marital.3", 
 "installp", "amount","foreign.1","purpose.1","history.0","housing.1","coapp","history.1", 
"telephon.1","employed", "property.4")

germanbis2<-germanbis[,varsel] 
  
length(varsel)

library(h2o)

h2o.init()


# aml = H2OAutoML(max_runtime_secs = 120)    
train<- as.h2o(germanbis2)

  
aml <- h2o.automl(x = 2:19,y=1,training_frame = train,max_models = 20,seed = 1)

lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)

#                                              model_id       auc   logloss mean_per_class_error      rmse
# 1            GLM_grid_1_AutoML_20190428_120526_model_1 0.8040881 0.4815283            0.2611905 0.3980979
# 2  StackedEnsemble_BestOfFamily_AutoML_20190428_120526 0.8039214 0.4834851            0.2609524 0.3970062
# 3     StackedEnsemble_AllModels_AutoML_20190428_120526 0.8020524 0.4843023            0.2557143 0.3970071
# 4   DeepLearning_grid_1_AutoML_20190428_120526_model_2 0.7973833 0.5249502            0.2647619 0.4070323
# 5                         GBM_3_AutoML_20190428_120526 0.7884762 0.4996318            0.2735714 0.4054799
# 6            GBM_grid_1_AutoML_20190428_120526_model_5 0.7878571 0.5010849            0.2754762 0.4062418
# 7            GBM_grid_1_AutoML_20190428_120526_model_1 0.7846381 0.5589271            0.2735714 0.4334643
# 8                         GBM_2_AutoML_20190428_120526 0.7823810 0.5047958            0.2819048 0.4077451
# 9   DeepLearning_grid_1_AutoML_20190428_120526_model_5 0.7817119 0.5415844            0.2864286 0.4111001
# 10                        GBM_4_AutoML_20190428_120526 0.7807738 0.5096925            0.2883333 0.4101473


train<- as.h2o(germanbis)

  
aml <- h2o.automl(x = 2:45,y=1,training_frame = train,max_models = 20,seed = 1)

lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)
#                                              model_id       auc   logloss mean_per_class_error      rmse 
# 1  StackedEnsemble_BestOfFamily_AutoML_20190428_122609 0.7895714 0.4974196            0.2785714 0.4045638
# 2     StackedEnsemble_AllModels_AutoML_20190428_122609 0.7875095 0.4977114            0.2800000 0.4042563
# 3            GLM_grid_1_AutoML_20190428_122609_model_1 0.7869714 0.4956200            0.2695238 0.4054846
# 4                         GBM_3_AutoML_20190428_122609 0.7829190 0.5002143            0.2878571 0.4058694
# 5                         GBM_2_AutoML_20190428_122609 0.7807667 0.5031380            0.2735714 0.4081890
# 6            GBM_grid_1_AutoML_20190428_122609_model_1 0.7756690 0.5592309            0.2885714 0.4336928
# 7                         GBM_4_AutoML_20190428_122609 0.7744905 0.5104462            0.2852381 0.4100034
# 8   DeepLearning_grid_1_AutoML_20190428_122609_model_5 0.7684333 0.6270297            0.2819048 0.4366436
# 9                         GBM_1_AutoML_20190428_122609 0.7673976 0.5198255            0.2916667 0.4161494
# 10           GBM_grid_1_AutoML_20190428_122609_model_5 0.7625619 0.5165648            0.2935714 0.4151754
# 11           GBM_grid_1_AutoML_20190428_122609_model_2 0.7609500 0.5735209            0.2921429 0.4406875
 

# CLARAMENTE ES MEJOR HACER SELECCIÓN DE VARIABLES ANTES



load("pima.Rda")

pima2<-pima[,c("diabetes","embarazos", "glucosa",
 "presion", "triceps", "insulina", "bodymass","diabfun", "edad")]


# Para clasificación hay que definir como factor la dependiente
pima2$diabetes<-as.factor(pima2$diabetes)

data<-pima2

full<-glm(diabetes~.,data=data,family = binomial(link="logit"))
null<-glm(diabetes~1,data=data,family = binomial(link="logit"))

selec1<-stepAIC(null,scope=list(upper=full),direction="both",trace=FALSE,family = binomial(link="logit"))

summary(selec1)

formula(selec1)


# CON TODAS

train<- as.h2o(pima2)

aml <- h2o.automl(x = 2:8,y=1,training_frame = train,max_models = 20,seed = 1)

lb <- aml@leaderboard

print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)

#                                              model_id       auc   logloss mean_per_class_error      rmse
# 1            GLM_grid_1_AutoML_20190429_125934_model_1 0.8263545 0.4872810            0.2430149 0.3972203
# 2   DeepLearning_grid_1_AutoML_20190429_125934_model_2 0.8251679 0.5065114            0.2513284 0.4060089
# 3  StackedEnsemble_BestOfFamily_AutoML_20190429_125934 0.8188619 0.4940984            0.2484776 0.4021016
# 4                         GBM_5_AutoML_20190429_125934 0.8181493 0.4938570            0.2530000 0.4040825
# 5     StackedEnsemble_AllModels_AutoML_20190429_125934 0.8169552 0.4980867            0.2518060 0.4049691
# 6   DeepLearning_grid_1_AutoML_20190429_125934_model_5 0.8122276 0.5299681            0.2504030 0.4089886
# 7            GBM_grid_1_AutoML_20190429_125934_model_2 0.8090299 0.5708384            0.2458657 0.4390193
# 8            GBM_grid_1_AutoML_20190429_125934_model_4 0.8078433 0.5779683            0.2634627 0.4424319
# 9   DeepLearning_grid_1_AutoML_20190429_125934_model_4 0.8073582 0.5227344            0.2611493 0.4143269
# 10                        GBM_3_AutoML_20190429_125934 0.8049664 0.5132999            0.2573284 0.4132279

pima2<-pima[,c("diabetes","embarazos", "glucosa",
 "presion", "insulina", "bodymass","diabfun", "edad")]

pima2$diabetes<-as.factor(pima2$diabetes)
# CON LAS SELECCIONADAS
train<- as.h2o(pima2)

aml <- h2o.automl(x = 2:8,y=1,training_frame = train,max_models = 20,seed = 1)

lb <- aml@leaderboard

print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)

#                                             model_id       auc   logloss mean_per_class_error      rmse
# 1  StackedEnsemble_BestOfFamily_AutoML_20190429_132522 0.8339701 0.4807002            0.2280746 0.3962624
# 2   DeepLearning_grid_1_AutoML_20190429_132522_model_2 0.8338358 0.4888846            0.2324179 0.3991418
# 3                         GBM_2_AutoML_20190429_132522 0.8305410 0.4851179            0.2372090 0.3985626
# 4     StackedEnsemble_AllModels_AutoML_20190429_132522 0.8284664 0.4865507            0.2399552 0.3994785
# 5            GLM_grid_1_AutoML_20190429_132522_model_1 0.8277724 0.4855018            0.2450746 0.3971999
# 6            GBM_grid_1_AutoML_20190429_132522_model_2 0.8254627 0.5580418            0.2372687 0.4326181
# 7                         GBM_1_AutoML_20190429_132522 0.8248881 0.4972250            0.2389403 0.4039403
# 8                         GBM_5_AutoML_20190429_132522 0.8240112 0.4872497            0.2496269 0.4014302
# 9                         GBM_4_AutoML_20190429_132522 0.8231418 0.4931037            0.2379403 0.4029054
# 10                        GBM_3_AutoML_20190429_132522 0.8213022 0.4952182            0.2448806 0.4041833
# 11           GBM_grid_1_AutoML_20190429_132522_model_1 0.8178209 0.5723678            0.2488209 0.4400585
# 12  DeepLearning_grid_1_AutoML_20190429_132522_model_5 0.8162276 0.5143044            0.2525224 0.4078199
  
