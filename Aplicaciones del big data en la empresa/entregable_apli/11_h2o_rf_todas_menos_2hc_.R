#-------------------
# Autor: Carlos Ortega
# Fecha: 2021_03_12
# Inputs: Datos entrada bombas
# Salida: Modelo inicial
# Comentarios: 
#-------------------

#--- Cargo librerías
suppressPackageStartupMessages({
  library(dplyr)          # Manipulacion de datos 
  library(data.table)     # Leer y procesar ultra-rapido
  library(ggplot2)        # La librería grafica
  library(inspectdf)      # EDAs automaticos
  library(ranger)         # Fast randomForest
  library(forcats)        # Tratar variables categoricas
})


#-- Leo ficheros
dattrainOr    <- fread(file = "./data/train.csv", data.table = FALSE )
dattrainLabOr <- fread(file = "./data/train_labels.csv", data.table = FALSE )
#dattestOr     <- fread(file = "./data/test.csv", data.table = FALSE  )
dattestOr     <- fread(file = "./data/test.csv")


# # Variable objetivo?
# # Ante la duda del mismo orden de etiquetas - hago merge
# dattrainOrlab <- merge(
#   datnum_df, dattrainLabOr,
#   by.x = c('id'), by.y = c('id'),
#   sort = FALSE
# )

#--- Niveles de las categoricas.
datcat_df <- dattrainOr %>% select(where(is.character))

numlev_df <- data.frame()
for (i in 1:ncol(datcat_df)) {
  col_tmp <- datcat_df[, i]
  num_lev <- length(unique(col_tmp))
  numlev_df[i, 1] <- names(datcat_df)[i]
  numlev_df[i, 2] <- num_lev
  print(numlev_df)
}
names(numlev_df) <- c('vars', 'levels')
numlev_df %>% arrange(levels)

#--- Me quedo con las castegorias que tienen valores de > 1 y < 100
vars_gd <- numlev_df %>%
  filter(levels < 10000, levels > 1) %>% 
  select(vars)
datcat_gd <- datcat_df[ , vars_gd$vars]

# Tengo que convertir cada columna en factor (por h2o)
for (i in 1:ncol(datcat_gd)) {
  col_tmp <- datcat_gd[ , i]
  datcat_gd[, i] <- as.factor(col_tmp)
}



#---- Corregir estas deficiencias.
#--- Solo las numericas
datnum_df <- dattrainOr %>% select(where(is.numeric))

train_end <- cbind(datcat_gd, datnum_df)
train_end$status_group <- as.factor(dattrainLabOr$status_group)

#----------  H2O ------------
#--- Modelo randomForest - H2O - Todas menos las dos de high-cardinality
library(h2o)
h2o.init()
y <- "status_group"
x <- setdiff( names(train_end), y)

dattrainOrlab_hex <- as.h2o(train_end)
dattrainOrlab_hex[,y] <- as.factor(dattrainOrlab_hex[,y])

# Split dataset giving the training dataset 75% of the data
dattrain_split <- h2o.splitFrame(data = dattrainOrlab_hex, ratios = 0.75)
print(dim(dattrain_split[[1]]))
print(dim(dattrain_split[[2]]))

# Create a training set from the 1st dataset in the split
dattrain_train <- dattrain_split[[1]]

# Create a testing set from the 2nd dataset in the split
dattrain_test <- dattrain_split[[2]]

#---- Modelo - 
model_rf <- h2o.randomForest(
  y              = y,  
  x              = x,
  training_frame = dattrain_train,
  ntrees         = 500,
  nfolds         = 3
)
# Estimación del error con el conjunto de test...
h2o.performance(model = model_rf, newdata = dattrain_test)

#-- Importancia Variables
var_imp <- h2o.varimp(model_rf)
var_imp
#-- Grafico con importancia de variables
h2o.varimp_plot(model_rf, 20)


#---- Submission----
#-- Primero convertimos el conjunto de test para el submission en un h2o.
#-- Quito del test (submission) wpt_name, subvillage
# Las variables a coger del train están en la variable "x" de h2o...
test_sub <- dattestOr[ , (x), with = FALSE]
test_h2o <- as.h2o(test_sub)
#-- Hago predicción
my_pred <- as.data.table(h2o.predict(model_rf, test_h2o))

#---
#------ Submission
my_sub <- data.table(
  id = dattestOr$id,
  status_group = my_pred$predict
)
# guardo submission
fwrite(my_sub, file = "./submissions/11_todas_menos2_h2orf_v2.csv" )

h2o.shutdown(prompt = FALSE)




#------ Resultados
# 00 - 0.7127946 - solo num - 0.7145
# 01 - 0.8104714 - num + cat (>1 & < 100) - 0.8153 
# 02 - 0.8112458 - num + cat (>1 & < 100) no dup - 0.8154 
# 03 - 0.8168687 - num + cat (>1 & < 1000) no dup -  0.8128 
# 04 - 0.8122559 - num + cat (>1 & lga) no dup -  0.8156 
# 04 - 0.8138047 - num + cat (>1 & funder) no dup - 0.8156 
# 05 - 0.8132323 - num + cat (>1 & funder) no dup- grid 700-6 - 0.8161
# 06 - 0.8122559 - num + cat (>1 & <1000) no dup- fe cyear/dist - 0.8174 
# 08 - 0.8145118 - num + cat (>1 & <1000) no dup- fe cyear/dist - funder freq - 0.8149
# 10 - 0.444     - num - 0.7110 (vs los 0.7145 de ranger)
# 11 - 0.3390814 - todas menos 2 - 0.8112

# #-------- Lista:  Viernes 12-Marzo.
# # Clase: 
# - Victor
# - Irene Salgado
# - Elena Balles
# - Tatiana
# - Sergio Carrillo
# - Igor
# - Mikel
# - Jose Doyague
# - Alberto
# - Gabriel Arenas
# - Benjamin
# 
# # Webex
# Guillermo Jesús Escribano Fernández (ONLINE)
# Sara de la Rubia Monroy (Online)
# Marcos Bowe Luque (Online)
# Miguel Ángel Buceta Cuéllar (Online)
# Gustavo Aguilera (online)
# David Arias
# Ciro Navarro Aceto
# Javier Amor Esteban
# 
# 
