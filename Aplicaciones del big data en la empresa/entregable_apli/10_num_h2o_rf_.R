#-------------------
# Autor: Carlos Ortega
# Fecha: 2021_03_05
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
dattestOr     <- fread(file = "./data/test.csv", data.table = FALSE  )


# # EDA--- (Exploratory Data Analysis) - hecho con inspectdf
# # Horizontal bar plot for categorical column composition
# x <- inspect_cat(dattrainOr) 
# show_plot(x)
# 
# # Correlation betwee numeric columns + confidence intervals
# x <- inspect_cor(dattrainOr)
# show_plot(x)
# 
# # Bar plot of most frequent category for each categorical column
# x <- inspect_imb(dattrainOr)
# show_plot(x)
# 
# # Bar plot showing memory usage for each column
# x <- inspect_mem(dattrainOr)
# show_plot(x)
# 
# # Occurence of NAs in each column ranked in descending order
# x <- inspect_na(dattrainOr)
# show_plot(x)
# 
# # Histograms for numeric columns
# x <- inspect_num(dattrainOr)
# show_plot(x)
# 
# # Barplot of column types
# x <- inspect_types(dattrainOr)
# show_plot(x)

# EDA - Results
# Mirar 0s construction_year
# gps_height 0 y negativos
# longitude valores de 0 ???
# population -> outliers? (300.000)
# 27 categóricas - 10 numéricas
# recorded_by es una sola categoria -> cte. (a quitar)

#---- Corregir estas deficiencias.
# Vamos a hacer un modelo con las variables numericas
# Vamos a la playa!
datnum_df <- dattrainOr %>% select(is.numeric)

# Variable objetivo?
# Ante la duda del mismo orden de etiquetas - hago merge
dattrainOrlab <- merge(
  datnum_df, dattrainLabOr,
  by.x = c('id'), by.y = c('id'),
  sort = FALSE
)

#- Del test solo las numericas para luego pasárselo a h2o.
datTest_num <- dattestOr %>% select(where(is.numeric))



#--- Modelo randomForest - H2O - Solo numéricas.
library(h2o)
h2o.init()
y <- "status_group"
x <- setdiff( names(dattrainOrlab), y)

dattrainOrlab_hex <- as.h2o(dattrainOrlab)
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
                              y = y,  
                              x = x,
                              training_frame = dattrain_train,
                              nfolds = 3
                             )
# Estimación del error con el conjunto de test...
h2o.performance(model = model_rf, newdata = dattrain_test)

#-- Importancia Variables
var_imp <- h2o.varimp(model_rf)
var_imp
#-- Grafico con importancia de variables
h2o.varimp_plot(model_rf)


#---- Submission----
#-- Primero convertimos el conjunto de test para el submission en un h2o.
test_h2o <- as.h2o(datTest_num)
#-- Hago predicción
my_pred <- as.data.table(h2o.predict(model_rf, test_h2o))

#---
#------ Submission
my_sub <- data.table(
  id = dattestOr$id,
  status_group = my_pred$predict
)
# guardo submission
fwrite(my_sub, file = "./submissions/10_num_h2orf.csv" )

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
# 10 -           - num - 0.7110 (vs los 0.7145 de ranger)




