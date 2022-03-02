#-------------------
# Autor: Miguel Angel Castaño
# Fecha: 
# Inputs: Datos entrada bombas
# Salida: 
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
  library(tictoc)         # Calcular tiempos
  library(missRanger)     # Fast imputatin of NAs.
})


#-- Leo ficheros
dattrainOr    <- fread(file = "./data/train.csv", data.table = FALSE )
dattrainLabOr <- fread(file = "./data/train_labels.csv", data.table = FALSE )
dattestOr     <- fread(file = "./data/test.csv", data.table = FALSE  )


### EDA (Exploratory Data Analysis) - hecho con inspectdf

# Horizontal bar plot for categorical column composition
x <- inspect_cat(dattrainOr)
show_plot(x)

# Correlation betwee numeric columns + confidence intervals
x <- inspect_cor(dattrainOr)
show_plot(x)

# Bar plot of most frequent category for each categorical column
x <- inspect_imb(dattrainOr)
show_plot(x)

# Bar plot showing memory usage for each column
x <- inspect_mem(dattrainOr)
show_plot(x)

# Occurence of NAs in each column ranked in descending order
x <- inspect_na(dattrainOr)
show_plot(x)

# Histograms for numeric columns
x <- inspect_num(dattrainOr)
show_plot(x)

# Barplot of column types
x <- inspect_types(dattrainOr)
show_plot(x)

# EDA - Results
# Mirar 0s construction_year
# gps_height 0 y negativos
# longitude valores de 0 ???
# population -> outliers? (300.000)
# 27 categóricas - 10 numéricas
# recorded_by es una sola categoria -> cte. (a quitar)

#------------------------------------------------------------------

### CAMBIOS EDA RESUlT

## Unión de conjuntos de datos
data <- merge(dattrainOr, dattestOr, all = T, sort = F)

#------------------------------------------------------------------

## Niveles de las categoricas.
datcat_df <- data %>% select(where(is.character))

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

#--- Me quedo con las castegorias que tienen valores de > 1 y < 3000
vars_gd <- numlev_df %>%
  filter(levels < 3000, levels > 1) %>% 
  select(vars)
datcat_gd <- datcat_df[ , vars_gd$vars]

## juntamos las categoricas con las numericas
datnum_df <- data %>% select(where(is.numeric))

datnumcat_df <- cbind(datnum_df, datcat_gd)

#------------------------------------------------------------------

## Vaariables sospechosas iguales

#--- longitud del dataframe,
# columns length
length(datnumcat_df[,1])
# rows length
length(datnumcat_df[1,])

#--- quantity y quantity_group  - Iguales
unique(datnumcat_df$quantity)
unique(datnumcat_df$quantity_group)
all.equal(datnumcat_df$quantity, datnumcat_df$quantity_group)

#--- payment y payment_type  - Iguales
unique(datnumcat_df$payment)
unique(datnumcat_df$payment_type)
all.equal(datnumcat_df$payment, datnumcat_df$payment_type)
head(datnumcat_df[, c('payment', 'payment_type')])

#elimino payment_type y quantity_group
datnumcat_df$payment_type <- NULL
datnumcat_df$quantity_group <- NULL

#añadimos el campo de la fecha
datnumcat_df$date_recorded <- data$date_recorded

#--------------------------------------------

### FEATURE ENGINEERING


#--------------------------------------------

### MODELO

## Separamos datos train y test

# dattrainOr    <- "./data/train.csv"
# dattrainLabOr <- "./data/train_labels.csv"
# dattestOr     <- "./data/test.csv"

# length train
length(dattrainOr[,1])
# length test
length(dattestOr[,1])
# length both joined
length(datnumcat_df[,1])

# Separamos los conjuntos train y test
dattrainOr_imp <- datnumcat_df[1:59400,]
dattestOr_imp <- datnumcat_df[59401:74250,]

#--------------------------------------------

## Variable objetivo

# merge del id
dattrainOrLab_obj_imp <- merge(
  dattrainOr_imp, dattrainLabOr,
  by.x = c('id'), by.y = c('id'),
  sort = FALSE
)

## status_group como factor
dattrainOrLab_obj_imp$status_group <- as.factor(dattrainOrLab_obj_imp$status_group)

## Genero el model
tic()
my_model <- ranger( 
  status_group ~ . , 
  importance = 'impurity',
  num.trees = 620,
  mtry = 5,
  data = dattrainOrLab_obj_imp,
  seed = 67583
)
# **Estimacion** del error / acierto **esperado**
acierto <- 1 - my_model$prediction.error
acierto
toc()

### Pintar importancia de variables

## Obtengo la importancia de las variagbles
impor_df <- as.data.frame(my_model$variable.importance)
names(impor_df)[1] <- c('Importance')
impor_df$vars <- rownames(impor_df)
# borro los ids(variables ya copiadas en columna)
rownames(impor_df) <- NULL

## Pinto la importancia
ggplot(impor_df, aes(fct_reorder(vars, Importance), Importance)) +
  geom_col(group = 1, fill = "darkred") +
  coord_flip() + 
  labs(x = 'Variables', y = 'Importancia', title = 'Importancia Variables') +
  theme_bw()
ggsave('./charts/imp_var_entregable.png')


### Prediccion

## Obtengo la prediccion
my_pred <- predict(my_model, dattestOr)

## File for submission
my_sub <- data.table(
  id = dattestOr$id,
  status_group = my_pred$predictions
)

## Guardo submission
fwrite(my_sub, file = "./submissions/sub_entregable.csv" )

##################################################################

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
# 11 - 0.3390814 - todas menos 2 - h2o - rf - ntrees 50  - 0.8112
# 12 - 0.3878334/0.3410365 (50/500)- todas menos 2 - h2o - gbm  - 0.8082
# 11 - 0.3390814 - todas menos 2 - h2o - rf - ntrees 500 - 0.8112

## my_models

# 1 - 0.8175421 - <3000 cat, - 0.8165

#------------------------------------------------------------------
#------------------------------------------------------------------
  
#---- FEATURE ENGINEERING -------
# construction_year = Edad + Decadas
# long /  lat -> Distancia

#-- Exploramos construction_year
# as.data.frame(table(dattrainOrlab$construction_year))
#-- Imputamos 0s con missRanger (0 == NAs)
dattrainOrlab$fe_cyear <- dattrainOrlab$construction_year
dattrainOrlab$fe_cyear <- ifelse( 
  dattrainOrlab$fe_cyear == 0, 
  NA, 
  dattrainOrlab$fe_cyear
)
# Train quito construction_year y status group 
dattrainOrlab$construction_year <- NULL
dattrainOrlab$status_group <- NULL
dattrainOrlab_imp <- missRanger(
  dattrainOrlab,  
  pmm.k = 3,
  num.trees = 100
)
#-- Test
dattestOr$fe_cyear <- dattestOr$construction_year
dattestOr$fe_cyear <- ifelse( 
  dattestOr$fe_cyear == 0, 
  NA, 
  dattestOr$fe_cyear
)
dattrainOrlab$construction_year <- NULL
dattrainOrlab_imp <- missRanger(
  dattrainOrlab,  
  pmm.k = 3,
  num.trees = 100
)
# ERROR - ERROR--- MUY MAL CARLOS!!!!.MALA PRÁCTICA
# JUNTAR TRAIN Y TEST - E IMPUTAR DE UNA VEZ - QUEDANDO CON NUM/CAT <1000.


#- Edad - Referencia año = 2014
dattrainOrlab$fe_age <- 2014 - dattrainOrlab$construction_year
dattestOr$fe_age     <- 2014 - dattestOr$construction_year

#- Decadas.
# Dudas sobre años recientes - riesgo productivizacion.


#-- Distancia -----
dattrainOrlab$fe_dist <- sqrt( dattrainOrlab$longitude^2 + dattrainOrlab$latitude^2)
dattestOr$fe_dist     <- sqrt( dattestOr$longitude^2 + dattestOr$latitude^2)




###########################################################3



 