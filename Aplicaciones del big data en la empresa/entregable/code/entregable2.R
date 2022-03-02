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
# Mirar 0s construction_year ------------------- 1
# gps_height 0 y negativos,  data$gps_heigh
# longitude valores de 0 ??? ------------------------1
# population -> outliers? (300.000) ----------- 1 no considerado outliers
# 27 categóricas - 10 numéricas - 2 booleanas ---------------------1
# recorded_by es una sola categoria -> cte. (a quitar) ------------------- 1

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

## Variables sospechosas iguales

#--- longitud del dataframe,
# columns length
length(datnumcat_df[,1])
# rows length
length(datnumcat_df[1,])

#--- quantity y quantity_group  - Iguales
unique(datnumcat_df$quantity)
unique(datnumcat_df$quantity_group)
all.equal(datnumcat_df$quantity, datnumcat_df$quantity_group)

# #--- payment y payment_type  - Iguales # modelo 7
# unique(datnumcat_df$payment)
# unique(datnumcat_df$payment_type)
# all.equal(datnumcat_df$payment, datnumcat_df$payment_type)
# head(datnumcat_df[, c('payment', 'payment_type')])



#elimino payment_type y quantity_group
datnumcat_df$quantity_group <- NULL
# datnumcat_df$payment_type <- NULL


# No los estraigo modelo 6

#--- management  - Iguales
# unique(datnumcat_df$management)
# unique(datnumcat_df$management_group)

# #--- extraction_type  - Iguales
# unique(datnumcat_df$extraction_type)
# unique(datnumcat_df$extraction_type_group)
# unique(datnumcat_df$extraction_type_class)
# all.equal(datnumcat_df$extraction_type, datnumcat_df$extraction_type_group)
# 
# datnumcat_df$management_group <- NULL
# 
# datnumcat_df$extraction_type <- NULL
# datnumcat_df$extraction_type_group <- NULL



# Añadimos campos no numericos ni categoricos

#añadimos el campo de la fecha
datnumcat_df$date_recorded <- data$date_recorded
#añadimos los campos booleanos
datnumcat_df$permit <- data$permit
datnumcat_df$public_meeting <- data$public_meeting





#---------------------------------

# disa desde el recorded --- MEJORA
datnumcat_df$fe_date_recorded <- as.numeric(as.Date('2013-12-03') - as.Date(data$date_recorded))

#-----------------------------

# AGE
# para valores atipicos muy grandes
datnumcat_df$fe_age <- 2014 - datnumcat_df$construction_year


# #-- Imputamos 0s con missRanger (0 == NAs)
# construction_year
datnumcat_df$fe_construction_year <- datnumcat_df$construction_year
datnumcat_df$fe_construction_year <- ifelse(
  datnumcat_df$fe_construction_year == 0,
  NA,
  datnumcat_df$fe_construction_year
)
# longitude
datnumcat_df$fe_longitude <- datnumcat_df$longitude
datnumcat_df$fe_longitude <- ifelse(
  datnumcat_df$fe_longitude < 10,
  NA,
  datnumcat_df$fe_longitude
)
# gps_he
datnumcat_df$fe_gps_height <- datnumcat_df$gps_height
datnumcat_df$fe_gps_height <- ifelse(
  datnumcat_df$fe_gps_height < 1,
  NA,
  datnumcat_df$fe_longitude
)


# # quito las variables imputadas
datnumcat_df$longitude <- NULL
datnumcat_df$construction_year <- NULL
datnumcat_df$gps_height <- NULL
# datnumcat_df$status_group <- NULL
datnumcat_df <- missRanger(
  datnumcat_df,
  pmm.k = 3,
  num.trees = 100
)

#-- Distancia con las longitudes imputadas -----
# datnumcat_df$fe_dist <- sqrt( datnumcat_df$fe_longitude^2 + datnumcat_df$latitude^2)
datnumcat_df$fe_dist <- sqrt( datnumcat_df$fe_longitude^2 + datnumcat_df$latitude^2)

datnumcat_df_aux <- datnumcat_df
# datnumcat_df <- datnumcat_df_aux

#------ FEATURE
# as.data.frame(table(datnumcat_df$fequantity))
# #added
# datnumcat_dt[ ,  fe_wpt_name := .N , by = wpt_name ]

# # guardamos len original
# len_datnumcat_col_df = ncol(datnumcat_df)
#   
# # numlev_df %>% arrange(levels)
# 
# for (i in 1:nrow(numlev_df)) {
#   # obtenemos las categoricas
#   nrow(numlev_df)
#   row_tmp <- numlev_df[i,] #i
#   # obtenemos el nombre de la categoria
#   name_var <- row_tmp[,1] #constante
#   #creamos la columna frecuencia
#   if(name_var != "wpt_name" && name_var != "subvillage" && name_var != "recorded_by" && name_var != "payment_type" && name_var != "quantity_group"){
#     #print(name_var) #debug
#     datnumcat_df[,paste("fe_",name_var)] <- datnumcat_df[,name_var]
#   }
# }
# ncol(datnumcat_df)
# ncol(datnumcat_dt)

# for (i in (len_datnumcat_col_df+1):ncol(datnumcat_dt)) {
#   print(i)
#   #print(datnumcat_dt[1,i])
#   datnumcat_dt[ , i := .N , by = funder ]
#   
# }


#-- funder y quantity - Sustituimos sus categoricas por la frecuencia

numlev_df %>% arrange(levels)

# to datatable
datnumcat_dt <- as.data.table(datnumcat_df)

datnumcat_dt[ , fe_funder := .N , by = funder ]
datnumcat_dt[ ,  fe_quantity := .N , by = quantity ]
# add
# datnumcat_dt[ ,  fe_recorded_by           := .N , by = recorded_by]
# datnumcat_dt[ ,  fe_source_class          := .N , by = source_class]
# datnumcat_dt[ ,  fe_management_group      := .N , by =  management_group]
datnumcat_dt[ ,  fe_quantity              := .N , by =  quantity]
# datnumcat_dt[ ,  fe_quantity_group        := .N , by =  quantity_group]
# datnumcat_dt[ ,  fe_quality_group         := .N , by =  quality_group]
# datnumcat_dt[ ,  fe_waterpoint_type_group := .N , by =  waterpoint_type_group]
datnumcat_dt[ ,  fe_extraction_type_class := .N , by =  extraction_type_class]  #esta no por que es mayor
datnumcat_dt[ ,  fe_payment               := .N , by =  payment]
# datnumcat_dt[ ,  fe_payment_type          := .N , by =  payment_type]
# datnumcat_dt[ ,  fe_source_type           := .N , by =  source_type]
# datnumcat_dt[ ,  fe_waterpoint_type       := .N , by =  waterpoint_type]
# datnumcat_dt[ ,  fe_water_quality         := .N , by =  water_quality ]
# datnumcat_dt[ ,  fe_basin                 := .N , by =  basin]
datnumcat_dt[ ,  fe_source                := .N , by =  source]
# datnumcat_dt[ ,  fe_management            := .N , by =  management]
# datnumcat_dt[ ,  fe_scheme_management     := .N , by =  scheme_management]
datnumcat_dt[ ,  fe_extraction_type_group := .N , by =  extraction_type_group]
datnumcat_dt[ ,  fe_extraction_type       := .N , by =  extraction_type]
datnumcat_dt[ ,  fe_region                := .N , by =  region]
datnumcat_dt[ ,  fe_lga                   := .N , by =  lga]
datnumcat_dt[ ,  fe_ward                  := .N , by =  ward]
# datnumcat_dt[ ,  fe_funder                := .N , by =  funder]
# datnumcat_dt[ ,  fe_installer             := .N , by =  installer]
datnumcat_dt[ ,  fe_scheme_name           := .N , by =  scheme_name] #esta no por que es mayor
# datnumcat_dt[ ,  fe_subvillage            := .N , by =  subvillage]
# datnumcat_dt[ ,  fe_wpt_name              := .N , by =  wpt_name]

# to dataframe
datnumcat_df <- as.data.frame(datnumcat_dt)

# No quito las variables originales, debido a la su importancia

#--------------------------------------------

# Considero las variables que menos importancia muestran en otros modelos

# numlev_df %>% arrange(levels) # model 7
# 
# datnumcat_df$num_private <- NULL
# datnumcat_df$public_meeting <- NULL
# datnumcat_df$source_type <- NULL



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


#-- Grid Search
my_ntree <- c(450,500,550,600,650,700,750)
my_mtry  <- c(4,5,6,7)
my_pars  <- expand.grid(my_ntree, my_mtry)
names(my_pars) <- c('myntree', 'mymtry')
my_pars$acierto <- 0

for (i in 1:nrow(my_pars)) {
  tic()
  my_model <- ranger(
    status_group ~ . ,
    importance = 'impurity',
    num.trees = my_pars$myntree[i],
    mtry = my_pars$mymtry[i],
    data = dattrainOrLab_obj_imp,
    seed = 67583
  )

  # **Estimacion** del error / acierto **esperado**
  acierto <- 1 - my_model$prediction.error
  acierto
  my_pars$acierto[i] <- acierto
  print(my_pars)
  toc()
}

# Corro modelo con el mejor grid




## Genero el model
tic()
my_model <- ranger( 
  status_group ~ . , 
  importance = 'impurity',
  num.trees = 700,
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
my_pred <- predict(my_model, dattestOr_imp)

## File for submission
my_sub <- data.table(
  id = dattestOr_imp$id,
  status_group = my_pred$predictions
)

## Guardo submission
fwrite(my_sub, file = "./submissions/sub_entregable_8.csv" )

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

# 1 - 0.8175421 - 0.8165 - <3000 cat,
# 2 - 0.8191582 - 0.8219 - imputar los restantes menos gps_height
# 3 - 0.8188384 - 0.8246 - imputar tambien gps_height
# 4 - 0.8189394 - 0.8244 - freq de todas la categoricas
# 5 - 0.8186027 - 0.8249 - quito las categoricas no isada
# 6 - 0.8173232 - 0.8242 - quito otras iguales
# 7 - 0.8179630 - 0.8257 - back to 5 y añado una quitada en clase (casi repetida) # MODELO GANADOR
# 8 - 0.8182997 - 0.8256 -  for para ver cual num_trees y mtry es mejor 


#------------------------------------------------------------------
#------------------------------------------------------------------



###########################################################3



