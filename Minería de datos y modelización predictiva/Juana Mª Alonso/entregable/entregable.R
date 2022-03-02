install.packages("readxl")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("knitr")
install.packages("lattice")
install.packages("pastecs")
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("cluster") #Cluster
install.packages("heatmaply")
install.packages("NbClust")
install.packages("readxl")
install.packages("kableExtra")

# files
library(readxl)
library(knitr)
# stadistica
library(pastecs)
library(lattice)
library(ggplot2)
library(corrplot)
#analisis componentes principades
library(factoextra)
library(FactoMineR)
# cluster
library(cluster) #Cluster
library(heatmaply)
library(NbClust)
# utills
library(readxl)
library(kableExtra)

getwd()
setwd("/Users/macasib/Desktop/entregable_temp")

# cargar datos
provincias <- read_excel("Provincias.xlsx")
datos<- as.data.frame(provincias)
rownames(datos)<-datos[,1] 
datos<-datos[,-1]

# summary
tabla_summary<-summary(datos)
knitr::kable(tabla_summary, caption = "Tabla resumen de las variables")

# descriptivas 
est <- stat.desc(datos, basic=FALSE)
knitr::kable(est, digits=2, caption="Estadisticos Descriptivas")

## EJERCICIO 1
# Calcular la matriz de correlaciones, y su representación gráfica 
# ¿Cuáles son las variables más correlacionadas de forma inversa?
cor_matrix<-cor(datos, method="pearson")
knitr::kable(cor_matrix, digits =2,caption = "Correlaciones")
# grafico
corrplot(cor_matrix, type="upper", order="hclust",tl.col="black", tl.srt=90)


## EJERCICIO 2
# Realizar un análisis de componentes principales sobre la matriz de 
# correlaciones, calculando 7 componentes. Estudiar los valores de 
# los autovalores obtenidos y las gráficas que los resumen. ¿Cuál es 
# el número adecuado de componentes?
fit <- PCA(datos,scale.unit=TRUE,ncp=7,graph=TRUE)
#table autovalores
eig <- get_eigenvalue(fit)
knitr::kable(eig, digits =2,caption = "Autovalores")
#graphs
fviz_eig(fit, geom="line")+ theme_grey() #4
fviz_eig(fit,addlabels=TRUE)


## EJERCICIO 3
# Hacer de nuevo el análisis sobre la matriz de correlaciones pero 
# ahora indicando el número de componentes principales que hemos 
# decidido retener (Que expliquen aproximadamente el 90%). Sobre 
# este análisis contestar los siguientes apartados.
fit <- PCA(datos,scale.unit=TRUE,ncp=4,graph=TRUE)


### A
# Mostrar los coeficientes para obtener las componentes principales 
# ¿Cuál es la expresión para calcular la primera Componente en 
# función de las variables originales?
knitr::kable(fit$svd$V, digits =3,caption = "Autovectores")


### B 
# Mostar una tabla con las correlaciones de las Variables con 
# las Componentes Principales. Para cada Componente indicar las 
# variables con las que está más correlacionada
#estadísticos asociados -> var
var<-get_pca_var(fit)
# coloreamos las mas correlacionadas
# no funciona he intentado usar el paquete kableExtra
# var_cor_color <- var$cor
# var_cor_color[1,1] <- cell_spec(var_cor_color[1,1], color = "red", bold = T)
# table
knitr::kable(var$cor, digits=3, caption="Correlaciones de la CP con las variables")

### C
# Comentar los gráficos que representan las variables en los 
# planos formados por las componentes, intentando explicar 
# lo que representa cada componente
#Representación gráfica variables
#1 y 2
fviz_pca_var(fit, axes = c(1,2), col.var="cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )
#1 y 3
fviz_pca_var(fit, axes = c(1,3), col.var="cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )
#1 y 4
fviz_pca_var(fit, axes = c(1,4), col.var="cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )
#2 y 3
fviz_pca_var(fit, axes = c(2,3), col.var="cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )
#2 y 4
fviz_pca_var(fit, axes = c(2,4), col.var="cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )
#3 y 4
fviz_pca_var(fit, axes = c(3,4), col.var="cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )


### D 
# Mostrar la tabla y los gráficos que nos muestran la proporción 
# de la varianza de cada variable que es explicado por cada componente.
# ¿Cuál de las variables es la que está peor explicada?
knitr::kable(var$cos2, digits =3,caption = "Cosenos al cuadrado")
#graph cosenos
corrplot(var$cos2,is.corr=FALSE)
# porcentaje  de variabilidad explicada por las 4 CP
fviz_cos2(fit,choice="var",axes=1:4)


### E
# Mostrar la tabla y los gráficos que nos muestran el porcentaje 
# de la varianza de cada Componente que es debido a cada variable. 
# ¿Que variables contribuyen más a cada Componente?
knitr::kable(var$contrib, digits =2,caption = "Contribuciones")
corrplot(var$contrib,is.corr=FALSE)
# fviz_contrib(fit,choice="var",axes=1:4)
# componente 1
fviz_contrib(fit,choice="var",axes=1)
# componente 2
fviz_contrib(fit,choice="var",axes=2)
# componente 3
fviz_contrib(fit,choice="var",axes=3)
# componente 4
fviz_contrib(fit,choice="var",axes=4)


### F 
# Sobre los gráficos que representan las observaciones en los nuevos 
# ejes y el gráfico Biplot., teniendo en cuenta la posición de 
# las provincias en el gráfico Comentar las provincias que tienen 
# una posición más destacada en cada componente, en positivo o negativo, 
# ¿Qué significa esto en términos socioeconómicos para estas provincias?
# 1 y 2
# fviz_pca_ind(fit, axes = c(1, 2), col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_ind(fit, axes = c(1, 2), gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
#Representación conjunta de los individuos y las variables en los plan os de las CP
fviz_pca_biplot(fit,  axes = c(1, 2), repel=TRUE, col.var="#2E9FDF", col.ind="#696969") # Variables y Individuals 
# 1 y 3
fviz_pca_ind(fit, axes = c(1, 3), gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
#Representación conjunta de los individuos y las variables en los plan os de las CP
fviz_pca_biplot(fit,  axes = c(1, 3), repel=TRUE, col.var="#2E9FDF", col.ind="#696969") # Variables y Individuals 
# 1 y 4
fviz_pca_ind(fit, axes = c(1, 4), gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
#Representación conjunta de los individuos y las variables en los plan os de las CP
fviz_pca_biplot(fit,  axes = c(1, 4), repel=TRUE, col.var="#2E9FDF", col.ind="#696969") # Variables y Individuals 


### G
# Si tuviéramos que construir un índice que valore de forma conjunta el 
# desarrollo económico de una provincia, como se podría construir utilizando
# una combinación lineal de todas las variables. ¿Cuál sería el valor de 
# dicho índice en Madrid? ¿Cual sería su valor en Melilla?
# ind<-get_pca_ind(fit)
# knitr::kable(ind$coord, digits =3,caption = "Valores de los individuos en las Cp")


## EJERCICIO 4
# Representar un mapa de calor de la matriz de datos, estandarizado y sin
# estandarizar para ver si se detectan inicialmente grupos de provincias.

# # cargar datos
# provincias <- read_excel("Provincias.xlsx")
# datos<- as.data.frame(provincias)
# rownames(datos)<-datos[,1]
# datos<-datos[,-1]

# grupos de calor juntando las mas parecidas
# ggheatmap(datos, seriate = "mean" )
# interactivo
heatmaply(datos, seriate = "mean", row_dend_left = TRUE, plot_method = "plotly")


### Datos sin estandarizar
# matriz distancia, valores sin estandarizar 
dist <- dist(datos, method = "euclidean") # distance matrix 
# mostramos las 6 primeras fila
dist6<-as.matrix(dist)[1:6, 1:6]
knitr::kable(dist6, digits =2,caption = "Distancias")

# mapa de calor, valores sin estandarizar 
fviz_dist(dist, show_labels = TRUE)
# reordenamos las observaciones más próximas y visualizar los posibles clusters
# ggheatmap(as.matrix(dist), seriate="mean")
# interactivo
heatmaply(as.matrix(dist), seriate = "OLO", row_dend_left = TRUE, plot_method = "plotly") 
# heatmaply(as.matrix(dist), seriate = "mean", row_dend_left = TRUE, plot_method = "plotly")

### Datos estandarizados
datos_ST <- scale(datos)

# matriz distancia con, valores estandarizados
dist_st <- dist(datos_ST, method = "euclidean") # distance matrix
# mostramos las 6 primeras fila
dist6_st<-as.matrix(dist_st)[1:6, 1:6]
knitr::kable(dist6_st, digits =2,caption = "Distancia")

# mapa de calor, valores estandarizados
fviz_dist(dist_st)
# reordenamos las observaciones más próximas y visualizar los posibles clusters
# ggheatmap(as.matrix(dist_st), seriate="mean")
# interactivo
heatmaply(as.matrix(dist_st), seriate = "OLO", row_dend_left = TRUE, plot_method = "plotly") 
# heatmaply(as.matrix(dist_st), seriate = "mean", row_dend_left = TRUE, plot_method = "plotly")

## EJERCICIO 5
# Realizar un análisis Jerárquico de clusters para determinar si existen 
# grupos de provincias con comportamiento similar.


### A
# A la vista del dendrograma ¿Cuántos clusters recomendarías?

# dendograma y agrupamos las observaciones, valores sin estandarizar 
res.hc <- hclust(dist, method="ward.D2")
fviz_dend(res.hc, cex = 0.5)

# dendograma y agrupamos las observaciones, valores estandarizados
res.hc_st <- hclust(dist_st, method="ward.D2")
fviz_dend(res.hc_st, cex = 0.5)


### B
# Representar los individuos agrupados según el número de clusters elegido.
# agrupar en 6 cluster
grp <- cutree(res.hc_st, k = 6)
head(grp, n = 10)
knitr::kable(table(grp), caption = "Número de individuos por cluster")

# visualizacion en el dendograma
# rownames(datos)[grp == 1]
fviz_dend(res.hc_st, k = 6, # Cut in four groups cex = 0.5, # label size
          k_colors = c("#ff0000", "#ffbb00", "#ffff00", "#00ff00", "#00ffff", "#0800ff"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE) # Add rectangle around groups

# visualizacion
fviz_cluster(list(data = datos_ST, cluster = grp), 
             palette = c("#ff0000", "#ffbb00", "#ffff00", "#00ff00", "#00ffff", "#0800ff"), 
             ellipse.type = "convex", repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# res.agnes <- agnes(x =datos, stand = TRUE, metric ="euclidean", method="ward")
# fviz_dend(res.agnes, cex = 0.6, k = 6)


### C
# ¿Qué número óptimo de clusters nos indican los criterios Silhoutte 
# y de Elbow?

# Elbow: numero optimo de clusters
fviz_nbclust(datos_ST, kmeans, method = "wss") + 
  geom_vline(xintercept =5, linetype = 2)+ labs(subtitle = "Elbow method")

# Silhouette: numero optimo de clusters
fviz_nbclust(datos_ST, kmeans, method = "silhouette") + 
  labs(subtitle = "Silhouette method")


### D
# Con el número de clusters decidido en el apartado anterior realizar un
# agrupamiento no jerárquico.

# misma semilla
RNGkind(sample.kind = "Rejection") 
set.seed(1234)

# No jerarquico
km.res <- kmeans(datos_ST, 5)

#### i
# Representar los clusters formados en los planos de las Componentes 
# principales. Relacionar la posición de cada cluster en el plano con 
# lo que representa cada componente principal.
fviz_cluster(km.res, datos_ST, axes = c(1, 2))
fviz_cluster(km.res, datos_ST, axes = c(1, 3))
fviz_cluster(km.res, datos_ST, axes = c(2, 3))

#### ii
# Evaluar la calidad de los clusters

# # 6 clusters
# sil <- silhouette(km.res$cluster, dist(datos_ST))
# rownames(sil) <- rownames(datos)
# head(sil[, 1:3])
# # grafica
# fviz_silhouette(sil)

# Criterio Elbow
# calidad de los clusters
sil <- silhouette(km.res$cluster, dist(datos_ST))
rownames(sil) <- rownames(datos)
head(sil[, 1:3])
# grafica
fviz_silhouette(sil)

# Metodo Silhouette
km.res2 <- kmeans(datos_ST, 2)
fviz_cluster(km.res2, datos_ST, axes = c(1, 2))
# calidad de los clusters
sil2 <- silhouette(km.res2$cluster, dist(datos_ST))
rownames(sil2) <- rownames(datos)
head(sil2[, 1:3])
# grafica
fviz_silhouette(sil2)

### E
# Explicar las provincias que forman cada uno de los clusters y comentar 
# cuales son las características socioeconómicas que las hacen pertenecer 
# a dicho cluster.

# grupo tabla
ordenado<-sort(km.res$cluster)
knitr::kable(ordenado, digits =2, caption = "Provincia y cluster")

# estadisticas cluster, datos estandarizados 
knitr::kable(km.res$centers, digits =2,caption = "Estadísticos de los clusters, datos STD")

# estadisticas cluster, datos originales estandarizados 
est_clust_orig<-aggregate(datos, by=list(km.res$cluster),mean)
knitr::kable(est_clust_orig, digits =2,caption = "Estadísticos de los clusters")


