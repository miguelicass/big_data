# Importar Cartografías a R

# existen diferentes sitios donde descargar cartografías
# La unión europea
# https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
# Instituto geográfico NAcional
# https://www.ign.es/web/cbg-area-cartografia
# El INE (proporciona información a nivel de distrito Censal)

# 1 Lectura de Shapes ----

library(sp)
library(rgdal)
#library(maptools) # también permite leer cartografías


# dir() para encontrar el nombre del dirctorio donde están guardados los
# ficheros shape con las cartografías
dir()

# puedo llamar dir() con el nombre del dirctorio
dir("cartografías")

# Read in shapefile with readOGR(): neighborhoods
CCAA_MAP<-readOGR("cartografías","CCAA_GEO_ETRS89", stringsAsFactors = FALSE)


#Si el directorio es el de trabajo también se puede usar
#CCAA_MAP<-readOGR(dsn=getwd(),"CCAA_GEO_ETRS89")
#CCAA_MAP<-readOGR(dsn="."","CCAA_GEO_ETRS89")



# summary() del Mapa
summary(CCAA_MAP)
proj4string(CCAA_MAP)

# puedo dibujar
plot(CCAA_MAP)


View(CCAA_MAP)
View(CCAA_MAP@data)

# Fíjate que ha nombrado a las filas del dataframe igual que a
# cada polígono "0", "1", "2",....
# Es importante que tengan el mismo nombre porque la primera fila del 
# dataframe contiene los datos del primer polígono, la segunda fila los datos
# del sgundo,... y aunque no es estrictamente necsario que tengan el mismo nobre
# ayuda a su identificación


# puedo hacr algún mapa más bonito con alguna de las muchas librerías disponibles
library(tmap)

tm_shape(CCAA_MAP)+
  tm_borders()
# tmap_mode("plot") para hacer mapa estático (se puede incluir un mapa interactivo con leaflet tmap_mode("view")

# voy a cargar algún dato
# me descargo desde el INE datos de SALARIOS

salarios<-read.csv("datos_CCAA/SALARIOS.csv",sep = ";")

library(dplyr)
CCAA_MAP@data<-dplyr::left_join(CCAA_MAP@data,salarios, by=c("cod_CCAA"="COD_CCAA"))
View(CCAA_MAP@data)


#o tambien con sp::spCbind(CCAA_MAP@data, tablanewdatos) los rownames deben ser los mismos
# rownames(CCAA_MAP@data)

# para cambiar el nombre a las filas y a los polígonos
# OJO!!!, al hacer l dplyr ha cambiado el nombre del data.frame
# Vamos a renombrar las filas y polígonos para que tengan el mismo nombre
# Esos nombrs tienen que sr texto, por lo que si no se ha leido como texto
# con stringsAsFactors = FALSE en readOGR hay qu canbiar el tipo de datos con
# as.character()

#Cambio el nombre de las filas dl dataframe
rownames(CCAA_MAP@data)<-CCAA_MAP@data$cod_CCAA
# y ahora cambio el nombr de los polígonos
new_IDs = rownames(CCAA_MAP@data)
for (i in 1:length(slot(CCAA_MAP, "polygons"))){
  slot(slot(CCAA_MAP, "polygons")[[i]], "ID") = new_IDs[i]
}

#Comprobamos
View(CCAA_MAP)
View(CCAA_MAP@data)

#también podríamos haber puesto directamente el nombre d la CCAA
# rownames(CCAA_MAP@data)<-CCAA_MAP@data$Nombre.CCAA



# Ahora puedo hacer algún dibujo d mapa algo más bonito
library(tmap)
tm_shape(CCAA_MAP) +
  tm_borders()
  
tm_shape(CCAA_MAP) +
  tm_polygons(col = "SALARIO")


tm_shape(CCAA_MAP) +
  tm_fill(palette ="Blues",col = "SALARIO",style = "quantile")



tm_shape(CCAA_MAP) +
  tm_fill(palette ="Blues",col = "SALARIO",style = "quantile")+
  tm_bubbles(size = "SALARIO",scale=1,style = "quantile", col = "SALARIO")


# para cambiar algunas opciondes de leyenda
map1 <- tm_shape(CCAA_MAP) +
  tm_fill(palette ="Blues",col = "SALARIO",style = "quantile")+
  tm_bubbles(size = "SALARIO",scale=1,style = "quantile", col = "SALARIO")+  
  tm_layout(legend.title.size = .7,
          legend.text.size =0.6,
          legend.position = c("right","bottom"),
          legend.bg.color = "white",
          legend.bg.alpha = 1,
          legend.stack = "horizontal",
          legend.width = 1.5,
          legend.height = 1.5)


# Ahora puedo Grabar mi nuva cartografía 
writeOGR(obj=CCAA_MAP, dsn="cartografías", layer="CCAA_SALARIOS", driver="ESRI Shapefile") 

map1

save_tmap(map1, filename="map1.jpeg", width=15, height=10, units="cm")


####### VOY A LEER OTRA CARTOGRAFIA
library(rgdal)


Munic_ESP<- rgdal::readOGR(dsn="cartografías",layer="Munic04_ESP_GEO_ETRS89_DAT")

# otras librerías que permiten leer shapes
# library(maptools)
# Munic_ESP<- maptools::readShapeSpatial("Munic04_ESP_GEO_ETRS89_DAT.shp",IDvar = "cod_ine", proj4string=CRS("+init=epsg:4326")) # Comprado al INE

#proj4string(Munic_ESP)
#proj4string(CCAA_MAP)


library(tmap)
tm_shape(CCAA_MAP) +
  tm_borders(col = "gray85")+
tm_shape(Munic_ESP)+
  tm_fill(col="PrecioIn16", style = "quantile" )







####
# Sistmas de proyección

#  1	EPSG:4230	Geogr?ficas en ED 50	HUSO 0	DATUM ED50
#  2	EPSG:4326	Geogr?ficas en WGS 84 (Cat dice que es WGS80, pero el SRS es WGS84)	HUSO 0	DATUM WGS84
#  3	EPSG:32627	UTM huso 27N en WGS 84	HUSO 27	DATUM WGS84	
#  4	EPSG:32628	UTM huso 28N en WGS 84	HUSO 28	DATUM WGS84
#  5	EPSG:32629	UTM huso 29N en WGS 84	HUSO 29	DATUM WGS84	
#  6	EPSG:32630	UTM huso 30N en WGS 84	HUSO 30	DATUM WGS84	
#  7	EPSG:32631	UTM huso 31N en WGS 84	HUSO 31	DATUM WGS84	
#  8	EPSG:23029	UTM huso 29N en ED50	HUSO 29	DATUM ED50	
#  9	EPSG:23030	UTM huso 30N en ED50	HUSO 30	DATUM ED50	
# 10	EPSG:23031	UTM huso 31N en ED50	HUSO 31	DATUM ED50	
# 11	EPSG:25829	UTM huso 29 en ETRS89	HUSO 29	DATUM ETRS89	
# 12	EPSG:25830	UTM huso 30 en ETRS89	HUSO 30	DATUM ETRS89	
# 13	EPSG:25831	UTM huso 31 en ETRS89	HUSO 31	DATUM ETRS89	
# 14	EPSG:4258	Geogr?ficas en ETRS89	HUSO 0	DATUM ETRS89

# Para consultar características de cada sistema de referencia
# http://epsg.io
# http://spatialreference.org
  
  
# CRS.new <- CRS("+init=epsg:3857") #PSeudomercator Este es el que usa Open Street Maps
# http://spatialreference.org/ref/epsg/3857/
# Projection used in many popular web mapping applications (Google/Bing/OpenStreetMap/etc). Sometimes known as EPSG:900913.
# EPSG:4326 WGS 84


# Para conocer exactamente los par?metros de cada proyecci?n con el EPSG se pueden sacar de  http://spatialreference

#Por ejemplo:

#  http://spatialreference.org/ref/epsg/23030/proj4/ 
#  +proj=utm +zone=30 +ellps=intl +units=m +no_defs   



#Para unir cartograf?as
# map_cpTOT<-spRbind(map_1, map_2)


#Para Cambiar el sistema de referencias
#library(rgdal)  
#CRS.new <- CRS("+init=epsg:4258") #Sistema de referencia Final
#Munic_ESP <- spTransform(Munic_ESP, CRS.new)  


#Para sacar los pol?gonos de las Provincias (tambi?n vldr?a para sacar los de las diferentes agrupaciones)
#MAPA_PROV<- unionSpatialPolygons(Munic_ESP, IDs=Munic_ESP$COD_PROV)
#plot(MAPA_PROV)



# PARA ENCONTRAR A QUE polígono pertenec
#id_polygoncp <- over(DATOSGEO,as(mapa,"SpatialPolygons"))

# PARA unir dos cartografías (OJ con identificadores de polígono diferente)
# Ahora utilizo spRbind para unir los dos spatialdataframes
#map_cpTOT<-spRbind(map_cpAcoruna3, map_cpLugo3)
