# Importar Cartografías a R

# existen diferentes sitios donde descargar cartografías
# La unión europea
# https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
# Instituto geográfico NAcional
# https://www.ign.es/web/cbg-area-cartografia
# El INE (proporciona información a nivel de distrito Censal)

# 1 Lectura de Shapes con sf ----

# Son objetos de tipo data.frame, pero que guardan las geometrías en uno de los campos (similar a la estructura que utiliza PostGis) 

# ese campo de geometrías es una lista, de clase `sfc`



library(sf)

methods(class = "sf")
# https://cran.r-project.org/web/packages/sf/vignettes/sf1.html


### Lectura de Cartgrafías

CCAA_MAP<-st_read("cartografías/CCAA_GEO_ETRS89.shp")

# también se podría leer un objeto sp y desués pasarlo a sf con st_as_sf(objeto.sp)

# summary() del Mapa
summary(CCAA_MAP)
class(CCAA_MAP)


# para obener el campo de geometría (lista `sfc`)
st_geometry(CCAA_MAP)
# y como es un lista puedo llamar a cada geometría 
st_geometry(CCAA_MAP)[[13]] 


# se ueden gestionar las geometrías con las funciones para geometrías `sfc` 
methods(class='sfc') # por ejemplo unir polígonos, buscar intersecciones, buscar bordes, cambiar de sistema de referencia (son funcioones parecidas a las implementadas en PostGis)


# por ejemplo para añadir  el centroide CCAA
CCAA_MAP2 <- cbind(CCAA_MAP, st_coordinates(st_centroid(CCAA_MAP)))

# puedo dibujar un mapa de cada variable
plot(CCAA_MAP[1])

plot(CCAA_MAP[1], reset=FALSE)
plot(CCAA_MAP[13,1], col="black", add=TRUE) # para dibujar MAdrid con otro color

# puedo hacr algún mapa más bonito con alguna de las muchas librerías disponibles
library(tmap)

tm_shape(CCAA_MAP)+
  tm_borders()


# tmap_mode("plot") para hacer mapa estático (se puede incluir un mapa interactivo con leaflet tmap_mode("view")

# librería ggplot
library(ggplot2)

ggplot(data=CCAA_MAP)+
  geom_sf()

ggplot(data=CCAA_MAP)+
  geom_sf()+
  theme_minimal()


# voy a cargar algún dato
# me descargo desde el INE datos de SALARIOS

salarios<-read.csv("datos_CCAA/SALARIOS.csv",sep = ";")

library(dplyr)
CCAA_MAP<-dplyr::left_join(CCAA_MAP,salarios, by=c("cod_CCAA"="COD_CCAA"))
View(CCAA_MAP)


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


library(ggplot2)
library(ggthemes)
library (gridExtra) # para poner m?s de un ggplot ennun mismo gr?fico
library (RColorBrewer) # para poner m?s de un ggplot ennun mismo gr?fico
library(classInt) # para obtener intervalos
library(gtools)


ggplot(data=CCAA_MAP)+
  geom_sf(aes(fill=quantcut(SALARIO,q = 5)), colour=NA)+      # mirar cut_number, cut_interval o cut(x, breaks)
  scale_fill_brewer("Salario",palette = "Blues")+
  theme_minimal()+
  theme(line = element_blank(),                          # remove axis lines .. 
      axis.text=element_blank(),                       # .. tickmarks..
      axis.title=element_blank(),                      # .. axis labels..
      panel.background = element_blank())            # .. background gridlines
  
# para añadir burbujar tendría que calcular el centroide CCAA
CCAA_MAP2 <- cbind(CCAA_MAP, st_coordinates(st_centroid(CCAA_MAP)))

ggplot(data=CCAA_MAP)+
  geom_sf(aes(fill=quantcut(SALARIO,q = 5)), colour=NA)+      # mirar cut_number, cut_interval o cut(x, breaks)
  scale_fill_brewer("Salario",palette = "Blues")+
  geom_point(aes(size=SALARIO,
                 x=st_coordinates(st_centroid(CCAA_MAP))[,1],
                 y=st_coordinates(st_centroid(CCAA_MAP))[,2],
                 color=cod_CCAA),
             alpha=0.20)+
  scale_size(range = c(0.1,24), name="SalarioBv")+  #breaks
  scale_color_discrete(guide=FALSE)+
  theme_minimal()+
  theme(line = element_blank(),                          # remove axis lines .. 
        axis.text=element_blank(),                       # .. tickmarks..
        axis.title=element_blank(),                      # .. axis labels..
        panel.background = element_blank())            # .. background gridlines




# Ahora puedo Grabar mi nuva cartografía 
st_write(obj=CCAA_MAP,dsn = "cartografías/CCAA_SALARIOS.shp")  # delte_layer=TRUE para sobreescribir un acartografía 



####### VOY A LEER OTRA CARTOGRAFIA
library(rgdal)


Munic_ESP<- st_read("cartografías/Munic04_ESP_GEO_ETRS89_DAT.shp")


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
map_cpTOT<-spRbind(map_1, map_2)


#Para Cambiar el sistema de referencias
library(rgdal)  
CRS.new <- CRS("+init=epsg:4258") #Sistema de referencia Final
Munic_ESP <- spTransform(Munic_ESP, CRS.new)  


#Para sacar los pol?gonos de las Provincias (tambi?n vldr?a para sacar los de las diferentes agrupaciones)
MAPA_PROV<- unionSpatialPolygons(Munic_ESP, IDs=Munic_ESP$COD_PROV)
plot(MAPA_PROV)



# PARA ENCONTRAR A QUE polígono pertenec
id_polygoncp <- over(DATOSGEO,as(mapa,"SpatialPolygons"))

# PARA unir dos cartografías (OJ con identificadores de polígono diferente)
#Ahora utilizo spRbind para unir los dos spatialdataframes
map_cpTOT<-spRbind(map_cpAcoruna3, map_cpLugo3)
