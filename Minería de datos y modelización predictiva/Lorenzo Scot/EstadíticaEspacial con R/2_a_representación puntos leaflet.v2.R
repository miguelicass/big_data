# libreía leaflet

library(leaflet)


library(sp)
library(rgdal)


CCAA_MAP<-readOGR("cartografías","CCAA_SALARIOS")
summary(CCAA_MAP)

leaflet(CCAA_MAP,options = leafletOptions(attributionControl = FALSE)) %>%
  addPolygons(data=CCAA_MAP, stroke=TRUE, opacity = 0.5, fillOpacity = 0.7,color="grey10",
              fillColor = ~colorQuantile("YlOrBr", n=9, SALARIO, na.color = "white")(SALARIO))


leaflet(CCAA_MAP,options = leafletOptions(attributionControl = FALSE)) %>%
  addTiles()%>%
  addPolygons(data=CCAA_MAP, stroke=TRUE, color="grey10")
  
leaflet(CCAA_MAP,options = leafletOptions(attributionControl = FALSE)) %>%
  addTiles()%>%
  addPolygons(data=CCAA_MAP, stroke=TRUE, opacity = 0.25, fillOpacity = 0.27,color="grey10",
              fillColor = ~colorQuantile("YlOrBr", n=9, SALARIO, na.color = "white")(SALARIO))


# La librería leaflet utilizada tambien por tmap
library(tmap)
# Para hacerlo Interactivo
tmap_mode("view")  # Esto pasa de ser un plot estático a uilizar leaflet y Viewer

tm_shape(CCAA_MAP) +
  tm_fill(palette ="Blues",col = "SALARIO",style = "quantile")


# para volver a hacer mapas con tmap en plot estático `tmap_mode("plot")`

# para añadir mapas base  + tm_basemap(server="OpenTopoMap")



# Para situar marcadores

datos_map<-data.frame(longx=c(-3.741274,-3.718765,-3.707027, -3.674605,-3.709559 ),
                      laty=c(40.38479, 40.36751, 40.45495, 40.50615, 40.42059))


leaflet(data=datos_map) %>%
  addTiles()%>%  
  addMarkers(data=datos_map,lng=~longx, lat=~laty)


# Cargamos datos desde fichero 
Datos <- read.csv(file="datos_CCAA/Data_Housing_Madrid.csv",header=TRUE)

hist(Datos$house.price)


m <- leaflet(data=Datos[sample(nrow(Datos),100),]) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=~longitude,
             lat=~latitude,
             popup=~paste0(type.house, " - ", house.price, " euros"))
m  # Print the map



