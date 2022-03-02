# **************************************
# Manejo de archivos y Workspace en R
# **************************************

# Definir el directorio de trabajo (working directory)
setwd("c:/")

# Obtener lista completa de archivos en el Working directory
dir()

# Obtener lista completa de data frames R en el Working directory
dir(pattern=".Rda")

# Obtener lista completa de scripts R en el Working directory
# El dolar $ implica que es final exacto de la extensión,
# no presenta archivos Rda por ejemplo
dir(pattern=".R$")

# Leer un archivo del working directory: no se pone path, pues
# es el path del setwd

load("saheartbis.Rda")

# Guardar  un archivo en el working directory: no se pone path, pues
# es el path del setwd

save(saheartbis,file="saheartbis.Rda")

# Grabar todos los objetos del Global Environment (Workspace)  
# en un archivo

save.image(file="todosobjetos.RData")

# Leer  objetos del Workspace de un archivo

load("todosobjetos.RData")

# Borrar (remover) objetos que ocupan RAM en el Global Environment

rm(saheartbis)

# Borrar todos los objetos del Global Environment 
# y borrar basura de la memoria

rm(list=ls()) 
gc()

# Borrar todos los plots
graphics.off()

# También vale 
# dev.off()


