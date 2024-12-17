rm(list=ls(all=TRUE))
getwd()
setwd("/.../...") 

#############################Cargar y explorar datos############################
datos = read.table("datos_grupo_1.txt", sep="\t", header=TRUE)
datos$Tipo.de.proyecto = as.factor(datos$Tipo.de.proyecto)

dim(datos)
str(datos)
summary(datos)
names(datos)

################Clacular e interpretar intervalos de confianza##################
