#........ADA 14........#
#...... EQUIPO 1 ......#

#Cargar librerias
library(cluster) 
library(factoextra) 
library(dplyr)

#Cargar Datos
vino <- read.csv("vino.csv")
datos <- vino
datos<-na.omit(datos) #eliminar filas que tengan alguna entrada vacia

# 178 observaciones
# 14 variables

#------------------ K MEDIAS ------------------#

#Particionamiento
datos[,2:14]<-scale(datos[,2:14]) # Estandarizar los datos (No estandarizar tipo)
set.seed(8)
clustermedia <- kmeans(datos[,2:14], centers=3) # size: 62, 65,51
clustermedia 

#Visualizar
fviz_cluster(clustermedia, datos[,2:14], geom = "point")
fviz_cluster(clustermedia, datos[,2:14], geom = "text")
fviz_cluster(clustermedia, datos[,2:14], ggtheme = theme_minimal())
fviz_cluster(clustermedia, datos[,2:14], ellipse.type = "norm")


#-------------------- AGNES --------------------#
cluster1 <- agnes(datos[,2:14], method = "ward") 
pltree(cluster1, cex = 0.6, hang = -1, main = "Dendograma AGNES")
rect.hclust(cluster1,k=3,border=2:20)
AGNES<-rep(c(1,2,3), times = c(64,58,56))


#----------------- Comparación -----------------#
resultados<-data.frame("Original"= vino$tipo,
                       "K.medias"= clustermedia$cluster,
                       "AGNES"=AGNES)
k=0
j=0
for(i in 1:178) {
  if( resultados$Original[i] == resultados$K.medias[i])
    k = k+1
  if( resultados$Original[i] == resultados$AGNES[i])
    j = j+1
}
k #172   valores coinciden
j #165 valores coinciden

CoincidenciaKmedias <- k*100/178
CoincidenciaAGNES <- j*100/178

Coincidencia <- matrix( c("Kmedias", "AGNES", CoincidenciaKmedias, CoincidenciaAGNES), nrow=2, ncol=2)
colnames(Coincidencia)<-c("Metodo","% de Coincidencia")
Coincidencia
