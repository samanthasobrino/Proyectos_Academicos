#       ADA 5       #

#Importar base de datos
base <- read.csv("C:/Users/Lap-lenovo/Downloads/Bases de Datos/ppg2008.csv")
base1<-base[,-c(1,4)] #Eliminar columna 1 por ser no numérica y la columna 4 PTS.

#1. Un análisis de componentes principales para reducir la dimensión. 
#El entregable es un documento word donde se describa el análisis para 
#determinar cuántas componentes permanecen y los pesos de las componentes. 
#También se entrega un archivo csv con la base de datos transformada sólo
#con las componentes que permanecieron (usa la función write.csv)
componentes<-princomp(base1,cor=T)
summary(componentes)

#Criterio Codo
screeplot(componentes,type="l")

#Calculamos escalares de cada componente
componentes$loadings #imprime las cargas

#Calculamos la base transformada
componentes$scores
summary(base1)

#Biplot... Nos da idea de los pesos de las componentes
biplot(componentes,cex=0.6,scale=0)
#Los números negros son las transformaciones de la base de datos a dos componentes (Componentes$scores)
#Las flechas se trazan con los valores de los coeficientes de las componentes (componentes$loading)
write.csv(componentes$scores[,-c(4:19)],"basenueva.csv")

#2. Para la columna PTS, realiza un procedimiento de suavizamiento. 
#El entregable es un documento word que indique los puntos de corte y la razón 
#por la que se seleccionaron éstos. De la misma manera se entrega un archivo 
#csv con la base de datos completa pero en lugar de tener la columna PTS original
#ya tenga los valores suavizados.

base2<-base  #para que tenga la columna PTS
summary(base2$PTS)  #min: 17.2, media: 20.86, max: 30.2

# queremos 5 grupos (binarización)
binning(base2$PTS,nbins=5)

#graficamos los breaks encontrados en binning, nbins=5
ggplot(data=base2,aes(x=base2$PTS))+
  geom_histogram(breaks=c(17.19999, 19.8, 22.4, 25, 27.6, 30.20),col="red",fill="cornflowerblue",alpha=.5)+
  labs(tittle="Histograma Ingreso",x="Puntos", y="Frecuencia")+
  xlim(c(17,31))

###Cambiamos los datos por su representante
base2$PTS=cut(base2$PTS,breaks=(binning(base2$PTS,nbins=5)$breaks),labels=(binning(base2$PTS,nbins=5)$x))

## Guardamos el excel
write.csv(base2,"base2.csv")
