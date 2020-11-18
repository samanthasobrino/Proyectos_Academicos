
###ADA 8 ###
##Equipo 1

######Método Backpropagation#####

#install.packages("neuralnet")
library(neuralnet)
ppg2008<-read.csv("ppg2008.csv")

####Creamos la variable Nivel
str(ppg2008)
ppg2008$Nivel<-ppg2008$PTS/ppg2008$MIN
ppg2008$Nivel<-ifelse(ppg2008$Nivel>0.68,"Muy Bueno",
                   ifelse(ppg2008$Nivel>0.55&ppg2008$Nivel<=0.68,"Bueno",
                          "Regular"))

ppg2008$Nivel<-as.factor(ppg2008$Nivel)
str(ppg2008)


#Traindata y testdata
set.seed(1)
muestra<-sample(1:nrow(ppg2008),25) #Toma 25 observaciones aleatorias de la base
traindata<-ppg2008[muestra,]
testdata<-ppg2008[-muestra,]

### 1)
#### Modelo con una capa oculta con dos nodos y función de activación sigmoide

actfunction<-function(x) 1/(1+exp(-x)) ###funcion de activacion sigmoide
model1<-neuralnet(formula = Nivel~FGP+FTP+X3PP,
                 data = traindata, 
                 learningrate = 0.01,
                  hidden=2,
                  act.fct = actfunction)

### Gráfica // Diagrama de la red
plot(model1,
     col.entry = "red",
     col.entry.synapse = "red",
     cex=0.8,
     col.hidden.synapse = "green",
     col.hidden = "blue",
     main="Red Neuronal")

# Implementacion al testdata
Predicciones.M1<-compute(model1,testdata)
salida1<-as.data.frame(Predicciones.M1[["net.result"]])

str(ppg2008$Nivel)
Predicciones.M1<-ifelse(salida1$V1>salida1$V2 & salida1$V1>salida1$V3,"Bueno",
                     ifelse(salida1$V2>salida1$V1 & salida1$V2>salida1$V3,"Muy Bueno",
                            "Regular"))


print(confusion<-table(Predicciones.M1,testdata$Nivel))
print(exactitud<-sum((confusion[1,1]+confusion[2,3])/sum(confusion))) # Exactitud del arbol de decision. Hay que tener cuidado con que la matriz sea cuadrada
print(sensivilidad<-confusion[1,1]/(confusion[1,1]+confusion[2,1]))
print(especifidad<-confusion[2,3]/(confusion[1,3]+confusion[2,3]))
print(Precision<-confusion[1,1]/(confusion[1,1]+confusion[1,2]+confusion[1,3])) 


#Predicciones y valores reales de los jugadores en la muestra
comprobacion<-data.frame("Jugador"=c(1:50)[-muestra],"Valor Real"=testdata$Nivel,"Estimado"=Predicciones.M1)
comprobacion 


#### 2)
#### Un análisis considerando una capa oculta con dos nodos y función de activación tangente hiperbólica.

actfunction2<-function(x) (exp(x)-exp(-x))/(exp(x)+exp(-x)) ###funcion de activacion sigmoide
model2<-neuralnet(formula = Nivel~FGP+FTP+X3PP,
                 data = traindata, 
                 learningrate = 0.01,
                 hidden = 2,
                 act.fct = actfunction2)

### Gráfica // Diagrama de la red
plot(model2,
     col.entry = "red",
     col.entry.synapse = "red",
     cex = 0.8,
     col.hidden.synapse = "green",
     col.hidden = "blue",
     main = "Red Neuronal")

# Implementacion al testdata
Predicciones.M2<-compute(model2,testdata)
salida2<-as.data.frame(Predicciones.M2[["net.result"]])

Predicciones.M2<-ifelse(salida2$V1>salida2$V2 & salida2$V1>salida2$V3,"Bueno",
                      ifelse(salida2$V2>salida2$V1 & salida2$V2>salida2$V3,"Muy Bueno",
                             "Regular"))

print(confusion2<-table(Predicciones.M2,testdata$Nivel))
print(exactitud2<-sum((confusion2[1,1]+confusion2[2,3])/sum(confusion2))) # Exactitud del arbol de decision
print(sensivilidad2<-confusion2[1,1]/(confusion2[1,1]+confusion2[2,1]))
print(especifidad2<-confusion2[2,3]/(confusion2[1,3]+confusion2[2,3]))
print(Precision2<-confusion2[1,1]/(confusion2[1,1]+confusion2[1,2]+confusion2[1,3])) 


#Predicciones y valores reales de los jugadores en la muestra
comprobacion<-data.frame("Jugador"=c(1:50)[-muestra],"Valor Real"=testdata$Nivel,"Estimado"=Predicciones.M2)
comprobacion 


# Del modelo 1 se obtuvo una exactitud del 52%, un error de 5.0868 y se llevo a cabo con un total de 6,698 pasos.
# Del modelo 2 se obutvo una exactitud del 48%, un error de 6.0165 y un total de pasos de 13,416.
# Es facil ver que el modelo 1 tiene una mayor exactitud, un error menor y se llego a el en un numero
# de pasos menor que el modelo 2. Por lo tanto, escogemos el MODELO 1 como el mejor modelo por tener un
# indice de exactitud MAYOR que el modelo 2, así como tambien un error menor.