# NAIVE BAYES #
library(readxl)
Titanic <- read_excel("Titanic.xlsx")

#Librerías requeridas
 library(e1071)
 library(naivebayes)
 library(caret)   #Para la matriz de confusión

#Preparacion de los datos
 #Se exporta la base
 #Se convierte todo a factor
 Titanic$Class<-as.factor(Titanic$Class)
 Titanic$Sex<-as.factor(Titanic$Sex)
 Titanic$Age<-as.factor(Titanic$Age)
 Titanic$Sobrevivir<-as.factor(Titanic$Sobrevivir)
 str(Titanic)

#La base tiene 2201 observaciones.
#Clase: Survived
#Atributos: Class,Sex, Age. 
#Se usara la regla de 80-20 para determinar el tamaño de la muestra de entrenamiento
 n<-floor(.8*nrow(Titanic))
 n #Se usaron 1760 datos de 
 #Modelo Naive Bayes: Clase, Sexo
###se divide en el grupo de prueba y de entrenamiento
 set.seed(2)
  index1<-sample(1:nrow(Titanic),n,replace=FALSE)
 train1<-Titanic[index1,] #Conjunto de datos de entrenamiento
 test1<-Titanic[-index1,]
 
#Modelo NB
 NB1<-naiveBayes(Sobrevivir ~ Class+Sex, data = train1)
 NB1

#Matriz de confusion
 predicciones1<-predict(NB1,newdata=test1) #Realiza la prediccion
 tab<-table(predicción=predicciones1,Valores_reales=test1$Sobrevivir) #Para visualizar
 confusionMatrix(tab,positive="Si")#Para visualizar

#regla de clasificacion
 base1<-data.frame(test1[c(1,2)],predicciones1)
 regla1<-base1[!duplicated(base1), ]
 regla1

#Modelo Naive Bayes: Sexo, Edad
###se divide en el grupo de prueba y de entrenamiento
 set.seed(4)
  index2<-sample(1:nrow(Titanic),n,replace=FALSE)
 train2<-Titanic[index2,] #Conjunto de datos de entrenamiento
 test2<-Titanic[-index2,]

#Modelo NB
 NB2<-naiveBayes(Sobrevivir ~ Sex+Age, data = train2)
 NB2

#Matriz de confusion
 predicciones2<-predict(NB2,newdata=test2) #Realiza la prediccion
 tab<-table(predicción=predicciones2,Valores_reales=test2$Sobrevivir) #Para visualizar
 confusionMatrix(tab,positive="Si")#Para visualizar

#regla de clasificacion
 base2<-data.frame(test2[,c(2,3)],predicciones2)
 regla2<-base2[!duplicated(base2), ]
 regla2

#Modelo Naive Bayes: Todas las variables
###se divide en el grupo de prueba y de entrenamiento
 set.seed(1)
  index3<-sample(1:nrow(Titanic),n,replace=FALSE)
 train3<-Titanic[index3,] #Conjunto de datos de entrenamiento
 test3<-Titanic[-index3,]

#Modelo NB
 NB3<-naiveBayes(Sobrevivir ~ ., data = train3)
 NB3

#Matriz de confusion
 predicciones3<-predict(NB3,newdata=test3) #Realiza la prediccion
 tab<-table(predicción=predicciones3,Valores_reales=test3$Sobrevivir) #Para visualizar
 confusionMatrix(tab,positive="Si")#Para visualizar

#regla de clasificacion
 base3<-data.frame(test3[c(1,2,3)],predicciones3)
 regla3<-base3[!duplicated(base3), ]
 regla3
