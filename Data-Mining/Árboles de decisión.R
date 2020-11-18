###LIBRERIAS
library(tidyverse) #para el procesamiento de datos
library(rpart) #realizar los árboles
library(rpart.plot) #graficar los árboles
###DATABASE
 vino<-read.csv("G:/R mineria/vino.csv")
 vino$tipo<-as.factor(vino$tipo)

#####VARIABLE:ALCOHOL 
 set.seed(5)
 n1<-sample(1:nrow(vino),size=100)
 #Entrenamiento
   train1<-vino[n1,]
   arbol1<-rpart(tipo~alcohol,data=train1)
   rpart.plot(arbol1)
 #Validación
   test1<-vino[-n1,]
   prediccion1<-predict(arbol1,newdata=test1,type='class') 
   confusion1<-table(prediccion1,test1$tipo)
   confusion1
   e<-0
   for(i in 1:nrow(confusion1)){
     e<-confusion1[i,i]+e
   }
   exactitud1<-e/nrow(vino[-n1,])
   exactitud1
   
   
#####VARIABLES:ASH,HUE  
 set.seed(3)
 n2<-sample(1:nrow(vino),size=100)
 #Entrenamiento
   train2<-vino[n2,]
   arbol2<-rpart(tipo~ash+hue,data=train2)
   rpart.plot(arbol2)
 #Validación
   test2<-vino[-n2,]
   prediccion2<-predict(arbol2,newdata=test2,type='class') 
   confusion2<-table(prediccion2,test2$tipo)
   confusion2
   e<-0
   for(i in 1:nrow(confusion2)){
     e<-confusion2[i,i]+e
   }
   exactitud2<-e/nrow(vino[-n2,])
   exactitud2

   
#####VARIABLES:ALCOHOL,MAGNESIO  
exac<-c()
   for(i in 1:3){
     set.seed(i+10)
     n<-sample(1:nrow(vino),size=140)
     #Entrenamiento
       train<-vino[n,]
       arbol<-rpart(tipo~alcohol+magnesium,data=train)
       rpart.plot(arbol)
     #Validación
       test<-vino[-n,]
       prediccion<-predict(arbol,newdata=test,type='class') 
       confusion<-table(prediccion,test$tipo)
       print(confusion)
      #Exactitud
        e<-0
        for(j in 1:nrow(confusion)){
          e<-confusion[j,j]+e
        }
        exac[i]<-e/nrow(vino[-n,])
   }
exactitud<-data.frame(muestra=c("1","2","3"),Exactitud=exac)
exactitud