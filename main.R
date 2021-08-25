# Transformar datos que se pueden confundir como numéricos en factores
titanic_data$Survived <- as.factor(titanic_data$Survived)
titanic_data$Pclass <- as.factor(titanic_data$Pclass)
titanic_data$Sex <- as.factor(titanic_data$Sex)
titanic_data$Embarked <- as.factor(titanic_data$Embarked)

#funcion sin_valor(dataframe) que desliega cuantos valores NA posee cada variable 
sin_valor <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores NA:",colSums(is.na(x[i])),"\n")
  }
}
sin_valor(titanic_data)

# funcion en_blanco(dataframe) que desliega cuantos valores en blanco posee cada variable 
en_blanco <- function(x){ 
  sum = 0 
  for(i in 1:ncol(x)) 
  { 
    cat("En la columna",colnames(x[i]),"total de valores en blanco:",colSums(x[i]==""),"\n") 
  } 
} 
en_blanco(titanic_data)


# Columnas de interés: Age y Embarked

# ¿Qué pasajeros tienen Embarked en blanco?
titanic_data$PassengerId[titanic_data$Embarked == ""]

# ¿En qué clase viajaban y cuánto les costó el boleto?
titanic_data$Pclass[titanic_data$PassengerId == 62]
titanic_data$Fare[titanic_data$PassengerId == 62]

titanic_data$Pclass[titanic_data$PassengerId == 830]
titanic_data$Fare[titanic_data$PassengerId == 830]

# solo ejecutar 1 vez
# library(dplyr)
# embark_fare <- titanic_data %>% filter(PassengerId != 62 & PassengerId != 830)

library(ggplot2)
library(scales)
ggplot(data = embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +  
  geom_boxplot() +  
  geom_hline(aes(yintercept = 80),  
             colour = "red", linetype = "dashed", lwd = 2) + 
  scale_y_continuous(labels = dollar_format()) +  
  theme_bw()

# De  esta  gráfica  vemos  que  la  tarifa  media  para  el  pasajero  de  primera 
# clase  que  sale  del  puerto  C(Charbourg)  coincide  muy  bien  con  los
# $  80  pagados  por  los  pasajeros  que  no  tienen  puerto  de 
# embarque. Entonces podemos reemplazar con seguridad  los datos en blanco de  aquellos pasajeros 
# con C

titanic_data$Embarked[c(62, 830)] <- "C"

## Visualización gráfica

# 1. Tasa de supervivencia

# a.Sobrevivencia de pasajeros
barplot(table(titanic_data$Survived), main="Pasajeros en Titanic", names=c("Murieron", "Sobrevivieron"))

# b. Pasajeros por clase
barplot(table(titanic_data$Pclass), main="Pasajeros de Titanic por Clase", names= c("Primera", "Segunda", "Tercera"))

# c. Pasajeros por género
barplot(table(titanic_data$Sex), main="Pasajeros del Titanic por Genero", names= c("Mujer", "Hombre"))

# d. Sobrevivencia de pasajeros por género
counts = table(titanic_data$Survived, titanic_data$Sex)
barplot(counts, col=c("green","yellow"), legend = c("Murieron", "Sobrevivieron"), main = "Sobreviviencia de Pasajeros por Genero")

# e. Sobrevivencia por clase
counts1 = table(titanic_data$Survived, titanic_data$Pclass)
barplot(counts1, col=c("green","yellow"), legend = c("Murieron","Sobrevivieron"), main = "Sobreviviencia de Pasajeros por Clase", names= c("Primera", "Segunda", "Tercera"))

# CONCLUSIONES PRELIMINARES 
# 
# Solo el 38,38% de los pasajeros que abordaron el Titanic sobrevivieron. 
# Abordaron muchos más pasajeros hombres que mujeres. 
# La mayoría de pasajeros pertenecían a la tercera clase. 
# Vemos  que  la  tasa  de  supervivencia  entre  las  mujeres  fue  significativamente  mayor  en 
# comparación con los hombres.  Explicado en parte por el protocolo de "mujeres y niños 
# primero". 
