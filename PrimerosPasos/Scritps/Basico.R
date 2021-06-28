if(!require(dplyr)) {install.packages("dplyr")}
if(!require(tidyverse)) {install.packages("tidyverse")}

library(tidyverse)
library(dplyr) #importatndo paquete 


vignette(package = "dplyr") #informacion del paquete
vignette(package = "dplyr", topic = "programming" )#documentacion del paquete

#vector
estudiantes <- c("thalia","karen","kelly","raquel","ingrid","margarita","adrea","odalis")
notas <- c(70,65,50,75,80,90,69,72)

#resultados

estudiantes[1]

#medidas
mean(notas) #media
median(notas) #mediana
min(notas) #minimo
max(notas) #maximo

#vector mixto
mixto <- c("thalia",70,"karen",65,"kelly",50,"raquel",75,"ingrid",80,"margarita",90,"adrea",69,"odalis",72)#coercion de datos todos de un mismo tipo


#estructura de datos
resultado_test <- data.frame(estudiantes, notas)

#acceder a la columna de una estructura de datos
resultado_test$notas

#media de las notas que se encuentran en la estructura de datos
mean(resultado_test$notas)

# identificar el tipo de dato de una variable 

autencticado <- TRUE

class(resultado_test)
class(autencticado)

#cambiar los tipos de datos a una variable o vector
categoria_frutas <- c("frutas","vegetales","productos secos")

class(categoria_frutas)

categoria_frutas <- factor(categoria_frutas)
class(categoria_frutas)

#comprobar el numero de elementos en un vector

length(categoria_frutas)
length(resultado_test)

#comprobar si es de un dato en especifico
cedula <- "1315915460"
sueldo <- 400.25

is.factor(cedula)
is.integer(sueldo)

#convertir tipos de datos
as.numeric("1.2")

#
ggplot(data=ChickWeight) +
  geom_smooth(mapping=aes(x=Time, y=weight, color=Diet), method = "loess" )
