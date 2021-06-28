library(tidyverse)
atributos <- c("ciudad_mpg","cilindros","cilindraje","conduccion","consumo_autopista","marca","modelo","clase","anio","tipo_transmision","velocidades_transmision", "emiciones_co2")
datos_vehiculos <- read_csv(file="Documentos/EspacioTrabajo/Rstudio/PrimerosPasos/Datos/vehiculos.csv", col_types="nnnfnfffffnn") #leemos el archivo 

#EXPLORACION DE DATOS

#La exploración de datos nos permite responder a preguntas como las siguientes - - - - - - 
#¿Cuántas filas y columnas hay en los datos? ¿Qué tipos de datos están presentes en nuestros datos? 
#¿Hay valores perdidos, incoherentes o duplicados en los datos? ¿Hay valores atípicos en los datos? 


names(datos_vehiculos)#nombres de las columnas o atributos
glimpse(datos_vehiculos) #vista previa de los archivos
names(datos_vehiculos) <- atributos  #cambiamos el idioma de las caracteristicas

summary(datos_vehiculos)# resumen del conjunto 
select(datos_vehiculos,clase) #seleccionamos la caracteristica que deseamos ver
summary(select(datos_vehiculos,clase,conduccion))

table(select(datos_vehiculos,clase)) #ver todos los valores del resumen con la funcion table

prop.table(table(select(datos_vehiculos, clase)))

datos_vehiculos %>% #tuberias para un codigo mas limpio
    select(clase) %>%
      table() %>%
        prop.table()


datos_vehiculos %>% #filtrado de los hechos
    filter( conduccion == "2-Wheel Drive"  ) %>%
      select(emiciones_co2) %>%
        summary()

#VISUALIZACION DE LOS DATOS

#diagrama de caja

datos_vehiculos %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = clase, y = emiciones_co2), fill = "red") +
  labs(title = "Boxplot of C02 Emissions by Vehicle Class", x = "Class", y =
         "C02 Emissions")

#diagrama de dispersion
datos_vehiculos %>%
  ggplot() +
  geom_point(mapping = aes(x = ciudad_mpg, y = emiciones_co2), color = "blue",
             size = 2) +
  labs(title = "Scatterplot of CO2 Emissions vs. City Miles per Gallon",
       x = "City MPG", y = "CO2 Emissions")

#diagrama de histograma
datos_vehiculos %>%
  ggplot() +
  geom_histogram(mapping = aes(x = emiciones_co2), bins = 30, fill =
                   "yellow", color = "black") +
  labs(title = "Histogram of CO2 Emissions", x = "CO2 Emissions", y =
         "Frequency")

# diagrama de composicion
datos_vehiculos %>%
  ggplot() +
    geom_bar(mapping = aes(x =anio, fill = conduccion), color = "black") +
      labs(title = "Stacked Bar Chart of Drive Type Composition by Year", x = "Model Year", y = "Number of Cars") +
        coord_flip()

#Preparacion de los datos

# valores pendientes como tratarlos con imputacion
#imputacion aleatoria, No es muy recomendado por que ignroa informacion
#imputcion por coincidencia 
#imputcion por coincidencia  caliente
#imputacion basada en distribucion utilizado en variables categoricas
#imputacion predictiva coste computacionl alto

datos_vehiculos %>% #obtener un resumen de las caracteristicas y ver sus valores pendienetes
  select(ciudad_mpg,cilindraje,consumo_autopista) %>%
  summary()


#imputacion a las caracteristicas con media y media

datos_vehiculos <- datos_vehiculos %>% 
  mutate(ciudad_mpg = ifelse(
    is.na(ciudad_mpg), 
    median(ciudad_mpg,na.rm = TRUE),
    ciudad_mpg)
  ) %>%
  mutate(consumo_autopista = ifelse(
      is.na(consumo_autopista), 
      median(consumo_autopista,na.rm = TRUE),
      consumo_autopista)
  )%>%
  mutate(cilindraje=ifelse(
    is.na(cilindraje),
    mean(cilindraje, na.rm=TRUE),
    cilindraje)
  )

datos_vehiculos %>% #verificamos que ya no tenemos valores pendientes
  select(ciudad_mpg,cilindraje,consumo_autopista) %>%
  summary()

#TRANSFORMACION DE DATOS
#normalizacion por escala decimal
datos_vehiculos %>%
  select(emiciones_co2) %>%
    summary()

datos_vehiculos%>%
  select(emiciones_co2)%>%
    mutate(emiciones_co2_d= emiciones_co2 / (10^4))%>%
      summary()

#puntuacion z
datos_vehiculos %>%
    select(emiciones_co2) %>%
      mutate(emiciones_co2_z=(emiciones_co2 - mean(emiciones_co2))/sd(emiciones_co2)) %>%
        summary()
#Normalizacion Min - Max
datos_vehiculos %>%
  select(emiciones_co2) %>%
    mutate(emiciones_co2_Min_Max = (emiciones_co2 - mean(emiciones_co2) / (max(emiciones_co2)- min(emiciones_co2))) * (1-0)+0) %>%
      summary()