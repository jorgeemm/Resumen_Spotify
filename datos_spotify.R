# Cargar las librerías necesarias -----------------------------------------

library(pacman)
p_load(jsonlite, # Para abrir los datos
       tidyverse, 
       lubridate) # Para crear las variables día del año y día de la semana


# Abrir las bases de datos ------------------------------------------------

canciones1 <- fromJSON("C:/Users/Lenovo/OneDrive/Escritorio/Resumen_Spotify/canciones1.json")
canciones2 <- fromJSON("C:/Users/Lenovo/OneDrive/Escritorio/Resumen_Spotify/canciones2.json")


# Si solo tienes una base de datos con las canciones, el siguiente paso no es necesario,
# y donde pone canciones1 escribe directamente spoti como nombre del df

# Juntar las dos bases de datos en una ------------------------------------

spoti <- bind_rows(canciones1,canciones2)

rm(canciones1,canciones2)  

# Editar las variables ----------------------------------------------------

spoti <- spoti %>%
  #Separar la primera columna con la fecha completa en año, mes, día y hora por separado
  separate(endTime, c("fecha","hora"), " ") %>% # Se separan en dos variables la fecha y la hora
  mutate(dia_año = yday(fecha),
         dia_semana = wday(fecha, week_start = 1) %>% # Se crea una nueva variable para el día del año
           factor(levels = 1:7,
                  labels = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))) %>% 
  separate(fecha, c("año", "mes", "dia"), "-") %>%
  mutate(franja_horaria=str_sub(hora,1,2),
         mes=factor(mes, levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), 
                    labels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))) %>% 

  # Eliminar las canciones del 2023
  filter(año=="2024") %>% 
  
  #Cambiar el nombre a las variables del cantante y la canción
  rename(Cantante=artistName,
         Canción=trackName) %>% 
  
  # Cambiar la variable del tiempo de escucha a segundos, minutos y horas
  mutate(Segundos=msPlayed/1000,
         Minutos=Segundos/60,
         Horas=Minutos/60) %>%

  # Eliminar las escuchas de menos de 20 segundos (canciones que has saltado)
  filter(Segundos>20)
