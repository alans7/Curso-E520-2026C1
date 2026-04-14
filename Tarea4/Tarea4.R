install.packages("tidyverse")
install.packages("nycflights13")


library(tidyverse)
library(nycflights13)


#EJERCICIOS 19.2.4

weather
airports
#1) Se puede conectar a partir de "origin" y "faa", sin embargo carece de utilidad
#en este contexto. Tiene más sentido que ambas enriquezcan los datos de la tabla transaccional
#flights que enriquecerse mutuamente.

#2) Si tuvieras toda la info de los aeropuertos de EEUU también podrías hacer conexión con la variable "dest".

#3)  Es un día donde se produce un atraso de hora.
weather |> 
  count(year, month,day,hour, origin) |> 
  filter(n > 1)

#4) Crearía una tabla de "Feriados" que contengan datos por día, mes y año y una descripción de la festividad.
#La fecha de la tabla "feriados" sería la primary key y la fecha de la tabla "flights" sería foreign key.
#Entiendo que se trata de una compundkey porque son 3 variables: año, mes y día.


#5a)
install.packages("Lahman")
library(Lahman)
Batting
Batting |> 
  count(playerID) |> 
  filter(n > 1)

People
People |> 
  count(playerID) |> 
  filter(n > 1)
#playerID es Primary KEY!

Salaries
Salaries |> 
  count(playerID,yearID,lgID, teamID) |> 
  filter(n > 1)
#En salaries sólo se puede establecer una primary KEY COMPUESTA, ya que sólo hay una observación n=1
#Si se tienen en cuenta las variables playerID,yearID,lgID, teamID. Lo único que no forma parte de 
#la primary key es "salary".

#Entonces puedo considerar a "Batting" como la tabla de transacciones. Desde ahí puedo conectar la tabla de People vía
#player ID y la tabla Salaries con la key compuesta  playerID,yearID,lgID, teamID.

Managers
AwardsManagers

#5b)Tiene más sentido mirar como tabla de "hechos" los AwardsManagers y conectarlo con la tabla
#Manager vía playerID, awardID, yearID y lgID. También se puede conectar con People via playerID.
Batting
Pitching
Fielding 


Pitching |> 
  count(playerID,yearID,stint,lgID, teamID, lgID) |> 
  filter(n > 1)

#5c) los datafranes batting, pitcing y fielding son tablas de "hechos", sin embargo como un jugador puede cumplir diversos
#roles en un mismo equipo podría conectarse vía la key compuesta playerID, yearID,stint,teamID,LgID.
#Quizás se quiere realizar una medida de qué jugador es más "balanceado" teniendo en cuenta
#las diferentes competencias en el Baseball y no sólo una. Entonces tendría sentido hacer algún tipo de Join
#para tener todas las estadísticas en un mismo dataframe y así poder operar.



#19.3.4 Exercises

#1
#Primero genero un nuevo dataframe que linkea flights con weather.
flightsW = left_join(flights,weather)

resultado <- flightsW |> 
  group_by(year, month, day) |> 
  summarise(
    delay_total_dia = sum(dep_delay, na.rm = TRUE),
    temp_promedio    = mean(temp, na.rm = TRUE),
    viento_promedio  = mean(wind_speed, na.rm = TRUE),
    precip_total     = sum(precip, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(suma_hoy_y_manana = delay_total_dia + lead(delay_total_dia)) |> 
  arrange(desc(suma_hoy_y_manana))

# Los peores días
head(resultado)
#Cae casi todo en junio y Julio. Supongo que tendrá que ver con algo relativo a vacaciones.

#2
top_dest <- flights2 |>
  count(dest, sort = TRUE) |>
  head(10)
top_dest
#Para encontrar todos los vuelos que van a estos destinos haría un filter join entre flights y top_dest
flights |> 
  semi_join(top_dest, join_by(dest == dest))

#3 Voy a usar un antijoin para buscar estos "baches"
vuelos_sin_clima <- flights |> 
  anti_join(weather, by = c("year", "month", "day", "hour", "origin"))

# Para ver cuántos son
nrow(vuelos_sin_clima)
#son 1556

# Para ver si hay algún patrón (por ejemplo, un mes específico)
vuelos_sin_clima |> 
  count(month, origin, sort = TRUE)
#muchísimo en diciembre. JFK pica en punta.

#4 Voy a usar de nuevo anti join
TailNumberUnmatched <- flights |> 
  anti_join(planes, by = join_by(tailnum))

#y ahora lo ordeno por aerolinea,
TailNumberUnmatched |> 
  count(carrier, sort = TRUE)
#Se ve que hay dos muy problemáticas MQ y AA


#5
library(nycflights13)
library(dplyr)
library(stringr)

# Creamos el resumen de carriers por cada avión
carrier_por_avion <- flights |> 
  filter(!is.na(tailnum)) |>              # Limpiamos los que no tienen patente
  distinct(tailnum, carrier) |>           # Nos quedamos con pares únicos (avión-aerolínea)
  group_by(tailnum) |> 
  summarise(
    lista_carriers = str_flatten(sort(carrier), collapse = ", ")
  )

# Lo pegamos a la tabla planes
planes_actualizada <- planes |> 
  left_join(carrier_por_avion, by = "tailnum")

# Verificamos el resultado
planes_actualizada |> 
  select(tailnum, manufacturer, lista_carriers) |> 
  filter(str_detect(lista_carriers, ",")) # Vemos los aviones que tuvieron más de una

#6 Voy a hacer una sucesión de dos Joins. Uno para origin y otro para dest

flights_1<- flights |> 
  left_join(
    airports |> select(faa, lat_ori = lat, long_ori = lon), 
    by = c("origin" = "faa")
  )
flights_final<- flights_1 |> 
  left_join(
    airports |> select(faa, lat_dest = lat, long_dest = lon), 
    by = c("dest" = "faa")
  )

# Probamos el resultado
flights_final |> 
  select(origin, lat_ori, long_ori,dest,lat_dest,long_dest) |> 
  head()

#Tiene más sentido ponerle el nombre directamente en el Select.

#7 

delay_por_destino <- flights |> 
  group_by(dest) |> 
  summarise(
    delay_promedio = mean(arr_delay, na.rm = TRUE),
    cantidad_vuelos = n()
  ) |> 
  arrange(desc(delay_promedio))


library(nycflights13)
library(dplyr)
library(ggplot2)


# . Calculamos el delay promedio por DESTINO
delay_destinos <- flights |> 
  group_by(dest) |> 
  summarise(delay_promedio = mean(arr_delay, na.rm = TRUE))

#  Unimos y graficamos
airports |> 
  # Usamos inner_join para traer la columna delay_promedio
  inner_join(delay_destinos, by = c("faa" = "dest")) |> 
  ggplot(aes(x = lon, y = lat)) +
  borders("state") +
  # Mapeamos color y tamaño al delay
  geom_point(aes(color = delay_promedio, size = delay_promedio), alpha = 0.7) +
  # Usamos una escala de colores que se entienda bien (Viridis es genial)
  scale_color_viridis_c(option = "magma") +
  coord_quickmap() +
  theme_minimal() +
  labs(
    title = "Mapa de Delays por Aeropuerto de Destino",
    subtitle = "Vuelos saliendo de NYC en 2013",
    color = "Delay (min)",
    size = "Delay (min)"
  )

#8 
library(nycflights13)
library(dplyr)
library(ggplot2)

# 1. Filtramos por fecha y calculamos el delay promedio por DESTINO
delay_destinos_dia <- flights |> 
  filter(year == 2013, month == 6, day == 13) |> # <--- Filtro específico
  group_by(dest) |> 
  summarise(
    delay_promedio = mean(arr_delay, na.rm = TRUE),
    n_vuelos = n()
  )

# 2. Unimos y graficamos
airports |> 
  inner_join(delay_destinos_dia, by = c("faa" = "dest")) |> 
  ggplot(aes(x = lon, y = lat)) +
  borders("state") +
  geom_point(aes(color = delay_promedio, size = delay_promedio), alpha = 0.8) +
  scale_color_viridis_c(option = "magma") +
  coord_quickmap() +
  theme_minimal() +
  labs(
    title = "Delays por Destino: 13 de Junio de 2013",
    subtitle = "Vuelos saliendo de NYC (EWR, JFK, LGA)",
    color = "Delay Promedio (min)",
    size = "Cantidad de Vuelos",
    x = "Longitud",
    y = "Latitud"
  )


git remote add origin https://github.com/alans7/Curso-E520-2026C1.git
git push -u origin main
