install.packages("tidyverse")
library(tidyverse)

library(palmerpenguins)

library(ggthemes)
penguins



glimpse(penguins)

penguins <- penguins %>%  
  rename(body_mass_g = body_mass)

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()
+
  geom_smooth(method = "lm")

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")



ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")


install.packages("ggthemes")
library(ggthemes)

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()


#Exercises

nrow(penguins) #cuento filas
ncol(penguins) #cuento columnas
#1)344 filas, 8 columnas

?penguins

#2) Abro la ayuda pero no me dice qué es cada cosa sino el tipo de variable: bill_depnumeric, bill depth (millimeters)

#3) A priori no se ve una relación pero aparentemente hay dos clústeres.
ggplot(
  data = penguins,
  mapping = aes(y = bill_dep, x = bill_len)
) +
  geom_point()

#4) Se nota que hay una diferencia de tamaño según especie. Quizás convenga hacer el gráfico anterior pero
#con una leyenda que indique colores por especies. O un gráfico por columnas por cada especie con valores promedio.

ggplot(
  data = penguins,
  mapping = aes(y = bill_dep, x = species)
 ) +
  geom_point()

#5) 
ggplot(data = penguins) + 
  geom_point()
#Da error porque faltan los ejes entonces no sabe cómo ubicar los puntos sobre el plano.
#lo arreglaría poniendo mapping.
ggplot(data = penguins,
  mapping = aes("x","y")
)+
  geom_point()
#También habría que indicarle QUÉ quiero plotear.

#6) #na.rm significa que borra lo que tenga datos faltantes. Por defecto te avisa que lo hace.
#Si le das a "TRUE" no te dice nada.
ggplot(
  data = penguins,
  mapping = aes(y = bill_dep, x = species)
) +
  geom_point(na.rm = TRUE)

#7
ggplot(
  data = penguins,
  mapping = aes(y = bill_dep, x = species)
) +
  geom_point(na.rm = TRUE)+
  labs(caption = "Data come from the palmerpenguins package.")

#8 me costó... no encontraba el cambio de método para geom_smooth
penguins 

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g,color = bill_dep)
)+
  geom_point() +
  geom_smooth(method = "loess")


#9
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)


#10 No son diferentes porque les estoy dando información redundante.

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()


ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )  
