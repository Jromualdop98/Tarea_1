library(tidyverse)
library(stringr)

# 1.Calcula los valores numericos:

#a
num <- 0.3*0.5
den <- 0.3*0.15 + 0.2*0.8 + 0.5*0.12
a <- num/den
round(a, 2)
#b
b <- (5^6/factorial(6))*exp(1)^-5
round(b, 2)
#c
c <- (factorial(20) / (factorial(7) * factorial(20-7))) * (0.4^7) * (0.6^13)
round(c, 2)

# 2.Realiza la siguiente suma:

#a
x <- seq(1,1000)
sum(x)
#b
range <- 0:10
sum_cuad <- function(range){
  cuad <- 2^range
  return(sum(cuad))
}
sum_cuad(range)

#3.El vector grupo representa el grupo al que pertenece una serie de alumnos

#a
length(grupo)
#b
which(grupo == 'A')

#4.El vector nota representa la nota de un examen de los alumnos que están en 
#los grupos del vector grupo.

#a
sum(nota)
#b
mean(nota)
#c
which(nota > 7)
#d
sort(nota, decreasing = T)
#e
which(nota == max(nota))

#5.A partir de los vectores grupo y nota definidos.

#a
sum(nota[1:10])
#b
length(grupo[grupo == 'C'])
#c (nota aprobatoria = 6)
length(nota[nota >= 6])
#d
length(nota[grupo == 'B' & nota >= 6])
#e
aprobados_c <- length(nota[grupo == "C" & nota >= 6])
total_c <- length(grupo[grupo == "C"])
porcentaje <- (aprobados_c / total_c) * 100
#f
which(nota == min(nota))
which(nota == max(nota))
#g
mean(nota[grupo %in% c('A','B') & nota >= 6])

#6. Calcula el percentil 66 de las notas de todos los alumnos, 
#y también de los alumnos del grupo C

quantile(nota, 0.66)
quantile(nota[grupo == 'C'], 0.66)

#7. Un alumno tiene una nota de 4.9. ¿Qué porcentaje, del total de alumnos, 
#tiene una nota menor o igual que la suya? ¿Y qué porcentaje tiene una nota 
#mayor o igual que la suya?

alumno <- 4.9
nota_mayor <- length(nota[nota >= alumno])/length(nota) * 100
nota_menor <- length(nota[nota <= alumno])/length(nota) * 100


#8. Realiza el gráfico de diagramas de caja de las notas de cada grupo, 
#para poder comparar el nivel de cada uno de ellos.

salon <- tibble(nota, grupo)
boxplot(salon$nota ~ salon$grupo)

#9. Si la variable conc recoge la concentración de plomo (en ppm) en el aire 
#de cierta zona durante un día completo

#a.
max(conc)
#b.
sum(conc > 40)
#c.
mean(conc)
#d.
sort(conc) %>% 
  head(10)
#e.
library(dplyr)
library(stringr)

date <- seq(
  as.POSIXct('2020-01-01 00:00'),
  as.POSIXct('2020-01-01 23:55'),
  by = '5 min'
)
hora <- str_sub(date, 12,16)
tabla <- tibble(index = 1:length(conc),hora)
tabla[which(conc == max(conc)),]