library(tidyverse)

#1.Graficar los puntos
x <- 1:10
y <- x^2
plot(x,y)

#2. Ingresa la matriz A en Rstudio
a1 <- seq(1,3)
a2 <- seq(2,6,2)
a3 <- seq(3,9,3)
a4 <- seq(4,12,4)
matrix_A<- rbind(a1,a2,a3,a4)

#3. Ingresa la matriz identidad de tamaño 3I
I <- diag(3)

#4. Crea una función que cree una matriz nula ingresando las dimensiones
matriz_nula <- function(r,c){
  return(diag(0,r,c));
}
matriz_nula(6,6)

#5. Modificar la matriz diag(4), para que se parezca a la matriz B 
matrix_B <- diag(4)
matrix_B[1,1] <- 0
matrix_B[2,2] <- 2
matrix_B[3,3] <- 3
matrix_B[4,4] <- 4
matrix_B

#6. Obtener la matriz transpuesta de A (ejercicio 2)
trans_A <- t(matrix_A)

#7.Realizar las siguientes operaciones
#a. Suma
dim(matrix_A)
dim(matrix_B)
matrix_A + matrix_B #Error: "Dos matrices tienen que tener un número igual de 
#filas y columnas para poder sumarlas."

#b. Resta
matrix_A - matrix_B #Error: "Dos matrices tienen que tener un número igual de 
#filas y columnas para poder sumarlas."

#c. 3B
3*matrix_B

#d. A*B
matrix_A %*% matrix_B #Error: Solo puede multiplicar dos matrices si el número de 
#columnas en la primera matriz es igual al número de renglones en la segunda matriz.

#8. Crea una función para calcular P^6
P <- rbind(c(1,2,3),c(-2,4,-2),c(1,0,1))

potencia_M <-function(M,n){
  Mi=M
  for(i in 2:n){
    Mi=Mi%*%M};
  print(Mi)}

potencia_M(P,6)

#9. Resolver el sistema de ecuaciones
matrix_sist <- matrix(c(3,-1,1,9,-2,1,3,1,-2), nrow = 3, byrow= T)
matrix_sol <- c(-1,-9,-9)
solve(matrix_sist,matrix_sol)

#10. Utilizando la ayuda de R, investigue para qué sirven las funciones eigen() y det()
eigen()
det()

#11. Calcular A.B - AB^t
b1 <- seq(1,10)
b2 <- seq(2,20,2)
b3 <- seq(3,30,3)
b4 <- seq(4,40,4)
b5 <- seq(5,50,5)
B <- cbind(b1,b2,b3,b4,b5)

A <- matrix(c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,0,1,1,0,1,1,0), nrow = 5, byrow = T)

#A %*% B # No se puede porque no tienen las mismas dimensiones 
t_B <- t(B)       
A %*% t_B #Resultado final !

#12.
X <- matrix(c(1,1,1,-1,1,0,1,1,1,2), ncol = 2, byrow = T)
Y <- matrix(c(0,0,1,1,3))
X_t <- t(X)

beta <- (solve(X_t %*% X) %*% X_t) %*% Y

#13. Corre el siguiente código para cargar los vectores year y co2 en memoria
data(co2)
means = aggregate(co2, FUN=mean)
year = as.vector(time(means))
co2 = as.vector(means)

dif <- co2 - dplyr::lag(co2)

dif_2020_2019 <- 2.64
year_2020 <- 2020

plot(x = year, y = dif, 
     type = "o", 
     xlab = "Año", 
     ylab = "CO2 aumento por año",
     xlim = c(1960, 2020),
     ylim = c(0.2, 2.7))
points(x = year_2020, y = dif_2020_2019, pch = 4, col = "red")

#14.
rainfall <- read.csv('Data/rainfall.csv')
result <- 
  rainfall %>%
  mutate(max = apply(result[3:11], 1, max)) %>%
  filter(max >= 180) %>% 
  select(name) %>% 
  as.vector()

