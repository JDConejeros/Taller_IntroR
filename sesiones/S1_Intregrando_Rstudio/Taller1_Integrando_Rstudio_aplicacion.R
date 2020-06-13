#**************************************************************************************************************************
############# TALLER 1: Integrando RSTUDIO #################################################################################
#**************************************************************************************************************************

# Facilitador: José Daniel Conejeros 
# Correo: jdconejeros@uc.cl
 
# Apoyo: Constanza Lemus 
# Correo: cplemus@uc.cl 

#**************************************************************************************************************************
# Objetivos ----------------------------------------------------------------
#**************************************************************************************************************************

# Los objetivos para este primer taller son los siguientes 

# 1. Instalar y explorar R-Rstudio en sus computadoras.
# 2. Configurar un espacio de trabajo 
# 3. Comprender la lógica de objetos y operar con ellos 
# 4. Compremder la finalidad de utilizar librerías

#**************************************************************************************************************************
# Tema 1: Primer código ----------------------------------------------------------------
#**************************************************************************************************************************

# 1.1 Punto de partida:  ----------------------------------------------------------------
 
print("Hola __ingresa_tu_nombre__") 

# ¿Cómo agregamos comentarios en R?

# : Comentarios que no se ejecutan como comandos
# + : Sigue el comando en la próxima linea
# ; : Para escribir más de una función en la misma línea

# 1.1 Aritmética Básica  ----------------------------------------------------------------

2+3 #Suma
2-3 #Resta
2*3 #Multiplicación
2/3 #División
potencia <- 2^3 #Potencia
 
#**************************************************************************************************************************
# Tema 2: Objetos ----------------------------------------------------------------
#**************************************************************************************************************************

# 2.1 Vectores y variables  ----------------------------------------------------------------

c(1) #Vector de un elemento
c(1, 2 , 3 , 4) #Crear un vector de números
c(1, "hola" , "chao" , 4) #Crear un vector números y carácteres

#Secuencia de valores
1:4
4:1
?seq
seq(1, 4) 
seq(4, 1)
-1:2
seq(-1, 2)
seq(2, 8, by=2)
seq(0, 1, by=0.1)
seq(0, 1, length=11)
rep(0:1, 10)
?length

#Variables 
numeros_palabras <- c(1, "hola" , "chao" , 4)
numeros_palabras

secuencia <- seq(0, 1, length=11)
secuencia

#Operaciones aritméticas con vectores
c(1, 2, 3, 4)/2
(1:4)/2
(1:4)*(4:1)

log(c(0.1, 1, 10, 100), base=10)

c(1, 2 , 3 , 4) + c(4, 3)
c(1, 2 , 3 , 4) + c(4, 3, 2) #Produce warning cuando no son múltiplos
(1:4)*(1:6) #Warning
(1:4)*(1:2)

#Operaciones aritméticas con variables 
secuencia <- seq(0, 1, length=11)
secuencia
secuencia <- secuencia*secuencia
secuencia

promedio <- sum(secuencia)/11
promedio

promedio2 <- mean(secuencia) 
promedio2


# 2.2 Factores  ----------------------------------------------------------------

#Almacenamiento de variables categóricas 
#¿Cuál es la diferencia con una variable continua?
#Dummy
sexo <- c("Femenino", "Masculino", "Masculino", "Femenino")
sexo 
sexo[3] 

#Nominales
animales <- c("Elephant", "Giraffe", "Donkey", "Horse")
animales
animales[1]

#Ordinal 
acuerdo <- c("Muy en Desacuerdo", "Desacuerdo", "Ni en desacuerdo", "Ni en desacuerdo" , 
             "Deacuerdo", "Muy deacuerdo")
acuerdo[5]

#¿Cuál es el problema con estas variables?
class(sexo)

#Debemos crear factores numéricos con etiquetas
sexo <- factor(c(0,1,1,0))
sexo
#Generamos etiquetas
sexo <- factor(sexo, labels = c("Mujer", "Hombre"))
sexo

#**************************************************************************************************************************
# Tema 3: Matrices y listas   ----------------------------------------------------------------
#**************************************************************************************************************************

# 3.1 Matrices  ----------------------------------------------------------------

#Ejemplo de matriz
matrix(1:9,3,3) #Matriz de 3filasx3columnas con valores del 1 al 9

#Matrices como objetos
x <- matrix(1:9,3,3)
x

y <- matrix(1:8,2,4,byrow = FALSE) #Genera una matriz con 2 filas y 4 columnas
y

z <- matrix(1:8,2,4,byrow = TRUE) #Genera la matriz completándola por filas
z
?matrix

#Podemos construir una matriz con datos
edad <- c(23, 45, 67, 89)
sexo <- c(1, 0, 1, 0)
peso <- c(80, 60, 70, 50)
altura <- c(180, 160, 200, 140)
promedio <- mean(altura)

matriz_a <- cbind(edad, sexo)
matriz_a
matriz_b <- cbind(peso, altura)
matriz_b

#Combinar matrices
matriz <- cbind(matriz_a, matriz_b)
matriz
matriz[1,2] #Podemos ver el elemento 1 de la columna 2

#Operaciones matemáticas con matrices
y
z
diff <- y - z
diff

sum <- y + z
sum

# 3.2 Listas  ----------------------------------------------------------------

#Creamos una lista
#matriz, valor, vector numérico, vetor de caracteres
objeto <- list(matriz, promedio, sexo, animales) 
objeto

#Vemos el elemento cuatro de la lista (animales)
objeto[[4]]

#**************************************************************************************************************************
# Tema 4: Explorar funciones y argumentos  ----------------------------------------------------------------
#**************************************************************************************************************************

# 4.1 Funciones  ----------------------------------------------------------------

log(1000) #Por DEFAULT es logaritmo natural, en base a euler 2,718.
log(2,718) #Cercano a 1 porque euler elevado a 1 = euler
log(100, base=10)
log10(100)

# 4.2 Argumentos  ----------------------------------------------------------------

# Podemos explorar los argumentos de una función 
help("log") #Para saber argumentos (elementos de la función)
?log
args("log")

?sum
args("sum")

# 4.3  Podemos crear nuestras propias funciones ----------------------------------

fun <- function(x){
  result <- round(sqrt(x /10), digits = 1)
  return(result)
}

fun2 <- function(x){mean(x)}

#Usamos las funciones
fun(100)
fun2(100)

#**************************************************************************************************************************
# Tema 5: Uso de librerías/paquetes  ----------------------------------------------------------------
#**************************************************************************************************************************

rm(list = ls()) #Limpiamos la memoria
library() #Puedo revisar los paquetes instalados
install.packages("libreria") #Para instalar
#Las librerías se instalan sólo una vez, pero 
#deben ser cargadas si se quieren utilizar en la sesión de trabajo
library("libreria") 

install.packages("dplyr") #Para manipulación de datos
install.packages("car") #"Companion to Applied Regression" (Fox & Weisberg)
library(dplyr)

#Sólo la primera vez. Este es un paquete que nos permite administrar otros paquetes.
install.packages("pacman") 
library(pacman)
pacman::p_load(dplyr, car)
pacman::p_load(dplyr,
               car) #Cada vez
#El camino más tradicional:

library(help = "dplyr")

library(help = "base") #Funciones base de R
library(dplyr)
library(car)
search() #Revisamos los paquetes y herramientas instaladas


#**************************************************************************************************************************
# Tema 6: Ejercicio Práctico  ----------------------------------------------------------------
#**************************************************************************************************************************

# 1. Genere su propia base de datos a partir de la tabla disponible con los casos de coronavirus para cada región del país

base <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto4/2020-06-11-CasosConfirmados-totalRegional.csv")[1:17,1:2]

View(base)
colnames(base)

colnames(base) <- c("region", "casos")

# 2. A partir de los datos genere un vector con la diferencia entre el valor para cada región y el promedio nacional (llamelo dif). 

media <- sum(base$casos)/nrow(base) 

mean(base$casos) #¿ Hay algún problema con este promedio?

# Ajustamos nuestra base a los datos de interés
base <- base[-17, 1:2] 

base <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto4/2020-06-11-CasosConfirmados-totalRegional.csv")[1:16,1:2]

# Estimamos las diferencias

dif <- (base$casos - media)

base$dif <- (base$casos - media)

head(base, n=16)

# 3. Agregue este último vector a su base de datos y luego estime otro vector con las diferencias calculadas al cuadrado (llamelo dif_2). 

dif_2 <- (base$dif)^2
base <- cbind(base, dif_2)

# 4. Aplique y explique el siguiente código: 
resultado2 <- sum(dif_2)/(nrow(base)-1)
base <- cbind(base, resultado2)

head(base, n=16)

# 5. Estime la raiz cuadrada del objeto resultado. ¿Qué representa esta estimación?
sd <- sqrt(resultado2)

base <- cbind(base, sd)

sd(base$casos)

head(base, n=16)


#**************************************************************************************************************************
# Tema 6: Próximo Taller  ----------------------------------------------------------------
#**************************************************************************************************************************

# Directorio de trabajo

getwd() # Se obtiene el directorio de trabajo actual
setwd("ruta") # Establecer directorio de trabajo

#Windows
setwd("~\Users\josedanielconejeros\Dropbox\Ayudantia_clases\Clases_R\Taller_IntroR") 

#Mac
setwd("~/Users/josedanielconejeros/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR")


############################################################################################################################
############# FIN TALLER 1: Integrando RSTUDIO #############################################################################
############################################################################################################################