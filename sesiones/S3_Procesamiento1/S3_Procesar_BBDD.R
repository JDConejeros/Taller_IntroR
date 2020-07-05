#**************************************************************************************************************************
############# TALLER 3: PROCESAMIENTO DE DATOS CON DPLYR ###################################################################
#**************************************************************************************************************************

# Facilitador: José Daniel Conejeros 
# Correo: jdconejeros@uc.cl
 
# Apoyo: Constanza Lemus 
# Correo: cplemus@uc.cl 

#**************************************************************************************************************************
# Objetivos ----------------------------------------------------------------
#**************************************************************************************************************************

# Los objetivos para este primer taller son los siguientes 

# 1. Definir un proyecto de trabajo en R
# 2. Encadenar operadores con el operador %>% 
# 3. Filtrar y ordenar una base de datos
# 4. Seleccionar Columnas
# 5. Generar nuevas variables
# 6. Recodificar y etiquetar variables 

#**************************************************************************************************************************
# Tema 1: Fijar un proyecto de trabajo  ----------------------------------------------------------------
#**************************************************************************************************************************

# Utilizamos este comando para desactivar notación científica 
options(scipen=999)

# 1.1 Directorio de trabajo y fijamos un proyecto ----------------------------------------------------------------

getwd() # Se obtiene el directorio de trabajo actual

# 1.2 Abrimos y exploramos nuestras base de datos ----------------------------------------------------------------
#install.packages("haven")
library(haven)
ts <- read_dta("TS3.dta")
ts <- as.data.frame(ts)

covid <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto4/2020-06-25-CasosConfirmados-totalRegional.csv")[1:17,1:8]

library(readr)
covid <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto4/2020-06-25-CasosConfirmados-totalRegional.csv")

covid <- as.data.frame(covid)

# 1.3 Exploramos nuestros objetos
# Base de datos termometro social
dim(ts)     
names(ts)   
colnames(ts)
head(ts)

#install.packages("vtable")
library(vtable)
vtable(ts, out="browser", index=TRUE, missing = TRUE)
?vtable

#install.packages("skimr")
library(skimr)
skim(ts)
summary(ts)

#install.packages("psych")
library(psych)
multi.hist(ts[123:127])

# Base de datos covid regional
dim(covid)     
names(covid)   
head(covid, n=17)    
vtable(covid, index=T, missing = T)
skim(covid)
summary(covid)
multi.hist(covid[2:7])

#**************************************************************************************************************************
# Tema 2: Preparar nuestras bases de datos ----------------------------------------------------------------
#**************************************************************************************************************************

# Vamos a utilizar solo una cantidad de variables en ambas bases de datos 
# Folio (folio)
# Sexo (a2)
# Región (region)
# Ingreso (f4)
# Lavarse las manos (d31)
# Permanecer en el hogar (d34)
# Factor de expansión

# De nuestra base de datos covid
# Región (Region)
# Casos activos confirmados (Casos.activos.confirmados)

# 2.1 Ajustamos los nombres de nuestras variables ----------------------------------------------------------------

colnames(ts) <- tolower(colnames(ts))

colnames(covid) <- tolower(colnames(covid))

# 2.2 Seleccionamos nuestras variables encadenando funciones --------------------------------------------------------------
#install.packages("dplyr")
library(dplyr)
# Termómetro social 
ts2 <- ts %>% select(folio, region, a2, f4, edad, d31, d34)
rm(ts2)
ts <- ts %>% select(folio, region, a2, f4, edad, d31, d34, factor)
# Variables auxiliares 
ts$dup <- duplicated(ts$folio)
table(ts$dup, useNA = "ifany")
ts$dup <- NULL # Eliminar variables

#Covid 
covid2 <- covid %>% select(1,8)
covid <- covid[,c(1,8)] 
# Hay que eliminar la última fila de los totales y ajustar el nombre
covid <- covid[-17,]
covid <- covid[1:16,]

rm(covid2)
# Pueden recurrir al head para ir viendo resultados

#**************************************************************************************************************************
# Tema 3: Modificar y unir nuestras dos bases de datos  ----------------------------------------------------------------
#**************************************************************************************************************************

# 3.1 ¿Qué pasa con la variable region en cada base de datos? -------------------------------------------------------------

table(ts$region) # Podemos ver el libro de códigos (en este caso tuve que hacer un labelbook en stata)
class(ts$region)
#También podría ajustar el parámetro label = attr(data, "label")
table(covid$region)
ts$region <- as.factor(ts$region) #Coerción explícita

#1 =Tarapaca
#2 =Antofagasta
#3 =Atacama
#4 =Coquimbo
#5 =Valparaiso
#6 =O'Higgins
#7 =Maule
#8 =Bio Bio
#9 =Araucania
#10 =Los Lagos
#11 =Aysen
#12 =Magallanes
#13 =Metropolitana
#14 =Los Rios
#15 =Arica y Parinacota
#16 =Ñuble

typeof(covid$region)
class(covid$region)
levels(covid$region)

covid$region2 <- as.factor(covid$region)
class(covid$region2)
levels(covid$region2)
covid$region2[1]

covid$aux <- as.numeric(covid$region2)

covid <- covid %>% arrange(aux) #Ordena por una variable

# Generar los códigos correctos
covid$region2 <- NA
covid$region2[covid$aux==1] <- 2     #Antofagasta
covid$region2[covid$aux==2] <- 9     #Araucanía
covid$region2[covid$aux==3] <- 15    #Arica y Parinacota
covid$region2[covid$aux==4] <- 3     #Atacama
covid$region2[covid$aux==5] <- 11    #Aysén
covid$region2[covid$aux==6] <- 8     #Bio-bío
covid$region2[covid$aux==7] <- 4     #Coquimbo
covid$region2[covid$aux==8] <- 10    #Los lagos
covid$region2[covid$aux==9] <- 14    #Los Ríos
covid$region2[covid$aux==10] <- 12   #Magallanes
covid$region2[covid$aux==11] <- 7    #Maule
covid$region2[covid$aux==12] <- 13   #Metropolitana
covid$region2[covid$aux==13] <- 16   #Ñuble
covid$region2[covid$aux==14] <- 6    #Ohiggins  
covid$region2[covid$aux==15] <- 1    #Tarapacá
covid$region2[covid$aux==16] <- 5    #Valparaíso

# Ordenamos
covid <- covid %>% arrange(region2)
covid <- covid %>% arrange(desc(region2))

# Seleccionamos 
covid <- covid %>% select(1,3,2)

# Ajustamos los nombres
colnames(covid) <- c("nregion", "region", "casos")
colnames(covid)

# Vemos nuestro resultado
head(covid, n=16)

#**************************************************************************************************************************
# Tema 4: Ajustar variables de una base de datos  ----------------------------------------------
#**************************************************************************************************************************

# 4.1 Generar nuevas variables: Sexo  ----------------------------------------------------------------

# Sexo:
class(ts$a2)
typeof(ts$a2)
table(ts$a2, useNA = "ifany")

###
# a. Generar y recodificar camino 1: Indexación 
###

ts$sexo <- as.numeric(ts$a2) #Coerción explícita 
table(ts$sexo)
class(ts$sexo)

ts$sexo[ts$sexo==1] <- 0 #Hombre
ts$sexo[ts$sexo==2] <- 1 #Mujer

# Verificamos la recodificación 
table(ts$sexo, ts$a2)

# Esta variable R la entiende como un número, entonces hay que ajustar esto para que sea un factor con categorías cuali
# Etiquetamos 
ts$sexo <- factor(ts$sexo, 
                  levels = c(0,1), 
                  labels = c("Hombre", "Mujer"))
class(ts$sexo)
typeof(ts$sexo)

###
# b. Mismos procedimiento con dplyr
###

#install.packages("dplyr")
library(dplyr)

ts <- ts %>% 
  mutate(sexo2 = as.factor(a2),
         sexo2 = recode(sexo2, "1" = 0, "2" = 1))

ts <- ts %>% 
  mutate(sexo3 = if_else(a2==2, 1, 0))  

# Recodificamos
ts$sexo2 <-  factor(ts$sexo2, 
         levels = c(0,1), 
         labels = c("Male", "Female"))

# Nos quedamos con nuestra variable inicial (proceso alternativo al select)
ts <- ts[,c(-10,-11)]

# 4.2 Generar nuevas variables: Ingreso  ----------------------------------------------------------------

table(ts$f4, useNA = "ifany") # ¿Qué podemos observar?
class(ts$f4)
typeof(ts$f4)

# Generamos la nueva variable
ts$ingreso <- as.numeric(ts$f4)
table(ts$ingreso, useNA = "ifany") # ¿Qué podemos observar?
class(ts$ingreso)
typeof(ts$ingreso)

# Revisamos los missing
which(is.na(ts$ingreso)) # Indica la fila 
table(is.na(ts$ingreso))

# Recodificamos los missing
ts$ingreso[ts$ingreso==-888] <- NA
ts$ingreso[ts$ingreso==-999] <- NA
table(is.na(ts$ingreso))
table(ts$ingreso, useNA = "ifany")

#install.packages("Hmisc")
library(Hmisc)
Hmisc::describe(ts$ingreso)
summary(ts$ingreso)
hist(ts$ingreso)

# Vamos a imputar el promedio de ingreso para no perder observaciones ¿Sería lo más apropiado?
ts$ingreso[is.na(ts$ingreso)] <- median(ts$ingreso, na.rm = T)
table(ts$ingreso, useNA = "ifany")

# 4.3 Generar nuevas variables: edad  ----------------------------------------------------------------
#install.packages("Hmisc")
#library(Hmisc)
Hmisc::describe(ts$edad)
summary(ts$edad)
class(ts$edad)
typeof(ts$edad)

# ¿Será necesario generar una nueva varible?
ts$edad <- as.numeric(ts$edad)

#**************************************************************************************************************************
# Tema 5: Vamos a filtrar nuestra base de datos  ----------------------------------------------------------------
#**************************************************************************************************************************

# Genero
table(ts$sexo)
ts_h <- ts %>% filter(sexo=="Hombre")
ts_m <- ts %>% filter(sexo=="Mujer")

# 50% superior de la mediana de ingreso
describe(ts$ingreso)
ts_sup <- ts %>% filter(ingreso>=401500)
describe(ts_sup$ingreso)

#**************************************************************************************************************************
# Tema 6: Vamos a exportar nuestra información para la próxima sesión  -----------------------------------------------------
#**************************************************************************************************************************
# Base Covid en formato dta
write.csv(covid, "covid_mod.csv")

# Base ts_h en formato csv 
write.csv(ts_h, file="ts_h.csv", row.names = FALSE)

# Base ts_m en formato txt
write.csv(ts_m,  file="ts_m.csv", row.names = FALSE)

#**************************************************************************************************************************
############# FIN TALLER 3: PROCESAMIENTO DE DATOS CON DPLYR ###############################################################
#**************************************************************************************************************************