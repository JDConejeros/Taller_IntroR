#**************************************************************************************************************************
############# TALLER 4: PROCESAMIENTO DE DATOS CON DPLYR II ###############################################################
#**************************************************************************************************************************

# Facilitador: José Daniel Conejeros 
# Correo: jdconejeros@uc.cl
 
# Apoyo: Constanza Lemus 
# Correo: cplemus@uc.cl 

#**************************************************************************************************************************
# Objetivos ----------------------------------------------------------------
#**************************************************************************************************************************

# Los objetivos para este primer taller son los siguientes 

# 1. Trabajar en la unión de bases de datos 
# 2. Exploración y manejo de casos pérdidos 
# 3. Realizar estadística descriptiva con dplyr
# 4. Realizar comparaciones entre grupos
# 5. Guardar tablas de resultados 

#**************************************************************************************************************************
# Tema 1: Abrir nuestro directorio y explorar la data  ----------------------------------------------------------------
#**************************************************************************************************************************

# Utilizamos este comando para desactivar notación científica 
options(scipen=999)

# 1.1 Directorio de trabajo y fijamos un proyecto ----------------------------------------------------------------

getwd() # Se obtiene el directorio de trabajo actual

# 1.2 Abrimos y exploramos nuestras base de datos ----------------------------------------------------------------
library(readr)
covid <- read_csv("sesiones/S4_Procesamiento2/data/covid_mod.csv")  #Recuerda ajustarlo a tu directorio propio 
ts_h <- read_csv("sesiones/S4_Procesamiento2/data/ts_h.csv")        #Recuerda ajustarlo en tu directorio propio 
ts_m <- read_csv("sesiones/S4_Procesamiento2/data/ts_m.csv")         #Recuerda ajustarlo en tu directorio propio 

covid <- as.data.frame(covid)
ts_h  <- as.data.frame(ts_h)  
ts_m  <- as.data.frame(ts_m)

# 1.3 Exploramos nuestros objetos
# Base de datos covid
dim(covid)     
colnames(covid)
head(covid)

# Ajuste observado
covid$X1 <- NULL
library(dplyr)
# Otros 3 caminos distintos
covid <- covid %>% select(nregion, region, casos) # Forma dplyr
covid <- covid %>% select(2:4)
covid <- covid[,c(2:4)] # Forma R base

# Base de datos termometro social (H)
dim(ts_h)     
colnames(ts_h)
head(ts_h)

# Base de datos termometro social (M)
dim(ts_m)     
colnames(ts_m)
head(ts_m)


#**************************************************************************************************************************
# Tema 2: Uniendo bases de datos  ----------------------------------------------------------------
#**************************************************************************************************************************

#install.packages("dplyr")
library(dplyr)


# 2.1 Ejemplo sencillo  ----------------------------------------------------------------

x <- data.frame(idx = 1:5, letras = letters[1:5])
y <- data.frame(idy = c(2:6,7), num = c(12:16,3))

x
y

# Trae los resultados de las tablas que cumplen con la condición de comparación entre columnas.
x %>% inner_join(y, by=c("idx"="idy")) #Utilizan una lleva para realizar el match

# Trae todos los resultados de las tablas que cumplen con la condición de comparación entre columnas
# y, adicionalmente, trae todos los datos de la tabla de la izquierda.
x %>% left_join(y, by=c("idx"="idy"))

# Trae todos los resultados de las tablas que cumplen con la condición de comparación entre columnas
# y, adicionalmente, trae todos los datos de la tabla de la derecha.
x %>% right_join(y, by=c("idx"="idy"))

# Trae los resultados de las tablas que cumplen con la condición de comparación entre columnas, 
# además de los resultados de las o registros de las tablas de la derecha y la izquierda.
x %>% full_join(y, by=c("idx"="idy"))


# 2.2 Apliquemos con nuestros datos  ----------------------------------------------------------------

# Solo con fines prácticos vamos a cambiar el nombre indicativo de la región 
colnames(covid)[2] <- "region_covid"
colnames(ts_h)[2] <- "region_ts"
colnames(ts_m)[2] <- "region_ts"

ts_h %>% inner_join(covid, by=c("region_ts"="region_covid"))

ts_h %>% left_join(covid, by=c("region_ts"="region_covid"))

ts_h %>% right_join(covid, by=c("region_ts"="region_covid"))

ts_h %>% full_join(covid, by=c("region_ts"="region_covid"))

# ¿Notamos algún cambio?

# ¿Qué pasaría si lo hicieramos al revés?

covid %>% inner_join(ts_h, by=c("region_covid"="region_ts"))

covid %>% left_join(ts_h, by=c("region_covid"="region_ts"))

covid %>% right_join(ts_h, by=c("region_covid"="region_ts"))

covid %>% full_join(ts_h, by=c("region_covid"="region_ts"))

# ¿Quedaron guardados estos elementos?  ¿Qué nos falta?

# 2.3 Ajustando la base final  ----------------------------------------------------------------

# Tenemos que unir todo en una misma base de datos
518 + 560
nrow(ts_h) + nrow(ts_m)
ts <- ts_h %>% add_row(ts_m) #Tidyverse: dplyr/tidyr
ts2 <- rbind(ts_h, ts_m)     #Rbase
rm(ts2)

# Vamos a juntar 
ts %>% full_join(covid, by=c("region_ts"="region_covid"))

ts <- ts %>% full_join(covid, by=c("region_ts"="region_covid"))

# Ahora lo vamos hacer todo en un solo paso (utilidad del %>%)
ts2 <- ts_h %>% 
  add_row(ts_m) %>% 
  full_join(covid, by=c("region_ts"="region_covid"))

#**************************************************************************************************************************
# Tema 3: Exploración y ajuste ----------------------------------------------------------------
#**************************************************************************************************************************

# 3.1 Ajustes para dejar operativa la base de datos  ----------------------------------------------------------------

# Removemos los objetos que no vamos a utilizar 
rm(list = c("covid", "ts_h", "ts_m", "x", "y", "ts2"))

# Ordenamos nuestras variables y sacamos lo que no vamos a utilizar
head(ts)
ts2 <- ts %>% select(region_ts, nregion, sexo, edad, ingreso, d31, d34, casos, factor)
head(ts2)

# Ordenemos de forma descreciente por el ingreso
ts2 <- ts2 %>% arrange(desc(ingreso))
head(ts2)

# Filtre sus casos por las personas que ganan menos de $1.000.000
ts2 <- ts2 %>% filter(ingreso<1000000) 
head(ts2)

# Ahora ordene sus casos por región de forma creciente
ts2 <- ts2 %>% arrange(region_ts)
ts2 <- ts2 %>% arrange(desc(nregion))

head(ts2)

# Genere una variable de tipo factor para el sexo 
ts2 <- ts2 %>% mutate(genero = as.factor(sexo))
class(ts2$sexo)
class(ts2$genero)
levels(ts2$genero)

# Genere otra variable con la razón de los casos de covid y el promedio regional de casos de covid
ts2 <- ts2 %>% 
  group_by(region_ts) %>% 
  mutate(razon_casos = casos/mean(casos))

# ¿Qué podríamos hacer con esto?
ts2 <- ts2 %>% 
  ungroup(region_ts) %>% 
  mutate(razon_casos = casos/mean(casos))

# Calcule el promedio de ingreso por región 
ts2 %>% 
  group_by(nregion) %>% 
  summarise(media_ing = mean(ingreso))

# 3.2 Encadenando funciones  ----------------------------------------------------------------

# Podemos hacer todo esto en una secuencia encadenada de pasos.
ts0 <- ts %>% 
  select(region_ts, nregion, sexo, edad, ingreso, d31, d34, casos, factor) %>% 
  arrange(desc(ingreso)) %>% 
  filter(ingreso<1000000) %>% 
  arrange(region_ts) %>% 
  mutate(genero = as.factor(sexo),
         razon_casos = casos/mean(casos))
  
# ¿Qué ocurre si incluyo el calculo de la media del ingreso por regiones?  
ts3 <- ts %>% 
  select(region_ts, nregion, sexo, edad, ingreso, d31, d34, casos, factor) %>% 
  arrange(desc(ingreso)) %>% 
  filter(ingreso<1000000) %>% 
  arrange(region_ts) %>% 
  mutate(genero = as.factor(sexo),
         razon_casos = casos/mean(casos)) %>% 
  group_by(nregion) %>% 
  summarise(media_ing = mean(ingreso))

# 3.3 Explorando la base final y los missing  ----------------------------------------------------------------

# Removemos lo que no vamos a utilizar
rm(list = c("ts2", "ts3"))

str(ts)
summary(ts)
names(ts)

# ¿Qué podemos concluir?
# Podemos hacer todo esto en una secuencia encadenada de pasos.
ts <- ts %>% 
  select(region_ts, nregion, sexo, edad, ingreso, d31, d34, casos, factor) %>% 
  arrange(desc(ingreso)) %>% 
  arrange(region_ts) %>% 
  mutate(genero = as.factor(sexo),
         razon_casos = casos/mean(casos))

ts <- ts %>% select(1:2,10,9,4:8,11)

#**************************************************************************************************************************
# Tema 4: Estadística Descriptiva ----------------------------------------------------------------
#**************************************************************************************************************************

# 4.1 Calculemos los descriptivos para variables continuas  ----------------------------------------------------------------

# Vamos a estimar estadísticos de tendencia central, desviación y posición 
ts %>%
  summarize(n = n(),
            mean = mean(edad, na.rm = T),
            sd = sd(edad, na.rm = T), 
            min = min(edad, na.rm = T),
            q25 = quantile(edad, probs = 0.25, na.rm = T),
            median = median(edad, na.rm = T),
            q75 = quantile(edad, probs = 0.75, na.rm = T),
            max = max(edad, na.rm = T)
  ) %>%
  rename(Frecuencia=n,
         Promedio=mean,
         Std = sd,
         Min = min,
         Q25 = q25,
         Q50 = median,
         Q75 = q75,
         Max = max
  )  

# ¿Cómo guardamos esto?
edad <- ts %>%
  dplyr::summarise(n = n(),
            mean = mean(edad, na.rm = T),
            sd = sd(edad, na.rm = T), 
            min = min(edad, na.rm = T),
            q25 = quantile(edad, probs = 0.25, na.rm = T),
            median = median(edad, na.rm = T),
            q75 = quantile(edad, probs = 0.75, na.rm = T),
            max = max(edad, na.rm = T)
  ) %>%
  dplyr::rename(Frecuencia=n,
         Promedio=mean,
         Std = sd,
         Min = min,
         Q25 = q25,
         Q50 = median,
         Q75 = q75,
         Max = max
  )  

# Podríamos hacer esto por otro camino
fun <- function(x) {
  list(
    Mean = round(mean(x, na.rm = T),2),
    Sd = round(sd(x, na.rm = T),2),
    Min = round(min(x, na.rm = T),2),
    Q25 = round(quantile(x, probs = 0.25, na.rm = T),2),
    Median = round(as.numeric(median(x, na.rm = T)),2),
    Q75 = round(quantile(x, probs = 0.75, na.rm = T),2),
    Max = round(max(x, na.rm = T)),2)
}

edad2 <- as.data.frame(fun(ts$edad))


# 4.2 Calculemos los descriptivos para variables categóricas  --------------------------------------------------------------
genero <- ts %>%
  group_by(genero) %>%
  summarize(n = n()) %>%
  mutate(pro = round(n/sum(n),2),
         prop = n/sum(n)*100, 
         prop = round(prop,1),
         prop = paste(prop, "%", sep="")) %>%
  rename(Género=genero,
         Frecuencia=n,  
         Proporción=pro,
         Porcentaje=prop)

# 4.3 Descriptivos para grupos 
region <- ts %>% 
  group_by(nregion, genero) %>%
  summarize(n = n()) %>%
  mutate(pro = round(n/sum(n),2),
         prop = n/sum(n)*100, 
         prop = round(prop,1),
         prop = paste(prop, "%", sep="")) %>%
  rename(Región=nregion,
         Género=genero,
         Frecuencia=n,  
         Proporción=pro,
         Porcentaje=prop)

#**************************************************************************************************************************
# Tema 5: Guardar nuestros resultados ----------------------------------------------------------------
#**************************************************************************************************************************

# 5.1 Guardamos las tablas con estadísticos ----------------------------------------------------------------
#install.packages("openxlsx")
library(openxlsx)
write.xlsx(edad, file="sesiones/S4_Procesamiento2/data/tabla_edad.xlsx") #Recuerda ajustarlo a tu directorio propio 
write.xlsx(genero, file="sesiones/S4_Procesamiento2/data/tabla_sexo.xlsx") #Recuerda ajustarlo a tu directorio propio 
write.xlsx(region, file="sesiones/S4_Procesamiento2/data/tabla_region.xlsx") #Recuerda ajustarlo a tu directorio propio 

# 5.2 Guardamos nuestra base de datos para la siguiente sesión -------------------------------------------------------------
#Recuerda ajustarlo a tu directorio propio 
write.csv(ts,  file="sesiones/S4_Procesamiento2/data/ts.csv", row.names = FALSE)

#**************************************************************************************************************************
############# FIN TALLER 4: PROCESAMIENTO DE DATOS CON DPLYR II ###########################################################
#**************************************************************************************************************************