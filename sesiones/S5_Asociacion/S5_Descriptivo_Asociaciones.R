#**************************************************************************************************************************
############# TALLER 5: DESCRIPTIVOS Y ASOCIACIÓN DE VARIABLES  ###########################################################
#**************************************************************************************************************************

# Facilitador: José Daniel Conejeros 
# Correo: jdconejeros@uc.cl
 
# Apoyo: Constanza Lemus 
# Correo: cplemus@uc.cl 

#**************************************************************************************************************************
# Objetivos ----------------------------------------------------------------
#**************************************************************************************************************************

# Los objetivos para este primer taller son los siguientes 

# 1. Trabajar análisis descriptivos en R 
# 2. Replicación de algunas cifras
# 3. Asociación para variables continuas 
# 4. Asociación para variables categóricas
# 5. Exploración de un modelo de regresión lineal simple

#**************************************************************************************************************************
# Tema 1: Abrir nuestro directorio y explorar la data  ----------------------------------------------------------------
#**************************************************************************************************************************

# Utilizamos este comando para desactivar notación científica 
options(scipen=999)

# 1.1 Directorio de trabajo y fijamos un proyecto ----------------------------------------------------------------

getwd() # Se obtiene el directorio de trabajo actual

# 1.2 Abrimos y exploramos nuestras base de datos ----------------------------------------------------------------
library(readr)
ts <- read_csv("sesiones/S5_Asociacion/ts.csv")        #Recuerda ajustarlo en tu directorio propio 

# 1.3 Exploramos nuestra base ----------------------------------------------------------------
# Base de datos covid
dim(ts)     
colnames(ts)
head(ts)
str(ts)

#install.packages("vtable")
library(vtable)
vtable(ts, out="viewer", index=TRUE, missing = TRUE)

# 1.4 Ajustamos nuestras variables ----------------------------------------------------------------
summary(ts)
table(ts$nregion)
table(ts$genero)
table(ts$d31)
table(ts$d34)

#install.packages("dplyr")
library(dplyr)

ts <- ts %>% 
  mutate(genero = factor(genero), 
         nom_region = factor(nregion), 
         lavar_manos = factor(d31, exclude = 3),
         casa = factor(d34, exclude = 3))

str(ts)
table(ts$nregion, ts$nom_region, useNA = "ifany")
table(ts$d31, ts$lavar_manos, useNA = "ifany")
table(ts$d34, ts$casa, useNA = "ifany")

# 1.5 Seleccionamos las variables de interés ----------------------------------------------------------------

ts <- ts %>% select(nom_region, genero, edad, ingreso, lavar_manos, casa, casos, factor) %>% 
  as.data.frame()
str(ts)
         
#**************************************************************************************************************************
# Tema 2: Análisis Descriptivos  ----------------------------------------------------------------
#**************************************************************************************************************************

ts <- as.data.frame(ts) # Generamos un data frame 

# 2.1 Exploración Descriptiva con R base ----------------------------------------------------------------
summary(ts)

#install.packages("Hmiscc")
library(Hmisc)
Hmisc::describe(ts)

# 2.2 Usemos la librería de stargazer ----------------------------------------------------------------
#install.packages("stargazer")
library(stargazer)
stargazer(ts, type="text", title = "Estadísticos Descriptivos")
stargazer(ts, type="text", title = "Estadísticos Descriptivos", out="sesiones/S5_Asociacion/tablas/descriptivos.txt")
stargazer(ts, type="latex", title = "Estadísticos Descriptivos", out="sesiones/S5_Asociacion/tablas/descriptivos2.tex")

# 2.3 También podríamos hacer esto con dplyr: Descriptivos para variables continuas ---------------------------------------
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

# 2.4 Descriptivos para variables categóricas ----------------------------------------------------------------
tabla_genero <- table(ts$genero)
prop.table(tabla_genero)
prop.table(tabla_genero)*100
round(prop.table(tabla_genero)*100, 1)
paste0(round(prop.table(tabla_genero)*100, 2), "%")

# A la dplyr: 
tabla_genero2 <- ts %>%
  group_by(genero) %>%
  summarize(n = n()) %>%
  mutate(prop = round(n/sum(n),2),
         porc = n/sum(n)*100, 
         porc = round(porc,1),
         porc = paste(porc, "%", sep="")) %>%
  rename(Género=genero,
         Frecuencia=n,  
         Proporción=prop,
         Porcentaje=porc)

# 2.5 Podríamos construir una función para obtener las estimaciones ------------------------------------------

summary_replica <- function(x) {
  tabla <- round(c(Mean = mean(x, na.rm = T),
                   Sd = sd(x, na.rm = T),
                   Min = min(x, na.rm = T),
                   Q25 = quantile(x, probs = 0.25, na.rm = T),
                   Median = as.numeric(median(x, na.rm = T)),
                   Q75 = quantile(x, probs = 0.75, na.rm = T),
                   Max = max(x, na.rm = T),
                   n =length(x)), 2)
  return(tabla)
}

summary_replica(ts$ingreso)
summary_replica(ts$casos)
summary_replica(ts$genero)

#**************************************************************************************************************************
# Tema 3: Replicación de resultados  ----------------------------------------------------------------
#**************************************************************************************************************************

# 3.1 Vamos a indicarle a R que vamos a trabajar con un diseño ponderados
# Primero vamos a crear un identificador de casos: 
id <- c(1:nrow(ts))

#install.packages("survey")
library(survey)
ts_w <- svydesign(ids = ~1, data = ts, weights = ts$factor)
?svydesign
summary(ts_w)

# 3.1 Vamos a generar estimaciones ponderadas para sexo ---------------------------------------------------------------
prop.table(table(ts$genero))
prop.table(svytable(~genero, design = ts_w))

# Como aparece en la presentación
tabla_genero <- as.data.frame(round((prop.table(svytable(~genero, design = ts_w)))*100,1))
tabla_genero$Freq <- paste0(tabla_genero$Freq, "%")
tabla_genero


# 3.2 Vamos a generar estimaciones ponderadas para grupos etarios -----------------------------------------------------
# 1: 18-35
# 2: 36-60
# 3: >60

ts <- ts %>% 
  mutate(etario = if_else(edad>=18 & edad<=35, 1, if_else(edad>36 & edad<=60, 2, 3)))
table(ts$edad, ts$etario, useNA = "ifany")

ts_w <- svydesign(ids = ~1, data = ts, weights = ts$factor)

prop.table(table(ts$etario))
prop.table(svytable(~etario, design = ts_w))

# Como aparece en la presentación
tabla_etario <- as.data.frame(round((prop.table(svytable(~etario, design = ts_w)))*100,1))
tabla_etario$Freq <- paste0(tabla_etario$Freq, "%")
tabla_etario

# 3.3 Otros caminos para hacer lo mismo: -----------------------------------------------------
# Sexo
ts$sexo <- as.numeric(ts$genero=="Mujer") # Genero una dummy
mujer <- weighted.mean(ts$sexo, ts$factor, na.rm = T) # Obtengo un promedio ponderado
hombre <- (1-mujer)

# Grupo etario 
#install.packages("srvyr")
library(srvyr)
ts_w2 <-  ts %>%
  as_survey_design(weights = factor)

tabla_etario <- ts_w2 %>%
  group_by(as.factor(ts_w2[["variables"]][["etario"]])) %>% 
  summarize(proportion = survey_mean(,na.rm=TRUE, vartype = NULL))

tabla_etario

tabla_etario$proportion <-  paste0(round((tabla_etario$proportion)*100,1),"%")
tabla_etario

#**************************************************************************************************************************
# Tema 4: Asociación para variables continuas  ----------------------------------------------------------------
#**************************************************************************************************************************

## 4.1 Correlaciones: asociación lineal entre esas dos variables -------------------------

ts$edad_2 <- (ts$edad)^2 
ts$ingreso_2 <- log(ts$ingreso)

ts$ingreso_2[ts$ingreso_2=="-Inf"] <- 0

# Estimamos las correlaciones
cor(ts$ingreso, ts$edad)
cor(ts$ingreso, ts$edad_2)

cor(ts$ingreso_2, ts$edad)
cor(ts$ingreso_2, ts$edad_2)

# Graficamos
par(mfrow=c(1,2))
plot(ts$ingreso, ts$edad)
plot(ts$ingreso, ts$edad^2)


## 4.2 Correlaciones: Matrices -----------------------------------------------------

# Matriz
round(cor(ts[, c(3,4,11,12)]),2)
pairs(ts[, c(3,4,11,12)])

install.packages("corrplot")
library(corrplot)
# Matris de correlaciones de pearson  
cor <- cor(ts[, c(3,4,11,12)])
corrplot(cor, method = "shade", shade.col = NA, type = "lower", addCoef.col = "black")

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(ts[, c(3,4,11,12)], histogram = T, pch=19)

#Gráfico de asociación 
#install.packages("ggplot2")
#install.packages("ggpubr")
library(ggplot2)
library(ggpubr)
ggplot(ts, aes(x=ingreso,y=edad_2)) +
  geom_point(position=position_jitter(w=0.1,h=0)) +
  geom_smooth(method="lm", se=FALSE) +
  xlab('Ingreso') +
  ylab('Edad al cuadrado') + theme_bw() +
  stat_cor(label.x = 3000000, label.y = 7500) 

# Otra manera
ggscatter(ts, x = "ingreso", y = "edad_2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

#**************************************************************************************************************************
# Tema 5: Asociación para variables categóricas ----------------------------------------------------------------
#**************************************************************************************************************************

## 5.1 Tablas de asociación bivariadas -------------------------

# Construyamos dummies
ts$casa <- as.numeric(ts$casa==1)
ts$lavar_manos <- as.numeric(ts$lavar_manos==1)

# CASA: 1=Se queda en casa; 2=sale de casa
table(ts$casa, useNA="ifany")
table(is.na(ts$casa)) # N=1063

tabla0 <- table(ts$casa)
tabla1 <- table(ts$casa, ts$genero)
tabla2 <- table(ts$casa, ts$etario)

tabla1
tabla2

# Probabilidades condicionales
prop.table(tabla0)
prop.table(tabla1) # % del total
prop.table(tabla1, margin=1) # % Filas
prop.table(tabla1, margin=2) # % Columnas

# Para grupos etarios
prop.table(tabla2, margin=2) # % Columnas

## 5.2 Asociaciones con variables categóricas -------------------------

# Chi-cuadrado:
chisq.test(ts$genero, ts$casa, correct=F) 
chisq.test(ts$genero, ts$casa, correct=T) #Corrección para un escenario más conservador 
chisq.test(ts$etario, ts$casa, correct=F) 

# Odds ratio:
prop.table(tabla1, margin=2)
oddsratio(table(ts$genero, ts$casa))
(0.91107078/0.08892922)/(0.81445312/0.18554688) # Las odds de permanecer en casa 2.33397 veces más en las mujeres que para los hombres
(95*502)/(49*417)

# Riesgo Relativo:
#install.packages("epitools")
library(epitools)
riskratio(table(ts$sexo, ts$casa))
(502/551) / (417/512) # El riesgo de quedarse en casa es 1.118629 veces mayor para las mujeres respecto a los hombres   

riskratio(table(ts$sexo, ts$casa), rev="r")

#**************************************************************************************************************************
# Tema 6: Modelos de regresión lineal ----------------------------------------------------------------
#**************************************************************************************************************************

## 6.1 Modelo de regresión lineal  -------------------------

m1 <- lm(ingreso~edad, data=ts)
summary(m1)

#install.package(texreg)
library(texreg)
screenreg(m1)

# Control Estadístico
m2 <- lm(ingreso~edad + edad_2, data=ts)
m3 <- lm(ingreso~edad + edad_2 + genero, data=ts)

screenreg(list(m1, m2, m3))

install.packages("sjPlot")
library(sjPlot)

# Algunos gráficos
plot_model(m3)
plot_model(m3, type = "pred") # Valores predichos
# Predicciones del modelo
plot_model(m3, type = "pred", terms = c("edad", "genero"))
plot_model(m3, type = "pred", terms = c("edad_2", "genero"))

# Graficamos
ggplot(ts, aes(x=edad, y=ingreso)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, colour="red") + 
  stat_smooth(method="lm", se=T, formula=y ~ x + I(x^3)) + 
  theme_bw()

# Por grupos
ggplot(ts, aes(x=edad, y=ingreso, color = genero)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, formula=y ~ x + I(x^3)) + 
  theme_bw()

# Ajustemos los ingresos
ts2 <- ts %>% filter(ingreso<=1000000, ingreso!=401500)

g1 <- ggplot(ts2, aes(x=edad, y=ingreso)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, formula=y ~ x + I(x^3)) + 
  theme_bw()

g2 <- ggplot(ts2, aes(x=edad, y=ingreso, color = genero)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, formula=y ~ x + I(x^3)) + 
  theme_bw()

ggarrange(g1, g2, ncol = 2, nrow = 1)

# Otra manera
ggplot(ts2, aes(x=edad, y=ingreso, color = genero)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, formula=y ~ x + I(x^3)) + 
  theme_bw() + facet_wrap(~genero)


## 6.2 Modelo de probabilidad lineal  -------------------------

# Estimamos los modelos
m1 <- lm(casa~edad + ingreso + genero, data=ts2) # Modelo de probabilidad lineal para permanecer en casa 
m2 <- lm(lavar_manos~edad + ingreso + genero, data=ts2) # Modelo de probabilidad lineal para lavarse las manos

screenreg(list(m1, m2), digits = 4)

#**************************************************************************************************************************
############# FIN TALLER 5: DESCRIPTIVOS Y ASOCIACIÓN DE VARIABLES ########################################################
#**************************************************************************************************************************