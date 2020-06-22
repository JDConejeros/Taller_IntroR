#**************************************************************************************************************************
############# TALLER 2: ABRIR - EXPLORAR BASES DE DATOS ###################################################################
#**************************************************************************************************************************

# Facilitador: José Daniel Conejeros 
# Correo: jdconejeros@uc.cl
 
# Apoyo: Constanza Lemus 
# Correo: cplemus@uc.cl 

#**************************************************************************************************************************
# Objetivos ----------------------------------------------------------------
#**************************************************************************************************************************

# Los objetivos para este primer taller son los siguientes 

# 1. Fijar un directorio / proyecto de trabajo 
# 2. Importar una base de datos en distintos formatos y conocer estructuras de datos y variables
# 3. Exploración y análisis Descriptivos para variables continuas y categóricas
# 4. Validar la información de nuestra base de datos 
# 5. Guardar nuestra base de datos

#**************************************************************************************************************************
# Tema 1: Fijar un directorio de trabajo  ----------------------------------------------------------------
#**************************************************************************************************************************

getwd() # Se obtiene el directorio de trabajo actual
setwd("ruta") # Establecer directorio de trabajo

#Windows
setwd("~\Users\josedanielconejeros\Dropbox\Ayudantia_clases\Clases_R\Taller_IntroR") 

#Mac
setwd("~/Users/josedanielconejeros/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR")

# Generar proyectos en R

#**************************************************************************************************************************
# Tema 2: Importar una base de datos y explorar  ----------------------------------------------------------------
#**************************************************************************************************************************

# 2.1 Base en formato .txt:  ----------------------------------------------------------------

data <- read.table("~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/data/data.txt", sep = "\t")

# 2.2 Base en formato .csv:  ----------------------------------------------------------------

data2 <- read.csv("~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/data/data.csv")

# 2.3 Base en formato .xlsx:  ----------------------------------------------------------------
library(openxlsx)
data3 <- read.xlsx("~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/data/data.xlsx")

#***************
library(haven)
#***************

# 2.4 Base en formato .sas:  ----------------------------------------------------------------
?read_sas()
data4 <- read_sas("~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/data/data.sas7bdat")

# 2.5 Base en formato .sps:  ----------------------------------------------------------------
?read_sav()
data5 <- read_sav("~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/data/data.sav")

# 2.6 Base en formato .dta:  ----------------------------------------------------------------
?read_dta
data6 <- read_dta("~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/data/data.dta")

# 2.7 Base en formato .Rdata:  ----------------------------------------------------------------
load("~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/data/data.Rdata")

#**************************************************************************************************************************
# Tema 3: Explorar una base de datos  ----------------------------------------------------------------
#**************************************************************************************************************************

# Vamos a remover nuestros objetos para trabajar con la base de interés
rm(data, data2, data3, data4, data5, data6)

# 3.1 Importar la base de datos ----------------------------------------------------------------
simce <- haven::read_dta("~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/data/simce8b2017.dta")
 
# Dada la heterogeneidad de nuestro objeto lo vamos a transformar en un marco de datos
typeof(simce)
simce <- as.data.frame(simce)

# 3.2 Explorar nuestro objeto con la data ------------------------------------------------------

dim(simce)   # Observaciones y variables

names(simce) # Nombre de nuestras variables 

str(simce)   # Visor de nuestras variables

head(simce, n=100)  # Primeras 6 observaciones
head(simce[2])      # por nombres
head(simce[[2]])    # por ubicación 

tail(simce, n=100)  # Últimas 6 observaciones
tail(simce[,4])
tail(simce[4,])
tail(simce)
tail(15,15)
tail(1:20, 15)

# 3.3 Obtener estadísticos descriptivos de mi base general ------------------------------------------------------

# Usando summary
summary(simce)
summary(simce$ptje_mate)
summary(simce[4])
summary(simce[,4])
summary(simce[4,])

# Podemos usar la librería skimr

library(skimr)
skim(simce)

#**************************************************************************************************************************
# Tema 4: Validar una base de datos  ----------------------------------------------------------------
#**************************************************************************************************************************

# 4.1 Identificar casos duplicados:  ----------------------------------------------------------------

# Ver casos únicos
unique(simce$idalumno)

# Ver casos duplicados
duplicated(simce$idalumno)

simce$caso <- duplicated(simce$idalumno)

# Dimensionar los casos duplicados
table(simce$caso)

simce[duplicated(simce$idalumno)]

# Encontrar las observaciones duplicadas
simce[duplicated(simce$idalumno),]

# Quitar los casos duplicados
simce2 <- simce[unique(simce$id),]
head(simce2)

# 4.1 Revisar valores fuera de rango:  ----------------------------------------------------------------

# Mínimo
min(simce$ptje_mate)
# Máximo
max(simce$ptje_mate)

# Tabla de valores
table(simce$genero)
table(simce$cod_depe2)
table(simce$ptje_mate)

# Descriptivoz
summary(simce$ptje_mate)
summary(simce$cod_depe2)

# 4.1 Explorar casos pérdidos:  ----------------------------------------------------------------

# Revisar casos perdidos 
table(simce$genero, useNA = "ifany")
table(simce$cod_depe2, useNA = "ifany")
table(is.na(simce$ptje_mate))
skim(simce)

# Mostrar los casos pérdidos
table(is.na(simce))
is.na(simce)

# Mostrar la observación
simce[is.na(simce$ptje_mate),]
which(is.na(simce$ptje_mate)) # Indica la fila 

# Eliminar casos perdidos
simce3 <- na.omit(simce)
simce3

# Recodificar casos perdidos (imputar)
simce4 <- simce
simce4$ptje_mate[is.na(simce$ptje_mate)] <- mean(simce$ptje_mate, na.rm = TRUE)
tail(simce)
tail(simce4)

#**************************************************************************************************************************
# Tema 5: Exportar una base de datos:  ----------------------------------------------------------------
#**************************************************************************************************************************

# Vamos a seleccionar solo una parte de la data y la vamos a exportar a distintos formatos

# 5.1 Base en formato .txt:  ----------------------------------------------------------------

write.table(simce4, file="~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/result/simce_mod.txt", sep="\t")

# 5.2 Base en formato .csv:  ----------------------------------------------------------------

write.csv(simce4, file="~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/result/simce_mod.csv", row.names = FALSE)

# 5.3 Base en formato .xlsx:  ----------------------------------------------------------------

library(openxlsx)
write.xlsx(simce4, file="~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/result/simce_mod.xlsx")

# 5.4 Base en formato .sav:  ----------------------------------------------------------------

write_sav(simce4, "~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/result/simce_mod.sav")

# 5.5 Base en formato .sas:  ----------------------------------------------------------------

write_sas(simce4, "~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/result/simce_mod.sas7bdat")

# 5.5 Base en formato .dta:  ----------------------------------------------------------------

write_dta(simce4, "~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/result/simce_mod.dta")

# 5.6 Base en formato .Rdata:  ----------------------------------------------------------------

save(simce4, file="~/Dropbox/Ayudantia_clases/Clases_R/Taller_IntroR/sesiones/S2_Explorar_BBDD/result/simce_mod.Rdata")

#**************************************************************************************************************************
# Próximo taller:  ----------------------------------------------------------------
#**************************************************************************************************************************

install.packages("dplyr")
library("dplyr")

#**************************************************************************************************************************
############# FIN TALLER 2: ABRIR - EXPLORAR BASES DE DATOS ###############################################################
#**************************************************************************************************************************