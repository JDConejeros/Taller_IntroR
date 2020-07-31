#**************************************************************************************************************************
############# TALLER 6: VISUALIZACIÓN DE DATOS CON GGPLOT  ################################################################
#**************************************************************************************************************************

# Facilitador: José Daniel Conejeros 
# Correo: jdconejeros@uc.cl

# Apoyo: Constanza Lemus 
# Correo: cplemus@uc.cl 

#**************************************************************************************************************************
# Objetivos ----------------------------------------------------------------
#**************************************************************************************************************************

# Los objetivos para este taller son los siguientes: 

# 1. Generar gráficos Simples con R-Base
# 2. Primera exploración al paquete ggplot
# 3. Ajustando parámetros en ggplot 
# 4. Generar gráficos descriptivos y de asociación 

#**************************************************************************************************************************
# Tema 1: Abrir nuestro directorio y explorar la data  ----------------------------------------------------------------
#**************************************************************************************************************************

# Utilizamos este comando para desactivar notación científica 
options(scipen=999)

# 1.1 Directorio de trabajo y fijamos un proyecto ----------------------------------------------------------------

getwd() # Se obtiene el directorio de trabajo actual

# 1.2 Abrimos y exploramos nuestras base de datos ----------------------------------------------------------------
library(readr)
psu <- read_csv("sesiones/S6_intro_vis/psu_sample.csv")        #Recuerda ajustarlo en tu directorio propio 

# 1.3 Exploramos nuestra base ----------------------------------------------------------------
# Muestra de datos PSU - 2016
dim(psu)     
colnames(psu)
head(psu)
str(psu)
summary(psu)

#**************************************************************************************************************************
# Tema 2: Generamos gráficos simples con R-Base  ----------------------------------------------------------------
#**************************************************************************************************************************

# 2.1 Histogramas ----------------------------------------------------------------
psu$mate <- as.numeric(psu$mate)

hist(psu$mate, na.rm=T, freq = T)

# Podemos aagregar argumentos para ajustar
hist(psu$mate, main = "Distribución puntaje de matemáticas PSU 2016",  na.rm=TRUE)

hist(psu$mate, main = "Distribución puntaje de matemáticas PSU 2016",  na.rm=T, 
     xlab="Distribución del puntaje", ylab="Frecuencia")

hist(psu$mate, main = "Distribución puntaje de matemáticas",  na.rm=T, 
     xlab="Distribución del puntaje", ylab="Frecuencia", col="blue", xlim=c(400, 700))

hist(psu$mate, main = "Distribución puntaje de matemáticas",  na.rm=T, 
     xlab="Distribución del puntaje", ylab="Frecuencia",  col="white", xlim=c(100, 900), breaks=50, freq=T)

hist(psu$mate, main = "Distribución puntaje de matemáticas",  na.rm=T, 
     xlab="Distribución del puntaje", ylab="Densidad",  col="white", xlim=c(100, 900), breaks=50, freq=F)
lines(density(psu$mate), lwd = 1, col = "blue")

# Podemos acumular especificaciones
# Las curvas de densidad no son una proporción, sino que la función de densidad de probabilidad. 
hist(psu$mate, main = "Distribución puntaje de matemáticas", 
     xlab="Distribución del puntaje", col="white", ylab="Densidad", xlim=c(100, 900), breaks=50, freq=F)
lines(density(psu$mate, adjust=0.5), lwd = 1, col = "blue")
lines(density(psu$mate, adjust=2), lty="dotted", lwd = 2, col = "red") 

# 2.2 Gráficos de Torta ----------------------------------------------------------------

pie(table(psu$mate), main="Gráfico de torta dependencia de la escuela")

pie(table(psu$grupo_depend), main="Gráfico de torta dependencia de la escuela")

pie(table(psu$grupo_depend), main="Gráfico de torta dependencia de la escuela", labels=c("Municipal", "Particular Pagado", "Particular subvencionado"))

# Usar una tabla para construir el gráfico
tabla <- as.data.frame(prop.table(table(psu$grupo_depend))*100)
tabla$Freq <- round(tabla$Freq,1)

pie(tabla$Freq, labels = paste(labels = tabla$Var1, paste0(tabla$Freq, "%"), sep=" "))

# 2.3 Gráficos de Barras ----------------------------------------------------------------
par(las=1)
barplot(prop.table(table(psu$sexo)), ylim = c(0,1), main="Gráfico de Barras: Género", cex.names=1, horiz = F)

# 2.4 Boxplot ----------------------------------------------------------------
par(las=3)

boxplot(psu$leng ~ psu$grupo_depend, horizontal=T, 
        ylab = " ",
        xlab = "Puntaje", 
        names = c("Municipal", "Part. Pagado", "Part. Subvencionado"), boxwex = 0.5, cex.axis=0.5)
title(main = "Gráfico de caja puntajes PSU de lenguaje", font.main= 1, cex.main = 1)

#**************************************************************************************************************************
# Tema 3: Primera exploración con la librería de ggplot  ----------------------------------------------------------------
#**************************************************************************************************************************
install.packages("ggplot2")
library(ggplot2)
ggplot(data=psu)

# 3.1 Argumentos Básicos ----------------------------------------------------------------

# Es importante ir agregando información por capas

ggplot(data=psu, aes(x=leng, y=mate)) #Agrego ejes: capa2

ggplot(data=psu, aes(x=leng, y=mate)) + 
  geom_point()  #Agrego geometría: points. capa3

ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") 

ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(color = "Género") +
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  theme(plot.title = element_text(size = 10, face="bold"),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10), 
        axis.title.y=element_text(size=10))

# 3.2 Aplicación de temas ----------------------------------------------------------------

# Podría ajustar temas genericos
ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(color = "Género") +
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  theme_bw()

ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(color = "Género") +
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  theme_classic()
  
ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(color = "Género") +
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  theme_light()

ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(color = "Género") +
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  theme_minimal()


# 3.3 Facetas  ----------------------------------------------------------------

# Podemos agregar facetas
ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth(method = "lm") + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  labs(color = "Género") +
  theme_bw() +
  facet_grid(~factor(psu$grupo_depend)) 


# 3.4 Guardar gráficos  ----------------------------------------------------------------

# Camino 1: Directo
ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  labs(color = "Género") +
  theme_bw() +
  facet_grid(~factor(psu$grupo_depend)) 
ggsave("sesiones/S6_intro_vis/graph/mi_primer_ggplot.png")

# Camino 2: Como objetos
g1 <-  ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  labs(color = "Género") +
  theme_minimal()

ggsave("sesiones/S6_intro_vis/graph/g1.png", plot=g1)

# Camino 3: Múltiples gráficos
g2 <- ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  labs(color = "Género") +
  theme_bw() +
  facet_grid(~factor(psu$grupo_depend)) 
ggsave("sesiones/S6_intro_vis/graph/g2.png", plot=g2)

library(ggpubr)
g3 <- ggarrange(g1, g2, ncol=1, nrow=2)
ggsave("sesiones/S6_intro_vis/graph/g3.png", plot=g3)

# Camino 4: Ejecutar y guardar
png(file = "sesiones/S6_intro_vis/graph/g4.png", height = 900, width = 1200)
ggarrange(g1, g2, ncol = 1, nrow = 2,  common.legend = TRUE, legend="right")
dev.off()

#**************************************************************************************************************************
# Tema 4: Gráficos Descriptivos  ----------------------------------------------------------------
#**************************************************************************************************************************

# 4.1 Histogramas ----------------------------------------------------------------

# Ejemplo 1: Agregar curva de densidad
ggplot(data=psu, aes(x=leng)) +
  geom_histogram(aes(y=..density..),
                 position = "identity", binwidth = 20,
                 colour="black", fill="white") +
  scale_x_continuous() +
  labs(title="Distribución Puntajes",
       x="PSU - Lenguaje", y = "Densidad") +
  geom_density(col="red") + 
  theme_bw()

# Ejemplo 1: Agregar curva normal
ggplot(data=psu, aes(x=educmadre)) +
  geom_histogram(aes(y=..density..),
                 position = "identity", binwidth = 1,
                 colour="black", fill="white") +
  scale_x_continuous(n.breaks = 14) +
  labs(title="Histograma",
       x="Años de educación de la madre", y = "Densidad") +
  geom_density(col="red") + 
  stat_function(fun = dnorm, n = 9623, args = list(mean = mean(psu$educmadre), sd = 2), colour = "blue") +
  theme_bw()

# 4.3 Gráficos de Barras ----------------------------------------------------------------

ggplot(data=psu, aes(x=grupo_depend, fill=as.factor(grupo_depend))) + 
  geom_bar(color="black") +
  xlab("Dependencia del estudiante") + 
  ylab("Cantidades") +
  labs(fill = "Dependencia") +
  ggtitle("Gráfico de Barras") 

# Podríamos hacer algunos ajustes 
ggplot(data=psu, aes(x=grupo_depend, fill=as.factor(grupo_depend))) + 
  geom_bar() +
  scale_fill_grey() +
  xlab("Dependencia del estudiante") + 
  ylab("Cantidades") +
  ggtitle("Gráfico de Barras") + 
  labs(fill = "Dependencia") +
  coord_flip()

# Podríamos utilizar nuestra tabla
ggplot(data=tabla, aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity",  fill="steelblue", width=0.5)+
  geom_text(aes(label=paste0(Freq, "%")), hjust=-0.5, size=5, fontface="bold") +
  xlab("Dependencia del estudiante") + 
  ylab("Cantidades") +
  ggtitle("Gráfico de Barras") +
  labs(fill = "Dependencia") +
  theme_bw() +
  coord_flip()

# 4.4 Box-Plots ----------------------------------------------------------------

ggplot(data=psu, aes(x=as.factor(sexo), y=cien, fill=factor(sexo))) + 
  geom_boxplot(width=0.3) + 
  xlab("Distribución Puntaje") + 
  ylab(" ") + 
  labs(fill = "Género") +
  ggtitle("Boxplot rendimiento")

# 4.5 Integración con DPLYR
library(dplyr)
# Completo
f1 <- psu %>% 
  ggplot(aes(x=as.factor(sexo), y=cien, fill=factor(sexo))) + 
  geom_boxplot(width=0.3) + 
  xlab("Distribución Puntaje") + 
  ylab("Género") + 
  labs(fill = "Género") +
  ggtitle("Boxplot rendimiento")

# Municipal
f2 <- psu %>% filter(factor(psu$grupo_depend) == "Municipal (Dependencia 1, 2, 5)") %>% 
  ggplot(aes(x=as.factor(sexo), y=cien, fill=factor(sexo))) + 
  geom_boxplot(width=0.3) + 
  xlab("Distribución Puntaje") + 
  ylab("Género") + 
  labs(fill = "Género") +
  ggtitle("Municipal")

# Particular pagado
f3 <- psu %>% filter(factor(psu$grupo_depend) == "Particular Pagado (Dependencia 4)") %>% 
  ggplot(aes(x=as.factor(sexo), y=cien, fill=factor(sexo))) + 
  geom_boxplot(width=0.3) + 
  xlab("Distribución Puntaje") + 
  ylab("Género") + 
  labs(fill = "Género") +
  ggtitle("Particular Pagado")


# Juntamos todo en un gráfico
ggarrange(f1, f2, f3, ncol = 3, nrow = 1,  common.legend = TRUE, legend="right")

# Guardamos el gráfico
png(file = "sesiones/S6_intro_vis/graph/g5.png", height = 900, width = 900)
ggarrange(f1, f2, f3, ncol = 3, nrow = 1,  common.legend = TRUE, legend="right")
dev.off()

#**************************************************************************************************************************
# Tema 5: Gráficos de asociación  ----------------------------------------------------------------
#**************************************************************************************************************************

# 5.1 Asociaciones lineales  ----------------------------------------------------------------

# Asociación entre el puntaje obtenido en ciencias y la educación de la madre/padre según tipo de colegio

a <- ggplot(psu, aes(x=cien, y=educmadre)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, formula=y ~ x) + 
  xlab("Distribución Puntaje PSU Ciencia") + 
  ylab("Años educación de la madre") + 
  theme_bw() + 
  facet_wrap(~grupo_depend)

b <- ggplot(psu, aes(x=cien, y=educpadre)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, formula=y ~ x) + 
  xlab("Distribución Puntaje PSU Ciencia") + 
  ylab("Años educación del padre") + 
  theme_bw() + 
  facet_wrap(~grupo_depend)

ggarrange(a, b, ncol = 1, nrow = 2)

# Asociación entre el puntaje obtenido en ciencias y la educación de la madre/padre según tipo de colegio y sexo

c <-  ggplot(psu, aes(x=cien, y=educmadre)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, formula=y ~ x) + 
  xlab("Distribución Puntaje PSU Ciencia") + 
  ylab("Años educación de la madre") + 
  theme_bw() + 
  facet_wrap(~sexo + grupo_depend)

d <- ggplot(psu, aes(x=cien, y=educpadre)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, formula=y ~ x) + 
  xlab("Distribución Puntaje PSU Ciencia") + 
  ylab("Años educación del padre") + 
  theme_bw() + 
  facet_wrap(~sexo + grupo_depend)


png(file = "sesiones/S6_intro_vis/graph/g6.png", height = 900, width = 900)
ggarrange(c, d, ncol = 1, nrow = 2)
dev.off()

#**************************************************************************************************************************
############# FIN TALLER 6: VISUALIZACIÓN DE DATOS CON GGPLOT  ############################################################
#**************************************************************************************************************************
