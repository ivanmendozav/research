#############################################################
#PLANTILLA DE FILTRADO DE DATOS 
#############################################################

library(readxl)
bicis <- read_excel("C:/Users/joe-a/OneDrive/Documents/U del Azuay/TRABAJO/Tesis final/PROYECTO UDA ESTUDIANTES BICICLETA/Análisis Finales en R del recorrido/Bitacoradatos.xlsx")
View(bicis)
#Para ver las dimenciones de la base de datos
dim(bicis)


# Para ver el tipo de dato con el que voy a trabajar la idea es trabajar con numéricos y factores 
str(bicis)
#para ver los nombreS de los títulos de mis columnas
names(bicis)



#Varialbles utilizadas 
#Cuantitativas: Edad (discreta), Velocidad (continua), tiempos de recorrido (continua)
#cualitativas Tipo de bicicleta (categorica), sexo (categórica), percepción (ordinal escala liker)

################################################################################################


#Histograma 
#EDAD
histograma<-hist(bicis$Edad, main="Edades Participantes", col="violet", ylim=c(0,500), xlab="Edades", breaks = 3)
abline(v=mean(bicis$Edad), col="red") #visualización media en el histograma
abline(v=median(bicis$Edad), col="blue") #visualización mediana en el histograma
#se debe usar moda  no es una distribución normal 

#Velocidad
histograma<-hist(bicis$Vel_prom, main="Velocidad Promedio", col="violet", ylim=c(0,200), xlab="Velocidad (km/h)", breaks = 20)
abline(v=mean(bicis$Vel_prom), col="red") #visualización media en el histograma
abline(v=median(bicis$Vel_prom), col="blue") #visualización mediana en el histograma
#distribución normal sesgo a la izq, se puede usar moda, no media por que hay un outlier que puede afectar

#Tiempo
histograma<-hist(bicis$Tiempo, main="Tiempo de recorrido", col="violet", ylim=c(0,300), xlab="Tiempo (min)", breaks = 13)
abline(v=mean(bicis$Tiempo), col="red") #visualización media en el histograma
abline(v=median(bicis$Tiempo), col="blue") #visualización mediana en el histograma
#Al no ser una distribución normal, se tomaria moda, la moda se muestra aproximadamente 5 minutos de recorrido 

#Distancia
histograma<-hist(bicis$Dist, main="Distancia recorrida", col="violet", ylim=c(0,250), xlab="Distancia (km)", breaks = 11)
abline(v=mean(bicis$Dist), col="red") #visualización media en el histograma
abline(v=median(bicis$Dist), col="blue") #visualización mediana en el histograma
#No se ve una clara distribución normal se podria tomar la moda viajes al rededor de 1 km

# iii.	Calcule las medidas de centralidad. Media, Mediana y Moda para dichas variables.5p

#Edad
media_Edad<-mean(bicis$Edad)
mediana_Edad<-median(bicis$Edad)
Minimo_Edad<-min(bicis$Edad)
maximo_Edad<-max(bicis$Edad)
summary(bicis$Edad)
#para detectar la moda
library(DescTools)
Mode(bicis$Edad) #Directamente nos entrega la moda

#Velocidad 
media_Vel<-mean(bicis$Vel_prom)
mediana_Vel<-median(bicis$Vel_prom)
Minimo_Vel<-min(bicis$Vel_prom)
maximo_Vel<-max(bicis$Vel_prom)
summary(bicis$Vel_prom)
#para detectar la moda
Mode(bicis$Vel_prom) #Directamente nos entrega la moda

#Tiempo 
media_time<-mean(bicis$Tiempo)
mediana_time<-median(bicis$Tiempo)
Minimo_time<-min(bicis$Tiempo)
maximo_time<-max(bicis$Tiempo)
summary(bicis$Tiempo)
#para detectar la moda
Mode(bicis$Tiempo) #Directamente nos entrega la moda

#Distancia  
media_dis<-mean(bicis$Dist)
mediana_dis<-median(bicis$Dist)
Minimo_dis<-min(bicis$Dist)
maximo_dis<-max(bicis$Dist)
summary(bicis$Dist)
#para detectar la moda
Mode(bicis$Dist) #Directamente nos entrega la moda

# iv.	Relacione estas medidas con el histograma dibujado e interprete la selección de las medidas de centralidad.5p

#Histograma 
#EDAD
histograma<-hist(bicis$Edad, main="Edades de los participantes en los viajes", col="violet", ylim=c(0,70), xlab="Edades", breaks = 10)

abline(v=mean(bicis$Edad), col="red") #visualización media en el histograma
abline(v=median(bicis$Edad), col="blue") #visualización mediana en el histograma
#se debe usar moda  no es una distribución normal 

#Velocidad
histograma<-hist(bicis$Vel_prom, main="Velocidad Promedio", col="violet", ylim=c(0,100), xlab="Velocidad (km/h)", breaks = 20)
abline(v=mean(bicis$Vel_prom), col="red") #visualización media en el histograma
abline(v=median(bicis$Vel_prom), col="blue") #visualización mediana en el histograma
#distribución normal sesgo a la izq, se puede usar moda, no media por que hay un outlier que puede afectar

#Tiempo
histograma<-hist(bicis$Tiempo, main="Tiempo de recorrido", col="violet", ylim=c(0,300), xlab="Tiempo (seg)", breaks = 13)
abline(v=mean(bicis$Tiempo), col="red") #visualización media en el histograma
abline(v=median(bicis$Tiempo), col="blue") #visualización mediana en el histograma
#Al no ser una distribución normal, se tomaria moda, la moda se muestra aproximadamente 5 minutos de recorrido 

#Distancia
histograma<-hist(bicis$Dist, main="Distancia recorrida", col="violet", ylim=c(0,250), xlab="Distancia (km)", breaks = 11)
abline(v=mean(bicis$Dist), col="red") #visualización media en el histograma
abline(v=median(bicis$Dist), col="blue") #visualización mediana en el histograma
#No se ve una clara distribución normal se podria tomar la moda viajes al rededor de 1 km

# v.	Interprete las medidas de dispersión relativas y presencia de valores extremos a través de un boxplot.5p

#Edad
#medidas de dispersión
#Rango
x <- min(bicis$Edad)
y <- max(bicis$Edad)
y-x 
range(bicis$Edad)
#varianza muestral
varianza_Edad<-var(bicis$Edad)
#Desviación estándar
desviacion_Edad<-sd(bicis$Edad)
#z-score
zscore_edad <- (bicis$Edad - mean(bicis$Edad))/sd(bicis$Edad)
# Medidas de dispersión relativa
#cuartiles 
quantile(bicis$Edad)

#frecuencia acumulada 
library(DescTools)
Desc(bicis$Edad, main="Edades Participantes")

#Edad
#medidas de dispersión
#Rango
x <- min(bicis$Vel_prom)
y <- max(bicis$Vel_prom)
y-x 
range(bicis$Vel_prom)
#varianza muestral
varianza_vel<-var(bicis$Vel_prom)
#Desviación estándar
desviacion_vel<-sd(bicis$Vel_prom)
#z-score
zscore_vel <- (bicis$Vel_prom - mean(bicis$Vel_prom))/sd(bicis$Vel_prom)
# Medidas de dispersión relativa
#cuartiles 
quantile(bicis$Vel_prom)

#frecuencia acumulada 
Desc(bicis$Vel_prom, main="Velocidad Promedio") 

#Tiempo
#medidas de dispersión
#Rango
x <- min(bicis$Tiempo)
y <- max(bicis$Tiempo)
y-x 
range(bicis$Tiempo)
#varianza muestral
varianza_time<-var(bicis$Tiempo)
#Desviación estándar
desviacion_time<-sd(bicis$Tiempo)
#z-score
zscore_time <- (bicis$Tiempo - mean(bicis$Tiempo))/sd(bicis$Tiempo)
# Medidas de dispersión relativa
#cuartiles 
quantile(bicis$Tiempo)

#frecuencia acumulada 
Desc(bicis$Tiempo, main="Tiempo Recorrido") 

#Distancia
#medidas de dispersión
#Rango
x <- min(bicis$Dist)
y <- max(bicis$Dist)
y-x 
range(bicis$Dist)
#varianza muestral
varianza_dis<-var(bicis$Dist)
#Desviación estándar
desviacion_dis<-sd(bicis$Dist)
#z-score
zscore_dis <- (bicis$Dist - mean(bicis$Dist))/sd(bicis$Dist)
# Medidas de dispersión relativa
#cuartiles 
quantile(bicis$Dist)

vel<- as.numeric(bicis$Vel_prom)

#frecuencia acumulada 
Stat.Desc(bicis$Dist, main="Distancia Recorrida") 

# vi.	Realice una interpretación resumida de las diferentes perspectivas exploradas 10p.
#Datos cuantitativos 
#sexo - tiempo 
qplot(factor(Sexo),Tiempo , data = bicis, geom = "violin", stackdir = "center", binaxis = "y",
      color = factor(Sexo), fill = factor(Sexo),main = "Tiempo en función del sexo",
      xlab = "Sexo", ylab = "Tiempo (min)")
qplot(factor(Sexo),Tiempo , data = bicis, geom=c("boxplot", "jitter"), fill = factor(Tipo_bici))

#Sexo - velocidad 
qplot(vel, data = bicis, bins=20, geom = "density", color = as.factor(Sexo))
qplot(factor(Sexo),vel , data = bicis, geom = "violin", stackdir = "center", binaxis = "y",
      color = factor(Sexo), fill = factor(Sexo))
ggplot(data = bicis) + geom_density(aes(x=vel,fill=factor(Sexo)),bins=10, position = "identity",alpha = 0.5)

#tipo de bicicleta - velocidad 
qplot(factor(Tipo_bici),vel , data = bicis, geom=c("boxplot"))
qplot(factor(Tipo_bici),vel , data = bicis, geom=c("boxplot", "jitter"), fill = factor(Sexo))
qplot(factor(Tipo_bici),vel , data = bicis, geom = "violin", stackdir = "center", binaxis = "y",
      color = factor(Tipo_bici), fill = factor(Tipo_bici),main = "Velocidad en función del tipo de bicicleta",
      xlab = "Tipo bicicleta", ylab = "Velocidad (km/h)")
ggplot(data = bicis) + geom_density(aes(x=vel,fill=factor(Tipo_bici)),bins=10, position = "identity",alpha = 0.5)

#polyplot
library(plotly)

p<- qplot(vel, data = bicis, bins=5, geom = "density", color = as.factor(Tipo_bici))
ggplotly(p)


######################################################################################

#Datos Cualitativos

tipo_bicicleta<- c(bicis$Tipo_bici)
sex<-c(bicis$Sexo)
seguridad<- c(bicis$`¿Qué tan seguro me sentí en el recorrido?`)
cansancio<- c(bicis$`¿Qué tan cansado me sentí en el recorrido?`)
table(tipo_bicicleta)
table(seguridad)
table(sex)
table(cansancio)
table(tipo_bici, seguridad)
table(sex, seguridad)
table(tipo_bicicleta, cansancio)
table(sex, cansancio)


#gráfica 
library(lattice)
histogram(~ seguridad| sex, data=bicis,layout=c(1,2))
histogram(~ seguridad| tipo_bicicleta, data=bicis,layout=c(1,2))

histogram(~ cansancio| sex, data=bicis,layout=c(1,2))
histogram(~ cansancio| tipo_bicicleta, data=bicis,layout=c(1,2))


###############################################################################################
# Análisis de regresión de los datos a través de 25p
###############################################################################################

# a.	Compruebe la relación lineal entre las variables a través de correlación y dibujando un gráfico de dispersión.5p

#Correlación 
newdata <- data.frame(bicis$Edad, bicis$Tiempo, bicis$Vel_prom, bicis$Dist)
View(newdata)
str(newdata)
cor(newdata)

library(PerformanceAnalytics)
chart.Correlation(newdata)

# la distancia y el tiempo tienen la mayor correlación 

#test de correlación para ver p-value y pearson 
cor(newdata$bicis.Tiempo,newdata$bicis.Dist)
cor.test(newdata$bicis.Tiempo,newdata$bicis.Dist)

# b.	Aplique regresión lineal simple y exprese la ecuación de la recta (y=a+bx). 5p

#Regresión lineal

regresion = lm(newdata$bicis.Dist ~ newdata$bicis.Tiempo)
summary(regresion)
plot(newdata$bicis.Tiempo, newdata$bicis.Dist, xlab = "Tiempo", ylab = "Distancia", main = "Relación Tiempo-Distancia")
abline(regresion, col="red")


predict(regresion, newdata=data.frame(newdata$bicis.Tiempo))

# c.	Interprete la bondad de ajuste con R2 (ej. 0.65 ??? nuestra ecuación explica el 65% de la variabilidad de la variable y). 5p

# modelo Distancia = 3.477e-01 + (2.189e-03)*Tiempo
#El modelo lineal explica aproximadamente el 52% de la distancia con respecto al tiempo de recorrido  
# las variables tienen representatividad y el p-value es inferior a 0,05 

# d.	Verifique supuestos de los residuos, con la alternativa de histograma. 10p

#Linealidad 

#los residuos se deben distribuir aleatoriamente entorno al valor 0. En este caso se acepta linealidad.

#La funcion lm() calcula y almacena los valores predichos por el modelo y los residuos.
newdata$prediccion <- regresion$fitted.values
newdata$residuos <- regresion$residuals
head(newdata)

ggplot(data = newdata, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  labs(title = "Distribucion de los residuos", x = "prediccion modelo",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#hay algunos valores que se alejan mucho de 0 sin envargo en general los reciduos estan en torno a 0
#se puede ver claramente que hay outliers 
#ademas se ve mayor concentración de datos al inicio que al final, en los primeros minutos 
###########################################################
# Histograma
ggplot(data = newdata, aes(x = residuos)) +
  geom_histogram(aes(y = ..density..)) +
  labs(title = "histograma de los residuos") +
  theme_light()

histograma_residuos<-hist(newdata$residuos, main="Histograma residuos", col="violet", ylim=c(0,200), xlim=c(-2,6), xlab="Residuos", breaks = 27)
abline(v=mean(newdata$residuos), col="red") #visualización media en el histograma
abline(v=median(newdata$residuos), col="blue") #visualización mediana en el histograma

#a pesar de tener los residuos la forma de una distribusión normal se puede ver un sesgo hacia la derecha por lo que no seria simétrica, esto debido a los outliers


#QQ-norm
qqnorm(regresion$residuals)
qqline(regresion$residuals)

#si vien en el centro del se alinea con el eje en los extremos se pierde alineación, sobre todo a la derecha que es donde se cuenta con valores extremos 

#Test normalidad
shapiro.test(regresion$residuals)

#no cumple con la normaliad ya que el valor debería estar por encima de 0,05 y es muy inferior 

#########################################################

#Homocedasticidad

ggplot(data = newdata, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  geom_smooth(se = FALSE, color = "firebrick") +
  labs(title = "Distribucion de los residuos", x = "prediccion modelo",
       y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#La viariaza de los residuos no es constante, por ello se muestra un patron cónico que nno es homogenio
#no cumple homocedasticidad 

########################################################
#Autocorrelacion de residuos

ggplot(data = newdata, aes(x = seq_along(residuos), y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_line(size = 0.3) +
  labs(title = "Distribucion de los residuos", x = "index", y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


quantile()

##en general se sigue una tendencia, sin embargo algunos valores extremos dañan la homeginidad de los residuos 

