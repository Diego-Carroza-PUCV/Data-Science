# Instalar paquetes
# Tools -> Install Packages 

# Cargar paquetes
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(forecast)
library(tseries) 
library(TSA)    
library(urca)    
library(ggplot2) 
library(dplyr)   
library(stats)   
library(seasonal)
library(tseries)
# Importar datos y guardar en variable
ETA <- read_excel("C:/Users/const/OneDrive/Desktop/Universidad/Data Science/BaseETA.xlsx")
View(ETA)

# Filtrar ETA sin N/A en tasa de ataque
ETA <- filter(ETA,!is.na(ETA$`Tasa Ataque`))
ETA <- filter(ETA,ETA$`Tasa Ataque`<=100)

# Filtro para solo casos confirmados
ETA_CasosConfirmados <- filter(ETA, ETA$`Conclusión del brote`=="Realizada, se confirma")
View(ETA_CasosConfirmados)

# Filtro casos atípicos
ETA_CasosConfirmados <- filter(ETA_CasosConfirmados, ETA_CasosConfirmados$Enfermos!=5470)
ETA_CasosConfirmados <- filter(ETA_CasosConfirmados, ETA_CasosConfirmados$Enfermos!=517)
view(ETA_CasosConfirmados)

# Seleccionar columnas con fecha y número de enfermos
ETA_Enfermos <- select(ETA_CasosConfirmados,4,11)
view(ETA_Enfermos)

# Renombrar columnas
colnames(ETA_Enfermos)
colnames(ETA_Enfermos)[1] <- "Fecha"
colnames(ETA_Enfermos)[2] <- "Enfermos"

# Agrupar número de enfermos por fecha (juntar todos los enfermos de una misma fecha)
ETA_Enfermos <- ETA_Enfermos %>%
  group_by(Fecha) %>%
  summarise(Enfermos=sum(Enfermos))
view(ETA_Enfermos)

# Crear columna que indique a que mes correlativo corresponde
# Ej: Enero a Diciembre 2011, va de los meses 1 a 12, Enero a Diciembre 2012, va de los meses 13 a 24, así sucesivamente.
ETA_Enfermos$Mes_Correlativo <- as.integer(format(ETA_Enfermos$Fecha, "%m")) + 12 * (as.integer(format(ETA_Enfermos$Fecha, "%Y")) - 2011)

# Agrupar número de enfermos por mes correlativo
ETA_Enfermos <- ETA_Enfermos %>%
  group_by(Mes_Correlativo) %>%
  summarise(Enfermos=sum(Enfermos))
view(ETA_Enfermos)

#Convertir a Serie de Tiempo
Enfermos=select(ETA_Enfermos,2)
view(Enfermos)
SerieEnfermos=ts(Enfermos$Enfermos,freq=12,start=c(2011,1))

#Graficar serie de tiempo
plot(SerieEnfermos, main="Enfermos enero 2011 - enero 2023",xlab="Año",ylab="Número de Enfermos")

#Se debe obtener Componente estacional, tendencia-ciclo, componente irregular y serie ajustada por estacionalidad.
Descom<- decompose(SerieEnfermos)
par(mfrow = c(2, 2)) #Se utiliza para dividir la ventana gráfica en una matriz de 2 filas y 2 columnas
plot(Descom$x, main = "Enfermos enero 2011 - enero 2023", col = "black", ylab = "Número de Enfermos", xlab = "Año")
plot(Descom$trend, main = "Tendencia", col = "blue", ylab = "Número de Enfermos", xlab = "Año")
plot(Descom$seasonal, main = "Estacionalidad", col = "red", ylab = "Número de Enfermos", xlab = "Año")
plot(Descom$random, main = "Irregularidad", col = "green", ylab = "Número de Enfermos", xlab = "Año")

#Aplicación modelo Arima

#Validar estacionalidad de la serie (Se dice que una serie es estacionaria cuando su media, varianza y autocovarianza son invariantes en el tiempo)
#Test de Dickey Fuller
#H0: Serie No estacionaria: Hay raiz unitaria H1: Serie Estacionaria: No hay raiz unitaria
adf.test(SerieEnfermos)
#Como el p-value = 0.01 < 0.05 entonces hay evidencia suficiente para rechazar la hipótesis nula (H0), esto implica que la serie temporal es estacionaria

#La función de autocorrelación (ACF): mide la correlación entre una serie de tiempo y sus valores retrasados.
#Con ello podemos obtener q, el cual es el orden componente de media móvil
#El cual utilizar errores pasados para predecir errores futuros
Acf(SerieEnfermos, main='Función de autocorrelación (ACF) - SerieEnfermos')

#Función de autocorrelación parcial (PACF) que mide la correlación entre una serie de tiempo y sus valores retrasados
Pacf(SerieEnfermos, main='Función de autocorrelación parcial (PACF) - SerieEnfermos')
#Detectar cúal es el mejor modelo a utilizar con Arima
auto.arima(SerieEnfermos)
#Obtenemos (1,0,0) y (1,0,1), pero dado el análisis de ACF y PACF, Arima(1,0,1) es una mejor alternativa.

#Analizamos que los residuos sean Ruido Blanco
#Hipótesis H0: No hay autocorrelación de los residuos H1: Existe autocorrelación de los residuos
ModeloArima=Arima(SerieEnfermos,order = c(1,0,1))
Box.test(ModeloArima$residuals, lag = 20, type = "Ljung-Box")
#p-value = 0.001095 < 0.05 por lo tanto existe autocorrelación de los residuos
#Se puede rechazar la hipótesis nula y concluir que la muestra no sigue una distribución normal.

#Realizamos pronóstico de los siguientes 3 meses y su correspondiente gráfica
Pronostico=forecast(ModeloArima,level= c(95), h=3)
plot(Pronostico)
Pronostico
#Obteniendo Febrero 2023 = 188.5405, Marzo 2023 = 289.4107 y Abril 2023 = 344.9104
