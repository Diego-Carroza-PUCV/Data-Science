# Instalar paquetes
# Tools -> Install Packages 
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

ETA <- filter(ETA,!is.na(ETA$`Tasa Ataque`))
ETA <- filter(ETA,ETA$`Tasa Ataque`<=100)

ETA_CasosConfirmados <- filter(ETA, ETA$`Conclusión del brote`=="Realizada, se confirma")
View(ETA_CasosConfirmados)

ETA_CasosConfirmados <- filter(ETA_CasosConfirmados, ETA_CasosConfirmados$Enfermos!=5470)
ETA_CasosConfirmados <- filter(ETA_CasosConfirmados, ETA_CasosConfirmados$Enfermos!=517)
view(ETA_CasosConfirmados)

ETA_Enfermos <- select(ETA_CasosConfirmados,4,11)
view(ETA_Enfermos)

colnames(ETA_Enfermos)
colnames(ETA_Enfermos)[1] <- "Fecha"
colnames(ETA_Enfermos)[2] <- "Enfermos"


ETA_Enfermos <- ETA_Enfermos %>%
  group_by(Fecha) %>%
  summarise(Enfermos=sum(Enfermos))
view(ETA_Enfermos)

ETA_Enfermos$Mes_Correlativo <- as.integer(format(ETA_Enfermos$Fecha, "%m")) + 12 * (as.integer(format(ETA_Enfermos$Fecha, "%Y")) - 2011)


ETA_Enfermos <- ETA_Enfermos %>%
  group_by(Mes_Correlativo) %>%
  summarise(Enfermos=sum(Enfermos))
view(ETA_Enfermos)


Enfermos=select(ETA_Enfermos,2)
view(Enfermos)
SerieEnfermos=ts(Enfermos$Enfermos,freq=12,start=c(2011,1))


plot(SerieEnfermos, main="Enfermos enero 2011 - enero 2023",xlab="Año",ylab="Número de Enfermos")


Descom<- decompose(SerieEnfermos)
par(mfrow = c(2, 2)) 
plot(Descom$x, main = "Enfermos enero 2011 - enero 2023", col = "black", ylab = "Número de Enfermos", xlab = "Año")
plot(Descom$trend, main = "Tendencia", col = "blue", ylab = "Número de Enfermos", xlab = "Año")
plot(Descom$seasonal, main = "Estacionalidad", col = "red", ylab = "Número de Enfermos", xlab = "Año")
plot(Descom$random, main = "Irregularidad", col = "green", ylab = "Número de Enfermos", xlab = "Año")

adf.test(SerieEnfermos)
Acf(SerieEnfermos, main='Función de autocorrelación (ACF) - SerieEnfermos')

Pacf(SerieEnfermos, main='Función de autocorrelación parcial (PACF) - SerieEnfermos')
auto.arima(SerieEnfermos)

Box.test(ModeloArima$residuals, lag = 20, type = "Ljung-Box")

Pronostico=forecast(ModeloArima,level= c(95), h=3)
plot(Pronostico)
Pronostico

