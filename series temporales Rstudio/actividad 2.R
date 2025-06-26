# Definir la ruta donde guardaste el archivo
ruta <- "C:/Users/yovan/Documents/estadistica/temperaturas_diarias.csv"

# Leer el archivo CSV
temp <- read.csv(ruta)

# Convertir la columna de fecha al formato correcto
temp$Fecha <- as.Date(temp$Fecha)

# Mostrar las primeras filas para verificar
head(temp)

library(ggplot2)
library(zoo)

# Crear la serie temporal con zoo
ts_temp <- zoo(temp$Temperatura, order.by = temp$Fecha)

# Graficar la serie temporal
ggplot(data.frame(Fecha=index(ts_temp), Temperatura=coredata(ts_temp)), 
       aes(x=Fecha, y=Temperatura)) +
  geom_line(color="blue") +
  ggtitle("Temperaturas Diarias (2015-2024)") +
  ylab("Temperatura (°C)") +
  xlab("Año") +
  theme_minimal()

# Convertir a serie temporal (formato TS)
ts_obj <- ts(temp$Temperatura, start=c(2015,1), frequency=365)

# Descomponer la serie (multiplicativa)
descomp <- decompose(ts_obj, type="multiplicative")

# Graficar la descomposición
plot(descomp)

library(forecast)
Acf(ts_obj)  # Autocorrelación simple
Pacf(ts_obj) # Autocorrelación parcial

modelo <- auto.arima(ts_obj)  # Encuentra automáticamente el mejor modelo ARIMA
forecasted <- forecast(modelo, h=365)  # Predicción para 1 año (365 días)
autoplot(forecasted)

accuracy(modelo)

plot(ts_obj, main="Serie Temporal Original", col="blue")


length(ts_obj)


modelo <- auto.arima(ts_obj)

AIC(modelo)



