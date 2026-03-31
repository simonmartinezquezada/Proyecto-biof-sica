#instalación de paquetes
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("readxl")
#cargar librerías
library(tidyverse)  
library(ggplot2)
library(readxl)
#cargar datos
balances <- read.table("Tarea 1/data_balance.txt", header = TRUE)
texturas <- read.csv("Tarea 1/data_textura.csv", header = TRUE)
head(balances)
head(texturas)

df_balances <-as.data.frame(balances)
df_texturas <- as.data.frame(texturas)
df_balances$Fecha <- as.Date(df_balances$Fecha)
class(df_balances$ETr)
class(df_balances$Fecha)
class(df_balances$Precipitacion)
class(df_texturas$Horizonte)
summary(df_balances)

df_balances$delta_S <- df_balances$Precipitacion - df_balances$ETr
summary(df_balances)

svg("grafico_balance_hidrico.svg", width = 11, height = 10)

par(mfrow = c(3, 1)) #3 filas; 1 columna
par(mar = c(1, 4, 1, 1))

#GRAFICO DE PRECIPITACION
plot(df_balances$Fecha, df_balances$Precipitacion,
     type = "h",         # líneas verticales
     col = "darkgreen",
     lwd = 2,
     xaxt="n", #esta línea suprime el eje x automático
     yaxt="n", # Suprime el eje Y automático para personalizarlo
     ylab = expression(Precipitacion~(mm~dia^{-1})),
     ylim = c(150,0), #con este set se invierten los ejes en el gráfico de precipitación
     main = "Precipitación a lo largo del tiempo")
axis(2, at = seq(min(df_balances$Precipitacion), max(df_balances$Precipitacion),by=20)) 

#GRAFICO DE ETr
plot(df_balances$Fecha, df_balances$ETr,
     type = "l",
     col = "red",
     lwd = 2,
     xaxt="n", # Suprime el eje X automático
     yaxt="n", # Suprime el eje Y automático para personalizarlo
     ylab = expression(Evapotranspiración~(mm~dia^{-1})),
     ylim=c(0,6),
     main = "Evapotranspiración a lo largo del tiempo")
axis(2, at = seq(0,6, by = 1.5)) # Para que el gráfico vaya de 0 a 6 cada 1.5

# GRAFICO DE Delta S
plot(df_balances$Fecha, df_balances$delta_S,
     type = "l",
     col = "brown",
     lwd = 2,
     xlab = "Fecha",
     yaxt="n", # Suprime el eje Y automático para personalizarlo
     ylab = expression(delta_S~(mm~dia^{-1})),
     main = "Delta S a lo largo del tiempo")
# Ajusta el rango y el 'by' según los valores reales de delta_S.
axis(2, at = seq(round(min(df_balances$delta_S), -1), round(max(df_balances$delta_S), -1), by = 50)) # Ejemplo: Eje Y de 50 en 50, ajustando al rango de datos

axis.Date(1,at = seq(min(df_balances$Fecha), max(df_balances$Fecha), by = "2 months"),
          format = "%m-%y", cex.axis = 1.3)

dev.off()