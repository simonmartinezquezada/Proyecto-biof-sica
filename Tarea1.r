#instalación de paquetes
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("here") #esta librería permite buscar los archivos en la carpeta asociada al script sin necesariamente fijar un directorio
#install.packages("soiltexture")

#usar ctrl + alt + r para correr todo

#cargar librerías
library(tidyverse)  
library(ggplot2)
library(readxl)
library(here)
library(soiltexture)
#cargar datos
balances <- read.table(here("data_balance.txt"), header = TRUE) #revisar directorio en el que está alojado
texturas <- read.csv(here("data_textura.csv"), header = TRUE)
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

df_balances$delta_S <- df_balances$Precipitacion - df_balances$ETr #no es necesario filtrar por >0 de precipitación?
summary(df_balances)

svg(here("graficos", "graficos_PP_Etr_deltaS.svg"), width = 11, height = 10)

par(mfrow = c(3, 1)) #3 filas; 1 columna
par(mar = c(1, 4, 1, 1)) #bottom, left, top, right

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

axis.Date(1,at = seq(min(df_balances$Fecha), max(df_balances$Fecha), by = "6 months"),
          format = "%m-%y", cex.axis = 1.3)

dev.off()

#########TEXTURA DE SUELO###########################################
head(texturas)
tail(texturas)
class(texturas)

sitio1 <- subset(texturas, sitio == 1)
sitio2 <- subset(texturas, sitio == 2)

class(sitio1)
head(sitio2)

lista_suelos <- list(sitio1, sitio2)
print(lista_suelos)

# Cambio de nombres, para identificar los datos
names(lista_suelos) <- c("Sitio_1", "Sitio_2")

# Lista vacía para guardar resultados
datos_finales <- vector("list", length(lista_suelos));datos_finales

# For para procesar cada hoja
for(i in 1:length(lista_suelos)){
  
  df <- lista_suelos[[i]]
  
  # Renombrar
  names(df) <- c("X", "SITIO", "HORIZONTE", "CLAY", "SILT", "SAND")
  
  # Normalizar, para evitar error de que no sumen exactamente 100%
  suma <- df$CLAY + df$SILT + df$SAND
  
  df$CLAY <- df$CLAY / suma * 100
  df$SILT <- df$SILT / suma * 100
  df$SAND <- df$SAND / suma * 100
  
  # Revisar consistencia
  TT.data.test(tri.data = df)
  #TT.data.test(tri.data = df, text.tol = 0.1) #cambio de tolerancia, puede reemplazar la normalización
  
  # Guardar resultado final en la lista
  datos_finales[[i]] <- data.frame(
    HORIZONTE = df$HORIZONTE,
    CLAY = df$CLAY,
    SILT = df$SILT,
    SAND = df$SAND
  )
}
print(datos_finales)

# Extraer resultados finales por sitio
datos1_final <- datos_finales[[1]]
datos2_final <- datos_finales[[2]]

# Para usar en TT.plot
text1 <- datos1_final[, c("CLAY", "SILT", "SAND")];text1
text2 <- datos2_final[, c("CLAY", "SILT", "SAND")];text2

# Colores de puntos por horizonte
colores_base <- c("red3", "blue3", "darkgreen", "orange3", "purple3",
                  "gray30","deeppink3", "goldenrod3", "cyan4" )

colores1 <- colores_base[1:nrow(datos1_final)]
colores2 <- colores_base[1:nrow(datos2_final)]

col_fondo <- c(
  "honeydew", "palegreen1", "palegreen3", "darkseagreen1",
  "darkseagreen4", "olivedrab1", "olivedrab3", "darkolivegreen1",
  "darkolivegreen4", "springgreen2", "seagreen2",
  "forestgreen"
)

svg(here("graficos", "Triangulo_textural_2sitios_T1.svg"),
  width = 14,
  height = 8)

par(mfrow = c(1, 2), pty = "s", mar = c(1, 1, 3, 1)) #modificacion de los márgenes de la imagen; mar=bottom, left, top, right


# -------- Sitio 1 --------
geo1 <- TT.plot(             #VISUALIZACION DEL TRIANGULO TEXTURAL CON LOS PUNTOS DEL SITIO 1
  class.sys = "USDA.TT",     #clasificacion de las texturas a utilizar
  class.p.bg.col = col_fondo,#colores del triangulo
  tri.data = text1,          #datos de textura del sitio 1 (df sin calumna de horizotes)
  pch = 19,                  #diseño del punto
  col = colores1,            #colores de los puntos 
  cex = 0.5,                 #tamaño de los puntos
  lwd.axis = 0.8,            #tamaño de las lineas del triangulo
  cex.lab = 0.9,             #tamaño de la letra de titulos de los ejes
  cex.axis = 0.8,            #tamaño de los números de los ejes
  lang = "en",               #idioma
  main = "Sitio 1"           #titulo del triángulo
)


TT.text(                     #ETIQUETAS DE LOS HORIZONTES 
  tri.data = text1,
  geo = geo1,
  labels = datos1_final$HORIZONTE,
  col = colores1,
  font = 2,
  cex = 0.85,
  pos = c(1, 3, 2, 4, 3, 3),  #posición de la etiqueta (abajo, izquierda, arriba, derecha)=(1, 2, 3, 4)
  offset = 0.2
)

legend(                      #LEYENDA CON EL DETALLE DE LOS HORIZONTES: PROFUNDIDAD Y COLOR DEL PUNTO
  "topright",
  legend = datos1_final$HORIZONTE,
  col = colores1,
  pch = 19,
  pt.cex = 1.1,
  cex = 0.8,
  bty = "n",
  title = "Horizontes"
)

# -------- Sitio 2 --------
geo2 <- TT.plot(
  class.sys = "USDA.TT",
  class.p.bg.col = col_fondo,
  tri.data = text2,
  pch = 19,
  col = colores2,
  cex = 0.5,
  lwd.axis = 0.8,
  cex.lab = 0.9,
  cex.axis = 0.8,
  lang = "en",
  main = "Sitio 2"
)

TT.text(
  tri.data = text2,
  geo = geo2,
  labels = datos2_final$HORIZONTE,
  col = colores2,
  font = 2,
  cex = 0.85,
  pos = c(3, 1, 4, 2, 3), #(rojo, azul, verde, amarillo, morado)
  offset = 0.2
)

legend(
  "topright",
  legend = datos2_final$HORIZONTE,
  col = colores2,
  pch = 19,
  pt.cex = 1.1,
  cex = 0.8,
  bty = "n",
  title = "Horizontes"
)

dev.off()
