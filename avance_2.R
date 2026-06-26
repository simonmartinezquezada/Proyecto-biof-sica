#Avance 2 - Script unificado:
#  Parte 1: Parámetros hidráulicos CLSoilMaps (ROSETTA_MEAN): mapas de θr, θs, αvg, n y Ks por horizonte.
#  Parte 2: Balance hídrico diario: ETo (Penman-Monteith FAO), ETc (método dual Kcb por clase de
#           cobertura), modelo bucket píxel a píxel, SVG dinámica anual del píxel seleccionado,
#           TIF almacenamiento día 365, TIF días de estrés hídrico. Estación INIA Las Puentes (Arauco).


#### CONFIGURACIÓN Y CARGA DE LIBRERÍAS ####
#paquetes (descomentar para instalar si es necesario)
#install.packages("sf")
#install.packages("terra")
#install.packages("here")
#install.packages("prettymapr")
#install.packages("readxl")
#install.packages("knitr")
#install.packages("beepr")
#install.packages("dplyr")  #necesario para el cálculo de porcentaje de LandCover por píxel

#librerias
library(beepr)
library(sf)
library(terra)
library(here)
library(prettymapr)
library(readxl)
library(knitr)
library(dplyr)  #cálculo de porcentaje y selección de cobertura dominante por píxel de suelo


#### DIRECTORIOS ####
fig_dir     <- "C:/Users/simon/Downloads/Trabajo_final_Grupo5/figuras"        #carpeta de salida de figuras y TIFs (debe existir previamente)
rosetta_dir <- "C:/Users/simon/Downloads/Trabajo_final_Grupo5/ROSETTA_MEAN"   #carpeta con rasters ROSETTA: θr, θs, αvg, n, Ks, FC
INIA_raw    <- read_xlsx("C:/Users/simon/Downloads/Trabajo_final_Grupo5/datos_INIA.xlsx")
lc_raw      <- rast("C:/Users/simon/Downloads/Trabajo_final_Grupo5/CLDynamicLandCover_2018_1.0.tif")


#### LECTURA DEL POLÍGONO DE ESTUDIO ####
#se lee el shapefile generado en el avance 1; contiene el polígono de 30 km² centrado en el punto de trabajo
crs_latlong <- "EPSG:4326"
polygon     <- read_sf(file.path(fig_dir, "polygon.shp"), layer = "polygon")
head(polygon)


#### FUNCIONES DE MANEJO DE RASTER ####

#función para recortar cualquier raster (r) al polígono de estudio
#reprojection del polígono al CRS del raster, crop por extensión rectangular y mask por forma exacta
poligono_recorte <- function(r, polygon){
  polygon_r <- st_transform(polygon, crs = crs(r))  #iguala el CRS del polígono al del raster
  polygon_v <- vect(polygon_r)                       #convierte el polígono a SpatVector (terra)
  r_crop    <- crop(r, polygon_v)                    #recorta al bounding box del polígono
  r_mask    <- mask(r_crop, polygon_v)               #aplica la máscara exacta del polígono (NA fuera)
  return(r_mask)
}

#función para agregar elementos cartográficos: borde del polígono, flecha norte y barra de escala
#permite mantener consistencia cartográfica en todos los mapas exportados
agregar_elementos_mapa <- function(r_plot, polygon,
                                   north_pos  = "topright",   scale_pos  = "bottomleft",
                                   north_pad  = c(0.95, 0.3), scale_pad  = c(1, 0.45),
                                   north_scale = 0.6){
  # borde del polígono
  plot(vect(st_transform(polygon, crs(r_plot))), add = TRUE, border = "black", lwd = 1.5)
  # flecha norte
  prettymapr::addnortharrow(pos = north_pos, padin = north_pad, scale = north_scale, text.col = "black")
  # barra de escala (en metros, compatible con CRS del polígono)
  prettymapr::addscalebar(plotunit = "m", pos = scale_pos, padin = scale_pad, label.cex = 0.75)
}

#función para guardar un raster recortado como .svg individual con elementos cartográficos
#recibe el raster original (r), el polígono, título, nombre del archivo de salida, método de reproyección y paleta
guardar_svg_raster <- function(r, polygon, titulo, archivo_salida, metodo = "bilinear",
                                paleta = viridisLite::viridis(100),
                                north_pos = "topright", scale_pos = "bottomleft"){
  r_cut  <- poligono_recorte(r, polygon)                           #recorta el raster al polígono
  r_plot <- project(r_cut, st_crs(polygon)$wkt, method = metodo)  #reproyecta al CRS del polígono (UTM)
  svg(file.path(fig_dir, archivo_salida), width = 8, height = 6)  #abre dispositivo svg con dimensiones fijas
  par(mar = c(5, 5, 4, 6))
  plot(r_plot, main = titulo, col = paleta, axes = TRUE)           #grafica el raster con título y ejes UTM
  agregar_elementos_mapa(r_plot, polygon, north_pos = north_pos, scale_pos = scale_pos)
  dev.off()
  return(r_plot)  #retorna el raster recortado y reproyectado para cálculos posteriores
}

#función para guardar un panel de 6 mapas (uno por horizonte) en un solo .svg con escala de color común
#evita la repetición del bloque svg + par + for + dev.off en cada parámetro
guardar_panel_rasters <- function(lista_rasters, horizontes, archivo_salida, titulo_base, paleta,
                                   zlim = NULL, ncol_panel = 3, nrow_panel = 2,
                                   agregar_norte = FALSE, agregar_escala = FALSE){
  svg(file.path(fig_dir, archivo_salida), width = 12, height = 8)
  par(mfrow = c(nrow_panel, ncol_panel), mar = c(3.2, 3.2, 3, 4.5), oma = c(1, 1, 1, 1))
  for (hz in horizontes) {
    r_actual <- lista_rasters[[hz]]
    plot(r_actual, main = paste(titulo_base, hz, "cm"), col = paleta, range = zlim, axes = TRUE) #range en lugar de zlim: fuerza tanto el mapeo de colores como los límites de la leyenda al rango global común
    plot(vect(st_transform(polygon, crs(r_actual))), add = TRUE, border = "black", lwd = 1)

    # FLECHA NORTE (opcional, activar con agregar_norte = TRUE)
    if (agregar_norte) {
      prettymapr::addnortharrow(pos = "topright", scale = 0.32, padin = c(0.12, 0.3), text.col = "black")
    }

    # BARRA DE ESCALA (opcional, activar con agregar_escala = TRUE)
    if (agregar_escala) {
      prettymapr::addscalebar(plotunit = "m", pos = "bottomleft", padin = c(0.18, 0.3), label.cex = 0.45)
    }
  }
  dev.off()
}


#### HORIZONTES ####
#los horizontes corresponden a las 6 profundidades de los archivos .tif de CLSoilMaps (ROSETTA_MEAN)
horizontes <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
espesores  <- c(5, 10, 15, 30, 40, 100)  #cm de espesor de cada horizonte; suma = 200 cm
prof_total <- 200                          #profundidad total del suelo modelada (cm)


#### ==================================================================== ####
####      PARTE 1 - PARÁMETROS HIDRÁULICOS CLSoilMaps (ROSETTA_MEAN)      ####
#### ==================================================================== ####


#### MAPA θr - CONTENIDO DE AGUA RESIDUAL ####
#θr (theta_r): fracción volumétrica de agua que no puede ser extraída por succión (cm³/cm³)
#archivos fuente: theta_r_0-5cm.tif, theta_r_5-15cm.tif, ..., theta_r_100-200cm.tif

theta_r_cut <- list()  #lista para almacenar los rasters recortados por horizonte
paleta_theta_r <- rev(hcl.colors(100, "Blues"))  #paleta azul: mayor θr = mayor agua residual

for (hz in horizontes) {
  archivo    <- paste0("theta_r_", hz, "cm.tif")                  #nombre del archivo según horizonte
  nombre_svg <- paste0("theta_r_", gsub("-", "_", hz), "cm.svg")  #nombre del .svg de salida

  theta_r_r <- rast(file.path(rosetta_dir, archivo))  #carga el raster del horizonte correspondiente

  theta_r_cut[[hz]] <- guardar_svg_raster(
    r             = theta_r_r,
    polygon       = polygon,
    titulo        = paste("θr (cm³/cm³) -", hz, "cm"),
    paleta        = paleta_theta_r,
    archivo_salida = nombre_svg,
    metodo        = "bilinear"
  )  #recorta, reproyecta, agrega cartografía y guarda el .svg individual
}

#mínimos y máximos globales de θr (para escala común en el panel)
mins_theta_r  <- sapply(horizontes, function(hz) global(theta_r_cut[[hz]], "min", na.rm = TRUE)[1,1])
maxs_theta_r  <- sapply(horizontes, function(hz) global(theta_r_cut[[hz]], "max", na.rm = TRUE)[1,1])
min_theta_r_g <- min(mins_theta_r, na.rm = TRUE)
max_theta_r_g <- max(maxs_theta_r, na.rm = TRUE)

#panel de θr por horizonte con escala de color común
guardar_panel_rasters(
  lista_rasters  = theta_r_cut,
  horizontes     = horizontes,
  archivo_salida = "theta_r_por_horizonte.svg",
  titulo_base    = "θr (cm³/cm³) -",
  paleta         = paleta_theta_r,
  zlim           = c(min_theta_r_g, max_theta_r_g),
  agregar_norte  = TRUE,
  agregar_escala = TRUE
)


#### MAPA θs - CONTENIDO DE AGUA A SATURACIÓN ####
#θs (theta_s): fracción volumétrica de agua cuando el suelo está completamente saturado (cm³/cm³)
#equivalente a la porosidad total del suelo en condiciones de saturación completa
#archivos fuente: theta_s_0-5cm.tif, theta_s_5-15cm.tif, ..., theta_s_100-200cm.tif

theta_s_cut <- list()
paleta_theta_s <- rev(hcl.colors(100, "Teal"))  #paleta verde-azul: valores típicos 0.30-0.65

for (hz in horizontes) {
  archivo    <- paste0("theta_s_", hz, "cm.tif")
  nombre_svg <- paste0("theta_s_", gsub("-", "_", hz), "cm.svg")

  theta_s_r <- rast(file.path(rosetta_dir, archivo))

  theta_s_cut[[hz]] <- guardar_svg_raster(
    r             = theta_s_r,
    polygon       = polygon,
    titulo        = paste("θs (cm³/cm³) -", hz, "cm"),
    paleta        = paleta_theta_s,
    archivo_salida = nombre_svg,
    metodo        = "bilinear"
  )
}

#mínimos y máximos globales de θs
mins_theta_s  <- sapply(horizontes, function(hz) global(theta_s_cut[[hz]], "min", na.rm = TRUE)[1,1])
maxs_theta_s  <- sapply(horizontes, function(hz) global(theta_s_cut[[hz]], "max", na.rm = TRUE)[1,1])
min_theta_s_g <- min(mins_theta_s, na.rm = TRUE)
max_theta_s_g <- max(maxs_theta_s, na.rm = TRUE)

#panel de θs por horizonte con escala de color común
guardar_panel_rasters(
  lista_rasters  = theta_s_cut,
  horizontes     = horizontes,
  archivo_salida = "theta_s_por_horizonte.svg",
  titulo_base    = "θs (cm³/cm³) -",
  paleta         = paleta_theta_s,
  zlim           = c(min_theta_s_g, max_theta_s_g),
  agregar_norte  = TRUE,
  agregar_escala = TRUE
)


#### MAPA αvg - PARÁMETRO DE ESCALA VAN GENUCHTEN ####
#αvg (alpha): parámetro de escala de la curva de retención de Van Genuchten (cm⁻¹)
#valores más altos indican suelos más gruesos (arenosos) con menor capacidad de retención de agua
#archivos fuente: alpha_0-5cm.tif, alpha_5-15cm.tif, ..., alpha_100-200cm.tif

alpha_cut <- list()
paleta_alpha <- rev(hcl.colors(100, "YlOrRd"))  #paleta amarillo-rojo: mayor alpha = suelo más grueso

for (hz in horizontes) {
  archivo    <- paste0("alpha_", hz, "cm.tif")
  nombre_svg <- paste0("alpha_vg_", gsub("-", "_", hz), "cm.svg")  #prefijo alpha_vg para distinguir de otros alpha

  alpha_r <- rast(file.path(rosetta_dir, archivo))

  alpha_cut[[hz]] <- guardar_svg_raster(
    r             = alpha_r,
    polygon       = polygon,
    titulo        = paste("αvg (cm⁻¹) -", hz, "cm"),
    paleta        = paleta_alpha,
    archivo_salida = nombre_svg,
    metodo        = "bilinear"
  )
}

#mínimos y máximos globales de αvg
mins_alpha  <- sapply(horizontes, function(hz) global(alpha_cut[[hz]], "min", na.rm = TRUE)[1,1])
maxs_alpha  <- sapply(horizontes, function(hz) global(alpha_cut[[hz]], "max", na.rm = TRUE)[1,1])
min_alpha_g <- min(mins_alpha, na.rm = TRUE)
max_alpha_g <- max(maxs_alpha, na.rm = TRUE)

#panel de αvg por horizonte con escala de color común
guardar_panel_rasters(
  lista_rasters  = alpha_cut,
  horizontes     = horizontes,
  archivo_salida = "alpha_vg_por_horizonte.svg",
  titulo_base    = "αvg (cm⁻¹) -",
  paleta         = paleta_alpha,
  zlim           = c(min_alpha_g, max_alpha_g),
  agregar_norte  = TRUE,
  agregar_escala = TRUE
)


#### MAPA n - PARÁMETRO DE FORMA VAN GENUCHTEN ####
#n: parámetro de forma de la curva de retención de Van Genuchten (adimensional)
#controla la pendiente de la curva de retención; valores mayores indican suelos más gruesos
#valores típicos: suelos arenosos n > 2, arcillosos n cercano a 1
#archivos fuente: n_0-5cm.tif, n_5-15cm.tif, ..., n_100-200cm.tif

n_cut <- list()
paleta_n <- rev(hcl.colors(100, "Purple-Orange"))  #paleta divergente: resalta la variación espacial de n

for (hz in horizontes) {
  archivo    <- paste0("n_", hz, "cm.tif")
  nombre_svg <- paste0("n_vg_", gsub("-", "_", hz), "cm.svg")  #prefijo n_vg para indicar parámetro de Van Genuchten

  n_r <- rast(file.path(rosetta_dir, archivo))

  n_cut[[hz]] <- guardar_svg_raster(
    r             = n_r,
    polygon       = polygon,
    titulo        = paste("n (adim.) -", hz, "cm"),
    paleta        = paleta_n,
    archivo_salida = nombre_svg,
    metodo        = "bilinear"
  )
}

#mínimos y máximos globales de n
mins_n  <- sapply(horizontes, function(hz) global(n_cut[[hz]], "min", na.rm = TRUE)[1,1])
maxs_n  <- sapply(horizontes, function(hz) global(n_cut[[hz]], "max", na.rm = TRUE)[1,1])
min_n_g <- min(mins_n, na.rm = TRUE)
max_n_g <- max(maxs_n, na.rm = TRUE)

#panel de n por horizonte con escala de color común
guardar_panel_rasters(
  lista_rasters  = n_cut,
  horizontes     = horizontes,
  archivo_salida = "n_vg_por_horizonte.svg",
  titulo_base    = "n (adim.) -",
  paleta         = paleta_n,
  zlim           = c(min_n_g, max_n_g),
  agregar_norte  = TRUE,
  agregar_escala = TRUE
)


#### MAPA Ks - CONDUCTIVIDAD HIDRÁULICA SATURADA ####
#Ks (ksat): conductividad hidráulica del suelo saturado (cm/día), según CLSoilMaps ROSETTA
#controla la velocidad máxima con que el agua puede moverse a través del suelo
#valores muy bajos en arcillas; valores muy altos en arenas
#archivos fuente: ksat_0-5cm.tif, ksat_5-15cm.tif, ..., ksat_100-200cm.tif

Ks_cut <- list()
paleta_Ks <- hcl.colors(100, "viridis", rev = FALSE)  #paleta viridis: convención habitual para Ks

for (hz in horizontes) {
  archivo    <- paste0("ksat_", hz, "cm.tif")
  nombre_svg <- paste0("Ks_", gsub("-", "_", hz), "cm.svg")

  ksat_r <- rast(file.path(rosetta_dir, archivo))

  Ks_cut[[hz]] <- guardar_svg_raster(
    r             = ksat_r,
    polygon       = polygon,
    titulo        = paste("Ks (cm/día) -", hz, "cm"),
    paleta        = paleta_Ks,
    archivo_salida = nombre_svg,
    metodo        = "bilinear"
  )
}

#mínimos y máximos globales de Ks
mins_Ks  <- sapply(horizontes, function(hz) global(Ks_cut[[hz]], "min", na.rm = TRUE)[1,1])
maxs_Ks  <- sapply(horizontes, function(hz) global(Ks_cut[[hz]], "max", na.rm = TRUE)[1,1])
min_Ks_g <- min(mins_Ks, na.rm = TRUE)
max_Ks_g <- max(maxs_Ks, na.rm = TRUE)

#panel de Ks por horizonte con escala de color común
guardar_panel_rasters(
  lista_rasters  = Ks_cut,
  horizontes     = horizontes,
  archivo_salida = "Ks_por_horizonte.svg",
  titulo_base    = "Ks (cm/día) -",
  paleta         = paleta_Ks,
  zlim           = c(min_Ks_g, max_Ks_g),
  agregar_norte  = TRUE,
  agregar_escala = TRUE
)


#### RESULTADOS NUMÉRICOS - PARTE 1 ####

cat("\n============================================\n",
    "PARÁMETROS HIDRÁULICOS CLSoilMaps (ROSETTA)\n",
    "============================================\n")

#tabla resumen de promedios por horizonte para cada parámetro
resumen_rosetta <- data.frame(
  horizonte = horizontes,
  theta_r   = NA,
  theta_s   = NA,
  alpha_vg  = NA,
  n_vg      = NA,
  Ks        = NA
)

for (i in seq_along(horizontes)) {
  hz <- horizontes[i]
  resumen_rosetta$theta_r[i]  <- round(global(theta_r_cut[[hz]], "mean", na.rm = TRUE)[1,1], 4)
  resumen_rosetta$theta_s[i]  <- round(global(theta_s_cut[[hz]], "mean", na.rm = TRUE)[1,1], 4)
  resumen_rosetta$alpha_vg[i] <- round(global(alpha_cut[[hz]],   "mean", na.rm = TRUE)[1,1], 6)
  resumen_rosetta$n_vg[i]     <- round(global(n_cut[[hz]],       "mean", na.rm = TRUE)[1,1], 4)
  resumen_rosetta$Ks[i]       <- round(global(Ks_cut[[hz]],      "mean", na.rm = TRUE)[1,1], 4)
}

cat("\n---- θr (cm³/cm³) ----\n")
cat("Mínimo global:", round(min_theta_r_g, 4), "\n",
    "Máximo global:", round(max_theta_r_g, 4), "\n")

cat("\n---- θs (cm³/cm³) ----\n")
cat("Mínimo global:", round(min_theta_s_g, 4), "\n",
    "Máximo global:", round(max_theta_s_g, 4), "\n")

cat("\n---- αvg (cm⁻¹) ----\n")
cat("Mínimo global:", round(min_alpha_g, 6), "\n",
    "Máximo global:", round(max_alpha_g, 6), "\n")

cat("\n---- n (adim.) ----\n")
cat("Mínimo global:", round(min_n_g, 4), "\n",
    "Máximo global:", round(max_n_g, 4), "\n")

cat("\n---- Ks (cm/día) ----\n")
cat("Mínimo global:", round(min_Ks_g, 4), "\n",
    "Máximo global:", round(max_Ks_g, 4), "\n")

cat("\n")
kable(resumen_rosetta,
      col.names = c("Horizonte", "θr (cm³/cm³)", "θs (cm³/cm³)", "αvg (cm⁻¹)", "n (adim.)", "Ks (cm/día)"),
      caption   = "Promedios espaciales de parámetros hidráulicos por horizonte (CLSoilMaps ROSETTA)")


#### ==================================================================== ####
####      PARTE 2 - BALANCE HÍDRICO DIARIO (MODELO BUCKET)                ####
#### ==================================================================== ####


#### TABLA 1: PARÁMETROS DE COBERTURA VEGETAL (Valores obtenidos del enunciado, solo para esta actividad) ####
#IAF: índice de área foliar (m² m⁻²), h_veg: altura de vegetación (m), kcbtab: Kcb de tabla FAO
#values = códigos de clase del raster CLDynamicLandCover_2018

tabla1 <- data.frame(
  value  = c(  1,   2,   3,   4,   5,   6,   7,   8,   9,  10,   11,   12,  13,   14,  15,  16),
  nombre = c("Agua", "Playas/dunas", "Bosque mediterráneo", "Bosque templado",
             "Plantación hoja ancha", "Árboles frutales", "Glaciar/nieve",
             "Vegetación ripariana", "Matorrales", "Plantación aciculada",
             "Praderas/anuales", "Praderas siempre verde", "Suelo desnudo",
             "Turberas", "Urbanización", "Plantación cosechada"),
  IAF    = c(  0,   0, 2.5, 3.5, 2.5, 2.0,   0, 2.0, 2.0, 3.5,  1.5,  2.0,  0,  2.0,  0,  0),
  h_veg  = c(  0,   0,  10,  20,  10, 6.0,   0, 4.0, 3.0,  20, 0.05, 0.02,  0, 0.02,  0,  0),
  kcbtab = c(  0,   0, 1.2, 1.2, 1.2, 1.0,   0, 0.9, 1.0, 1.2,  1.2,  0.8,  0,  0.8,  0,  0)
)
#Ajuste de coberturas sin vegetación:
#- Agua (clase 1): se conserva en el mapa de cobertura, pero se excluye del bucket de suelo,
#  del almacenamiento S, del cabezal h y del conteo de estrés hídrico.
#- Suelo desnudo (clase 13): no tiene transpiración (Tp=0), pero sí evaporación del suelo (Ep).
#- Las demás clases sin parametrización específica conservan ET=0 en el alcance de este avance.


#### CONSTANTES DEL MODELO DUAL (enunciado Avance 2) ####
Fc_cte  <- 0.1   #fracción de cobertura fija para todos los píxeles
fw_cte  <- 1.0   #fracción de superficie humedecida por precipitación
kcmin   <- 0.15  #coeficiente Kcb mínimo bajo condiciones muy secas
kr      <- 1.0   #coeficiente de reducción de evaporación (=1 equivale a etapa 1 de evaporación)
few_cte <- min(1 - Fc_cte, fw_cte)  #fracción de suelo expuesto a la atmósfera (0.9, constante)

# Para suelo desnudo se aplican literalmente las constantes generales del enunciado:
# Kcb = 0 (Tabla 1), Kr = 1, Fc = 0.1, Fw = 1 y few = 0.9.
# Como h_veg = 0, Kcmax = 1.20 y entonces:
# Ke = min[1*(1.20 - 0), 0.9*1.20] = 1.08; por tanto, Ep = 1.08 * ETo.


#### CONSTANTES DE LA ESTACIÓN INIA LAS PUENTES (ARAUCO) ####
#coordenadas aproximadas según página INIA Agrometeorología
latitud_inia <- -37.30  #grados decimales, hemisferio sur (negativo)
msnm_inia    <- 120     #metros sobre el nivel del mar de la estación
#NOTA: altura del anemómetro se asume 2 m (estándar INIA); no se aplica corrección a u2


#### FECHAS DE SIMULACIÓN #### Los datos del INIA están desde 2020-05-01 a 2026-05-01
fecha_datos <- as.POSIXct("2022-11-11")
fecha_fin   <- as.POSIXct("2025-07-16")
#el balance hídrico usa los primeros 365 días desde fecha_datos
n_dias_bal       <- 365
fecha_bal_inicio <- as.Date(fecha_datos)
fecha_bal_fin    <- fecha_bal_inicio + (n_dias_bal - 1)


#### PARÁMETROS DE FEDDES (estrés hídrico) ####
h3_feddes <- -8000   #cm de agua: inicio del estrés hídrico (α disminuye linealmente desde 1)
h4_feddes <- -15000  #cm de agua: punto de marchitez permanente (α = 0)


#### FUNCIONES ETo - PENMAN-MONTEITH FAO ####

#presión atmosférica en función de la altitud (kPa)
P_atm <- function(z){
  return(101.3 * ((293 - 0.0065 * z) / 293)^5.26)  ##kPa
}

#constante psicrométrica (kPa °C⁻¹)
psi_fn <- function(P){
  return(0.665 * 10^-3 * P)  ##kPa °C-1
}

#presión de vapor de saturación a temperatura T (kPa)
e0_fn <- function(T){
  return(0.6108 * exp((17.27 * T) / (T + 237.3)))
}

#pendiente de la curva de presión de vapor de saturación (kPa °C⁻¹)
pendiente_fn <- function(T){
  return((4098 * (0.6108 * exp((17.27 * T) / (T + 237.3)))) / ((T + 237.3)^2))
}

#presión de vapor real (kPa), calculada con T mínima y máxima y HR mínima y máxima
ea_fn <- function(Tmin, Tmax, HRmax, HRmin){
  eotmin <- 0.6108 * exp((17.27 * Tmin) / (Tmin + 237.3))
  eotmax <- 0.6108 * exp((17.27 * Tmax) / (Tmax + 237.3))
  return(((eotmin * (HRmax / 100)) + (eotmax * (HRmin / 100))) / 2)
}

#radiación extraterrestre (MJ m⁻² d⁻¹); latitud en grados decimales (negativa hemisferio sur)
Ra_fn <- function(Juliano, latitud_grados_decimales){
  radianes <- (pi / 180) * latitud_grados_decimales
  dr       <- 1 + (0.033 * cos(((2 * pi) / 365) * Juliano))              #distancia relativa inversa tierra-sol
  ds       <- 0.409 * sin((((2 * pi) / 365) * Juliano) - 1.39)           #declinación solar (radianes)
  ws       <- acos(-tan(radianes) * tan(ds))                              #ángulo solar al atardecer
  ra       <- ((24 * 60) / pi) * 0.082 * dr *
              ((ws * sin(radianes) * sin(ds)) + (cos(radianes) * cos(ds) * sin(ws)))
  return(ra)  ##MJ m-2 d-1
}

#radiación neta de onda corta (MJ m⁻² d⁻¹); albedo = 0.23 para cultivo de referencia (FAO)
Rns_fn <- function(Rs, albedo){
  return((1 - albedo) * Rs)
}

#radiación neta de onda larga (MJ m⁻² d⁻¹)
Rnl_fn <- function(Ra, msnm, ea, Tmax, Tmin, Rs){
  Tmaxk    <- Tmax + 273.15
  Tmink    <- Tmin + 273.15
  Temp     <- (Tmaxk^4 + Tmink^4) / 2
  presion  <- 0.34 - (0.14 * sqrt(ea))
  Rso      <- (0.75 + 2 * (10^-5) * msnm) * Ra     #radiación solar en día despejado
  ratio    <- Rs / Rso
  ratio    <- pmin(pmax(ratio, 0.3), 1)             #se limita ratio para evitar valores fuera de rango
  radiacion <- (1.35 * ratio) - 0.35
  Rnl      <- 4.903 * (10^-9) * Temp * presion * radiacion
  return(Rnl)  ##MJ m-2 d-1
}

#evapotranspiración de referencia ETo por Penman-Monteith FAO (mm d⁻¹)
Penman_fn <- function(pendiente, Rn, G, ctepsicrometrica, velviento_ms, es, ea, Tmedia){
  ETo <- ((0.408 * pendiente * (Rn - G)) +
          (ctepsicrometrica * (900 / (Tmedia + 273)) * velviento_ms * (es - ea))) /
         (pendiente + (ctepsicrometrica * (1 + (0.34 * velviento_ms))))
  return(ETo)  ##mm d-1
}

#Kcb ajustado por IAF (índice de área foliar), viento y humedad relativa mínima (FAO dual)
#h_veg: altura del cultivo en metros; u2: velocidad del viento a 2 m (m/s); HRmin: HR mínima (%)
kcbIAF_fn <- function(kcmin, IAF, kcbtab, u2, HRmin, h_veg){
  temp    <- (h_veg / 3)^0.3
  kcbfull <- kcbtab + (0.04 * (u2 - 2) - 0.004 * (HRmin - 45)) * temp
  kcbIAF  <- kcmin + ((kcbfull - kcmin) * (1 - exp(-0.7 * IAF)))
  return(kcbIAF)
}

#Kcb máximo (límite superior para ke); usa pmax para operación vectorizada sobre series diarias
kcmax_fn <- function(u2, HRmin, h_veg, kcb){
  temp   <- (h_veg / 3)^0.3
  kcmax1 <- 1.2 + (0.04 * (u2 - 2) - 0.004 * (HRmin - 45)) * temp
  kcmax2 <- kcb + 0.05
  return(pmax(kcmax1, kcmax2))  #pmax para vectorización sobre toda la serie diaria
}

#coeficiente de evaporación del suelo ke (adimensional); usa pmin para vectorización
ke_fn <- function(kr, kcmax, kcb, few){
  return(pmin(kr * (kcmax - kcb), few * kcmax))  #pmin para vectorización sobre serie diaria
}


#### FUNCIONES HIDRÁULICAS (Van Genuchten y Feddes) ####

#inversión analítica de la curva de retención de Van Genuchten: calcula el cabezal de presión h (cm)
#a partir del contenido de agua θ, dados los parámetros θr, θs, α (cm⁻¹) y n (adimensional)
#devuelve h ≤ 0 (tensión); h = 0 cuando θ = θs (saturado)
VG_h <- function(theta, theta_r, theta_s, alpha_vg, n_vg){
  m  <- 1 - 1 / n_vg                                                #parámetro de forma m = 1 - 1/n
  Se <- (theta - theta_r) / (theta_s - theta_r)                     #saturación efectiva [0, 1]
  Se <- pmin(pmax(Se, 1e-6), 1.0)                                   #clip para evitar división por cero
  h  <- ifelse(Se >= 1.0 - 1e-6,
               0,                                                    #suelo saturado: h = 0
               -(1 / alpha_vg) * (Se^(-1 / m) - 1)^(1 / n_vg))     #h < 0 (presión de succión, cm)
  return(h)
}

#función de respuesta al estrés hídrico de Feddes
#h: cabezal de presión (cm, negativo); h3 = -8000 cm; h4 = -15000 cm
#α(h) = 1 para h >= h3; decrece linealmente hasta α = 0 en h4; no hay estrés por exceso de agua
feddes_alpha <- function(h, h3 = h3_feddes, h4 = h4_feddes){
  alpha <- ifelse(h >= h3, 1,                        #sin estrés (zona de absorción óptima)
           ifelse(h <= h4, 0,                        #marchitez permanente: absorción nula
                  (h - h4) / (h3 - h4)))             #zona de estrés: decrecimiento lineal
  return(pmax(0, pmin(1, alpha)))                    #garantía de que α ∈ [0, 1]
}


#### CARGA DE DATOS CLIMÁTICOS INIA - LAS PUENTES, ARAUCO ####
#los datos se cargan desde datos_INIA.xlsx, período 01-05-2020 al 01-05-2026
#renombrar columnas para acceso con $ (make.names convierte espacios y caracteres especiales a puntos)
names(INIA_raw) <- make.names(names(INIA_raw), unique = TRUE)

#convertir fechas y forzar columnas numéricas
Tiempo_UTC4   <- as.Date(INIA_raw[[1]], format = "%d-%m-%Y")
INIA_diario   <- data.frame(Tiempo_UTC4, sapply(INIA_raw[2:ncol(INIA_raw)], as.numeric))
INIA_diario$Tiempo_UTC4 <- as.Date(INIA_diario$Tiempo_UTC4, format = "%d-%m-%Y")

#verificar si existe la columna de precipitación (puede no estar incluida en algunas descargas de INIA)
if (!"Precipitación.Acumulada.mm" %in% names(INIA_diario)) {
  warning(paste("ATENCIÓN: columna 'Precipitación Acumulada mm' no encontrada en datos_INIA.xlsx.",
                "Descargar desde agrometeorologia.cl e incluir en el archivo.",
                "Se asigna Pp = 0 como valor provisional; el balance hídrico no será válido."))
  INIA_diario$Precipitación.Acumulada.mm <- 0
}

#día juliano del año (1-365/366), necesario para Ra
year_col <- format(INIA_diario$Tiempo_UTC4, "%Y")
INIA_diario$Dia_Juliano_Anual <- as.numeric(INIA_diario$Tiempo_UTC4 -
                                   as.Date(paste0(year_col, "-01-01"))) + 1


#### CÁLCULO DE ETo (PENMAN-MONTEITH FAO) ####

#presión y constante psicrométrica
INIA_diario$Presion_atm_kpa   <- INIA_diario$Presión.Atmosférica.mbar / 10  #mbar → kPa
INIA_diario$ctepsicrometrica   <- psi_fn(INIA_diario$Presion_atm_kpa)

#temperatura media y presiones de vapor de saturación y real
INIA_diario$Tmedia <- (INIA_diario$Temperatura.del.Aire.Máxima.ºC +
                       INIA_diario$Temperatura.del.Aire.Mínima.ºC) / 2
eoTmax             <- e0_fn(INIA_diario$Temperatura.del.Aire.Máxima.ºC)       #presión de vapor a Tmax
eoTmin             <- e0_fn(INIA_diario$Temperatura.del.Aire.Mínima.ºC)       #presión de vapor a Tmin
INIA_diario$es     <- (eoTmax + eoTmin) / 2                                   #es: presión de vapor saturación media
INIA_diario$ea     <- ea_fn(Tmin  = INIA_diario$Temperatura.del.Aire.Mínima.ºC,
                            Tmax  = INIA_diario$Temperatura.del.Aire.Máxima.ºC,
                            HRmax = INIA_diario$Humedad.Relativa.Máxima..,
                            HRmin = INIA_diario$Humedad.Relativa.Mínima..)    #ea: presión de vapor real

#pendiente de la curva de vapor, radiaciones y balance neto
INIA_diario$pendiente <- pendiente_fn(INIA_diario$Tmedia)
INIA_diario$ra        <- Ra_fn(Juliano                = INIA_diario$Dia_Juliano_Anual,
                               latitud_grados_decimales = latitud_inia)        #radiación extraterrestre
INIA_diario$rns       <- Rns_fn(Rs = INIA_diario$Radiación.Solar.Mj.m., albedo = 0.23)  #onda corta
INIA_diario$rnl       <- Rnl_fn(Ra   = INIA_diario$ra,
                                 msnm = msnm_inia,
                                 ea   = INIA_diario$ea,
                                 Tmax = INIA_diario$Temperatura.del.Aire.Máxima.ºC,
                                 Tmin = INIA_diario$Temperatura.del.Aire.Mínima.ºC,
                                 Rs   = INIA_diario$Radiación.Solar.Mj.m.)    #onda larga
INIA_diario$RN        <- INIA_diario$rns - INIA_diario$rnl                    #radiación neta total

#velocidad del viento: convertir de km/h a m/s antes de usar en Penman
INIA_diario$velviento_m_s <- (INIA_diario$Velocidad.de.Viento.km.h * 1000) / 3600

#ETo por Penman-Monteith FAO (mm d⁻¹); G = 0 para escala diaria (supuesto FAO 56)
INIA_diario$ETo_mm <- Penman_fn(pendiente      = INIA_diario$pendiente,
                                Rn             = INIA_diario$RN,
                                G              = 0,
                                ctepsicrometrica = INIA_diario$ctepsicrometrica,
                                velviento_ms   = INIA_diario$velviento_m_s,    #m/s
                                es             = INIA_diario$es,
                                ea             = INIA_diario$ea,
                                Tmedia         = INIA_diario$Tmedia)
INIA_diario$ETo_mm <- pmax(INIA_diario$ETo_mm, 0)  #valores ligeramente negativos se reemplazan por 0

#filtrar el período de simulación
INIA_diario <- subset(INIA_diario,
                      Tiempo_UTC4 >= as.character.POSIXt(fecha_datos) &
                      Tiempo_UTC4 <= as.character.POSIXt(fecha_fin - 1))

#subconjunto de 365 días para el balance hídrico, comenzando en fecha_datos
INIA_balance <- INIA_diario[INIA_diario$Tiempo_UTC4 >= fecha_bal_inicio &
                             INIA_diario$Tiempo_UTC4 <= fecha_bal_fin, ]
if (nrow(INIA_balance) < n_dias_bal) {
  stop("No hay suficientes datos climáticos para 365 días desde fecha_datos. Revisar datos_INIA.xlsx.")
}
INIA_balance <- INIA_balance[1:n_dias_bal, ]  #asegurar 365 filas

#vectores climáticos diarios del período de balance (365 valores)
Pp_mm_bal  <- INIA_balance$Precipitación.Acumulada.mm          #precipitación (mm d⁻¹)
ETo_mm_bal <- INIA_balance$ETo_mm                               #ETo (mm d⁻¹)
u2_bal     <- INIA_balance$velviento_m_s                        #viento a 2 m (m/s)
HRmin_bal  <- INIA_balance$Humedad.Relativa.Mínima..            #HR mínima (%)
fechas_bal <- INIA_balance$Tiempo_UTC4                          #fechas del balance


#### CÁLCULO DE ETc POR CLASE DE COBERTURA (MÉTODO DUAL Kcb) ####
#ETc = Tp + Ep: evapotranspiración potencial del cultivo, sin reducción por estrés hídrico.
#ETa = α(h) × Tp + Ep: evapotranspiración real, reducida por estrés hídrico (α < 1 cuando h < h3).
#ETa se calcula dentro del bucle diario para el píxel seleccionado.
# Para cada clase se calcula la serie diaria de Tp y Ep [mm d-1].
# - Coberturas vegetadas: método dual del enunciado, ETc = Tp + Ep.
# - Playa/Dunas, Suelo desnudo y Plantación cosechada (clases 2, 13 y 16): Tp = 0 y Ep = Ke * ETo.
# - Agua (clase 1): se calcula ET0 pero se enmascara del balance hídrico del suelo.

clases_unicas <- unique(tabla1$value)  #todas las clases de Tabla 1 (1 a 16)

# Matrices de resultados: Tp y Ep [365 filas x 16 columnas]
Tp_mm_por_clase <- matrix(0, nrow = n_dias_bal, ncol = nrow(tabla1))
Ep_mm_por_clase <- matrix(0, nrow = n_dias_bal, ncol = nrow(tabla1))

#Serie de evaporación para suelo desnudo bajo las constantes generales del enunciado.
#Se calcula mediante las mismas funciones del método dual, no mediante un multiplicador arbitrario.
kcb_suelo_sin_veg_cultivo  <- rep(0, n_dias_bal)
kcmax_suelo_sin_veg_cultivo <- kcmax_fn(
  u2    = u2_bal,
  HRmin = HRmin_bal,
  h_veg = 0,
  kcb   = kcb_suelo_sin_veg_cultivo
)
ke_suelo_sin_veg_cultivo <- ke_fn(
  kr    = kr,
  kcmax = kcmax_suelo_sin_veg_cultivo,
  kcb   = kcb_suelo_sin_veg_cultivo,
  few   = few_cte
)

kc_agua <- 1.05  #Kc para cuerpos de agua libre < 2 m (FAO)

# Con Kr=1, few=0.9 y h_veg=0, ke suelo desnudo y plantación cosechada será de 1.08.

for (j in seq_along(clases_unicas)) {
  cls     <- tabla1$value[j]
  IAF_j   <- tabla1$IAF[j]
  h_veg_j <- tabla1$h_veg[j]
  kcb_j   <- tabla1$kcbtab[j]

  if (cls %in% c(2, 13, 16)) {
    # Playas/dunas, suelo desnudo y plantación cosechada: solo evaporación del suelo expuesto
    Tp_mm_por_clase[, j] <- 0
    Ep_mm_por_clase[, j] <- ETo_mm_bal * ke_suelo_sin_veg_cultivo

  } else if (cls == 1) {
    # Agua libre: se modela como 1.05 * ETo según FAO
    Tp_mm_por_clase[, j] <- 0
    Ep_mm_por_clase[, j] <- ETo_mm_bal * kc_agua

  } else if (cls %in% c(7, 15)) {
    # Glaciar/nieve y Urbanización: sin transpiración ni evaporación
    Tp_mm_por_clase[, j] <- 0
    Ep_mm_por_clase[, j] <- 0

  } else {
    # Coberturas vegetadas: método dual Kcb + Ke
    kcb_diario <- kcbIAF_fn(
      kcmin  = kcmin,
      IAF    = IAF_j,
      kcbtab = kcb_j,
      u2     = u2_bal,
      HRmin  = HRmin_bal,
      h_veg  = h_veg_j
    )

    kcmax_diario <- kcmax_fn(
      u2    = u2_bal,
      HRmin = HRmin_bal,
      h_veg = h_veg_j,
      kcb   = kcb_diario
    )

    ke_diario <- ke_fn(
      kr    = kr,
      kcmax = kcmax_diario,
      kcb   = kcb_diario,
      few   = few_cte
    )

    Tp_mm_por_clase[, j] <- ETo_mm_bal * kcb_diario
    Ep_mm_por_clase[, j] <- ETo_mm_bal * ke_diario
  }
}

# Convertir Tp, Ep y precipitación de mm a cm para el balance hídrico (S se expresa en cm)
Tp_cm_por_clase <- Tp_mm_por_clase / 10
Ep_cm_por_clase <- Ep_mm_por_clase / 10
Pp_cm_bal       <- Pp_mm_bal / 10


#### CARGA DE PARÁMETROS ROSETTA Y FC POR HORIZONTE ####
#se define un raster de referencia (θr 0-5 cm proyectado al CRS del polígono) al cual se
#resamplan todos los demás rasters, asegurando una grilla común para el balance píxel a píxel

#raster de referencia: θr del horizonte superficial (0-5 cm)
ref_raw  <- rast(file.path(rosetta_dir, "theta_r_0-5cm.tif"))
ref_cut  <- poligono_recorte(ref_raw, polygon)
ref_proj <- project(ref_cut, st_crs(polygon)$wkt, method = "bilinear")
#ref_proj es el raster de referencia de alineación para todos los demás parámetros

#función auxiliar interna: carga, recorta, reproyecta y resampa un raster al raster de referencia
cargar_param_hz <- function(nombre_archivo, metodo = "bilinear"){
  r     <- rast(file.path(rosetta_dir, nombre_archivo))
  r_cut <- poligono_recorte(r, polygon)
  r_prj <- project(r_cut, st_crs(polygon)$wkt, method = metodo)
  resample(r_prj, ref_proj, method = metodo)  #alineación a grilla de referencia
}

#cargar θr, θs, αvg, n y FC por horizonte (listas de 6 rasters, uno por horizonte)
theta_r_hz <- lapply(horizontes, function(hz) cargar_param_hz(paste0("theta_r_", hz, "cm.tif")))
theta_s_hz <- lapply(horizontes, function(hz) cargar_param_hz(paste0("theta_s_", hz, "cm.tif")))
alpha_hz   <- lapply(horizontes, function(hz) cargar_param_hz(paste0("alpha_",   hz, "cm.tif")))
n_hz       <- lapply(horizontes, function(hz) cargar_param_hz(paste0("n_",       hz, "cm.tif")))
FC_hz      <- lapply(horizontes, function(hz) cargar_param_hz(paste0("FC.",      hz, "cm.tif")))
#FC: contenido de agua a capacidad de campo (h = -330 cm), cm³/cm³; es la condición inicial θ₀


#### PARÁMETROS EQUIVALENTES DEL PERFIL (PROMEDIO PONDERADO POR ESPESOR) ####
#se usan parámetros equivalentes como señala el enunciado: promedio ponderado por espesor de horizonte
#prof_total = 200 cm; todos los horizontes suman 200 cm

#función que calcula el promedio ponderado de una lista de rasters por los espesores (cm)
prom_ponderado <- function(lista_hz, espesores, prof_total){
  pesos <- espesores / prof_total  #fracción de profundidad total de cada horizonte
  r_ponderado <- Reduce(`+`, mapply(function(r, p) r * p, lista_hz, pesos, SIMPLIFY = FALSE))
  return(r_ponderado)
}

theta_r_eq <- prom_ponderado(theta_r_hz, espesores, prof_total)  #θr equivalente (cm³/cm³)
theta_s_eq <- prom_ponderado(theta_s_hz, espesores, prof_total)  #θs equivalente (cm³/cm³)
alpha_eq   <- prom_ponderado(alpha_hz,   espesores, prof_total)  #αvg equivalente (cm⁻¹)
n_eq       <- prom_ponderado(n_hz,       espesores, prof_total)  #n equivalente (adimensional)

#almacenamiento inicial S₀ = sum(FC_hz × espesor_hz) [cm de agua]
#corresponde a la capacidad de campo (CC, h = -330 cm) de todo el perfil
S_init_raster  <- Reduce(`+`, mapply(function(r, e) r * e, FC_hz, espesores, SIMPLIFY = FALSE))

#límites físicos del almacenamiento [cm]:
#S_max = θs × 200 (suelo saturado); S_min = θr × 200 (contenido residual)
S_max_raster   <- theta_s_eq * prof_total
S_min_raster   <- theta_r_eq * prof_total


#### COBERTURA DOMINANTE POR PÍXEL DE SUELO: PORCENTAJE EXACTO DE INTERSECCIÓN ####
#El LandCover tiene píxeles de 30 m y CLSoilMaps tiene píxeles de ~100 m.
#Se debe identificar qué cobertura ocupa el mayor porcentaje en cada
#píxel del mapa de suelos. Cada píxel de la grilla de suelo se convierte en polígono y se calcula la
#fracción exacta de LandCover que intersecta ese píxel mediante terra::extract(exact = TRUE).

#Cargar y recortar el raster categórico LandCover al polígono de trabajo
lc_cut  <- poligono_recorte(lc_raw, polygon)

#Reproyectar LandCover al mismo CRS de la grilla de suelo, preservando categorías con near.
#No se resamplea aún a 100 m, pues se requiere conservar el detalle de los píxeles de 30 m.
lc_proj <- project(lc_cut, crs(ref_proj), method = "near")
names(lc_proj) <- "landcover"

#Crear un raster identificador con la misma grilla de suelo utilizada en el balance hídrico.
#Los NA de ref_proj corresponden a celdas fuera del polígono o sin dato válido de suelo.
soil_id <- ref_proj
values(soil_id) <- seq_len(ncell(soil_id))
soil_id <- mask(soil_id, ref_proj)
names(soil_id) <- "cell_suelo"

#Convertir cada píxel válido del mapa de suelo en un polígono conservando su número de celda.
soil_pixels <- as.polygons(soil_id, values = TRUE, na.rm = TRUE)

#Extraer la fracción exacta de cada celda LandCover que intersecta cada píxel de suelo.
#La columna fraction permite estimar el porcentaje relativo de cada cobertura por píxel.
lc_extraido <- terra::extract(lc_proj, soil_pixels, exact = TRUE)
lc_extraido$cell_suelo <- soil_pixels$cell_suelo[lc_extraido$ID]

#Calcular el porcentaje que representa cada clase LandCover dentro de cada píxel de suelo.
lc_por_pixel <- lc_extraido %>%
  filter(!is.na(landcover)) %>%
  group_by(cell_suelo, landcover) %>%
  summarise(
    fraccion_clase = sum(fraction, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(cell_suelo) %>%
  mutate(
    fraccion_total = sum(fraccion_clase, na.rm = TRUE),
    porcentaje = (fraccion_clase / fraccion_total) * 100
  ) %>%
  ungroup()

#Detectar empates en la cobertura de mayor porcentaje; si existen, deben justificarse en el informe.
empates_lc <- lc_por_pixel %>%
  group_by(cell_suelo) %>%
  filter(abs(porcentaje - max(porcentaje, na.rm = TRUE)) < 1e-8) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(empates_lc) > 0) {
  warning(paste("Se detectaron", length(unique(empates_lc$cell_suelo)),
                "píxeles con empate en la cobertura dominante.",
                "El código seleccionará la clase de menor código; justificar esta decisión en el informe."))
}

#Seleccionar la cobertura de mayor porcentaje en cada píxel de suelo.
#En caso de empate se selecciona de manera reproducible la clase con código LandCover menor.
lc_dominante <- lc_por_pixel %>%
  group_by(cell_suelo) %>%
  arrange(desc(porcentaje), landcover, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(cobertura_dominante = tabla1$nombre[match(landcover, tabla1$value)])

#Crear raster de cobertura dominante con la misma geometría de ref_proj.
#Este objeto conserva el nombre lc_soil utilizado posteriormente en el balance hídrico.
lc_soil <- ref_proj
values(lc_soil) <- NA
lc_soil[lc_dominante$cell_suelo] <- lc_dominante$landcover
names(lc_soil) <- "lc_dominante"

#Crear raster auxiliar con el porcentaje de cobertura dominante para validación y reporte.
porcentaje_lc_dominante <- ref_proj
values(porcentaje_lc_dominante) <- NA
porcentaje_lc_dominante[lc_dominante$cell_suelo] <- lc_dominante$porcentaje
names(porcentaje_lc_dominante) <- "porcentaje_lc_dominante"

#Guardar un raster auxiliar de control metodológico; no reemplaza los TIF exigidos del balance.
writeRaster(porcentaje_lc_dominante,
            file.path(fig_dir, "porcentaje_cobertura_dominante_por_pixel_suelo.tif"),
            overwrite = TRUE)


#### EXTRACCIÓN DE VALORES PARA EL BALANCE PÍXEL A PÍXEL ####
#se extraen los valores de todos los rasters como vectores para el bucle del balance
#los píxeles válidos son aquellos con dato no-NA en el raster de referencia (θr 0-5 cm)

valid_mask <- !is.na(values(ref_proj)[, 1])  #máscara de píxeles válidos (dentro del polígono)
n_pix      <- sum(valid_mask)                 #número de píxeles válidos

#extraer parámetros equivalentes como vectores [n_pix]
theta_r_vec <- values(theta_r_eq)[valid_mask, 1]
theta_s_vec <- values(theta_s_eq)[valid_mask, 1]
alpha_vec   <- values(alpha_eq)[valid_mask, 1]
n_vec       <- values(n_eq)[valid_mask, 1]

#límites de almacenamiento [cm] y estado inicial [cm]
S_max_vec   <- values(S_max_raster)[valid_mask, 1]
S_min_vec   <- values(S_min_raster)[valid_mask, 1]
S_init_vec  <- values(S_init_raster)[valid_mask, 1]
S_init_vec  <- pmin(pmax(S_init_vec, S_min_vec), S_max_vec)  #clip inicial por seguridad

#clase y porcentaje de cobertura dominante por píxel [n_pix]
#si no existe cobertura válida en un píxel, se trata como sin vegetación (ETc = 0)
lc_vec      <- values(lc_soil)[valid_mask, 1]
lc_pct_vec  <- values(porcentaje_lc_dominante)[valid_mask, 1]
lc_vec[is.na(lc_vec)] <- 0  #clase 0 → no en Tabla 1 → ETc = 0

#índice de columna en las matrices Tp/Ep (columna j corresponde a clases_unicas[j])
#si la clase del píxel no está en Tabla 1 se asigna columna de ETc = 0 (clase 1 u otra de ceros)
lc_col_vec  <- match(lc_vec, clases_unicas)         #índice 1-16 en la matriz de ETc por clase
lc_col_vec[is.na(lc_col_vec)] <- 1                  #clase sin match → columna 1 (ET = 0)

# El modelo bucket representa un perfil de suelo; por ello, los píxeles de agua
# se mantienen en LandCover pero se excluyen de S, h y estrés hídrico.
mask_agua_vec <- lc_vec == 1


#### BALANCE HÍDRICO DIARIO (365 DÍAS, VECTORIZADO) ####
#modelo de tipo estanque (bucket) simplificado, sin drenaje explícito
#ecuaciones: ΔS_i = P_i - (α(h_i) × Tp_i + Ep_i) [cm d⁻¹]
#            S_i  = S_{i-1} + ΔS_i, limitado a [S_min, S_max]
#α(h): función de Feddes; raíces distribuidas homogéneamente en 200 cm (θ único para perfil)
#ETc = Tp + Ep (potencial, sin reducción por estrés); ETa = α(h) × Tp + Ep (real, con reducción)
#la transpiración se multiplica por α(h) en ETa; la evaporación no se reduce por α

S_current       <- S_init_vec          #almacenamiento actual [cm]; inicia en CC para píxeles de suelo
stress_days_vec <- rep(0, n_pix)       #contador de días con estrés hídrico alpha < 1 por píxel de suelo

# Los píxeles de agua no tienen un perfil de suelo modelable de 200 cm.
# Permanecen como NA en los productos de almacenamiento y estrés hídrico.
S_current[mask_agua_vec]       <- NA_real_
stress_days_vec[mask_agua_vec] <- NA_real_

#series temporales del píxel seleccionado (se llenarán después de identificar el píxel)
S_serie       <- matrix(NA, nrow = n_dias_bal, ncol = 1)  #almacenamiento diario (rellena post-selección)
h_serie       <- matrix(NA, nrow = n_dias_bal, ncol = 1)  #cabezal de presión diario
dS_serie      <- matrix(NA, nrow = n_dias_bal, ncol = 1)  #ΔS diario
ETc_mm_serie  <- matrix(NA, nrow = n_dias_bal, ncol = 1)  #ETc potencial diaria (Tp + Ep) [mm d⁻¹]
ETa_mm_serie  <- matrix(NA, nrow = n_dias_bal, ncol = 1)  #ETa real diaria (α×Tp + Ep) [mm d⁻¹]
alpha_serie   <- matrix(NA, nrow = n_dias_bal, ncol = 1)  #α(h) diario

xy_todos <- xyFromCell(ref_proj, which(valid_mask))  #coordenadas [n_pix × 2]

# Tabla auxiliar con clase dominante, porcentaje y coordenadas de cada píxel válido
pixeles_validos <- data.frame(
  idx_vec    = seq_along(lc_vec),      #índice dentro de los vectores del balance
  cell_suelo = which(valid_mask),      #número de celda en la grilla raster original
  landcover  = lc_vec,
  porcentaje = lc_pct_vec,
  x          = xy_todos[, 1],
  y          = xy_todos[, 2]
)

pixeles_validos$cobertura_dominante <- tabla1$nombre[match(pixeles_validos$landcover, tabla1$value)]

# Resumen de representatividad de cada clase dominante dentro del polígono
resumen_lc_dom <- pixeles_validos %>%
  filter(landcover %in% tabla1$value, !is.na(porcentaje)) %>%
  group_by(landcover, cobertura_dominante) %>%
  summarise(
    n_pixeles = n(),
    porcentaje_promedio_dominancia = mean(porcentaje, na.rm = TRUE),
    porcentaje_max_dominancia      = max(porcentaje, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_pixeles), desc(porcentaje_promedio_dominancia))

print(resumen_lc_dom)

# Clases candidatas de interés para representar la dinámica anual.
# Bosque templado = 4; Plantación hoja ancha = 5.
clases_candidatas <- c(4, 5)

resumen_candidatas <- resumen_lc_dom %>%
  filter(landcover %in% clases_candidatas)

if (nrow(resumen_candidatas) > 0) {
  # Si existen Bosque templado o Plantación hoja ancha en el polígono,
  # se selecciona la más representada espacialmente.
  clase_objetivo <- resumen_candidatas %>%
    arrange(desc(n_pixeles), desc(porcentaje_promedio_dominancia), landcover) %>%
    slice(1) %>%
    pull(landcover)
} else {
  # Si ninguna de las dos clases está presente, se usa como respaldo la cobertura vegetada
  # más abundante del polígono. Esto evita seleccionar agua, nieve, urbanización o suelo sin vegetación.
  clases_vegetadas <- tabla1$value[tabla1$IAF > 0 | tabla1$kcbtab > 0]
  resumen_vegetadas <- resumen_lc_dom %>%
    filter(landcover %in% clases_vegetadas)

  if (nrow(resumen_vegetadas) > 0) {
    clase_objetivo <- resumen_vegetadas %>%
      arrange(desc(n_pixeles), desc(porcentaje_promedio_dominancia), landcover) %>%
      slice(1) %>%
      pull(landcover)
    warning("No se encontraron las clases 4 o 5. Se seleccionó la cobertura vegetada más representativa.")
  } else {
    # Último respaldo: píxel válido con mayor porcentaje de cobertura dominante,
    # excluyendo agua y glaciar/nieve.
    clase_objetivo <- pixeles_validos %>%
      filter(!(landcover %in% c(1, 7)), !is.na(porcentaje)) %>%
      arrange(desc(porcentaje), landcover) %>%
      slice(1) %>%
      pull(landcover)
    warning("No se encontraron coberturas vegetadas. Se seleccionó el píxel no acuático/no glaciar con mayor dominancia.")
  }
}

# Filtrar píxeles de la clase objetivo
pixeles_objetivo <- pixeles_validos %>%
  filter(landcover == clase_objetivo, !is.na(porcentaje))

# Centro espacial de la clase objetivo
x_centro_clase <- mean(pixeles_objetivo$x, na.rm = TRUE)
y_centro_clase <- mean(pixeles_objetivo$y, na.rm = TRUE)

pixeles_objetivo <- pixeles_objetivo %>%
  mutate(
    dist_centro_clase = sqrt((x - x_centro_clase)^2 + (y - y_centro_clase)^2)
  )

# Selección final: mayor porcentaje de cobertura dominante y, si hay empate, menor distancia al centro de la clase
pixel_sel <- pixeles_objetivo %>%
  arrange(desc(porcentaje), dist_centro_clase) %>%
  slice(1)

idx_sel <- pixel_sel$idx_vec[1]

# Nombre de la clase seleccionada
clase_sel_val <- lc_vec[idx_sel]
if (length(clase_sel_val) == 0 || is.na(clase_sel_val)) {
  clase_sel_nom <- "desconocida"
} else if (clase_sel_val %in% tabla1$value) {
  clase_sel_nom <- tabla1$nombre[tabla1$value == clase_sel_val]
} else {
  clase_sel_nom <- "desconocida"
}

cat("\n============================================\n",
    "SELECCIÓN DE PÍXEL REPRESENTATIVO\n",
    "============================================\n")
cat("Clase objetivo:", tabla1$nombre[tabla1$value == clase_objetivo], "(clase", clase_objetivo, ")\n")
cat("Criterio: mayor porcentaje de cobertura dominante dentro de la clase objetivo; desempate por cercanía al centro de esa clase.\n")
cat("Píxel seleccionado - cobertura dominante:", clase_sel_nom, "(clase", clase_sel_val, ")\n")
cat("Porcentaje de cobertura dominante:", round(lc_pct_vec[idx_sel], 2), "%\n")
cat("Coordenadas:", round(xy_todos[idx_sel, 1], 0), "E,",
    round(xy_todos[idx_sel, 2], 0), "N\n")

#bucle diario del balance hídrico (vectorizado sobre todos los píxeles simultáneamente)
for (t in 1:n_dias_bal) {
  #contenido de agua del perfil completo [cm³/cm³] = almacenamiento [cm] / profundidad [cm]
  theta_t   <- S_current / prof_total

  #cabezal de presión h [cm] por inversión de Van Genuchten con parámetros equivalentes
  h_t       <- VG_h(theta_t, theta_r_vec, theta_s_vec, alpha_vec, n_vec)

  #coeficiente de estrés hídrico α(h) según Feddes
  alpha_h_t <- feddes_alpha(h_t)

  #ETc potencial del día t para cada píxel según su clase de cobertura dominante [cm d⁻¹]
  Tp_t <- Tp_cm_por_clase[t, lc_col_vec]  #transpiración potencial
  Ep_t <- Ep_cm_por_clase[t, lc_col_vec]  #evaporación potencial

  #balance diario: ΔS = Pp - (α × Tp + Ep) [cm d⁻¹]
  dS_t  <- Pp_cm_bal[t] - (alpha_h_t * Tp_t + Ep_t)

  #actualizar almacenamiento y aplicar límites físicos
  S_new <- S_current + dS_t
  S_new <- pmin(pmax(S_new, S_min_vec), S_max_vec)  #S ∈ [S_min, S_max]

  #acumular días de estrés hídrico (α < 1 equivale a h < -8000 cm)
  stress_days_vec <- stress_days_vec + as.integer(alpha_h_t < 1)

  #guardar serie temporal del píxel seleccionado
  S_serie[t, 1]      <- S_new[idx_sel]
  h_serie[t, 1]      <- h_t[idx_sel]                              #h al inicio del día (antes de actualizar)
  dS_serie[t, 1]     <- S_new[idx_sel] - S_current[idx_sel]       #ΔS real después de aplicar límites
  ETc_mm_serie[t, 1] <- (Tp_t[idx_sel] + Ep_t[idx_sel]) * 10     #ETc potencial [mm d⁻¹]: Tp + Ep sin reducción
  ETa_mm_serie[t, 1] <- (alpha_h_t[idx_sel] * Tp_t[idx_sel] +
                          Ep_t[idx_sel]) * 10                      #ETa real [mm d⁻¹]: α×Tp + Ep
  alpha_serie[t, 1]  <- alpha_h_t[idx_sel]

  S_current <- S_new  #actualizar para el siguiente día
}


#### SVG DINÁMICA ANUAL DEL PÍXEL SELECCIONADO ####
#figura de 4 paneles con la dinámica del balance hídrico del píxel seleccionado durante 365 días
#ejes: Precipitación [mm d⁻¹], ETc y ETa [mm d⁻¹], ΔS [mm d⁻¹] y cabezal de presión h [cm]
#panel 2: ETc (línea sólida) y ETa (línea punteada); ylim ajustado al máximo de ETc (la mayor)

svg(file.path(fig_dir, "dinamica_anual_pixel_seleccionado.svg"), width = 10, height = 10)

par(mfrow = c(4, 1))
par(mar = c(1, 5.5, 2.5, 2))

# panel 1: precipitación [mm d⁻¹], eje invertido (lluvia "cae")
# max(Pp_mm_bal) puede ser 0 (sin lluvia) o -Inf (todo NA), generando ylim=c(0,0) o
# ylim=c(-Inf,0) que barplot rechaza con "se necesitan valores finitos de 'ylim'".
# Se garantiza un mínimo de 1 mm en el eje para que el gráfico siempre sea válido.
pp_ymax <- max(Pp_mm_bal, na.rm = TRUE)
pp_ymax <- if (!is.finite(pp_ymax) || pp_ymax <= 0) 1 else pp_ymax * 1.05
barplot(Pp_mm_bal,
        ylab   = expression(Pp~(mm~día^{-1})),
        ylim   = c(pp_ymax, 0),  #eje invertido: lluvia "cae"
        col    = "steelblue",
        border = NA,
        cex.lab = 1.1)
mtext("Dinámica anual del píxel seleccionado", side = 3, line = 0.8, cex = 1.1, font = 2)
mtext(paste0("Cobertura: ", clase_sel_nom, " | Período: ",
             format(fecha_bal_inicio, "%d-%m-%Y"), " - ",
             format(fecha_bal_fin,    "%d-%m-%Y")),
      side = 3, line = -0.3, cex = 0.8)

# panel 2: ETc potencial (sólida) y ETa real (punteada) [mm d⁻¹]
# ylim ajustado al máximo de ETc (la mayor, al no aplicar α), no al máximo de ETa
par(mar = c(1, 5.5, 0.5, 2))
etc_ymax <- max(ETc_mm_serie[, 1], na.rm = TRUE)
etc_ymax <- if (!is.finite(etc_ymax) || etc_ymax <= 0) 1 else etc_ymax * 1.05
plot(fechas_bal, ETc_mm_serie[, 1],
     type    = "l", col = "darkgreen", lwd = 1.5,
     xaxt    = "n",
     ylab    = expression(ET~(mm~día^{-1})),
     ylim    = c(0, etc_ymax),   #ylim fijado por ETc (máximo posible del panel)
     cex.lab = 1.1)
lines(fechas_bal, ETa_mm_serie[, 1],
      col = "lightgreen", lwd = 1.5, lty = 2)  #ETa en línea punteada del mismo color
abline(h = 0, col = "gray70", lty = 2)
legend("topright",
       legend = c(expression(ETc~(Tp + Ep)), expression(ETa~(alpha(h) %*% Tp + Ep))),
       col    = c("darkgreen", "lightgreen"),
       lty    = c(1, 2), lwd = c(1.5, 1.5),
       cex    = 0.8, bty = "n")

# panel 3: ΔS diario [mm d⁻¹] (convertido de cm a mm para comparabilidad con Pp y ETc)
par(mar = c(1, 5.5, 0.5, 2))
dS_mm_plot <- dS_serie[, 1] * 10  #cm → mm para el gráfico
col_dS     <- ifelse(dS_mm_plot >= 0, "darkorange", "tomato3")  #naranja: ganancia; rojo: pérdida
# agregar ylim explícito; sin él, si dS_mm_plot tiene solo NA R no puede calcular el rango
ds_ymin <- min(dS_mm_plot, na.rm = TRUE)
ds_ymax <- max(dS_mm_plot, na.rm = TRUE)
if (!is.finite(ds_ymin) || !is.finite(ds_ymax)) { ds_ymin <- -1; ds_ymax <- 1 }
ds_ylim <- c(min(ds_ymin * 1.05, ds_ymin - 0.1), max(ds_ymax * 1.05, ds_ymax + 0.1))
barplot(dS_mm_plot,
        col     = col_dS,
        border  = NA,
        ylim    = ds_ylim,
        ylab    = expression(Delta*S~(mm~día^{-1})),
        cex.lab = 1.1)
abline(h = 0, col = "gray40", lty = 1, lwd = 0.8)

# panel 4: cabezal de presión h [cm] con eje de fecha y líneas de referencia de Feddes
par(mar = c(5.5, 5.5, 0.5, 2))
plot(fechas_bal, h_serie[, 1],
     type    = "l", col = "sienna4", lwd = 1.5,
     xaxt    = "n",
     ylab    = "h (cm)",
     xlab    = "Fecha",
     cex.lab = 1.0)
abline(h = h3_feddes, col = "orange2", lty = 2, lwd = 1.2)  #inicio de estrés (h3 = -8000 cm)
abline(h = h4_feddes, col = "red3",    lty = 2, lwd = 1.2)  #marchitez permanente (h4 = -15000 cm)
legend("bottomleft",
       legend = c("h(t)", paste("h3 =", h3_feddes, "cm"), paste("h4 =", h4_feddes, "cm")),
       col    = c("sienna4", "orange2", "red3"),
       lty    = c(1, 2, 2), lwd = c(1.5, 1.2, 1.2),
       cex    = 0.75, bty = "n")
axis.Date(1,
          at     = seq(min(fechas_bal), max(fechas_bal), by = "2 months"),
          format = "%m-%Y",
          cex.axis = 0.9)

dev.off()


#### EXPORTACIÓN DE ARCHIVOS TIF ####

#función auxiliar: construye un raster de salida a partir de un vector de valores válidos
vector_a_raster <- function(valores_vec, raster_ref, valid_mask){
  r_out            <- raster_ref                       #usa ref como plantilla (CRS, extent, resolución)
  vals_full        <- rep(NA_real_, ncell(raster_ref)) #vector del tamaño total del raster (con NA)
  vals_full[valid_mask] <- valores_vec                 #rellena solo los píxeles válidos
  values(r_out)    <- vals_full
  return(r_out)
}

#TIF 1: almacenamiento S al día 365 [cm de agua], píxel a píxel
S_day365_vec    <- S_current                           #S_current al terminar el bucle = S día 365
S_day365_raster <- vector_a_raster(S_day365_vec, ref_proj, valid_mask)
writeRaster(S_day365_raster,
            file.path(fig_dir, "S_dia365.tif"),
            overwrite = TRUE)

S365 <- rast(file.path(fig_dir, "S_dia365.tif"))

svg(file.path(fig_dir, "S_dia365_mapa.svg"), width = 8, height = 6)
par(mar = c(5, 5, 4, 6))
plot(S365,
     main  = "Almacenamiento S - Día 365 (cm de agua)",
     col   = rev(hcl.colors(100, "Blues")),
     zlim  = c(global(S365, "min", na.rm = TRUE)[1,1],
               global(S365, "max", na.rm = TRUE)[1,1]),
     axes  = TRUE)
agregar_elementos_mapa(S365, polygon)
dev.off()

#TIF 2: número de días con estrés hídrico (α(h) < 1) durante el año [días], píxel a píxel
estres_raster <- vector_a_raster(as.numeric(stress_days_vec), ref_proj, valid_mask)
writeRaster(estres_raster,
            file.path(fig_dir, "dias_estres_hidrico.tif"),
            overwrite = TRUE)

DHS <- rast(file.path(fig_dir, "dias_estres_hidrico.tif"))

svg(file.path(fig_dir, "DHS_mapa.svg"), width = 8, height = 6)
par(mar = c(5, 5, 4, 6))
plot(DHS,
     main  = "Días de estrés hídrico - Cantidad de días",
     col   = rev(hcl.colors(100, "Oranges")),
     zlim  = c(global(DHS, "min", na.rm = TRUE)[1,1],
               global(DHS, "max", na.rm = TRUE)[1,1]),
     axes  = TRUE)
agregar_elementos_mapa(DHS, polygon)
dev.off()


#### RESULTADOS NUMÉRICOS - PARTE 2 ####

cat("\n============================================\n",
    "ETo PENMAN-MONTEITH (período balance)\n",
    "============================================\n")
cat("ETo mínima:", round(min(ETo_mm_bal, na.rm = TRUE), 2), "mm d⁻¹\n",
    "ETo máxima:", round(max(ETo_mm_bal, na.rm = TRUE), 2), "mm d⁻¹\n",
    "ETo media: ", round(mean(ETo_mm_bal, na.rm = TRUE), 2), "mm d⁻¹\n",
    "ETo acumulada:", round(sum(ETo_mm_bal, na.rm = TRUE), 1), "mm año⁻¹\n",
    "Precipitación acumulada:", round(sum(Pp_mm_bal, na.rm = TRUE), 1), "mm año⁻¹\n")

cat("\n============================================\n",
    "PÍXEL SELECCIONADO\n",
    "============================================\n")
cat("Cobertura dominante:", clase_sel_nom, "(clase", clase_sel_val, ")\n",
    "Porcentaje de cobertura dominante:", round(lc_pct_vec[idx_sel], 2), "%\n",
    "Coordenadas (CRS polígono):", round(xy_todos[idx_sel, 1], 0), "E,",
                                   round(xy_todos[idx_sel, 2], 0), "N\n",
    "S inicial (CC):", round(S_init_vec[idx_sel], 2), "cm\n",
    "S mínimo (θr × 200):", round(S_min_vec[idx_sel], 2), "cm\n",
    "S máximo (θs × 200):", round(S_max_vec[idx_sel], 2), "cm\n",
    "S día 365:", round(S_current[idx_sel], 2), "cm\n",
    "Días de estrés hídrico:", stress_days_vec[idx_sel], "días\n",
    "ETc acumulada (potencial):", round(sum(ETc_mm_serie[, 1], na.rm = TRUE), 1), "mm año⁻¹\n",
    "ETa acumulada (real):",      round(sum(ETa_mm_serie[, 1], na.rm = TRUE), 1), "mm año⁻¹\n")

cat("\n============================================\n",
    "BALANCE HÍDRICO ESPACIAL (día 365)\n",
    "============================================\n")
cat("S día 365 - mínimo:", round(global(S_day365_raster, "min", na.rm = TRUE)[1,1], 2), "cm\n",
    "S día 365 - máximo:", round(global(S_day365_raster, "max", na.rm = TRUE)[1,1], 2), "cm\n",
    "S día 365 - media: ", round(global(S_day365_raster, "mean", na.rm = TRUE)[1,1], 2), "cm\n")
cat("Días de estrés - mínimo:", min(stress_days_vec, na.rm = TRUE), "días\n",
    "Días de estrés - máximo:", max(stress_days_vec, na.rm = TRUE), "días\n",
    "Días de estrés - media: ", round(mean(stress_days_vec, na.rm = TRUE), 1), "días\n")

cat("\n============================================\n",
    "TRATAMIENTO DE SUPERFICIES ESPECIALES\n",
    "============================================\n")
cat("Agua: excluida del balance edáfico (S, h y estrés hídrico reportados como NA).\n")
cat("Suelo desnudo: Tp = 0; Kr =", kr, "; few =", few_cte,
    "; Ke =", round(unique(ke_suelo_sin_veg_cultivo)[1], 2),
    "; Ep =", round(unique(ke_suelo_sin_veg_cultivo)[1], 2), "x ETo.\n")

cat("\n")
kable(data.frame(
  Clase    = tabla1$nombre,
  ETc_acum = round(colSums(Tp_mm_por_clase + Ep_mm_por_clase, na.rm = TRUE), 1)
), col.names = c("Clase cobertura", "ETc potencial acumulada (mm año⁻¹)"),
   caption = "Pérdida potencial ETc acumulada durante el año por clase de cobertura")

beep(8)  #sonido al terminar la ejecución
