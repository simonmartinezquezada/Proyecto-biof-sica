#Avance 2 - Parámetros hidráulicos CLSoilMaps (ROSETTA_MEAN): mapas de θr, θs, αvg, n y Ks por horizonte


#### CONFIGURACIÓN Y CARGA DE LIBRERÍAS ####
#paquetes (descomentar para instalar si es necesario)
#install.packages("sf")
#install.packages("terra")
#install.packages("here")
#install.packages("prettymapr")
#install.packages("knitr")
#install.packages("beepr")

#librerias
library(beepr)
library(sf)
library(terra)
library(here)
library(prettymapr)
library(knitr)


#### DIRECTORIOS ####
fig_dir     <- here("figuras")        #carpeta de salida de figuras .svg (debe existir previamente)
rosetta_dir <- here("ROSETTA_MEAN")   #carpeta con los rasters de parámetros ROSETTA (θr, θs, αvg, n, Ks)


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
                                   north_pos  = "topright",  scale_pos  = "bottomleft",
                                   north_pad  = c(0.95, 0.3), scale_pad = c(1, 0.45),
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
    plot(r_actual, main = paste(titulo_base, hz, "cm"), col = paleta, zlim = zlim, axes = TRUE)
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
#los horizontes corresponden a las profundidades de los archivos .tif de CLSoilMaps (ROSETTA_MEAN)
horizontes <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")


#### MAPA θr - CONTENIDO DE AGUA RESIDUAL ####
#θr (theta_r): fracción volumétrica de agua que no puede ser extraída por succión (cm³/cm³)
#archivos fuente: theta_r_0-5cm.tif, theta_r_5-15cm.tif, ..., theta_r_100-200cm.tif

theta_r_cut <- list()  #lista para almacenar los rasters recortados por horizonte
paleta_theta_r <- rev(hcl.colors(100, "Blues"))  #paleta azul: mayor θr = mayor agua residual

for (hz in horizontes) {
  archivo   <- paste0("theta_r_", hz, "cm.tif")                     #nombre del archivo según horizonte
  nombre_svg <- paste0("theta_r_", gsub("-", "_", hz), "cm.svg")    #nombre del .svg de salida

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


#### RESULTADOS NUMÉRICOS ####

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

beep(8)
