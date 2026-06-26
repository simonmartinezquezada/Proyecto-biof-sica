#Primera modificación: Simón Martínez (generación polígono, LandCover, DEM, PieChart con la distribución de cobertura)
#Segunda Modificación: Alvaro Carrizo (modificación código simón, generación de función para el polígono para cortar fácilmente y guardar .svg rápido igual, calculo de texturas, cc y pmp por horizontes)
#Tercera Modificación: Joaquín Alvear (agregar sección de almacenamiento para cada horizonte y conversión a camiones aljibe de 10m3)
#Cuarta Modificación: incorporación correcta de escala, flecha norte y coordenadas UTM???; zona de resultados y triángulo textural


#### CONFIGURACIÓN Y CARGA DE LIBRERÍAS ####
#paquetes
#install.packages("leaflet")
#install.packages("raster")
#install.packages("RColorBrewer")
#install.packages("sfheaders")
#install.packages("terra")
#install.packages("ggplot2")
#install.packages("here")
#install.packages("paletteer")
#install.packages("prettymapr")
#install.packages("beepr")
#install.packages("soiltexture")
#install.packages("knitr")

#librerias
library(beepr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(raster)
library(sfheaders)
library(terra)
library(ggplot2)
library(here)
library(paletteer)
library(prettymapr)
library(soiltexture)
library(knitr)


#### Fijar directorio de trabajo
fig_dir <- "C:/Users/simon/Downloads/Trabajo_final_Grupo5/figuras"
#soil_dir <- here("SoilMaps_MEAN")
#rosetta_dir <- here("ROSETTA_MEAN")

lc <- rast("C:/Users/simon/Downloads/Trabajo_final_Grupo5/CLDynamicLandCover_2018_1.0.tif")
DEM <- rast("C:/Users/simon/Downloads/Trabajo_final_Grupo5/DEM.Chile.Continental.TIF")

direct_texture <- "C:/Users/simon/Downloads/Trabajo_final_Grupo5/SoilMaps_MEAN"
direct_prop_hid <- "C:/Users/simon/Downloads/Trabajo_final_Grupo5/ROSETTA_MEAN"

#### Paquete sf y leaflet
map <- leaflet()
addTiles(map = map)
map <- addProviderTiles(map = map, "Esri.WorldImagery", group = "ESRI")
map


#### CREACIÓN DEL PUNTO Y VISUALIZACIÓN EN LEAFLET ####
#### Punto seleccionado (UTM)
coordenada <- c(este = 121165.911441601, sur = 5863133.22043047, label = "mi punto")
##esto es solo un vector con los datos de su punto, aún no es un objeto espacial
coordenada_m <- as.matrix(t(coordenada))
coordenada_df <- as.data.frame.matrix(coordenada_m)
coordenada_df[,1:2] <- apply(coordenada_df[1:2], 2, as.numeric)
##transformarlo a un objeto espacial
punto <- st_as_sf(coordenada_df,         ##aqui lee un dataframe por lo tanto puede agregar muchos puntos
                  coords = c("este", "sur"), ##aqui van los nombres de sus columnas
                  crs = 9155)              ###este CRS es para UTM
punto

#### Ubicar punto en el map.
crs_latlong <- "EPSG:4326" #sistema de referencia (definición limpia, válida para todos los st_transform del script)
punto_latlng <- st_transform(punto, crs = crs_latlong)
map <- addMarkers(map = map, lat = st_coordinates(punto_latlng)[2], lng = st_coordinates(punto_latlng)[1], popup = c(punto_latlng$label))
map


#### GENERAR EL POLÍGONO DE ESTUDIO ####
#### Crear un polígono cuadrado, a partir de un punto (centroide)
# Área deseada: 30 km2
area_m2 <- 30 * 1000000 #30km2 a Xm2
lado_m <- sqrt(area_m2)
mitad <- lado_m / 2

# Coordenadas del centro
xy <- st_coordinates(punto)[1,]
x <- xy[1]
y <- xy[2]

# Crear cuadrado centrado en punto #deberían ser 4 puntos/vértices
coords_cuadrado <- matrix(c(
  x - mitad, y - mitad,
  x + mitad, y - mitad,
  x + mitad, y + mitad,
  x - mitad, y + mitad,
  x - mitad, y - mitad
), ncol = 2, byrow = TRUE)

##transformar el polígono a un objeto espacial
cuadrado <- st_sf(
  label = "poligono 30 km2",
  geometry = st_sfc(st_polygon(list(coords_cuadrado)), crs = st_crs(punto))
)
cuadrado

# Area del polígono (m2)
st_area(cuadrado)

# Pasar a lat/long para leaflet
punto_latlng <- st_transform(punto, crs = crs_latlong)
cuadrado_latlng <- st_transform(cuadrado, crs = crs_latlong)


#### GENERACIÓN DEL MAPA DE LOCALIZACIÓN ####
# visualizar polígono en el Mapa
map <- leaflet()
map <- addTiles(map)
map <- addProviderTiles(map, "Esri.WorldImagery", group = "ESRI")
map <- addMarkers(map, data = punto_latlng, popup = ~label)                          #punto en el mapa
map <- addPolygons(map, data = cuadrado_latlng, color = "red", weight = 2,           #polígono en el mapa
                   fillColor = "red", fillOpacity = 0.2, popup = ~label)             #opacidad
map

#### Guardar el polígono como shapefile en la carpeta
st_write(cuadrado, dsn = file.path(fig_dir, "polygon.shp"), driver = "ESRI Shapefile", delete_layer = TRUE) #fig_dir es el directorio general, para que se pueda abrir en cualquier pc sin problema, delete_layer = TRUE es para sobreescribir el archivo cada vez que se corra el cod

#### Leer el shapefile guardado
polygon <- read_sf(file.path(fig_dir, "polygon.shp"), layer = "polygon")
head(polygon)
polygon_latlng <- st_transform(polygon, crs = crs_latlong)

# Mapa con nuevo polígono
map <- leaflet()
map <- addTiles(map)
map <- addProviderTiles(map, "Esri.WorldImagery", group = "ESRI")
map <- addPolygons(map, data = polygon_latlng, color = "red", weight = 2,
                   fillColor = "red", fillOpacity = 0.2, popup = ~label)
map


#### FUNCIONES DE MANEJO DE RASTER ####

#se genera código con función para recortar cualquier raster (r, tal como DEM_Chile Continental, LandCover, SoilMap, etc) con el polígono generado anteriormente (polygon)
#se define con function(variables independientes, en este caso raster y polígono), luego de { se comienza a definir los pasos de esta función
poligono_recorte <- function(r, polygon){
  polygon_r <- st_transform(polygon, crs = crs(r)) #esto hace que el polígono (ahora polygon_r) tenga mismo sistema de coordenadas que el raster a cortar
  polygon_v <- vect(polygon_r)                     #se transforma el polígono a formato SpatVector del paquete terra, importante para pasos posteriores
  r_crop <- crop(r, polygon_v)                     #recorta el raster por el área rectangular del polígono vectorizado (polygon_v), no específico a la forma
  r_mask <- mask(r_crop, polygon_v)                #respeta la forma del polígono y los puntos que estaban en el área, pero no en la forma del polígono se pasan a valores NA
  return(r_mask)                                   #es el = para una función r_mask = f(r, polygon) como si fuese y = f(x,z)
}

#función para agregar elementos cartográficos: borde del polígono, flecha norte y barra de escala
#permite mantener consistencia cartográfica en todos los mapas exportados
agregar_elementos_mapa <- function(r_plot, polygon,
                                   north_pos = "topright", scale_pos = "bottomleft",
                                   north_pad = c(0.95, 0.3), scale_pad = c(1, 0.45),
                                   north_scale = 0.6){
  # borde del polígono
  plot(vect(st_transform(polygon, crs(r_plot))), add = TRUE, border = "black", lwd = 1.5)
  # flecha norte
  prettymapr::addnortharrow(pos = north_pos, padin = north_pad, scale = north_scale, text.col = "black")
  # barra escala
  prettymapr::addscalebar(plotunit = "m", pos = scale_pos, padin = scale_pad, label.cex = 0.75)
}

#se define una función para guardar los rasters recortados a archivos .svg, para evitar colocarlo siempre.
# sigue con la forma de función, en este caso, tiene más archivos de entrada, raster(r), polígono (polygon)
# Además, define el título que tendrán los gráficos, el nombre del archivo de salida y el método de reproyección para el raster, en este caso bilinear
# Incluye flecha norte y barra de escala mediante agregar_elementos_mapa
guardar_svg_raster <- function(r, polygon, titulo, archivo_salida, metodo = "bilinear",
                                paleta = viridisLite::viridis(100),
                                north_pos = "topright", scale_pos = "bottomleft"){
  r_cut <- poligono_recorte(r, polygon)                              #llama a la función anterior, es decir, para recortar el raster al polígono
  r_plot <- project(r_cut, st_crs(polygon)$wkt, method = metodo)   #transforma el raster cortado por el polígono a coordenadas consistentes entre DEM y polígono
  svg(file.path(fig_dir, archivo_salida), width = 8, height = 6)    #esto indica que se va a hacer un archivo .svg, define donde se va a guardar, con qué nombre y las dimensiones
  par(mar = c(5, 5, 4, 6))
  plot(r_plot, main = titulo, col = paleta, axes = TRUE)             #indica que se va a graficar
  agregar_elementos_mapa(r_plot, polygon, north_pos = north_pos, scale_pos = scale_pos)
  dev.off()
  return(r_plot)
}

#función para guardar paneles de múltiples rasters (por horizontes) en un solo .svg con escala de color común
#evita la repetición del bloque svg + par + for + dev.off en cada sección de textura y propiedades hidráulicas
guardar_panel_rasters <- function(lista_rasters, horizontes, archivo_salida, titulo_base, paleta,
                                   zlim = NULL, ncol_panel = 3, nrow_panel = 2,
                                   agregar_norte = FALSE, agregar_escala = FALSE){
  svg(file.path(fig_dir, archivo_salida), width = 12, height = 8)
  par(mfrow = c(nrow_panel, ncol_panel), mar = c(3.2, 3.2, 3, 4.5), oma = c(1, 1, 1, 1))
  for (hz in horizontes) {
    r_actual <- lista_rasters[[hz]]
    plot(r_actual, main = paste(titulo_base, hz, "cm"), col = paleta, range = zlim, axes = TRUE)
    plot(vect(st_transform(polygon, crs(r_actual))), add = TRUE, border = "black", lwd = 1)
    
    # FLECHA NORTE
    if (agregar_norte) {
      prettymapr::addnortharrow(pos = "topright", scale = 0.32, padin = c(0.12, 0.3), text.col = "black")
    }

    # ESCALA
    if (agregar_escala) {
      prettymapr::addscalebar(plotunit = "m", pos = "bottomleft", padin = c(0.18, 0.3), label.cex = 0.45)
    }
  }
  dev.off()
}


#### COBERTURA DE SUELO DEL POLÍGONO ####
#### Recortar Land cover según el polígono
#lc <- rast(here::here("CLDynamicLandCover_2018_1.0.tif")) #elemento raster
lc
plot(lc)

lc_cut <- poligono_recorte(lc, polygon)

# reproyección final para dejarlo consistente con el polígono
lc.proj <- project(lc_cut, st_crs(polygon)$wkt, method = "near")
lc.cat <- as.factor(lc.proj)
plot(lc.proj, main = "Land Cover área de estudio")

# tabla completa de clases
clases <- data.frame(
  value = 1:16,
  cobertura = c(
    "Agua",
    "Playas/dunas",
    "Bosque mediterráneo",
    "Bosque templado",
    "Plantación de hoja ancha",
    "Árboles frutales",
    "Glaciar/nieve",
    "Vegetación ripariana",
    "Matorrales",
    "Plantación exótica de hojas aciculadas",
    "Praderas y plantaciones anuales",
    "Praderas siempre verde",
    "Suelo desnudo",
    "Turberas",
    "Urbanización",
    "Plantación cosechada"
  )
)

# clases presentes en el raster
vals_presentes <- freq(lc.proj)[, "value"]
clases_presentes <- clases[clases$value %in% vals_presentes, ] #no hay glaciares ni turberas

# convertir a raster categórico
lc.cat <- as.factor(lc.proj)
levels(lc.cat) <- clases

plot(lc.cat, type = "classes")

RColorBrewer::display.brewer.all() # paletas de colores disponibles

colores <- c(
  "1"  = "#2C7FB8",  # Agua (azul)
  "2"  = "#FDD49E",  # Playas/dunas (arena)
  "3"  = "#66C2A5",  # Bosque mediterráneo (verde-azulado suave)
  "4"  = "#1B7837",  # Bosque templado (verde oscuro intenso)
  "5"  = "#B8E186",  # Plantación hoja ancha (verde-amarillento)
  "6"  = "#E6AB02",  # Árboles frutales (amarillo/naranja distintivo)
  "8"  = "#41B6C4",  # Vegetación ripariana (turquesa)
  "9"  = "#FD8D3C",  # Matorrales (naranjo)
  "10" = "#6BAED6",  # Plantación aciculada (azulado)
  "11" = "#9E9AC8",  # Praderas/anuales (lila)
  "12" = "#C7E9C0",  # Praderas siempre verde (muy claro)
  "13" = "#8C510A",  # Suelo desnudo (café)
  "15" = "#969696",  # Urbanización (gris)
  "16" = "#3182BD"   # Plantación cosechada (azul)
) #jugar con los colores para mejor representación

svg(file.path(fig_dir, "landcover_poligono.svg"), width = 8, height = 6)
par(mar = c(5, 5, 4, 12))
plot(lc.cat, main = "LandCover Polígono", col = colores, axes = TRUE,
     plg = list(x = xmax(lc.cat), y = ymax(lc.cat), cex = 0.7)) #leyenda posicionada fuera del mapa
agregar_elementos_mapa(lc.cat, polygon)
dev.off()

#### Grafico de Torta
# frecuencia de pixeles sobre el lc proyectado (raster categórico)
freq_lc <- as.data.frame(freq(lc.proj))
freq_lc <- na.omit(freq_lc)
#preguntar si se pueden eliminar las coberturas con poco porcentaje para mejorar la representación

# unir clases con frecuencias
lut <- freq_lc[, c("value", "count")]                              # crea un nuevo data frame llamado lut, tomando solo las columnas value y count
names(lut)[names(lut) == "count"] <- "npix"                       # Cambia el nombre de la columna "count" a "npix"
i <- match(lut$value, clases$value)                               # Busca en que posición de clases$value aparece cada valor de lut$value
cols_extra <- setdiff(names(clases), "value")                     # Identifica qué columnas de "clases" se van a agregar a "lut"
lut[cols_extra] <- clases[i, cols_extra, drop = FALSE]            # Agrega a "lut" las columnas de "clases" según la coincidencia de "value"
lut$prop <- round(100 * lut$npix / sum(lut$npix), 2)             # Calcula el porcentaje de cada cobertura en polygon
lut$plot_name <- paste0(lut$cobertura, " ", lut$prop, "%")       # Crear una nueva columna de texto llamada "plot_name", que ira en la leyenda del gráfico
lut

names(colores) <- unique(lut$plot_name) #se asignan los nombres de plot_name a los colores para poder graficar

# grafico de torta (pie chart)
pie_chart <- lut %>% ggplot(aes(x = '', y = prop, fill = plot_name)) +
  geom_bar(
    stat = 'identity', width = 1,
    color = "white"
  ) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = colores) + # remove background, grid, numeric labels
  labs(fill = 'Cobertura', title = 'Porcentaje del área cubierto por cada cobertura de suelo')

svg(file.path(fig_dir, "pie_chart_landcover.svg"), width = 8, height = 6)
print(pie_chart)
dev.off()


#### MANEJO DEM ####
######recorte raster DEM
# cargar raster DEM #descargar del drive del anuncio del avance 1
#DEM <- rast(here("DEM.Chile.Continental.tif"))
paleta_dem <- hcl.colors(100, "viridis", rev = FALSE)

# recortar y enmascarar
DEM_cut <- poligono_recorte(DEM, polygon)

# reproyectar a coordenadas del polígono
DEM.proj <- project(DEM_cut, st_crs(polygon)$wkt, method = "bilinear")

# guardar svg con elementos cartográficos
svg(file.path(fig_dir, "DEM_poligono.svg"), width = 8, height = 6)
par(mar = c(5, 5, 4, 6))
plot(DEM.proj, main = "DEM proyectado", col = paleta_dem, axes = TRUE)
agregar_elementos_mapa(DEM.proj, polygon)
dev.off()

# extraer valores para estadísticas descriptivas
valores_elev <- values(DEM.proj, na.rm = TRUE)[,1]

#mínimo y máximo
valores <- global(DEM.proj, fun = c("min", "max"), na.rm = TRUE)
elev_min <- valores[1,1]
elev_max <- valores[1,2]


#### MAPA DE TEXTURAS ####
###Mapas usando CLSoilMaps
#artículo https://doi.org/10.1038/s41597-023-02536-x
#database https://zenodo.org/records/7464210?preview_file=FileDesc.txt
#descargar SoilMaps_MEAN y ROSETTA_MEAN

#crear horizontes
horizontes <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")

#### Fijar carpeta de mapa de texturas
#direct_texture <- here("SoilMaps_MEAN")


#corte por arcillas
Clay_cut <- list() #creará una lista para guardar los raster que serán llenados por el for siguiente
paleta_arcilla <- rev(hcl.colors(100, "YlOrBr"))

for (hz in horizontes) {
  archivo <- paste0("Clay.", hz, "cm.tif")                      #busca el archivo raster correspondiente al horizonte analizado
  nombre_svg <- paste0("arcilla_", gsub("-", "_", hz), "cm.svg") #le asigna un nombre al .svg que entregará
  clay_r <- rast(file.path(direct_texture, archivo))            #busca en la carpeta de texturas, el archivo "Clay.hz.cm.tif" y lo transforma en raster
  Clay_cut[[hz]] <- guardar_svg_raster(
    r = clay_r,
    polygon = polygon,
    titulo = paste("Arcilla (%) -", hz, "cm"),
    paleta = paleta_arcilla,
    archivo_salida = nombre_svg,
    metodo = "bilinear"
  ) #guarda en svg, según la función guardar_svg_raster para el raster hz buscado en el directorio
}

# mínimos y máximos globales de arcilla (usando sapply para evitar stack con resoluciones distintas)
mins_arcilla <- sapply(horizontes, function(hz) global(Clay_cut[[hz]], "min", na.rm = TRUE)[1,1])
maxs_arcilla <- sapply(horizontes, function(hz) global(Clay_cut[[hz]], "max", na.rm = TRUE)[1,1])
min_global <- min(mins_arcilla, na.rm = TRUE)
max_global <- max(maxs_arcilla, na.rm = TRUE)

# panel de arcilla por horizonte con escala común
guardar_panel_rasters(
  lista_rasters = Clay_cut,
  horizontes = horizontes,
  archivo_salida = "porcentaje_arcilla_en_cada_nivel.svg",
  titulo_base = "Arcilla (%) -",
  paleta = paleta_arcilla,
  zlim = c(min_global, max_global),
  agregar_norte = TRUE,
  agregar_escala = TRUE
)

sands_cut <- list()
paleta_arena <- colorRampPalette(c("#fff7bc", "#fee391", "#fec44f", "#fe9929", "#d95f0e"))(100)

for (hz in horizontes) {
  archivo <- paste0("Sand.", hz, "cm.tif")
  nombre_svg <- paste0("arena_", gsub("-", "_", hz), "cm.svg")
  sand_r <- rast(file.path(direct_texture, archivo))
  sands_cut[[hz]] <- guardar_svg_raster(
    r = sand_r,
    polygon = polygon,
    titulo = paste("Arena (%) -", hz, "cm"),
    archivo_salida = nombre_svg,
    metodo = "bilinear",
    paleta = paleta_arena
  )
}

# mínimos y máximos globales de arena
mins_arena <- sapply(horizontes, function(hz) global(sands_cut[[hz]], "min", na.rm = TRUE)[1,1])
maxs_arena <- sapply(horizontes, function(hz) global(sands_cut[[hz]], "max", na.rm = TRUE)[1,1])
min_sand_global <- min(mins_arena, na.rm = TRUE)
max_sand_global <- max(maxs_arena, na.rm = TRUE)

# panel de arena por horizonte con escala común
guardar_panel_rasters(
  lista_rasters = sands_cut,
  horizontes = horizontes,
  archivo_salida = "porcentaje_arena_en_cada_nivel.svg",
  titulo_base = "Arena (%) -",
  paleta = paleta_arena,
  zlim = c(min_sand_global, max_sand_global),
  agregar_norte = TRUE,
  agregar_escala = TRUE
)

silts_cut <- list()
paleta_silt <- rev(hcl.colors(100, "PuBuGn"))

for (hz in horizontes) {
  archivo <- paste0("Silt.", hz, "cm.tif")
  nombre_svg <- paste0("limo_", gsub("-", "_", hz), "cm.svg")
  silt_r <- rast(file.path(direct_texture, archivo))
  silts_cut[[hz]] <- guardar_svg_raster(
    r = silt_r,
    polygon = polygon,
    titulo = paste("Limo (%) -", hz, "cm"),
    paleta = paleta_silt,
    archivo_salida = nombre_svg,
    metodo = "bilinear"
  )
}

# mínimos y máximos globales de limo
mins_silts <- sapply(horizontes, function(hz) global(silts_cut[[hz]], "min", na.rm = TRUE)[1,1])
maxs_silts <- sapply(horizontes, function(hz) global(silts_cut[[hz]], "max", na.rm = TRUE)[1,1])
min_silt_global <- min(mins_silts, na.rm = TRUE)
max_silt_global <- max(maxs_silts, na.rm = TRUE)

# panel de limo por horizonte con escala común
guardar_panel_rasters(
  lista_rasters = silts_cut,
  horizontes = horizontes,
  archivo_salida = "porcentaje_limo_en_cada_nivel.svg",
  titulo_base = "Limo (%) -",
  paleta = paleta_silt,
  zlim = c(min_silt_global, max_silt_global),
  agregar_norte = TRUE,
  agregar_escala = TRUE
)

# Clases texturales promedio ##
# Dataframe vacío para guardar los resultados
textura_promedio <- data.frame(
  horizonte = horizontes,
  arena = NA,
  limo  = NA,
  arcilla = NA
)

for (i in seq_along(horizontes)) {
  hz <- horizontes[i]
  textura_promedio$arena[i]   <- global(sands_cut[[hz]], "mean", na.rm = TRUE)[1,1]
  textura_promedio$limo[i]    <- global(silts_cut[[hz]], "mean", na.rm = TRUE)[1,1]
  textura_promedio$arcilla[i] <- global(Clay_cut[[hz]], "mean", na.rm = TRUE)[1,1]
}

# Redondear a un decimal
#textura_promedio[,2:4] <- round(textura_promedio[,2:4], 2)

textura_promedio$suma <- textura_promedio$arena + textura_promedio$limo + textura_promedio$arcilla
textura_promedio$arena   <- textura_promedio$arena   / textura_promedio$suma * 100
textura_promedio$limo    <- textura_promedio$limo    / textura_promedio$suma * 100
textura_promedio$arcilla <- textura_promedio$arcilla / textura_promedio$suma * 100
textura_promedio$suma <- NULL

datos_promedio <- data.frame(
  CLAY = textura_promedio$arcilla,
  SILT = textura_promedio$limo,
  SAND = textura_promedio$arena
)
rownames(datos_promedio) <- textura_promedio$horizonte  # nombres para etiquetas
print(round(datos_promedio))

# Para usar en TT.plot
text1 <- datos_promedio[, c("CLAY", "SILT", "SAND")]; text1

# Colores de puntos por horizonte
colores_base <- c("red3", "blue3", "darkgreen", "orange3", "purple3",
                  "gray30", "deeppink3", "goldenrod3", "cyan4")
colores1 <- colores_base[1:nrow(datos_promedio)]

col_fondo <- c(
  "honeydew", "palegreen1", "palegreen3", "darkseagreen1",
  "darkseagreen4", "olivedrab1", "olivedrab3", "darkolivegreen1",
  "darkolivegreen4", "springgreen2", "seagreen2",
  "forestgreen"
)

par(mfrow = c(1, 1), pty = "s", mar = c(1, 1, 3, 1)) #modificacion de los márgenes de la imagen; mar=bottom, left, top, right

# -------- Triángulo Textural --------
geo1 <- TT.plot(              #VISUALIZACION DEL TRIANGULO TEXTURAL CON LOS PUNTOS DEL SITIO 1
  class.sys = "USDA.TT",      #clasificacion de las texturas a utilizar
  class.p.bg.col = col_fondo, #colores del triangulo
  tri.data = text1,           #datos de textura del sitio 1 (df sin columna de horizontes)
  pch = 19,                   #diseño del punto
  col = colores1,             #colores de los puntos
  cex = 0.5,                  #tamaño de los puntos
  lwd.axis = 0.8,             #tamaño de las lineas del triangulo
  cex.lab = 0.9,              #tamaño de la letra de titulos de los ejes
  cex.axis = 0.8,             #tamaño de los números de los ejes
  lang = "en",                #idioma
  main = "Clase textura promedio por horizonte" #titulo del triángulo
)

legend(                       #LEYENDA CON EL DETALLE DE LOS HORIZONTES: PROFUNDIDAD Y COLOR DEL PUNTO
  "topright",
  legend = horizontes,
  col = colores1,
  pch = 19,
  pt.cex = 1.1,
  cex = 0.8,
  bty = "n",
  title = "Horizontes"
)

clases_tt <- TT.points.in.classes( #renombrado de 'clases' para evitar conflicto con variable landcover definida anteriormente
  tri.data = datos_promedio,
  class.sys = "USDA.TT"
)

# Extraer el nombre de la clase
textura_promedio$clase_usda <- colnames(clases_tt)[max.col(clases_tt)]

traduccion <- c(
  Cl     = "Arcilloso",
  SiCl   = "Arcillo limoso",
  SaCl   = "Arcillo arenoso",
  ClLo   = "Franco arcilloso",
  SiClLo = "Franco arcillo limoso",
  SaClLo = "Franco arcillo arenoso",
  Lo     = "Franco",
  SiLo   = "Franco limoso",
  SaLo   = "Franco arenoso",
  Si     = "Limoso",
  LoSa   = "Areno franco",
  Sa     = "Arenoso"
)
textura_promedio$clase <- traduccion[textura_promedio$clase_usda]


#### PROPIEDADES HIDRÁULICAS ####
#direct_prop_hid <- here("ROSETTA_MEAN")

Capacidad_de_campo_cut <- list()
paleta_cc <- rev(hcl.colors(100, "Plasma"))

for (hz in horizontes){
  archivo <- paste0("FC.", hz, "cm.tif")
  nombre_svg <- paste0("capacidad_campo", gsub("-", "_", hz), "cm.svg")
  capacidad_campo_r <- rast(file.path(direct_prop_hid, archivo))
  Capacidad_de_campo_cut[[hz]] <- guardar_svg_raster(
    r = capacidad_campo_r,
    polygon = polygon,
    titulo = paste("Capacidad de campo - ", hz, "cm"),
    paleta = paleta_cc,
    archivo_salida = nombre_svg,
    metodo = "bilinear"
  )
}

# mínimos y máximos globales de CC
mins_cc <- sapply(horizontes, function(hz) global(Capacidad_de_campo_cut[[hz]], "min", na.rm = TRUE)[1,1])
maxs_cc <- sapply(horizontes, function(hz) global(Capacidad_de_campo_cut[[hz]], "max", na.rm = TRUE)[1,1])
min_cc_global <- min(mins_cc, na.rm = TRUE)
max_cc_global <- max(maxs_cc, na.rm = TRUE)

# panel CC por horizonte con escala común
guardar_panel_rasters(
  lista_rasters = Capacidad_de_campo_cut,
  horizontes = horizontes,
  archivo_salida = "cc_cada_nivel.svg",
  titulo_base = "Capacidad de campo -",
  paleta = paleta_cc,
  zlim = c(min_cc_global, max_cc_global),
  agregar_norte = TRUE,
  agregar_escala = TRUE
)

Punto_marchitez_cut <- list()
paleta_pm  <- rev(hcl.colors(100, "Heat"))

for (hz in horizontes){
  archivo <- paste0("PWP.", hz, "cm.tif")
  nombre_svg <- paste0("punto de marchitez", gsub("-", "_", hz), "cm.svg")
  punto_marchitez_r <- rast(file.path(direct_prop_hid, archivo))
  Punto_marchitez_cut[[hz]] <- guardar_svg_raster(
    r = punto_marchitez_r,
    polygon = polygon,
    titulo = paste("Punto de marchitez - ", hz, "cm"),
    archivo_salida = nombre_svg,
    paleta = paleta_pm,
    metodo = "bilinear"
  )
}

# mínimos y máximos globales de PMP
mins_pmp <- sapply(horizontes, function(hz) global(Punto_marchitez_cut[[hz]], "min", na.rm = TRUE)[1,1])
maxs_pmp <- sapply(horizontes, function(hz) global(Punto_marchitez_cut[[hz]], "max", na.rm = TRUE)[1,1])
min_pmp_global <- min(mins_pmp, na.rm = TRUE)
max_pmp_global <- max(maxs_pmp, na.rm = TRUE)

# panel PMP por horizonte con escala común
guardar_panel_rasters(
  lista_rasters = Punto_marchitez_cut,
  horizontes = horizontes,
  archivo_salida = "pmp_cada_nivel.svg",
  titulo_base = "Punto de Marchitez -",
  paleta = paleta_pm,
  zlim = c(min_pmp_global, max_pmp_global),
  agregar_norte = TRUE,
  agregar_escala = TRUE
)

# propiedades hidráulicas promedio por horizonte
prop_promedio <- data.frame(
  horizonte = horizontes,
  pmp = NA,
  cc = NA
)

for (i in seq_along(horizontes)) {
  hz <- horizontes[i]
  prop_promedio$pmp[i] <- round(global(Punto_marchitez_cut[[hz]], "mean", na.rm = TRUE)[1,1], 3)
  prop_promedio$cc[i]  <- round(global(Capacidad_de_campo_cut[[hz]], "mean", na.rm = TRUE)[1,1], 3)
}


#### CÁLCULO DEL ALMACENAMIENTO ####
#(CC - PMP) * espesor

espesores <- c(5, 10, 15, 30, 40, 100)  # cm de cada horizonte en archivos .tif
almacenamiento_cut <- list()
paleta_alm <- rev(hcl.colors(100, "Blues"))
ref_raster <- NULL

#.svg por cada horizonte
for (i in seq_along(horizontes)) {
  hz <- horizontes[i]
  esp <- espesores[i]

  # reproyectar CC y PMP al CRS del polígono antes de operar, para evitar NA por distintas resoluciones
  cc <- project(Capacidad_de_campo_cut[[hz]], st_crs(polygon)$wkt, method = "bilinear")
  pmp <- project(Punto_marchitez_cut[[hz]], st_crs(polygon)$wkt, method = "bilinear")
  alm_hz <- (cc - pmp) * esp

  if (is.null(ref_raster)) {
    ref_raster <- alm_hz                                    #primer horizonte define la grilla de referencia
  } else {
    alm_hz <- resample(alm_hz, ref_raster, method = "bilinear") #resampleo a grilla común para evitar NA al apilar
  }

  almacenamiento_cut[[hz]] <- alm_hz
  nombre_svg <- paste0("almacenamiento_", gsub("-", "_", hz), "cm.svg")

  svg(file.path(fig_dir, nombre_svg), width = 8, height = 6)
  par(mar = c(5, 5, 4, 6))
  plot(alm_hz,
       main = paste("Almacenamiento por horizonte -", hz, "cm"),
       col = paleta_alm,
       axes = TRUE)
  agregar_elementos_mapa(alm_hz, polygon)
  dev.off()
}

#Almacenamiento total por píxel
#Apilar todos los horizontes y sumarlos
alm_stack <- rast(almacenamiento_cut)
alm_total <- app(alm_stack, fun = sum, na.rm = TRUE)
#sistema común
alm_total_proj <- project(alm_total, st_crs(polygon)$wkt, method = "bilinear")

svg(file.path(fig_dir, "almacenamiento_total_por_pixel.svg"), width = 8, height = 6)
par(mar = c(5, 5, 4, 6))
plot(alm_total_proj,
     main = "Almacenamiento total del suelo por píxel (cm de agua)",
     col = paleta_alm,
     axes = TRUE)
agregar_elementos_mapa(alm_total_proj, polygon)
dev.off()

#Valores mínimo y máximo del almacenamiento total
min_alm <- global(alm_total_proj, "min", na.rm = TRUE)[1,1]
max_alm <- global(alm_total_proj, "max", na.rm = TRUE)[1,1]

#### Panel de almacenamiento por horizonte con escala común
mins_alm <- sapply(horizontes, function(hz) global(almacenamiento_cut[[hz]], "min", na.rm = TRUE)[1,1])
maxs_alm <- sapply(horizontes, function(hz) global(almacenamiento_cut[[hz]], "max", na.rm = TRUE)[1,1])
min_alm_hz <- min(mins_alm, na.rm = TRUE)
max_alm_hz <- max(maxs_alm, na.rm = TRUE)

guardar_panel_rasters(
  lista_rasters = almacenamiento_cut,
  horizontes = horizontes,
  archivo_salida = "almacenamiento_por_horizonte.svg",
  titulo_base = "Almacenamiento (cm agua) -",
  paleta = paleta_alm,
  zlim = c(min_alm_hz, max_alm_hz),
  agregar_norte = TRUE,
  agregar_escala = TRUE
)

# promedio de almacenamiento por horizonte
promedio_alm_hz <- data.frame(
  horizonte = horizontes,
  alm = NA
)

for (i in seq_along(horizontes)) {
  hz <- horizontes[i]
  promedio_alm_hz$alm[i] <- round(global(almacenamiento_cut[[hz]], "mean", na.rm = TRUE)[1,1], 2)
}

# Mostrar los valores
promedio_total <- global(alm_total, "mean", na.rm = TRUE)[1,1]

#Volumen de agua en polígono

# Resolución espacial del raster (tamaño de píxel en metros)
res_m <- res(alm_total_proj)                       # devuelve c(ancho, alto)
area_pixel_m2 <- abs(res_m[1] * res_m[2])          # área por píxel en m², abs() previene negativos en CRS con ejes invertidos

# Suma del almacenamiento de todos los píxeles (en cm)
suma_alm_cm <- global(alm_total_proj, "sum", na.rm = TRUE)[1,1]
volumen_total_m3 <- (suma_alm_cm / 100) * area_pixel_m2 #cm>metros prof. * área
capacidad_aljibe_m3 <- 10  # m³

# Número de camiones
n_aljibes <- volumen_total_m3 / capacidad_aljibe_m3


##### RESULTADOS NUMÉRICOS ####


cat("\n============================================\n", "MANEJO DEM\n",
    "============================================\n")

cat("Elevación máxima:", round(max(valores_elev), 2), "m\n",
    "Elevación mínima:", round(min(valores_elev), 2), "m\n",
    "Resumen cuantiles DEM", "\n", "Min. 1st Qu. Median Mean 3rd Qu.Max.", "\n", round(summary(valores_elev), 2), "\n",
    "IQR DEM", "\n", round(IQR(valores_elev), 2), "\n",
    "Desviación estándar DEM", "\n", round(sd(valores_elev), 2), "\n",
    "Coeficiente de variación DEM", "\n", round(sd(valores_elev)/mean(valores_elev), 2)) #Coeficiente de variación


cat("\n============================================\n", "MAPA DE TEXTURAS\n",
    "============================================\n")
cat("Mínimo porcentaje de arcilla:", round(min_global, 2), "%", "\n",
    "Máximo porcentaje de arcilla:", round(max_global, 2), "%", "\n",
    "Mínimo porcentaje de arena:", round(min_sand_global, 2), "%", "\n",
    "Máximo porcentaje de arena:", round(max_sand_global, 2), "%", "\n",
    "Mínimo porcentaje de limo:", round(min_silt_global, 2), "%", "\n",
    "Máximo porcentaje de limo:", round(max_silt_global, 2), "%")
kable(textura_promedio[, c("horizonte", "arena", "limo", "arcilla", "clase")], caption = "Clasificación USDA por horizonte")

cat("\n============================================\n",
    "PROPIEDADES HIDRÁULICAS\n",
    "============================================\n")
cat("Mínimo valor de CC:", round(min_cc_global, 2), "cm^3/cm^3", "\n",
    "Máximo valor de CC:", round(max_cc_global, 2), "cm^3/cm^3", "\n",
    "Mínimo valor de PMP:", round(min_pmp_global, 2), "cm^3/cm^3", "\n",
    "Máximo valor de PMP:", round(max_pmp_global, 2), "cm^3/cm^3", "\n")
kable(prop_promedio, caption = "Propiedades Hidráulicas por horizonte")

cat("\n============================================\n",
    "ALMACENAMIENTOS\n",
    "============================================\n")
cat("Almacenamiento mínimo por pixel:", round(min_alm, 2), "cm\n",
    "Almacenamiento máximo por pixel:", round(max_alm, 2), "cm\n",
    "Mínimo global de almacenamiento por horizontes:", round(min_alm_hz, 3), "cm\n",
    "Máximo global de almacenamiento por horizontes:", round(max_alm_hz, 3), "cm\n")

kable(promedio_alm_hz, caption = "Almacenamiento promedio por horizonte")
cat("Almacenamiento total promedio por pixel:", round(promedio_total, 2), "cm\n")
print(paste("Suma total de almacenamiento (cm):", round(suma_alm_cm, 2)))
print(paste("Volumen total de agua en el polígono:", round(volumen_total_m3, 2), "m³"))
print(paste("Equivalente en camiones aljibe (10 m³ c/u):", round(n_aljibes, 0), "camiones"))
beep(8)
