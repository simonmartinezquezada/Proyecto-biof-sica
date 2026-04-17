# Buscar los archivos en su computador
rm(list=ls(all=TRUE))

#paquetes
#install.packages("leaflet")
#install.packages("raster")
#install.packages("RColorBrewer")
#install.packages("sfheaders")
#install.packages("terra")
#install.packages("ggplot2")

#librerias
library(sf)
library("leaflet")
library("RColorBrewer")
library("raster")
library(sfheaders)
library(terra)
library(ggplot2)

#### Fijar directorio de trabajo
directorio <- c("C:/Users/simon/Downloads/Biofísica ambiental/Avance 1") #del cuadrante inferior derecho selecionan el engranaje donde al lado dice more y le dan a "copy folder path to clipboard"
setwd(directorio)
archivos <- list.files(directorio, full.names = T)
archivos

#### Paquete sf y leaflet
map <- leaflet()
addTiles(map = map)
map <- addProviderTiles(map = map, "Esri.WorldImagery", group = "ESRI")
map

#### Punto seleccionado (UTM)             #el código dice sur, pero en el sheet sale como norte 
coordenada <- c(este = 121165.911441601 , sur = 5863133.22043047, label = "mi punto") ##esto es solo un vector con los datos de su punto, aún no es un objeto espacial
coordenada_m <- as.matrix(t(coordenada))
coordenada_df <- as.data.frame.matrix(coordenada_m)
coordenada_df[,1:2] <- apply(coordenada_df[1:2], 2, as.numeric)
##transformarlo a un objeto espacial
punto <- st_as_sf(coordenada_df, ##aqui lee un dataframe por lo tanto puede agregar muchos puntos
                  coords = c("este", "sur"), ##aqui van los nombres de sus columnas
                  crs = 9155) ###este CRS es para UTM
punto

#### Ubicar punto en el map. 
crs_latlong <- crs("+proj=longlat +ellps=GRS80 +datum=WGS84 +units=m +no_defs")#sistema de referencia 
punto_latlng <- st_transform(punto, crs = crs_latlong)
map <- addMarkers(map = map, lat = st_coordinates(punto_latlng)[2], lng = st_coordinates(punto_latlng)[1], popup = c(punto_latlng$label))
map

#### Crear un poligono cuadrado, a partir de un punto (centroide)
# Área deseada: 30 km2
area_m2 <- 30 * 1000000 #30km2 a Xm2
lado_m <- sqrt(area_m2)
mitad <- lado_m / 2

# Coordenadas del centro
xy <- st_coordinates(punto)[1,]
x <- xy[1] 
y <- xy[2]

# Crear cuadrado centrado en punto  #deberían ser 4 puntos/vertices
coords_cuadrado <- matrix(c(
  x - mitad, y - mitad,
  x + mitad, y - mitad,
  x + mitad, y + mitad,
  x - mitad, y + mitad,
  x - mitad, y - mitad
), ncol = 2, byrow = TRUE)

##transformar el poligono a un objeto espacial
cuadrado <- st_sf(
  label = "poligono 30 km2",
  geometry = st_sfc(st_polygon(list(coords_cuadrado)), crs = st_crs(punto))
)
cuadrado

# Area del polígono (m2)
st_area(cuadrado)

# Pasar a lat/long para leaflet
crs_latlong <- crs("+proj=longlat +ellps=GRS80 +datum=WGS84 +units=m +no_defs") #repetición código línea 45
punto_latlng <- st_transform(punto, crs = crs_latlong)
cuadrado_latlng <- st_transform(cuadrado, crs = crs_latlong)

# visualizar polígono en el Mapa
map <- leaflet()
map <- addTiles(map)
map <- addProviderTiles(map, "Esri.WorldImagery", group = "ESRI")
map <- addMarkers(map, data = punto_latlng, popup = ~label) #punto en el mapa
map <- addPolygons(map, data = cuadrado_latlng, color = "red", weight = 2, #poligono en el mapa
                   fillColor = "red", fillOpacity = 0.2, popup = ~label)
                                      #opacidad

map

#### Guardar el poligono como shapefile en la carpeta
getwd()
st_write(cuadrado, dsn = "figuras/polygon.shp", driver = "ESRI Shapefile", append = T)

#### Leer el shapefile guardado
polygon <- read_sf(dsn = "figuras/polygon.shp", layer = "polygon")
head(polygon)
crs_latlong <- crs("+proj=longlat +ellps=GRS80 +datum=WGS84 +units=m +no_defs")
crs_UTM <- crs("+init=EPSG:9155")
polygon_latlng <- st_transform(polygon, crs = crs_latlong)

# Mapa con nuevo poligono
map <- leaflet()
map <- addTiles(map)
map <- addProviderTiles(map, "Esri.WorldImagery", group = "ESRI")
map <- addPolygons(map, data = polygon_latlng, color = "red", weight = 2, #poligono en el mapa
                   fillColor = "red", fillOpacity = 0.2, popup = ~label)
map

#### Recortar Land cover según el polígono
lc<- rast(file.path(directorio, "CLDynamicLandCover_2018.1.0beta.tif"))#elemento raster
lc
plot(lc)
# transformar polygon al sistema de referencia del raster
polygon_wgs84 <- st_transform(polygon, crs = st_crs(lc))
# pasar el polígono a formato terra
polygon_v <- vect(polygon_wgs84)
# recorte de lc según la extensión del polígono (recorte exacto)
lc_crop <- crop(lc, polygon_v)
# recorte con la forma del polígono, pasando los pixeles que no están completos a valores de NA.
lc_mask <- mask(lc_crop, polygon_v)

plot(lc_crop)# recorta lc segun la forma extacta de polygon
plot(lc_mask)# elimina los pixeles que quedan incompletos en el borde de lc_crop

# el lc quede con el mismo sistema de referencia, misma resolución, tamaño de píxel y alineación
lc.proj = project(lc_mask, polygon, method = "near") #revisar sistema de referencia en ejes
plot(lc.proj, main="LC proyectado")

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
#colores = RColorBrewer::brewer.pal(12, "Paired") #hay 14 clases pero solo 12 colores
#colores <- colorRampPalette(brewer.pal(12, "Paired"))(14) #colores muy similares y sin sentido ambiental

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

svg("figuras/landcover_poligono.svg", width = 8, height = 6)

plot(lc.cat, main = "LandCover Polígono", col = colores)

dev.off()

#### Grafico de Torta
# frecuencia de pixeles sobre el lc proyectado (raster categórico)
freq_lc <- as.data.frame(freq(lc.proj))
freq_lc <- na.omit(freq_lc)
#preguntar si se pueden eliminar las coberturas con poco porcentaje para mejorar la representación

# unir clases con frecuencias
lut <- freq_lc[, c("value", "count")]# crea un nuevo data frame llamado lut, tomando solo las columnas value y count 
names(lut)[names(lut) == "count"] <- "npix"# Cambia el nombre de la columna "count" a "npix"
i <- match(lut$value, clases$value)# Busca en que posición de clases$value aparece cada valor de lut$value
cols_extra <- setdiff(names(clases), "value")# Identifica qué columnas de "clases" se van a agregar a "lut",
lut[cols_extra] <- clases[i, cols_extra, drop = FALSE]# Agrega a "lut" las columnas de "clases" según la coincidencia de "value"
lut$prop <- round(100 * lut$npix / sum(lut$npix), 2)# Calcula el porcentaje de cada cobertura en polygon
lut$plot_name <- paste0(lut$cobertura, " ", lut$prop, "%")# Crear una nueva columna de texto llamada "plot_name", que ira en la leyenda del gráfico 
lut

names(colores) <- unique(lut$plot_name) #se asignan los nombres de plot names a los colores para poder graficar

# grafico de torta (pie chart)
pie_chart <- lut %>% ggplot(aes(x = '', y = prop, fill = plot_name))+
  geom_bar( 
    stat = 'identity', width = 1,
    color="white"
  )+
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_manual(values = colores)+# remove background, grid, numeric labels
  labs(fill = 'Cobertura', title = 'Porcentaje del área cubierto por cada cobertura de suelo')

svg("figuras/pie_chart_landcover.svg", width = 8, height = 6)
print(pie_chart)
dev.off()



######recorte raster DEM
# cargar raster DEM #descargar del drive del anuncio del avance 1
DEM <- rast(file.path(directorio, "DEM.Chile.Continental.tif"))

# transformar polígono al CRS del raster
polygon_proj <- st_transform(polygon, crs = st_crs(DEM))

# convertir a formato terra
polygon_v <- vect(polygon_proj)

# recortar y enmascarar
DEM_crop <- crop(DEM, polygon_v)
DEM_mask <- mask(DEM_crop, polygon_v)

plot(DEM_crop)
plot(DEM_mask)
# visualizar resultado
DEM.proj = project(DEM_mask, polygon, method = "near")

svg("figuras/DEM_poligono.svg", width = 8, height = 6)

plot(DEM.proj, main = "DEM proyectado")

dev.off()

#minimo y máximo
valores <- global(DEM.proj, fun = c("min", "max"), na.rm = TRUE)

elev_min <- valores[1,1]
elev_max <- valores[1,2]

cat("Elevación mínima:", elev_min, "m\n")
cat("Elevación máxima:", elev_max, "m\n")

###Mapas usando CLSoilMaps
#artículo https://doi.org/10.1038/s41597-023-02536-x
#database https://zenodo.org/records/7464210?preview_file=FileDesc.txt
#descargar SoilMaps_MEAN y ROSETTA_MEAN 


