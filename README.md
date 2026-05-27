# Proyecto Semestral Grupo 5 

Trabajo grupal para asignatura de ICH3600 Biofísica Ambiental, Pontificia Universidad Católica de Chile

## Contenido del repositorio

- `avance_2_CLSoilMaps`: script que genera los paneles de las propiedades hidráulicas del suelo.
- `avance_2_balance`: script con el balance en un pixel, almacenamiento en el día 365 y días bajo estrés hídrico.
- `datos_INIA`: datos agrometereológicos de las estación Las Puentes, Arauco desde 01-05-2020 hasta 01-05-2026.
- `figuras/`: carpeta donde se guardan los gráficos generados
- `ROSETTA_MEAN/`: Carpeta que debe contener los Ráster con propiedades hidráulicas
- En la carpeta "maestra" debe estar los scripts, datos_INIA.xlsx, CLDynamicLandCover_2018_1.0 y carpetas "figuras", "ROSETTA_MEAN" 

## Cosas importantes

Antes de ejecutar el script:
- El paquete **here** permite utilizar las rutas relativas de los archivos. **Importante que los archivos a leer se encuentren siempre en la misma carpeta que el Script**.
- Para que les corra todo el Rscript, deben colocar en la misma carpeta del .Rproject y script, los archivos de SoilMaps, Rosseta_mean, LandCover y DEM
- instalar los siguientes paquetes en R si no están disponibles:
```r
install.packages(c("tidyverse", "here"))
