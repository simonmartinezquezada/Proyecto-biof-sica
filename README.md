# Proyecto Semestral Grupo 5 

Trabajo grupal para asignatura de ICH3600 Biofísica Ambiental, Pontificia Universidad Católica de Chile

## Contenido del repositorio

- `avance_1`: script que genera los paneles de las propiedades hidráulicas del suelo.
- `avance_2`: script con el balance en un pixel, almacenamiento en el día 365 y días bajo estrés hídrico.
- `trabajo_final_hydrus`: script que genera un excel para el input de Hydrus, después procesa el output de Hydrus y genera el svg.
- `datos_INIA`: datos agrometereológicos de las estación Las Puentes, Arauco desde 01-05-2020 hasta 01-05-2026.
- `Hydrus_Avance_final.h1d`: archivo de setup para Hydrus.
- `figuras/`: carpeta donde se guardan los gráficos generados (incluye el svg comparación S_discreto y S_hydrus)
    - `input_data_hydrus`: excel dentro de figuras con el input para Hydrus.
    - `balance_diario_hydrus`: excel dentro de figuras con el balance de Hydrus.
- `Hydrus_Avance_final/`: carpeta donde se guardan los archivos de output de Hydrus

## Carpetas y archivos que deben estar al momento de ejecutar
- `DEM.Chile.Continental`: Tif de elevaciones para el avance 1.
- `CLDynamicLandCover_2018_1.0`: Tif de coberturas, clave para el avance 1, 2 y final.
- `ROSETTA_MEAN/`: Carpeta que debe contener los Ráster con propiedades hidráulicas avance 1, 2 y final.
- `SoilMaps_MEAN/`: Carpeta que debe contener los Ráster con propiedades texturales avance 1 y final.

- En la carpeta "maestra" debe estar los scripts, datos_INIA.xlsx, CLDynamicLandCover_2018_1.0 y carpetas "figuras", "ROSETTA_MEAN" 

## Cosas importantes

Antes de ejecutar el script:
- **EL SCRIPT trabajo_final_hydrusv3 REQUIERE DE LA PREVIA EJECUCIÓN DE avance_2_balance_final, ya que utiliza elementos generados**
- El paquete **here** permite utilizar las rutas relativas de los archivos. **Importante que los archivos a leer se encuentren siempre en la misma carpeta que el Script**.
- Para que les corra todo el Rscript, deben colocar en la misma carpeta del .Rproject y script, los archivos de SoilMaps, Rosseta_mean, LandCover y DEM
- instalar los siguientes paquetes en R si no están disponibles:

```r
install.packages(c("tidyverse", "here"))
