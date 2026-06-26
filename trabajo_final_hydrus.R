#Trabajo Final - Script HYDRUS 1D
#Genera: (1) input_data_hydrus.xlsx con datos de suelo y clima para HYDRUS,
#        (2) lectura y procesamiento de salidas de HYDRUS (Obs_Node.out y T_level.out),
#        (3) SVG comparativo dinámica anual píxel seleccionado: modelo discreto vs HYDRUS.
#
#DEPENDENCIAS: este script debe ejecutarse DESPUÉS de avance_2_balance_final.R, ya que utiliza
#los objetos calculados en ese script (S_serie, Tp_mm_por_clase, Ep_mm_por_clase, idx_sel,
#lc_col_vec, fechas_bal, Pp_mm_bal, theta_r_hz, theta_s_hz, alpha_hz, n_hz, FC_hz,
#Punto_marchitez_cut, valid_mask, ref_proj, espesores, horizontes, etc.).
#
#NOTA SOBRE PARÁMETROS DE SUELO PARA HYDRUS:
#Se usan los parámetros VAN GENUCHTEN de cada horizonte INDIVIDUAL del píxel seleccionado
#(extraídos de los rasters cargados en avance_2_balance_final.R), NO los parámetros equivalentes
#ponderados. Los parámetros equivalentes son una simplificación para el modelo bucket (un único θ
#del perfil); HYDRUS resuelve Richards por horizonte, por lo que requiere los valores reales de
#cada capa. Esto es consistente con el formato de la hoja "suelo" de input_data.xlsx.


#### LIBRERÍAS ADICIONALES (cargar junto con las de avance_2_balance_final.R) ####
#install.packages("writexl")
#install.packages("data.table")
library(writexl)
library(data.table)
library(sf)
library(terra)
library(here)
library(readxl)
library(dplyr)

#### DIRECTORIOS ####
fig_dir      <- "C:/Users/simon/Downloads/Trabajo_final_Grupo5/figuras"         #misma carpeta de salida usada en avances anteriores
hydrus_dir   <- "C:/Users/simon/Downloads/Trabajo_final_Grupo5/Hydrus_Avance_final" #carpeta con los archivos de salida de HYDRUS
direct_texture <- "C:/Users/simon/Downloads/Trabajo_final_Grupo5/SoilMaps_MEAN"
direct_prop_hid <- "C:/Users/simon/Downloads/Trabajo_final_Grupo5/ROSETTA_MEAN"

#NOTA: hydrus_dir debe contener Obs_Node.out y T_level.out generados por HYDRUS 1D


#### PERÍODO DE SIMULACIÓN PARA HYDRUS ####
#el enunciado define el año de simulación 11-11-2022 al 11-11-2023 (365 días)
fecha_hydrus_ini <- as.Date("2022-11-11")
fecha_hydrus_fin <- as.Date("2023-11-10")  #365 días: día 1 = 11-11-2022, día 365 = 10-11-2023
fechas_hydrus    <- seq(fecha_hydrus_ini, fecha_hydrus_fin, by = "day")
n_dias_hydrus    <- length(fechas_hydrus)  #debe ser 365


#### EXTRACCIÓN DE PARÁMETROS DE SUELO DEL PÍXEL SELECCIONADO ####
#Se extraen los parámetros Van Genuchten de cada horizonte en el píxel idx_sel.
#theta_r_hz, theta_s_hz, alpha_hz, n_hz son listas de 6 rasters (uno por horizonte),
#cargadas en avance_2_balance_final.R. FC_hz y Punto_marchitez_cut también se cargan allí.

#Verificar que idx_sel existe en el entorno (heredado de avance_2_balance_final.R)
if (!exists("idx_sel")) stop("idx_sel no encontrado. Ejecutar primero avance_2_balance_final.R.")

#Índice de celda en la grilla raster correspondiente al píxel seleccionado
cell_idx_sel <- which(valid_mask)[idx_sel]

#Construir tabla de parámetros de suelo por horizonte para el píxel seleccionado
#(formato equivalente a la hoja "suelo" de input_data.xlsx)
#espesores acumulados para nodos de observación de HYDRUS (cm desde superficie)
#Los nodos de HYDRUS se ubican en el centro de cada horizonte
nodos_hydrus <- c(2, 10, 30, 60, 100, 160)  #puntos de observación (cm), coherentes con la hoja "suelo"

#Profundidades inferior de cada horizonte (para definir la estratigrafía en HYDRUS)
prof_inf_hz <- c(5, 15, 30, 60, 100, 200)  #cm

#Nombres de horizontes en el formato de HYDRUS (equivalentes a la hoja "suelo")
nombres_hz_hydrus <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")

#Cargar también datos de arcilla, limo, arena y densidad aparente para completar la hoja "suelo"
#Estos datos vienen de SoilMaps_MEAN (disponibles desde avance_1 scripts)
#direct_texture <- here("SoilMaps_MEAN")
#direct_prop_hid <- here("ROSETTA_MEAN")

#Extraer textura y densidad aparente del píxel seleccionado por horizonte
#SoilMaps_MEAN contiene: Clay, Sand, Silt y BulkDensity por horizonte
prefijos_hz_soil <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")

cat("Extrayendo parámetros de suelo del píxel seleccionado...\n")

tabla_suelo <- data.frame(
  n_horizonte = 1:6,
  Horizonte   = nombres_hz_hydrus,
  A_pct       = NA,  #arena %
  L_pct       = NA,  #limo %
  a_pct       = NA,  #arcilla %
  CC          = NA,  #cm³/cm³
  PMP         = NA,  #cm³/cm³
  Da          = NA,  #densidad aparente (g/cm³)
  Qr          = NA,  #θr Van Genuchten
  Qs          = NA,  #θs Van Genuchten
  alpha_vg    = NA,  #αvg Van Genuchten (cm⁻¹)
  n_vg        = NA,  #n Van Genuchten (adim.)
  Ks          = NA,  #conductividad saturada (cm/día)
  nodos       = nodos_hydrus
)

for (i in seq_along(horizontes)) {
  hz  <- horizontes[i]
  pfx <- prefijos_hz_soil[i]

  #función interna: carga, recorta, reproyecta, resampa y extrae el valor en cell_idx_sel
  #recarga siempre desde disco para evitar punteros inválidos de SpatRaster entre scripts
  cargar_y_extraer <- function(archivo, directorio, metodo = "bilinear"){
    r     <- rast(file.path(directorio, archivo))
    r_cut <- poligono_recorte(r, polygon)
    r_prj <- project(r_cut, st_crs(polygon)$wkt, method = metodo)
    r_res <- resample(r_prj, ref_proj, method = metodo)
    as.numeric(terra::values(r_res)[cell_idx_sel, 1])
  }

  #parámetros ROSETTA (θr, θs, αvg, n): recargados directamente desde ROSETTA_MEAN
  tabla_suelo$Qr[i]       <- cargar_y_extraer(paste0("theta_r_", hz, "cm.tif"), direct_prop_hid)
  tabla_suelo$Qs[i]       <- cargar_y_extraer(paste0("theta_s_", hz, "cm.tif"), direct_prop_hid)
  tabla_suelo$alpha_vg[i] <- cargar_y_extraer(paste0("alpha_",   hz, "cm.tif"), direct_prop_hid)
  tabla_suelo$n_vg[i]     <- cargar_y_extraer(paste0("n_",       hz, "cm.tif"), direct_prop_hid)

  #CC (capacidad de campo, FC) y PMP (punto de marchitez permanente, PWP)
  tabla_suelo$CC[i]  <- cargar_y_extraer(paste0("FC.",  hz, "cm.tif"), direct_prop_hid) #Este es el contenido de agua en el suelo inicial?
  tabla_suelo$PMP[i] <- cargar_y_extraer(paste0("PWP.", hz, "cm.tif"), direct_prop_hid)

  #Ks: ROSETTA_MEAN almacena en log10(cm/día) → convertir a cm/día
  ks_log            <- cargar_y_extraer(paste0("ksat_", hz, "cm.tif"), direct_prop_hid)
  tabla_suelo$Ks[i] <- ks_log #10^ks_log

  #Textura: Arena, Limo, Arcilla (%) desde SoilMaps_MEAN
  tabla_suelo$A_pct[i] <- cargar_y_extraer(paste0("Sand.", pfx, "cm.tif"), direct_texture)
  tabla_suelo$L_pct[i] <- cargar_y_extraer(paste0("Silt.", pfx, "cm.tif"), direct_texture)
  tabla_suelo$a_pct[i] <- cargar_y_extraer(paste0("Clay.", pfx, "cm.tif"), direct_texture)

  #Densidad aparente (BulkDensity de SoilMaps_MEAN)
  bd_archivo <- paste0("Bulkd.", pfx, "cm.tif")
  if (file.exists(file.path(direct_texture, bd_archivo))) {
    tabla_suelo$Da[i] <- cargar_y_extraer(bd_archivo, direct_texture)
  } else {
    tabla_suelo$Da[i] <- NA  #archivo no disponible; columna queda NA sin interrumpir la ejecución
    if (i == 1) warning(paste("BulkDensity no encontrado en", direct_texture,
                              "- columna Da quedará NA. Verificar nombre del archivo."))
  }
}

cat("Extracción de parámetros de suelo completada.\n")


#### TABLA DE CLIMA PARA HYDRUS (hoja "clima") ####
#Período 11-11-2022 al 10-11-2023 (365 días).
#Si INIA_diario no cubre este período exacto, se usa el subconjunto disponible (mismo que balance).
#Tp y Ep corresponden al píxel seleccionado (columna j = lc_col_vec[idx_sel] en las matrices).

j_sel <- lc_col_vec[idx_sel]  #índice de clase del píxel seleccionado en las matrices Tp/Ep

#Precipitación del período (desde INIA_diario, ya cargado en avance_2_balance_final.R)
#Se usa el mismo subconjunto de 365 días utilizado en el balance hídrico (INIA_balance)
Pp_hydrus  <- INIA_balance$Precipitación.Acumulada.mm  #mm d⁻¹
Tp_hydrus  <- Tp_mm_por_clase[, j_sel]                 #mm d⁻¹, píxel seleccionado
Ep_hydrus  <- Ep_mm_por_clase[, j_sel]                 #mm d⁻¹, píxel seleccionado

tabla_clima <- data.frame(
  dia_hydrus      = 1:n_dias_hydrus,
  Fecha           = fechas_hydrus,
  precipitacion_mm = Pp_hydrus,
  Tp_mm           = Tp_hydrus,
  Ep_mm           = Ep_hydrus,
  precipitacion_cm = Pp_hydrus / 10,
  Tp_cm           = Tp_hydrus / 10,
  Ep_cm           = Ep_hydrus / 10
)


#### TABLA "Soil profile summary" (contenido de agua inicial = CC por horizonte) ####
#El enunciado indica que el contenido de agua inicial es la Capacidad de Campo (CC, h = -330 cm).
#La hoja original de input_data.xlsx usaba PMP; aquí se reemplaza por CC por horizonte del píxel.
tabla_soil_summary <- data.frame(
  `CC (cm3/cm3)`  = tabla_suelo$CC,
  `Mat (horizonte)` = 1:6,
  check.names = FALSE
)


#### EXPORTACIÓN DE input_data_hydrus.xlsx ####
#Se generan tres hojas:
# - "clima": precipitación, Tp y Ep diarios del píxel seleccionado para los 365 días
# - "suelo": parámetros Van Genuchten por horizonte del píxel seleccionado
# - "Soil profile summary": contenido de agua inicial (CC) por horizonte

#Construir lista de hojas
hojas_hydrus <- list(
  clima                = tabla_clima,
  suelo                = tabla_suelo,
  `Soil profile summary` = tabla_soil_summary
)

#Guardar Excel
archivo_excel_hydrus <- file.path(fig_dir, "input_data_hydrus.xlsx")
write_xlsx(hojas_hydrus, path = archivo_excel_hydrus)

cat("\nArchivo generado:", archivo_excel_hydrus, "\n")
cat("Hojas: 'clima' (", nrow(tabla_clima), "días), 'suelo' (6 horizontes), 'Soil profile summary'\n")


#### LECTURA Y PROCESAMIENTO DE SALIDAS DE HYDRUS ####
#Se leen Obs_Node.out (θ por nodo) y T_level.out (balance hídrico de HYDRUS).
#INSTRUCCIÓN: configurar en HYDRUS los nodos de observación en las profundidades: 2, 10, 22, 45, 80, 160 cm
#(centros de cada horizonte). HYDRUS debe tener 365 pasos de tiempo diarios.

#Función para leer nodos de observación de HYDRUS (idéntica a post_processing_script.R, adaptada a here)
read.obs_node <- function(project.path, out.file = "Obs_Node.out",
                          obs.output = NULL, obs.nodes, ...){
  options(warn = -1)
  obs_node_out <- data.table::fread(
    input             = file.path(project.path, out.file),
    fill              = TRUE,
    blank.lines.skip  = FALSE
  )

  node_ind     <- grep("Node", data.frame(obs_node_out)[, 1])
  output_names <- unlist(unclass(obs_node_out[node_ind + 2]))
  output_names <- unique(output_names[!is.na(output_names)])
  output_names <- output_names[output_names != ""]
  output_names <- output_names[2:length(output_names)]

  obs_node_out <- obs_node_out[-c(1:(node_ind + 2), nrow(obs_node_out)),]
  obs_node_out <- obs_node_out[, colnames(obs_node_out) := lapply(.SD, as.numeric), .SDcols = colnames(obs_node_out)]

  all_na_ind   <- sapply(X = obs_node_out, function(x) !all(is.na(x)))
  obs_node_out <- obs_node_out[, (all_na_ind), with = FALSE]

  output_names_rep <- rep(output_names, times = length(obs.nodes))
  obs_nodes_rep    <- rep(obs.nodes, each = length(output_names))
  output_names_all <- paste(output_names_rep, obs_nodes_rep, sep = "_")
  colnames(obs_node_out) <- c("Time", output_names_all)
  obs_node_out <- data.frame(obs_node_out, row.names = NULL, check.names = FALSE)

  if (is.null(obs.output) | missing(obs.output)) {
    obs_node_out <- obs_node_out
  } else {
    output_cols  <- grepl(obs.output, names(obs_node_out))
    in_cols      <- c("Time", names(obs_node_out)[output_cols])
    obs_node_out <- obs_node_out[, in_cols]
  }

  t1        <- obs_node_out[1,]
  t1$Time   <- 0
  tstep     <- diff(obs_node_out$Time)
  tstep     <- tstep[length(tstep)]
  remainder <- obs_node_out$Time %% tstep
  rem_ind   <- which(remainder == 0)

  obs_node_out <- rbind(t1, obs_node_out[rem_ind,])
  row.names(obs_node_out) <- NULL

  options(warn = 0)
  return(obs_node_out)
}

#Verificar existencia de los archivos de HYDRUS antes de intentar leerlos
hydrus_obs_file <- file.path(hydrus_dir, "Obs_Node.out")
hydrus_tlevel   <- file.path(hydrus_dir, "T_level.out")

hydrus_disponible <- file.exists(hydrus_obs_file) && file.exists(hydrus_tlevel)

if (!hydrus_disponible) {
  warning(paste("Archivos de HYDRUS no encontrados en:", hydrus_dir,
                "\nSe omite la lectura de salidas de HYDRUS.",
                "\nUna vez ejecutada la simulación, colocar Obs_Node.out y T_level.out en esa carpeta",
                "y volver a correr este script."))
} else {

  #Leer nodos de observación (θ por profundidad)
  #nodos de observación: 2, 10, 30, 60, 100, 160 cm (centros de cada horizonte)
  nodos <- read.obs_node(
    hydrus_dir,
    out.file  = "Obs_Node.out",
    obs.output = NULL,
    obs.nodes  = nodos_hydrus
  )
  nodos <- nodos[-1, ]  #eliminar primera fila (tiempo 0 duplicado al inicio)

  #Leer T_level.out (balance hídrico global de HYDRUS)
  tlevel_out <- read.table(
    file.path(hydrus_dir, "T_level.out"),
    header            = TRUE,
    sep               = "",
    dec               = ".",
    na.strings        = "NA",
    as.is             = TRUE,
    skip              = 6,
    check.names       = FALSE,
    fill              = TRUE,
    strip.white       = FALSE,
    blank.lines.skip  = TRUE,
    comment.char      = "#"
  )
  tlevel_out <- tlevel_out[-1, ]          #eliminar primera fila de datos extra
  tlevel_out <- tlevel_out[-nrow(tlevel_out), ]  #eliminar última fila (end)

  #Construir dataframe de balance con fechas del período de simulación HYDRUS
  Tr_hydrus          <- as.numeric(tlevel_out$vRoot) * 10   #transpiración real HYDRUS (mm)
  Percolacion_hydrus <- as.numeric(tlevel_out$vBot)  * 10   #percolación HYDRUS (mm)
  Escorrentia_hydrus <- as.numeric(tlevel_out$RunOff) * 10  #escorrentía HYDRUS (mm)
  Er_hydrus          <- as.numeric(tlevel_out$vTop)  * 10   #evaporación real HYDRUS (mm)

  #Percolación: HYDRUS reporta flujo saliente como negativo; se fuerza a ≤ 0
  Percolacion_hydrus <- ifelse(Percolacion_hydrus > 0, 0, Percolacion_hydrus)

  datos.balance.hydrus <- data.frame(
    fechas       = fechas_hydrus,
    Precipitacion = Pp_hydrus,
    Tr           = Tr_hydrus,
    Percolacion  = Percolacion_hydrus,
    Escorrentia  = Escorrentia_hydrus,
    Er           = Er_hydrus,
    Tp           = Tp_hydrus,
    Ep           = Ep_hydrus
  )

  #Almacenamiento HYDRUS: θ × espesor de cada horizonte, sumado para el perfil completo (200 cm)
  #Los espesores de cada nodo son los mismos del balance (espesores = c(5,10,15,30,40,100) cm)
  #Se accede a las columnas theta_<nodo> generadas por read.obs_node
  #Nombres de columnas theta: "theta_2", "theta_10", "theta_22", "theta_45", "theta_80", "theta_160"
  col_theta <- paste0("theta_", nodos_hydrus)

  #Verificar que las columnas theta existen (pueden variar si HYDRUS nombra diferente)
  col_theta_disp <- col_theta[col_theta %in% names(nodos)]
  if (length(col_theta_disp) < length(col_theta)) {
    warning(paste("Columnas theta no encontradas:", paste(setdiff(col_theta, col_theta_disp), collapse = ", "),
                  "\nVerificar nombres en Obs_Node.out. Columnas disponibles:", paste(names(nodos), collapse = ", ")))
  }

  #Almacenamiento HYDRUS [cm] = sum(θ_hz × espesor_hz) para los 6 horizontes
  almacenamiento_hydrus_cm <- as.numeric(
    nodos[[col_theta[1]]] * 5 +
    nodos[[col_theta[2]]] * 15 +
    nodos[[col_theta[3]]] * 25 +
    nodos[[col_theta[4]]] * 25 +
    nodos[[col_theta[5]]] * 50 +
    nodos[[col_theta[6]]] * 80
  )
#almacenamiento_cm=nodos$theta_2*5+nodos$theta_10*15+nodos$theta_30*25+nodos$theta_60*25+nodos$theta_100*50+nodos$theta_160*80
#así está en el script de post_processing
  balance.hydrus <- datos.balance.hydrus %>%
    mutate(almacenamiento_cm = almacenamiento_hydrus_cm)

  #Guardar balance procesado de HYDRUS
  write_xlsx(balance.hydrus, file.path(fig_dir, "balance_diario_hydrus.xlsx"))
  cat("\nBalance diario HYDRUS exportado:", file.path(fig_dir, "balance_diario_hydrus.xlsx"), "\n")


  #### MÉTRICAS DE COMPARACIÓN (modelo discreto vs HYDRUS) ####
  #El modelo discreto se considera el simulado y HYDRUS el observado (referencia), según enunciado.
  S_discreto <- as.numeric(S_serie[, 1])   #almacenamiento diario del modelo bucket [cm]
  S_hydrus   <- almacenamiento_hydrus_cm    #almacenamiento diario de HYDRUS [cm]

  #R² (coeficiente de determinación)
  SS_res <- sum((S_hydrus - S_discreto)^2, na.rm = TRUE)
  SS_tot <- sum((S_hydrus - mean(S_hydrus, na.rm = TRUE))^2, na.rm = TRUE)
  R2_val <- 1 - SS_res / SS_tot

  #PBIAS (sesgo porcentual): positivo = modelo sobreestima respecto a HYDRUS
  PBIAS_val <- 100 * sum(S_discreto - S_hydrus, na.rm = TRUE) / sum(S_hydrus, na.rm = TRUE)

  cat("\n============================================\n",
      "MÉTRICAS DE COMPARACIÓN (discreto vs HYDRUS)\n",
      "============================================\n")
  cat("R²   :", round(R2_val, 4), "\n")
  cat("PBIAS:", round(PBIAS_val, 2), "%\n")
  cat("R² > 0.6 sugiere buena concordancia en la dinámica temporal.\n",
      "PBIAS: positivo = modelo discreto sobreestima almacenamiento respecto a HYDRUS;\n",
      "       negativo = modelo discreto subestima.\n")


  #### SVG DINÁMICA ANUAL COMPARATIVA (modelo discreto vs HYDRUS) ####
  #Figura de 4 paneles según enunciado Trabajo Final:
  # Panel 1: Precipitación diaria [mm d⁻¹]
  # Panel 2: Transpiración potencial Tp [mm d⁻¹]
  # Panel 3: Evaporación potencial Ep [mm d⁻¹]
  # Panel 4: Almacenamiento S [cm] - modelo discreto vs HYDRUS (ambos en el mismo gráfico)

  svg(file.path(fig_dir, "dinamica_anual_comparativa_hydrus.svg"), width = 10, height = 11)

  par(mfrow = c(4, 1))
  par(mar = c(1, 5.5, 2.5, 2))

  #Panel 1: Precipitación [mm d⁻¹] - eje invertido (lluvia "cae")
  pp_ymax_h <- max(Pp_hydrus, na.rm = TRUE)
  pp_ymax_h <- if (!is.finite(pp_ymax_h) || pp_ymax_h <= 0) 1 else pp_ymax_h * 1.05
  barplot(Pp_hydrus,
          ylab   = expression(Pp~(mm~día^{-1})),
          ylim   = c(pp_ymax_h, 0),
          col    = "steelblue",
          border = NA,
          cex.lab = 1.1)
  mtext("Dinámica anual del píxel seleccionado - Modelo discreto vs HYDRUS 1D",
        side = 3, line = 0.8, cex = 1.0, font = 2)
  mtext(paste0("Cobertura: ", clase_sel_nom, " | Período: ",
               format(fecha_hydrus_ini, "%d-%m-%Y"), " - ",
               format(fecha_hydrus_fin, "%d-%m-%Y")),
        side = 3, line = -0.3, cex = 0.8)

  #Panel 2: Transpiración potencial Tp [mm d⁻¹]
  par(mar = c(1, 5.5, 0.5, 2))
  tp_ymax_h <- max(Tp_hydrus, na.rm = TRUE)
  tp_ymax_h <- if (!is.finite(tp_ymax_h) || tp_ymax_h <= 0) 1 else tp_ymax_h * 1.05
  plot(fechas_hydrus, Tp_hydrus,
       type    = "l", col = "darkorange2", lwd = 1.5,
       xaxt    = "n",
       ylab    = expression(T[p]~(mm~día^{-1})),
       ylim    = c(0, tp_ymax_h),
       cex.lab = 1.1)
  abline(h = 0, col = "gray70", lty = 2)

  #Panel 3: Evaporación potencial Ep [mm d⁻¹]
  par(mar = c(1, 5.5, 0.5, 2))
  ep_ymax_h <- max(Ep_hydrus, na.rm = TRUE)
  ep_ymax_h <- if (!is.finite(ep_ymax_h) || ep_ymax_h <= 0) 1 else ep_ymax_h * 1.05
  plot(fechas_hydrus, Ep_hydrus,
       type    = "l", col = "deepskyblue2", lwd = 1.5,
       xaxt    = "n",
       ylab    = expression(E[p]~(mm~día^{-1})),
       ylim    = c(0, ep_ymax_h),
       cex.lab = 1.1)
  abline(h = 0, col = "gray70", lty = 2)

  #Panel 4: Almacenamiento S [cm] - modelo discreto vs HYDRUS en el mismo gráfico
  par(mar = c(5.5, 5.5, 0.5, 2))
  s_ymin_h <- min(c(S_discreto, S_hydrus), na.rm = TRUE) * 0.95
  s_ymax_h <- max(c(S_discreto, S_hydrus), na.rm = TRUE) * 1.05
  if (!is.finite(s_ymin_h)) s_ymin_h <- 0
  if (!is.finite(s_ymax_h)) s_ymax_h <- 1

  plot(fechas_hydrus, S_discreto,
       type    = "l", col = "sienna4", lwd = 2,
       xaxt    = "n",
       ylab    = "Almacenamiento S (cm)",
       xlab    = "Fecha",
       ylim    = c(s_ymin_h, s_ymax_h),
       cex.lab = 1.1)
  lines(fechas_hydrus, S_hydrus, col = "steelblue", lwd = 2, lty = 2)
  legend("bottomright",
         legend = c(paste0("Modelo discreto  |  R² = ", round(R2_val, 3),
                           "  |  PBIAS = ", round(PBIAS_val, 1), "%"),
                    "HYDRUS 1D"),
         col    = c("sienna4", "steelblue"),
         lty    = c(1, 2),
         lwd    = c(2, 2),
         cex    = 0.8,
         bty    = "n")
  axis.Date(1,
            at     = seq(min(fechas_hydrus), max(fechas_hydrus), by = "2 months"),
            format = "%m-%Y",
            cex.axis = 0.9)

  dev.off()

  cat("\nSVG comparativo generado:", file.path(fig_dir, "dinamica_anual_comparativa_hydrus.svg"), "\n")

}  #fin bloque condicional hydrus_disponible

cat("\nScript trabajo_final_hydrus.R completado.\n")
cat("Si HYDRUS aún no fue ejecutado, el SVG comparativo se generará cuando los archivos",
    "\nObs_Node.out y T_level.out estén disponibles en:", hydrus_dir, "\n")
