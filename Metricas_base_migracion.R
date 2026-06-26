# ==============================================================================
# TRABAJO FINAL - CIENCIA DE DATOS (CURSO E520)
# Script Independiente: Métricas Topológicas Globales (Crisis Completas)
# ==============================================================================

library(data.table)
library(igraph)

# ==============================================================================
# 1. CONFIGURACIÓN PARAMÉTRICA DE PERÍODOS
# ==============================================================================

ruta_archivo <- "D:/Ciencia de Datos/Curso-E520-2026C1/TrabajoFinal/MLER.csv"

# Definimos SOLO las crisis completas ordenadas cronológicamente
periodos_analisis <- data.table(
  inicio = c(
    199812, # Inicio Crisis de Convertibilidad (Placeholder - Ajustar si es necesario)
    200810, # Crisis de las Hipotecas
    201803  # Inicio Crisis Cambiaria (Placeholder - Ajustar si es necesario)
  ),
  fin = c(
    200209, # Fin Crisis de Convertibilidad
    200908, # Fin Crisis de las Hipotecas
    201912  # Fin Crisis Cambiaria
  ),
  nombre = c(
    "1998 - 2002 (Crisis Fin de la Convertibilidad)", 
    "2008 - 2009 (Crisis de las Hipotecas)",
    "2018 - 2019 (Crisis Cambiaria)"
  )
)

# Ordenamos cronológicamente por fecha de inicio para que la tabla final quede prolija
setorder(periodos_analisis, inicio, fin)

# Clasificamos directamente todo como "Crisis"
periodos_analisis[, tipo_periodo := "Crisis"]

# Extraemos dinámicamente solo las fechas únicas que necesitamos leer del CSV
fechas_interes <- unique(c(periodos_analisis$inicio, periodos_analisis$fin))

# ==============================================================================
# 2. LECTURA OPTIMIZADA DE DATOS
# ==============================================================================

columnas_necesarias <- c("id_trabajador", "tiempo", "rem_tot", "r34")
datos_raw <- fread(ruta_archivo, select = columnas_necesarias)

# Filtramos la base gigante dejando exclusivamente los meses que vamos a analizar
datos <- datos_raw[tiempo %in% fechas_interes]
rm(datos_raw)
gc()

# ==============================================================================
# 3. MOTOR DE CÁLCULO DE MÉTRICAS (FUNCIÓN)
# ==============================================================================

calcular_metricas_red <- function(f_inicio, f_final, nombre_periodo, clasificacion) {
  
  dt_sub <- datos[tiempo %in% c(f_inicio, f_final)]
  setorder(dt_sub, id_trabajador, tiempo, -rem_tot)
  dt_unicos <- dt_sub[, .SD[1], by = .(id_trabajador, tiempo)]
  
  df_i <- dt_unicos[tiempo == f_inicio, .(id_trabajador, sector_origen = r34)]
  df_f <- dt_unicos[tiempo == f_final, .(id_trabajador, sector_destino = r34)]
  
  transiciones <- merge(df_i, df_f, by = "id_trabajador")[sector_origen != sector_destino]
  edges_p <- transiciones[, .(peso = .N), by = .(sector_origen, sector_destino)]
  
  codigos <- unique(c(edges_p$sector_origen, edges_p$sector_destino))
  nodos_base <- data.table(codigo = codigos)
  
  g_dirigido <- graph_from_data_frame(d = edges_p, vertices = nodos_base, directed = TRUE)
  
  # --- CÁLCULO DE MÉTRICAS NUEVAS Y EXISTENTES ---
  
  total_empleos_inicio <- nrow(df_i) 
  total_migraciones <- sum(E(g_dirigido)$peso) 
  tasa_migracion <- total_migraciones / total_empleos_inicio 
  
  n_nodos <- vcount(g_dirigido)
  n_aristas <- ecount(g_dirigido)
  densidad <- edge_density(g_dirigido)
  
  # Entropía de Shannon
  probabilidades <- E(g_dirigido)$peso / total_migraciones
  entropia_global <- -sum(probabilidades * log2(probabilidades))
  
  # Cálculo de meses transcurridos analizando el formato YYYYMM
  meses_transcurridos <- (floor(f_final / 100) - floor(f_inicio / 100)) * 12 + (f_final %% 100 - f_inicio %% 100)
  
  resultado <- data.table(
    Clasificacion       = clasificacion, 
    Periodo             = nombre_periodo,
    Total_Empleos       = total_empleos_inicio,
    Total_Migraciones   = total_migraciones,
    Tasa_Migracion      = tasa_migracion,
    Nodos               = n_nodos,
    Aristas             = n_aristas,
    Densidad            = round(densidad, 4),
    Entropia_Global     = round(entropia_global, 3),
    Meses_Transcurridos = meses_transcurridos
  )
  
  return(resultado)
}

# ==============================================================================
# 4. EJECUCIÓN VECTORIZADA (BATCH PROCESSING)
# ==============================================================================

cat(sprintf("Procesando las %d ventanas temporales. Por favor espere...\n", nrow(periodos_analisis)))

lista_resultados <- lapply(1:nrow(periodos_analisis), function(i) {
  calcular_metricas_red(
    f_inicio       = periodos_analisis$inicio[i],
    f_final        = periodos_analisis$fin[i],
    nombre_periodo = periodos_analisis$nombre[i],
    clasificacion  = periodos_analisis$tipo_periodo[i]
  )
})

tabla_resumen_topologia <- rbindlist(lista_resultados)

fwrite(tabla_resumen_topologia, "D:/Ciencia de Datos/Curso-E520-2026C1/TrabajoFinal/Tabla_Metricas_Topologia_Extendida.csv")

# ==============================================================================
# 5. VISUALIZACIÓN PROFESIONAL DE LAS MÉTRICAS (Librería gt)
# ==============================================================================
library(gt)
library(dplyr)

tabla_visual_metricas <- tabla_resumen_topologia %>%
  # Excluimos las columnas que indicaste antes para la versión visual
  select(-Total_Empleos, -Nodos, -Aristas) %>% 
  group_by(Clasificacion) %>% 
  gt() %>%
  
  tab_header(
    title = md("**Evolución de Métricas de Transición Laboral**"),
    subtitle = md("Comparativa de la red de asalariados: Crisis Históricas (Fuente: MLER)")
  ) %>%
  
  cols_label(
    Periodo = "Período Analizado",
    Total_Migraciones = "Total Migraciones",
    Tasa_Migracion = "% Migración",
    Densidad = "Densidad",
    Entropia_Global = "Entropía Global",
    Meses_Transcurridos = "Meses Transcurridos"
  ) %>%
  
  fmt_number(
    columns = Total_Migraciones,
    decimals = 0,
    sep_mark = "."
  ) %>%
  
  fmt_percent(
    columns = Tasa_Migracion,
    decimals = 2,
    dec_mark = ",",
    sep_mark = "."
  ) %>%
  
  fmt_number(
    columns = Densidad,
    decimals = 4
  ) %>%
  
  fmt_number(
    columns = Entropia_Global,
    decimals = 3
  ) %>%
  
  # Formato para la columna de meses
  fmt_number(
    columns = Meses_Transcurridos,
    decimals = 0
  ) %>%
  
  tab_footnote(
    footnote = "El % de migración se calculó como el total de migraciones / empleos al inicio del período.",
    locations = cells_column_labels(columns = Tasa_Migracion)
  ) %>%
  
  tab_options(
    table.font.names = "sans-serif",
    heading.title.font.size = px(30),
    heading.subtitle.font.size = px(14),
    
    row_group.background.color = "#34495E", 
    row_group.font.weight = "bold",
    
    column_labels.font.weight = "bold",
    column_labels.background.color = "#2C3E50", 
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    data_row.padding = px(8)
  ) %>%
  
  tab_style(
    style = cell_text(color = "white"),
    locations = cells_column_labels()
  ) %>%
  
  cols_align(
    align = "center",
    columns = c(Total_Migraciones, Tasa_Migracion, Densidad, Entropia_Global, Meses_Transcurridos)
  ) %>%
  cols_align(
    align = "left",
    columns = Periodo
  )

tabla_visual_metricas

gtsave(tabla_visual_metricas, "D:/Ciencia de Datos/Curso-E520-2026C1/TrabajoFinal/Tabla_Visual_Metricas_Extendida.png", vwidth = 1200)

rm(datos, lista_resultados)
gc()