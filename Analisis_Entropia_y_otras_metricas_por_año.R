# ==============================================================================
# TRABAJO FINAL - CIENCIA DE DATOS (CURSO E520)
# Script Independiente: Métricas Topológicas Globales (Crisis vs. Expansión)
# ==============================================================================

library(data.table)
library(igraph)

# ==============================================================================
# 1. CONFIGURACIÓN PARAMÉTRICA DE PERÍODOS
# ==============================================================================

ruta_archivo <- "D:/Ciencia de Datos/Curso-E520-2026C1/TrabajoFinal/MLER.csv"

# Definimos todos los períodos (Crisis agudas y Años Buenos) en un solo data.table
periodos_analisis <- data.table(
  inicio = c(
    # Crisis (Último año de cada una)
    200109, 201812,
    # Años buenos
    200209, 200409, 200509, 200609, 200709,
    201009, 201109, 201209, 201409, 201509, 201609
  ),
  fin = c(
    # Fines de Crisis
    200209, 201912,
    # Fines de Años buenos
    200409, 200509, 200609, 200709, 200809,
    201109, 201209, 201309, 201509, 201609, 201709
  ),
  nombre = c(
    # Nombres Crisis
    "2001 - 2002 (Último Año Conv.)", 
    "2018 - 2019 (Último Año Cambiaria)", 
    # Nombres Años Buenos
    "2003 - 2004", 
    "2004 - 2005", 
    "2005 - 2006", 
    "2006 - 2007", 
    "2007 - 2008", 
    "2010 - 2011", 
    "2011 - 2012", 
    "2012 - 2013", 
    "2014 - 2015", 
    "2015 - 2016", 
    "2016 - 2017"
  )
)

# Ordenamos cronológicamente por fecha de inicio para que la tabla final quede prolija
setorder(periodos_analisis, inicio, fin)

# Creamos una columna auxiliar para distinguir el tipo de período en la tabla visual
periodos_analisis[, tipo_periodo := fifelse(like(nombre, "Conv|Subprime|Cambiaria"), "Crisis", "Expansión (Períodos de un Año)")]

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
  
  total_empleos_inicio <- nrow(df_i) # N° de trabajadores al inicio del período
  fuerza_global <- sum(E(g_dirigido)$peso) # N° total de trabajadores que migraron
  tasa_migracion <- fuerza_global / total_empleos_inicio # Ratio de migración
  
  n_nodos <- vcount(g_dirigido)
  n_aristas <- ecount(g_dirigido)
  densidad <- edge_density(g_dirigido)
  
  # Entropía de Shannon
  probabilidades <- E(g_dirigido)$peso / fuerza_global
  entropia_global <- -sum(probabilidades * log2(probabilidades))
  
  resultado <- data.table(
    Clasificacion   = clasificacion, 
    Periodo         = nombre_periodo,
    Total_Empleos   = total_empleos_inicio,
    Fuerza_Global   = fuerza_global,
    Tasa_Migracion  = tasa_migracion,
    Nodos           = n_nodos,
    Aristas         = n_aristas,
    Densidad        = round(densidad, 4),
    Entropia_Global = round(entropia_global, 3)
  )
  
  return(resultado)
}

# ==============================================================================
# 4. EJECUCIÓN VECTORIZADA (BATCH PROCESSING) Y PROMEDIOS
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

# ---> NUEVO: Cálculo de promedios impresos en consola (sin empleos, nodos ni aristas)
cat("\n=== PROMEDIOS POR FASE ECONÓMICA ===\n")
promedios_consolidados <- tabla_resumen_topologia[, lapply(.SD, mean, na.rm = TRUE), 
                                                  by = Clasificacion, 
                                                  .SDcols = c("Fuerza_Global", "Tasa_Migracion", "Densidad", "Entropia_Global")]
print(promedios_consolidados)
cat("======================================\n\n")

# Guardamos la tabla completa (con todas las columnas) en el CSV por si a futuro las necesitas
fwrite(tabla_resumen_topologia, "D:/Ciencia de Datos/Curso-E520-2026C1/TrabajoFinal/Tabla_Metricas_Topologia_Extendida.csv")

# ==============================================================================
# 5. VISUALIZACIÓN PROFESIONAL DE LAS MÉTRICAS (Librería gt)
# ==============================================================================
library(gt)
library(dplyr)

tabla_visual_metricas <- tabla_resumen_topologia %>%
  # Excluimos las columnas que no queremos mostrar antes de armar la tabla
  select(-Total_Empleos, -Nodos, -Aristas) %>% 
  group_by(Clasificacion) %>% 
  gt() %>%
  
  tab_header(
    title = md("**Evolución de Métricas de Transición Laboral**"),
    subtitle = md("Comparativa de la red de asalariados (Fuente: MLER)")
  ) %>%
  
  cols_label(
    Periodo = "Período Analizado",
    Fuerza_Global = "Total Migraciones",
    Tasa_Migracion = "% Migración",
    Densidad = "Densidad",
    Entropia_Global = "Entropía Global"
  ) %>%
  
  fmt_number(
    columns = Fuerza_Global,
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
  
  summary_rows(
    groups = everything(),
    columns = Fuerza_Global,
    fns = list("Promedio" = ~mean(., na.rm = TRUE)),
    fmt = ~ fmt_number(., decimals = 0, sep_mark = ".")
  ) %>%
  summary_rows(
    groups = everything(),
    columns = Tasa_Migracion,
    fns = list("Promedio" = ~mean(., na.rm = TRUE)),
    fmt = ~ fmt_percent(., decimals = 2, dec_mark = ",", sep_mark = ".")
  ) %>%
  summary_rows(
    groups = everything(),
    columns = c(Densidad, Entropia_Global),
    fns = list("Promedio" = ~mean(., na.rm = TRUE)),
    fmt = ~ fmt_number(., decimals = 3)
  ) %>%
  
  cols_move(
    columns = c(Fuerza_Global, Tasa_Migracion),
    after = Periodo
  ) %>%
  
  # ---> NUEVO: Nota al pie explicativa para la Tasa de Migración
  tab_footnote(
    footnote = "El % de migración se calculó como el total de migraciones / empleos al inicio del período.",
    locations = cells_column_labels(columns = Tasa_Migracion)
  ) %>%
  
  data_color(
    columns = Entropia_Global,
    palette = c("white", "#E74C3C"), 
    alpha = 0.5
  ) %>%
  data_color(
    columns = Tasa_Migracion,
    palette = c("white", "#27AE60"), 
    alpha = 0.5
  ) %>%
  data_color(
    columns = Densidad,
    palette = c("white", "#2980B9"), 
    alpha = 0.5
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
  
  tab_style(
    style = list(
      cell_text(weight = "bold", style = "italic"),
      cell_fill(color = "#F4F6F7")
    ),
    locations = cells_summary()
  ) %>%
  
  cols_align(
    align = "center",
    columns = c(Fuerza_Global, Tasa_Migracion, Densidad, Entropia_Global)
  ) %>%
  cols_align(
    align = "left",
    columns = Periodo
  )

tabla_visual_metricas

gtsave(tabla_visual_metricas, "D:/Ciencia de Datos/Curso-E520-2026C1/TrabajoFinal/Tabla_Visual_Metricas_Extendida.png", vwidth = 1200)

rm(datos, lista_resultados, promedios_consolidados)
gc()
