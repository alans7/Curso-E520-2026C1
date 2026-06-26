# ==============================================================================
# TRABAJO FINAL - CIENCIA DE DATOS (CURSO E520)
# Análisis de Transiciones Laborales (MLER) mediante Grafos
# ==============================================================================

# 1. Instalación y Carga de Librerías
# install.packages(c("data.table", "igraph", "readxl", "ggraph", "ggplot2", "dplyr", "stringr"))
library(data.table)
library(igraph)
library(readxl)
library(ggraph)
library(ggplot2)
library(dplyr)
library(stringr)

# 2. Definición de Rutas y Parámetros
ruta_archivo <- "D:/Ciencia de Datos/Curso-E520-2026C1/TrabajoFinal/MLER.csv"
ruta_excel   <- "D:/Ciencia de Datos/Curso-E520-2026C1/TrabajoFinal/diccionarior34.xlsx"

fecha_inicio <- 201803 # AAAAMM
fecha_final  <- 201912 # AAAAMM

# ==============================================================================
# FASE 1: PROCESAMIENTO DE LA BASE PRINCIPAL (MLER)
# ==============================================================================

# 3. Lectura eficiente
columnas_necesarias <- c("id_trabajador", "tiempo", "rem_tot", "r34")
datos <- fread(ruta_archivo, select = columnas_necesarias)

# 4. Filtrado por fechas
datos_filtrados <- datos[tiempo %in% c(fecha_inicio, fecha_final)]

# 5. Tratamiento de pluriempleo (duplicados)
setorder(datos_filtrados, id_trabajador, tiempo, -rem_tot)
datos_unicos <- datos_filtrados[, .SD[1], by = .(id_trabajador, tiempo)]

# ==============================================================================
# FASE 2: CONSTRUCCIÓN DE LAS ARISTAS (EDGES)
# ==============================================================================

# 6. Separación en origen y destino
df_inicio <- datos_unicos[tiempo == fecha_inicio, .(id_trabajador, sector_origen = r34)]
df_final  <- datos_unicos[tiempo == fecha_final, .(id_trabajador, sector_destino = r34)]

# 7. Cruce para identificar transiciones
transiciones <- merge(df_inicio, df_final, by = "id_trabajador")

# 8. Agrupación para calcular el peso de las aristas
edges <- transiciones[, .(peso = .N), by = .(sector_origen, sector_destino)]

# ==============================================================================
# FASE 3: CONSTRUCCIÓN DE LOS NODOS (DICCIONARIO)
# ==============================================================================

# 9. Identificar códigos únicos en el grafo
codigos_unicos <- unique(c(edges$sector_origen, edges$sector_destino))
nodos_base <- data.table(codigo = codigos_unicos)

# 10. Lectura del Excel de descriptores
diccionario_excel <- read_excel(ruta_excel)
setDT(diccionario_excel)

# 11. Estandarización de nombres
setnames(diccionario_excel, 
         old = c("r34", "descripcion"), 
         new = c("codigo", "descripcion"))

# 12. Cruce de nodos con descripciones
nodos_completos <- merge(nodos_base, diccionario_excel, by = "codigo", all.x = TRUE)

# 13. Manejo de valores faltantes
nodos_completos[is.na(descripcion), descripcion := paste("Sector no especificado - Código", codigo)]

# ==============================================================================
# FASE 4: ENSAMBLAJE DEL GRAFO Y LIMPIEZA
# ==============================================================================

# 14. Creación del objeto igraph
grafo_laboral <- graph_from_data_frame(d = edges, 
                                       vertices = nodos_completos, 
                                       directed = TRUE)

# 15. Limpieza de memoria RAM
rm(datos, datos_filtrados, datos_unicos, df_inicio, df_final, transiciones, nodos_base, diccionario_excel)
gc()

# ==============================================================================
# FASE 5: VISUALIZACIÓN ESTÁTICA PROFESIONAL (MODO PROYECTOR) - TOP 5
# ==============================================================================

# 1. Extracción y Formateo Dinámico de Fechas para el Título
generar_texto_fecha <- function(yyyymm) {
  anio <- substr(as.character(yyyymm), 1, 4)
  mes_num <- as.numeric(substr(as.character(yyyymm), 5, 6))
  meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  return(paste(meses_es[mes_num], "de", anio))
}

texto_inicio <- generar_texto_fecha(fecha_inicio)
texto_final  <- generar_texto_fecha(fecha_final)
titulo_dinamico <- paste("Estructura de Atracción y Expulsión Laboral entre", texto_inicio, "y", texto_final)

# 2. Sincronizar Atributos (Flujos, Rankings y Valor Absoluto)
flujo_in  <- strength(grafo_laboral, mode = "in", weights = E(grafo_laboral)$peso)
flujo_out <- strength(grafo_laboral, mode = "out", weights = E(grafo_laboral)$peso)
flujo_neto <- flujo_in - flujo_out

V(grafo_laboral)$flujo_neto <- flujo_neto
V(grafo_laboral)$rank_atractor <- rank(-flujo_neto, ties.method = "first")
V(grafo_laboral)$rank_expulsor <- rank(flujo_neto, ties.method = "first")

# NUEVO: Calculamos el valor absoluto del flujo neto para usarlo como tamaño
V(grafo_laboral)$abs_flujo_neto <- abs(flujo_neto)

# 3. Coordenadas Fijas del Layout (Espiral Áurea)
n_nodos <- vcount(grafo_laboral)
ranking_posicion <- rank(-flujo_neto, ties.method = "first")
radio <- sqrt((ranking_posicion - 1) / (n_nodos - 1))
angulo <- pi * (1 + sqrt(5)) * (ranking_posicion - 1)

layout_manual <- data.frame(
  x = radio * cos(angulo),
  y = radio * sin(angulo)
)

# 4. Construcción de la Leyenda Lateral (TOP 5)
datos_nodos <- data.frame(
  desc  = V(grafo_laboral)$descripcion,
  flujo = V(grafo_laboral)$flujo_neto,
  r_a   = V(grafo_laboral)$rank_atractor,
  r_e   = V(grafo_laboral)$rank_expulsor
)

datos_nodos <- datos_nodos %>%
  mutate(
    texto_leyenda = case_when(
      r_a <= 5 ~ sprintf("[+ %s] %s", r_a, str_trunc(desc, 45)),
      r_e <= 5 ~ sprintf("[- %s] %s", r_e, str_trunc(desc, 45)),
      flujo > 0 ~ "Resto Atractores",
      flujo < 0 ~ "Resto Expulsores",
      TRUE      ~ "Neutro"
    ),
    texto_nodo = case_when(
      r_a <= 5 ~ paste0("+", r_a),
      r_e <= 5 ~ paste0("-", r_e),
      TRUE      ~ ""
    ),
    alpha_nodo = ifelse(texto_nodo == "", 0.35, 0.95)
  )

niveles_atractores <- datos_nodos %>% filter(r_a <= 5) %>% arrange(r_a) %>% pull(texto_leyenda)
niveles_expulsores <- datos_nodos %>% filter(r_e <= 5) %>% arrange(r_e) %>% pull(texto_leyenda)
orden_leyenda      <- c(niveles_atractores, niveles_expulsores, "Resto Atractores", "Resto Expulsores", "Neutro")

V(grafo_laboral)$leyenda    <- factor(datos_nodos$texto_leyenda, levels = orden_leyenda)
V(grafo_laboral)$texto_nodo <- datos_nodos$texto_nodo
V(grafo_laboral)$alpha_nodo <- datos_nodos$alpha_nodo

# 5. Configuración de Paletas de Colores de Alto Contraste (Proyector)
colores_leyenda <- c(
  setNames(rep("#1E8449", 5), niveles_atractores), # Verde Intenso
  setNames(rep("#CB4335", 5), niveles_expulsores), # Rojo Fuerte
  "Resto Atractores" = "#1E8449",                   
  "Resto Expulsores" = "#CB4335",                   
  "Neutro"           = "#95A5A6"                   
)

# 6. Ejecución del Renderizado Gráfico
ggraph(grafo_laboral, layout = layout_manual) +
  
  # Líneas de Transiciones
  geom_edge_link(aes(edge_alpha = peso, edge_width = peso), 
                 color = "#BDC3C7", show.legend = FALSE) +
  scale_edge_width(range = c(0.2, 1.8)) +
  scale_edge_alpha(range = c(0.05, 0.35)) +
  
  # Nodos definidos por el Valor Absoluto del Flujo Neto
  geom_node_point(aes(fill = leyenda, size = abs_flujo_neto, alpha = alpha_nodo), 
                  shape = 21, color = "white", stroke = 1) +
  
  # Texto de nodos más grande para proyector
  geom_node_text(aes(label = texto_nodo), 
                 color = "white", size = 5.5, fontface = "bold", vjust = 0.38) +
  
  # Escala de relleno
  scale_fill_manual(
    values = colores_leyenda, 
    breaks = c(niveles_atractores, niveles_expulsores), 
    name = "Panel de Control (Top 5)"
  ) +
  
  # Escala de tamaño aumentada para proyector (ajustá el max si hace falta)
  scale_size_continuous(range = c(4, 28), guide = "none") +
  scale_alpha_identity() + 
  
  labs(
    title = titulo_dinamico,
    subtitle = "Tamaño del nodo = Magnitud del cambio (Valor absoluto de la atracción o expulsión neta)",
    caption = "Elaborado en base a MLER"
  ) +
  
  # Tema visual adaptado a presentaciones
  theme_graph(background = "white", base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 22, color = "#2C3E50", hjust = 0.5, vjust = 1, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, color = "#34495E", hjust = 0.5, vjust = 1, margin = margin(b = 20)),
    plot.caption = element_text(size = 14, color = "#7F8C8D", face = "italic", hjust = 0.05, vjust = 0),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 16, color = "#2C3E50", margin = margin(b = 10)),
    legend.text = element_text(face = "bold",size = 14, color = "#34495E"),
    legend.key.size = unit(1.2, "cm"),
    legend.key = element_rect(fill = "transparent", color = "transparent"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  guides(fill = guide_legend(override.aes = list(size = 8, alpha = 1, shape = 21, color = "white")))

# ==============================================================================
# EXPORTACIÓN AUTOMÁTICA EN ALTA RESOLUCIÓN
# ==============================================================================

ggsave("Estructura_Laboral_Top5_Magnitud_Proyector_2018.png", width = 20, height = 11, dpi = 300)

