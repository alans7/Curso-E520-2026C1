# ==============================================================================
# TRABAJO FINAL - CIENCIA DE DATOS (CURSO E520)
# Script Independiente: Evolución Histórica de Sectores ABSORBENTES
# ==============================================================================

library(data.table)
library(igraph)
library(ggplot2)
library(stringr)
library(readxl)
library(ggrepel)

# ==============================================================================
# 1. CONFIGURACIÓN Y LECTURA OPTIMIZADA DE DATOS
# ==============================================================================
ruta_archivo <- "D:/Ciencia de Datos/Curso-E520-2026C1/TrabajoFinal/MLER.csv"
ruta_excel   <- "D:/Ciencia de Datos/Curso-E520-2026C1/TrabajoFinal/diccionarior34.xlsx"

fechas_crisis <- c(199812, 200209,  # Convertibilidad
                   200810, 200908,  # Subprime
                   201803, 201912)  # Cambiaria/Local

columnas_necesarias <- c("id_trabajador", "tiempo", "rem_tot", "r34")
datos_raw <- fread(ruta_archivo, select = columnas_necesarias)

datos <- datos_raw[tiempo %in% fechas_crisis]
rm(datos_raw)
gc()

diccionario_excel <- read_excel(ruta_excel)
setDT(diccionario_excel)
setnames(diccionario_excel, 
         old = c("r34", "descripcion"), 
         new = c("codigo", "descripcion"))

# ==============================================================================
# 2. MOTOR DE CÁLCULO DE TRANSICIONES (ENFOQUE ABSORBENTES)
# ==============================================================================
analizar_ranking_absorbentes <- function(f_inicio, f_final, nombre_periodo) {
  
  dt_sub <- datos[tiempo %in% c(f_inicio, f_final)]
  setorder(dt_sub, id_trabajador, tiempo, -rem_tot)
  dt_unicos <- dt_sub[, .SD[1], by = .(id_trabajador, tiempo)]
  
  df_i <- dt_unicos[tiempo == f_inicio, .(id_trabajador, sector_origen = r34)]
  df_f <- dt_unicos[tiempo == f_final, .(id_trabajador, sector_destino = r34)]
  
  transiciones <- merge(df_i, df_f, by = "id_trabajador")[sector_origen != sector_destino]
  edges_p <- transiciones[, .(peso = .N), by = .(sector_origen, sector_destino)]
  
  codigos <- unique(c(edges_p$sector_origen, edges_p$sector_destino))
  nodos_base <- data.table(codigo = codigos)
  nodos_p <- merge(nodos_base, diccionario_excel, by = "codigo", all.x = TRUE)
  nodos_p[is.na(descripcion), descripcion := paste("Sector no especificado - Código", codigo)]
  
  g <- graph_from_data_frame(d = edges_p, vertices = nodos_p, directed = TRUE)
  
  f_in  <- strength(g, mode = "in", weights = E(g)$peso)
  f_out <- strength(g, mode = "out", weights = E(g)$peso)
  f_neto <- f_in - f_out
  
  resultado <- data.table(
    codigo        = V(g)$name,
    descripcion   = V(g)$descripcion,
    flujo_neto    = f_neto,
    rank_atractor = rank(-f_neto, ties.method = "first"), 
    Periodo       = nombre_periodo
  )
  
  return(resultado)
}

# ==============================================================================
# 3. PROCESAMIENTO E INTEGRACIÓN HISTÓRICA
# ==============================================================================
res_2001     <- analizar_ranking_absorbentes(199812, 200209, "Fin de la Convertibilidad\n(1998-2002)")
res_subprime <- analizar_ranking_absorbentes(200810, 200908, "Crisis Subprime\n(2008-2009)")
res_2018     <- analizar_ranking_absorbentes(201803, 201912, "Crisis Cambiaria/Local\n(2018-2019)")

historico_rankings <- rbind(res_2001, res_subprime, res_2018)

# Limpieza y abreviación (Sin truncar, permitiendo que se lea todo)
historico_rankings[, desc_limpia := str_replace_all(descripcion, regex("Fabricación de|Elaboración de", ignore_case = TRUE), "Fab.")]
historico_rankings[, desc_limpia := str_replace_all(desc_limpia, regex("Servicios de|Actividades de", ignore_case = TRUE), "Serv.")]
historico_rankings[, desc_limpia := str_replace_all(desc_limpia, regex("Comercio al por mayor y al por menor", ignore_case = TRUE), "Comercio")]
historico_rankings[, desc_limpia := str_replace_all(desc_limpia, regex("Administración Pública", ignore_case = TRUE), "Admin. Pub.")]
historico_rankings[, desc_limpia := str_replace_all(desc_limpia, " y ", " & ")]

# Top 5 dinámico de Atractores/Absorbentes
codigos_top5_historico <- historico_rankings[rank_atractor <= 5, unique(codigo)]
plot_evolucion <- historico_rankings[codigo %in% codigos_top5_historico]

# Wrap de texto: acomoda en renglones sin cortar con puntos suspensivos
plot_evolucion[, desc_corta := str_wrap(desc_limpia, width = 25)]
plot_evolucion[, Periodo := factor(Periodo, 
                                   levels = c("Fin de la Convertibilidad\n(1998-2002)", 
                                              "Crisis Subprime\n(2008-2009)", 
                                              "Crisis Cambiaria/Local\n(2018-2019)"))]

# Distribución Equidistante
plot_evolucion[, orden_visual := rank(rank_atractor), by = Periodo]

niveles_periodo <- levels(plot_evolucion$Periodo)
plot_evolucion[, etiqueta_izq := fcase(Periodo == niveles_periodo[1], desc_corta, default = NA_character_)]
plot_evolucion[, etiqueta_der := fcase(Periodo == niveles_periodo[3], desc_corta, default = NA_character_)]

# Paleta de alto contraste
paleta_proyector <- c(
  "#1F77B4", "#D62728", "#2CA02C", "#FF7F0E", "#9467BD", 
  "#8C564B", "#E377C2", "#17BECF", "#BCBD22", "#393B79", 
  "#8C6D31", "#843C39"
)

# ==============================================================================
# 4. RENDERIZADO DEL BUMP CHART
# ==============================================================================
grafico_ranking_absorbentes <- ggplot(plot_evolucion, aes(x = Periodo, y = orden_visual, group = desc_corta, color = desc_corta)) +
  
  geom_line(linewidth = 1.5, alpha = 0.85) +
  geom_point(size = 9) +
  
  geom_text(aes(label = rank_atractor), color = "white", fontface = "bold", size = 4) +
  
  # Etiquetas laterales
  geom_text_repel(aes(label = etiqueta_izq), direction = "y", hjust = 1, nudge_x = -0.15, size = 4, fontface = "bold", lineheight = 0.85, segment.color = NA) +
  geom_text_repel(aes(label = etiqueta_der), direction = "y", hjust = 0, nudge_x = 0.15, size = 4, fontface = "bold", lineheight = 0.85, segment.color = NA) +
  
  scale_y_reverse() +
  scale_color_manual(values = paleta_proyector) +
  
  labs(
    title = "Evolución de los Principales Sectores Atractores de Empleo",
    subtitle = "Trayectoria del ranking de los sectores que integraron el Top 5 de atracción neta en alguna de las crisis.",
    x = NULL,
    y = NULL,
    caption = "Fuente: Elaboración propia en base a la Muestra Longitudinal de Empleo Registrado (MLER)"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 24, color = "#2C3E50"), 
    plot.subtitle = element_text(size = 12, color = "#34495E", margin = margin(b = 20)),
    legend.position = "none",
    axis.text.x = element_text(face = "bold", size = 13, color = "#2C3E50"),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D5D8DC", linewidth = 0.8),
    # Aumentamos los márgenes laterales (izquierdo y derecho) a 180 para que entre el texto completo
    plot.margin = margin(20, 180, 20, 180) 
  )

print(grafico_ranking_absorbentes)
ggsave("Historico_Absorbentes_Legible.png", plot = grafico_ranking_absorbentes, width = 20, height = 8.5, dpi = 300)

rm(datos, diccionario_excel, res_2001, res_subprime, res_2018)
gc()