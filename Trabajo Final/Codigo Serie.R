#
library(data.table)
library(ggplot2)
library(patchwork)
library(writexl)
library(scales)
library(lubridate)
library(zoo)

# ─── Configuración ────────────────────────────────────────────────────────────

DATA_PATH  <- r"(C:\Users\jvess\Documents\UBA\Economicas\Datos\Final\Data\MLER.csv)"
OUTPUT_DIR <- dirname(rstudioapi::getSourceEditorContext()$path)
# Si corrés fuera de RStudio: OUTPUT_DIR <- "C:/ruta/de/tu/elección"

# ─── 2. Leer MLER ─────────────────────────────────────────────────────────────



dt <- fread(
  DATA_PATH,
  select     = c("tiempo", "pondera", "rem_tot"),
  colClasses = list(integer = "tiempo", numeric = c("pondera","rem_tot"))
)

# Cortar antes de la pandemia
dt <- dt[tiempo <= 202002]



# Filtrar rem_tot > 0 para estadísticos de salario
dt_sal <- dt[rem_tot > 0]

# ─── 3. Agregar empleo mensual ────────────────────────────────────────────────

empleo <- dt[, .(puestos = sum(pondera, na.rm = TRUE)), by = tiempo]
empleo[, puestos_miles := puestos / 1000]
empleo[, anio := as.integer(substr(as.character(tiempo), 1, 4))]
empleo[, mes  := as.integer(substr(as.character(tiempo), 5, 6))]
empleo[, fecha := as.Date(paste(anio, sprintf("%02d", mes), "01", sep = "-"))]
setorder(empleo, fecha)

# ─── 4. Agregar salarios mensuales ────────────────────────────────────────────



# ─── 5. Detección de crisis de empleo ────────────────────────────────────────
# Una "gran caída" = caída sostenida >= 12 meses desde un pico local,
# con magnitud >= 3%. Pico y valle definidos en la serie completa.

detectar_crisis <- function(serie_dt, min_caida = -3, ventana_valle = 6) {
  
  y      <- serie_dt$puestos_miles
  fechas <- serie_dt$fecha
  n      <- length(y)
  
  # --- Paso 1: valles locales ---
  es_valle <- logical(n)
  for (i in (ventana_valle + 1L):(n - ventana_valle))
    if (y[i] == min(y[(i - ventana_valle):(i + ventana_valle)])) es_valle[i] <- TRUE
  valle_idxs <- which(es_valle)
  
  # --- Paso 2: valle final (crisis truncada al corte de serie) ---
  inicio_final <- if (length(valle_idxs) > 0L) tail(valle_idxs, 1L) + 1L else 1L
  if (inicio_final <= n) {
    vf <- inicio_final - 1L + which.min(y[inicio_final:n])
    if (!(vf %in% valle_idxs)) valle_idxs <- sort(c(valle_idxs, vf))
  }
  
  # --- Paso 3: construir episodios crudos ---
  episodios  <- list()
  prev_idx   <- 1L
  
  for (vi in valle_idxs) {
    pi        <- prev_idx - 1L + which.max(y[prev_idx:vi])
    pico_val  <- y[pi]
    valle_val <- y[vi]
    caida_pct <- (valle_val - pico_val) / pico_val * 100
    
    if (caida_pct <= min_caida) {
      episodios[[length(episodios) + 1L]] <- list(
        pi = pi, vi = vi, pico_val = pico_val, valle_val = valle_val
      )
    }
    prev_idx <- vi
  }
  
  if (length(episodios) == 0L) return(NULL)
  
  # --- Paso 4: fusionar episodios del mismo ciclo bajista ---
  # Dos episodios consecutivos pertenecen al mismo ciclo si entre el valle
  # del primero y el pico del segundo el empleo nunca supera el pico original.
  fusionados <- list(episodios[[1]])
  
  for (j in seq_along(episodios)[-1]) {
    ep_prev <- fusionados[[length(fusionados)]]
    ep_curr <- episodios[[j]]
    # ¿El empleo entre v_prev y p_curr supera el pico original?
    entre    <- y[ep_prev$vi:ep_curr$pi]
    if (max(entre) < ep_prev$pico_val) {
      # Mismo ciclo: fusionar quedándose con el pico original y el valle más profundo
      nuevo_vi  <- if (ep_curr$valle_val < ep_prev$valle_val) ep_curr$vi else ep_prev$vi
      fusionados[[length(fusionados)]] <- list(
        pi        = ep_prev$pi,
        vi        = nuevo_vi,
        pico_val  = ep_prev$pico_val,
        valle_val = y[nuevo_vi]
      )
    } else {
      fusionados[[length(fusionados) + 1L]] <- ep_curr
    }
  }
  
  # --- Paso 5: calcular recuperación y armar tabla final ---
  crisis_list <- list()
  
  for (ep in fusionados) {
    pi        <- ep$pi
    vi        <- ep$vi
    pico_val  <- ep$pico_val
    valle_val <- ep$valle_val
    duracion  <- vi - pi
    caida_pct <- (valle_val - pico_val) / pico_val * 100
    
    k <- vi + 1L
    while (k <= n && y[k] < pico_val) k <- k + 1L
    
    crisis_list[[length(crisis_list) + 1L]] <- list(
      pico_fecha         = fechas[pi],
      pico_val           = pico_val,
      valle_fecha        = fechas[vi],
      valle_val          = valle_val,
      caida_pct          = caida_pct,
      duracion_meses     = duracion,
      rec_fecha          = if (k <= n) fechas[k] else as.Date(NA),
      rec_val            = if (k <= n) y[k] else NA_real_,
      meses_recuperacion = if (k <= n) k - vi else NA_integer_,
      pico_idx           = pi,
      valle_idx          = vi
    )
  }
  
  rbindlist(lapply(crisis_list, as.data.table))
}

crisis <- detectar_crisis(empleo)
message("\n── Grandes caídas detectadas ──")
print(crisis[, .(pico_fecha, pico_val = round(pico_val,0),
                 valle_fecha, valle_val = round(valle_val,0),
                 caida_pct = round(caida_pct,1),
                 duracion_meses, rec_fecha, meses_recuperacion)])

# ─── 6. Gráfico 1: Empleo con crisis anotadas ────────────────────────────────

# Etiquetas de crisis para el gráfico
crisis_labels <- data.table(
  fecha = as.Date(c("1998-12-01", "2018-03-01")),
  label = c("Crisis\nConvertibilidad", "Crisis 2018-\nCOVID")
)

y_max <- max(empleo$puestos_miles) * 1.04

ggplot(empleo, aes(x = fecha, y = puestos_miles)) +
  
  # Sombreado de caídas
  {
    if (!is.null(crisis)) {
      mapply(function(p_f, v_f) {
        annotate("rect",
                 xmin  = p_f, xmax  = v_f,
                 ymin  = -Inf, ymax  = Inf,
                 fill  = "#e74c3c", alpha = 0.07
        )
      }, crisis$pico_fecha, crisis$valle_fecha, SIMPLIFY = FALSE)
    }
  } +
  
  # Serie mensual
  geom_line(color = "#1a6fc4", linewidth = 1.4) +
  
  # Picos
  {
    if (!is.null(crisis)) {
      geom_point(
        data = data.table(fecha = crisis$pico_fecha, puestos_miles = crisis$pico_val),
        aes(x = fecha, y = puestos_miles),
        color = "#27ae60", size = 5, shape = 17
      )
    }
  } +
  
  # Valles
  {
    if (!is.null(crisis)) {
      geom_point(
        data = data.table(fecha = crisis$valle_fecha, puestos_miles = crisis$valle_val),
        aes(x = fecha, y = puestos_miles),
        color = "#e74c3c", size = 5, shape = 25, fill = "#e74c3c"
      )
    }
  } +
  
  
  # Líneas horizontales de pico (nivel a recuperar)
  {
    if (!is.null(crisis)) {
      mapply(function(p_f, v_f, r_f, p_v) {
        fin <- if (!is.na(r_f)) r_f else max(empleo$fecha)
        annotate("segment",
                 x = p_f, xend = fin,
                 y = p_v, yend = p_v,
                 linetype = "dashed", color = "#27ae60", alpha = 0.6, linewidth = 0.5
        )
      }, crisis$pico_fecha, crisis$valle_fecha, crisis$rec_fecha, crisis$pico_val,
      SIMPLIFY = FALSE)
    }
  } +
  
  # Label: Pico
  {
    if (!is.null(crisis)) {
      geom_label(
        data = data.table(
          fecha         = crisis$pico_fecha,
          puestos_miles = crisis$pico_val,
          label         = paste0("Pico\n", format(crisis$pico_fecha, "%b %Y"))
        ),
        aes(label = label),
        vjust = -0.1, hjust = 0.5,
        size = 6, color = "#1a5e34", fill = "#eafaf1",
        label.size = 0.6, label.padding = unit(0.25, "lines"),
        fontface = "bold"
      )
    }
  } +
  
  # Label: Valle
  {
    if (!is.null(crisis)) {
      geom_label(
        data = data.table(
          fecha         = crisis$valle_fecha,
          puestos_miles = crisis$valle_val,
          label         = paste0("Valle\n", format(crisis$valle_fecha, "%b %Y"))
        ),
        aes(label = label),
        vjust = 1, hjust = 0.8,
        size = 6, color = "#922b21", fill = "#fdedec",
        label.size = 0.6, label.padding = unit(0.25, "lines"),
        fontface = "bold"
      )
    }
  } +
  

  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",", suffix = "k"),
    expand = expansion(mult = c(0.10, 0.14))
  ) +
  labs(
    title    = "Empleo registrado en Argentina — Serie mensual",
    subtitle = "Puestos de trabajo (miles) · ▲ Pico  ▼ Valle",
    caption  = "Fuente: MLER (Ministerio de Trabajo). Elaboración propia.",
    x = NULL, y = "Puestos de trabajo (miles)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title    = element_text(face = "bold", size = 28, family = "serif"),
    plot.subtitle = element_text(color = "#555", size = 18),
    plot.caption  = element_text(color = "#888", size = 11),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "#ececec"),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 16),
    axis.text.y  = element_text(size = 15),
    axis.title.y = element_text(size = 16),
    plot.margin  = margin(16, 20, 12, 12)
    
  )

ggsave("plot.png", width = 16, height = 9, dpi = 150)

# ─── 7. Exportar MLER filtrada para meses clave ───────────────────────────────
# Lee el CSV completo (todas las columnas) y filtra solo los meses de
# pico, valle y recuperación detectados por detectar_crisis().
# Agrega dos columnas: crisis_nombre y mes_rol (pico / valle / recuperacion).

message("Construyendo mapa de meses clave...")

# Armar tabla de referencia: tiempo → crisis + rol
roles <- rbindlist(lapply(seq_len(nrow(crisis)), function(i) {
  cr <- crisis[i]
  
  # Nombre de cada crisis según su pico
  nombre <- fcase(
    year(cr$pico_fecha) %in% 1998:1999, "Convertibilidad",
    year(cr$pico_fecha) %in% 2008:2009, "2009",
    year(cr$pico_fecha) %in% 2018:2019, "2018-2019",
    default = paste0("Crisis ", year(cr$pico_fecha))
  )
  
  filas <- list(
    data.table(
      tiempo        = as.integer(format(cr$pico_fecha,  "%Y%m")),
      crisis_nombre = nombre,
      mes_rol       = "pico"
    ),
    data.table(
      tiempo        = as.integer(format(cr$valle_fecha, "%Y%m")),
      crisis_nombre = nombre,
      mes_rol       = "valle"
    )
  )
  
  if (!is.na(cr$rec_fecha)) {
    filas[[3]] <- data.table(
      tiempo        = as.integer(format(cr$rec_fecha, "%Y%m")),
      crisis_nombre = nombre,
      mes_rol       = "recuperacion"
    )
  }
  
  rbindlist(filas)
}))

meses_clave <- sort(unique(roles$tiempo))
message(sprintf("Meses clave: %s", paste(meses_clave, collapse = ", ")))

# Releer el CSV completo con TODAS las columnas, filtrando al vuelo en chunks
message("Leyendo MLER completa (todas las columnas) para meses clave...")

chunks     <- list()
chunk_size <- 500000L
conn       <- file(DATA_PATH, "r")
header     <- readLines(conn, n = 1L)   # leer encabezado
col_names  <- strsplit(header, ",")[[1]]
close(conn)

mler_clave <- fread(
  DATA_PATH,
  select = NULL          # todas las columnas
)[tiempo %in% meses_clave]

# Adjuntar nombre de crisis y rol del mes
mler_clave <- merge(mler_clave, roles, by = "tiempo", all.x = TRUE)

# Reordenar: identificadores primero, luego rol, luego resto
cols_meta  <- c("crisis_nombre", "mes_rol", "tiempo")
cols_resto <- setdiff(names(mler_clave), cols_meta)
setcolorder(mler_clave, c(cols_meta, cols_resto))
setorder(mler_clave, crisis_nombre, mes_rol, tiempo)

message(sprintf("Filas en MLER filtrada: %s", format(nrow(mler_clave), big.mark = ".")))

# Exportar como CSV (el volumen supera el límite de 1M filas de Excel)
csv_clave <- file.path(dirname(DATA_PATH), "mler_meses_clave.csv")
fwrite(mler_clave, file = csv_clave)
message("CSV exportado: ", csv_clave)

#fin