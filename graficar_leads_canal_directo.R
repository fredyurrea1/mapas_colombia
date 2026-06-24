# Graficos de presentacion: leads/cotizaciones del canal directo.
#
# Todo el tratamiento de datos se hace desde el archivo original:
# - Negocios Autos 2025.xlsx
# - departamentos_colombia.shp
#
# En R/RStudio:
# source("graficar_leads_canal_directo.R")

library(sf)
library(dplyr)
library(readxl)
library(readr)
library(ggplot2)
library(stringr)
library(stringi)
library(scales)
library(tidyr)

# -------------------------------------------------------------------------
# 1. Parametros y lectura de archivos originales
# -------------------------------------------------------------------------

archivo_leads <- "Negocios Autos 2025.xlsx"
archivo_departamentos <- "departamentos_colombia.shp"
hoja_leads <- "Negocios de Autos Nuevos 2025"

archivos_requeridos <- c(archivo_leads, archivo_departamentos)
faltantes <- archivos_requeridos[!file.exists(archivos_requeridos)]

if (length(faltantes) > 0) {
  stop(
    paste0(
      "No se encontraron estos archivos en el directorio actual: ",
      paste(faltantes, collapse = ", ")
    )
  )
}

leads_original <- read_excel(archivo_leads, sheet = hoja_leads)

departamentos <- st_read(archivo_departamentos, quiet = TRUE) %>%
  st_transform(4326)

# -------------------------------------------------------------------------
# 2. Funciones de limpieza, geografia y estilo
# -------------------------------------------------------------------------

limpiar_texto <- function(x) {
  x %>%
    str_to_upper() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^A-Z0-9]+", " ") %>%
    str_squish()
}

limpiar_categoria <- function(x, vacio = "Sin dato") {
  x <- str_squish(as.character(x))
  if_else(is.na(x) | x == "", vacio, x)
}

normalizar_depto <- function(depto) {
  case_when(
    depto %in% c("BOGOTA", "BOGOTA D C", "BOGOTA DC") ~ "BOGOTA D C",
    depto == "NORTE DE SANTADER" ~ "NORTE DE SANTANDER",
    depto == "CASANERO" ~ "CASANARE",
    depto == "GUIAJIRA" ~ "LA GUAJIRA",
    depto == "SAN ANDRES" ~
      "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA",
    depto == "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA" ~
      "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA",
    TRUE ~ depto
  )
}

extraer_ciudad <- function(ciudad_raw) {
  ciudad_raw <- str_squish(as.character(ciudad_raw))
  ciudad <- str_remove(ciudad_raw, "\\s*\\([^\\)]*\\)\\s*$")
  ciudad <- str_split_fixed(ciudad, ",", 2)[, 1]
  str_squish(ciudad)
}

extraer_departamento <- function(ciudad_raw) {
  ciudad_raw <- str_squish(as.character(ciudad_raw))
  depto_parentesis <- str_match(ciudad_raw, "\\(([^\\)]*)\\)")[, 2]
  partes <- str_split(ciudad_raw, ",")
  depto_comas <- vapply(
    partes,
    function(x) {
      x <- str_squish(x)
      x <- x[x != ""]
      if (length(x) >= 2) {
        paste(tail(x, 2), collapse = " ")
      } else {
        NA_character_
      }
    },
    character(1)
  )

  depto <- coalesce(depto_parentesis, depto_comas)
  depto_key <- normalizar_depto(limpiar_texto(depto))

  case_when(
    str_detect(limpiar_texto(ciudad_raw), "BOGOTA") ~ "BOGOTA D C",
    TRUE ~ depto_key
  )
}

fecha_excel <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  as.Date(as.numeric(x), origin = "1899-12-30")
}

colores_regional <- c(
  "Bogota" = "#005F73",
  "Antioquia y eje" = "#0A9396",
  "Occidente y centro" = "#94D2BD",
  "Oriente" = "#EE9B00",
  "Costa atlantica" = "#CA6702",
  "Resto del pais" = "#9B2226",
  "Sin regional" = "#8A8F98"
)

tema_presentacion <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 18, color = "#1F2933"),
      plot.subtitle = element_text(size = 11, color = "#52616B"),
      axis.title = element_text(color = "#1F2933"),
      axis.text = element_text(color = "#52616B"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

guardar_png <- function(nombre, grafico, width = 11, height = 7) {
  ggsave(
    nombre,
    grafico,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

# -------------------------------------------------------------------------
# 3. Regionales Allianz por departamento
# -------------------------------------------------------------------------

regionales_departamento <- tribble(
  ~departamento, ~regional,
  "Quindio", "Antioquia y eje",
  "Caldas", "Antioquia y eje",
  "Antioquia", "Antioquia y eje",
  "Risaralda", "Antioquia y eje",
  "Cundinamarca", "Bogota",
  "Bogota", "Bogota",
  "Atlantico", "Costa atlantica",
  "Bolivar", "Costa atlantica",
  "Cordoba", "Costa atlantica",
  "La Guajira", "Costa atlantica",
  "Magdalena", "Costa atlantica",
  "Sucre", "Costa atlantica",
  "Cesar", "Costa atlantica",
  "Valle del Cauca", "Occidente y centro",
  "Tolima", "Occidente y centro",
  "Huila", "Occidente y centro",
  "Narino", "Occidente y centro",
  "Cauca", "Occidente y centro",
  "Santander", "Oriente",
  "Norte de Santander", "Oriente",
  "Boyaca", "Oriente",
  "Meta", "Oriente",
  "Caqueta", "Resto del pais",
  "Amazonas", "Resto del pais",
  "Putumayo", "Resto del pais",
  "Choco", "Resto del pais",
  "San Andres", "Resto del pais",
  "Casanare", "Resto del pais",
  "Arauca", "Resto del pais",
  "Guainia", "Resto del pais",
  "Guaviare", "Resto del pais",
  "Vaupes", "Resto del pais",
  "Vichada", "Resto del pais"
) %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(departamento))
  ) %>%
  select(depto_key, regional)

# -------------------------------------------------------------------------
# 4. Tratamiento desde Negocios Autos 2025.xlsx
# -------------------------------------------------------------------------

leads <- leads_original %>%
  transmute(
    negocio = `Nombre del negocio`,
    etapa = limpiar_categoria(`Etapa del negocio`),
    fecha_creacion = fecha_excel(`Fecha de creación`),
    fecha_cotizacion = fecha_excel(`Fecha de cotización`),
    fecha_base = coalesce(fecha_cotizacion, fecha_creacion),
    mes = as.Date(format(fecha_base, "%Y-%m-01")),
    propietario = limpiar_categoria(`Propietario del negocio`),
    ciudad_raw = limpiar_categoria(Ciudad),
    ciudad = extraer_ciudad(ciudad_raw),
    depto_key = extraer_departamento(ciudad_raw),
    fuente_utm = limpiar_categoria(`Fuente UTM`),
    medio_utm = limpiar_categoria(`Medio UTM`),
    campana_utm = limpiar_categoria(`Campaña UTM`),
    marca = limpiar_categoria(Marca),
    referencia = limpiar_categoria(Referencia),
    tipo_pago = limpiar_categoria(`Tipo de pago`),
    motivo_perdido = limpiar_categoria(`Motivos perdidos (autos)`),
    valor = as.numeric(Valor),
    contactos = as.numeric(`Número de veces contactado`),
    emitido = limpiar_texto(etapa) == "EMITIDO",
    perdido = limpiar_texto(etapa) == "CIERRE PERDIDO"
  ) %>%
  left_join(regionales_departamento, by = "depto_key") %>%
  mutate(
    regional = if_else(is.na(regional), "Sin regional", regional),
    fuente_utm = str_to_title(fuente_utm),
    medio_utm = str_to_lower(medio_utm),
    campana_utm = str_to_lower(campana_utm)
  )

# -------------------------------------------------------------------------
# 5. Exploracion tabular
# -------------------------------------------------------------------------

resumen_general <- tibble(
  total_leads = nrow(leads),
  emitidos = sum(leads$emitido, na.rm = TRUE),
  perdidos = sum(leads$perdido, na.rm = TRUE),
  tasa_emision = emitidos / total_leads,
  departamentos = n_distinct(leads$depto_key),
  ciudades = n_distinct(leads$ciudad)
)

tabla_etapa <- leads %>%
  count(etapa, name = "leads", sort = TRUE) %>%
  mutate(participacion = leads / sum(leads))

tabla_fuente_utm <- leads %>%
  group_by(fuente_utm) %>%
  summarise(
    leads = n(),
    emitidos = sum(emitido, na.rm = TRUE),
    perdidos = sum(perdido, na.rm = TRUE),
    tasa_emision = emitidos / leads,
    .groups = "drop"
  ) %>%
  mutate(participacion = leads / sum(leads)) %>%
  arrange(desc(leads))

tabla_medio_utm <- leads %>%
  group_by(medio_utm) %>%
  summarise(
    leads = n(),
    emitidos = sum(emitido, na.rm = TRUE),
    perdidos = sum(perdido, na.rm = TRUE),
    tasa_emision = emitidos / leads,
    .groups = "drop"
  ) %>%
  mutate(participacion = leads / sum(leads)) %>%
  arrange(desc(leads))

tabla_campana_utm <- leads %>%
  group_by(campana_utm) %>%
  summarise(
    leads = n(),
    emitidos = sum(emitido, na.rm = TRUE),
    perdidos = sum(perdido, na.rm = TRUE),
    tasa_emision = emitidos / leads,
    .groups = "drop"
  ) %>%
  mutate(participacion = leads / sum(leads)) %>%
  arrange(desc(leads))

tabla_utm_completa <- leads %>%
  group_by(fuente_utm, medio_utm, campana_utm) %>%
  summarise(
    leads = n(),
    emitidos = sum(emitido, na.rm = TRUE),
    perdidos = sum(perdido, na.rm = TRUE),
    tasa_emision = emitidos / leads,
    .groups = "drop"
  ) %>%
  mutate(participacion = leads / sum(leads)) %>%
  arrange(desc(leads), desc(emitidos))

tabla_ciudad <- leads %>%
  count(ciudad, depto_key, regional, name = "leads", sort = TRUE)

tabla_departamento <- leads %>%
  group_by(depto_key, regional) %>%
  summarise(
    leads = n(),
    emitidos = sum(emitido, na.rm = TRUE),
    tasa_emision = emitidos / leads,
    .groups = "drop"
  ) %>%
  arrange(desc(leads))

tabla_regional <- leads %>%
  group_by(regional) %>%
  summarise(
    leads = n(),
    emitidos = sum(emitido, na.rm = TRUE),
    tasa_emision = emitidos / leads,
    .groups = "drop"
  ) %>%
  mutate(participacion = leads / sum(leads)) %>%
  arrange(desc(leads))

tabla_regional_fuente <- leads %>%
  group_by(regional, fuente_utm) %>%
  summarise(
    leads = n(),
    emitidos = sum(emitido, na.rm = TRUE),
    tasa_emision = emitidos / leads,
    .groups = "drop"
  ) %>%
  group_by(regional) %>%
  mutate(participacion_regional = leads / sum(leads)) %>%
  ungroup() %>%
  arrange(regional, desc(leads))

tabla_tendencia_regional <- leads %>%
  filter(!is.na(mes)) %>%
  group_by(mes, regional) %>%
  summarise(
    leads = n(),
    emitidos = sum(emitido, na.rm = TRUE),
    tasa_emision = emitidos / leads,
    .groups = "drop"
  ) %>%
  arrange(mes, regional)

tabla_tendencia_fuente <- leads %>%
  filter(!is.na(mes)) %>%
  group_by(mes, fuente_utm) %>%
  summarise(
    leads = n(),
    emitidos = sum(emitido, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mes, fuente_utm)

print(resumen_general)
print(tabla_etapa)
print(head(tabla_fuente_utm, 15))
print(tabla_regional)

# -------------------------------------------------------------------------
# 6. Mapas por departamento
# -------------------------------------------------------------------------

departamentos_mapa <- departamentos %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(DPTO_CNMBR))
  ) %>%
  left_join(tabla_departamento, by = "depto_key") %>%
  mutate(
    leads = if_else(is.na(leads), 0L, leads),
    emitidos = if_else(is.na(emitidos), 0, emitidos),
    tasa_emision = if_else(is.na(tasa_emision), 0, tasa_emision),
    regional = if_else(is.na(regional), "Sin regional", regional)
  )

leads_no_cruzan_shp <- tabla_departamento %>%
  anti_join(
    departamentos_mapa %>% st_drop_geometry() %>% select(depto_key),
    by = "depto_key"
  )

print("Departamentos de leads que no cruzan con shapefile:")
print(leads_no_cruzan_shp)

puntos_departamentos <- st_point_on_surface(departamentos_mapa)

mapa_leads_departamento <- ggplot() +
  geom_sf(
    data = departamentos_mapa,
    aes(fill = leads),
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf_text(
    data = puntos_departamentos %>% filter(leads > 0),
    aes(label = comma(leads, big.mark = ".", decimal.mark = ",")),
    size = 2.2,
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt",
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Leads del canal directo por departamento",
    subtitle = "Cotizaciones de autos nuevos 2025",
    fill = "Leads"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#1F2933"),
    plot.subtitle = element_text(size = 11, color = "#52616B"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_tasa_emision_departamento <- ggplot() +
  geom_sf(
    data = departamentos_mapa,
    aes(fill = tasa_emision),
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf_text(
    data = puntos_departamentos %>% filter(leads > 0),
    aes(label = percent(tasa_emision, accuracy = 0.1, decimal.mark = ",")),
    size = 2.2,
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option = "D",
    labels = percent_format(accuracy = 0.1, decimal.mark = ",")
  ) +
  labs(
    title = "Tasa de emision por departamento",
    subtitle = "Emitidos sobre leads del canal directo",
    fill = "Tasa"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#1F2933"),
    plot.subtitle = element_text(size = 11, color = "#52616B"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

print(mapa_leads_departamento)
print(mapa_tasa_emision_departamento)

guardar_png("mapa_leads_2025_departamento.png", mapa_leads_departamento, width = 9, height = 11)
guardar_png("mapa_tasa_emision_2025_departamento.png", mapa_tasa_emision_departamento, width = 9, height = 11)

# -------------------------------------------------------------------------
# 7. Graficos regionales y origen de leads
# -------------------------------------------------------------------------

grafico_leads_regional <- ggplot(
  tabla_regional,
  aes(x = reorder(regional, leads), y = leads, fill = regional)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes(
      label = paste0(
        comma(leads, big.mark = ".", decimal.mark = ","),
        " (",
        percent(participacion, accuracy = 0.1, decimal.mark = ","),
        ")"
      )
    ),
    hjust = -0.08,
    size = 3.7,
    color = "#1F2933"
  ) +
  coord_flip() +
  scale_fill_manual(values = colores_regional, guide = "none") +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.24))
  ) +
  labs(
    title = "Leads por regional",
    subtitle = "Cotizaciones del canal directo",
    x = NULL,
    y = "Leads"
  ) +
  tema_presentacion()

grafico_tasa_emision_regional <- ggplot(
  tabla_regional,
  aes(x = reorder(regional, tasa_emision), y = tasa_emision, fill = regional)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes(label = percent(tasa_emision, accuracy = 0.1, decimal.mark = ",")),
    hjust = -0.08,
    size = 3.7,
    color = "#1F2933"
  ) +
  coord_flip() +
  scale_fill_manual(values = colores_regional, guide = "none") +
  scale_y_continuous(
    labels = percent_format(accuracy = 1, decimal.mark = ","),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Tasa de emision por regional",
    subtitle = "Emitidos sobre leads del canal directo",
    x = NULL,
    y = "Tasa de emision"
  ) +
  tema_presentacion()

top_fuentes <- tabla_fuente_utm %>%
  slice_head(n = 8) %>%
  pull(fuente_utm)

grafico_fuente_utm <- tabla_fuente_utm %>%
  slice_head(n = 12) %>%
  ggplot(aes(x = reorder(fuente_utm, leads), y = leads)) +
  geom_col(width = 0.68, fill = "#005F73") +
  geom_text(
    aes(
      label = paste0(
        comma(leads, big.mark = ".", decimal.mark = ","),
        " (",
        percent(participacion, accuracy = 0.1, decimal.mark = ","),
        ")"
      )
    ),
    hjust = -0.08,
    size = 3.6,
    color = "#1F2933"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "Origen de leads por fuente UTM",
    subtitle = "Top fuentes del canal directo",
    x = NULL,
    y = "Leads"
  ) +
  tema_presentacion()

grafico_regional_fuente <- tabla_regional_fuente %>%
  filter(fuente_utm %in% top_fuentes) %>%
  ggplot(aes(x = regional, y = leads, fill = fuente_utm)) +
  geom_col(width = 0.72) +
  coord_flip() +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Origen de leads por regional",
    subtitle = "Distribucion de las principales fuentes UTM",
    x = NULL,
    y = "Leads",
    fill = "Fuente UTM"
  ) +
  tema_presentacion()

grafico_tendencia_regional <- ggplot(
  tabla_tendencia_regional,
  aes(x = mes, y = leads, color = regional)
) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.1) +
  scale_color_manual(values = colores_regional) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  labs(
    title = "Tendencia mensual de leads por regional",
    subtitle = "Cotizaciones del canal directo por mes",
    x = NULL,
    y = "Leads",
    color = "Regional"
  ) +
  tema_presentacion()

grafico_tendencia_fuente <- tabla_tendencia_fuente %>%
  filter(fuente_utm %in% top_fuentes) %>%
  ggplot(aes(x = mes, y = leads, color = fuente_utm)) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.1) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  labs(
    title = "Tendencia mensual de leads por fuente",
    subtitle = "Principales fuentes UTM",
    x = NULL,
    y = "Leads",
    color = "Fuente UTM"
  ) +
  tema_presentacion()

print(grafico_leads_regional)
print(grafico_tasa_emision_regional)
print(grafico_fuente_utm)
print(grafico_regional_fuente)
print(grafico_tendencia_regional)
print(grafico_tendencia_fuente)

guardar_png("grafico_leads_2025_regional.png", grafico_leads_regional)
guardar_png("grafico_tasa_emision_2025_regional.png", grafico_tasa_emision_regional)
guardar_png("grafico_fuente_utm_leads_2025.png", grafico_fuente_utm)
guardar_png("grafico_regional_fuente_utm_leads_2025.png", grafico_regional_fuente)
guardar_png("grafico_tendencia_leads_regional.png", grafico_tendencia_regional)
guardar_png("grafico_tendencia_leads_fuente_utm.png", grafico_tendencia_fuente)

# -------------------------------------------------------------------------
# 8. Tablas de salida
# -------------------------------------------------------------------------

write_csv(resumen_general, "tabla_leads_resumen_general.csv")
write_csv(tabla_etapa, "tabla_leads_etapa.csv")
write_csv(tabla_fuente_utm, "tabla_leads_fuente_utm.csv")
write_csv(tabla_medio_utm, "tabla_leads_medio_utm.csv")
write_csv(tabla_campana_utm, "tabla_leads_campana_utm.csv")
write_csv(tabla_utm_completa, "tabla_leads_utm_completa.csv")
write_csv(tabla_ciudad, "tabla_leads_ciudad.csv")
write_csv(tabla_departamento, "tabla_leads_departamento.csv")
write_csv(tabla_regional, "tabla_leads_regional.csv")
write_csv(tabla_regional_fuente, "tabla_leads_regional_fuente_utm.csv")
write_csv(tabla_tendencia_regional, "tabla_tendencia_leads_regional.csv")
write_csv(tabla_tendencia_fuente, "tabla_tendencia_leads_fuente_utm.csv")
write_csv(leads_no_cruzan_shp, "departamentos_leads_no_cruzan_shp.csv")
