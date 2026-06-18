# Grafica ventas de carros electricos 2025 por departamento en Colombia.
#
# Este script hace todo el tratamiento desde el archivo original:
# - tabla_maestra.csv
# - departamentos_colombia.shp
#
# En R/RStudio:
# source("graficar_electricos_2025_departamentos.R")

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(stringi)
library(scales)

# -------------------------------------------------------------------------
# 1. Parametros y lectura de archivos originales
# -------------------------------------------------------------------------

archivo_ventas <- "tabla_maestra.csv"
archivo_departamentos <- "departamentos_colombia.shp"
anio_objetivo <- 2025

archivos_requeridos <- c(archivo_ventas, archivo_departamentos)
faltantes <- archivos_requeridos[!file.exists(archivos_requeridos)]

if (length(faltantes) > 0) {
  stop(
    paste0(
      "No se encontraron estos archivos en el directorio actual: ",
      paste(faltantes, collapse = ", ")
    )
  )
}

ventas_original <- read_csv(
  archivo_ventas,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

departamentos <- st_read(archivo_departamentos, quiet = TRUE) %>%
  st_transform(4326)

# -------------------------------------------------------------------------
# 2. Normalizacion de texto para cruces
# -------------------------------------------------------------------------

limpiar_texto <- function(x) {
  x %>%
    str_to_upper() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^A-Z0-9]+", " ") %>%
    str_squish()
}

normalizar_depto <- function(depto) {
  case_when(
    depto %in% c("BOGOTA", "BOGOTA D C") ~ "BOGOTA D C",
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

# -------------------------------------------------------------------------
# 3. Tratamiento desde tabla_maestra.csv
# -------------------------------------------------------------------------

ventas_2025 <- ventas_original %>%
  filter(anio == anio_objetivo) %>%
  mutate(
    unidades = as.numeric(unidades),
    tecnologia_key = limpiar_texto(tecnologia),
    depto_key = normalizar_depto(limpiar_texto(departamento)),
    es_electrico = tecnologia_key == "ELECTRICO",
    es_electrificado = tecnologia_key %in% c(
      "ELECTRICO",
      "HIBRIDO ENCHUFABLE",
      "HIBRIDO NO ENCHUFABLE"
    )
  )

ventas_departamento <- ventas_2025 %>%
  group_by(depto_key) %>%
  summarise(
    ventas_totales = sum(unidades, na.rm = TRUE),
    ventas_electricos = sum(if_else(es_electrico, unidades, 0), na.rm = TRUE),
    ventas_electrificados = sum(if_else(es_electrificado, unidades, 0), na.rm = TRUE),
    participacion_electricos = ventas_electricos / ventas_totales,
    participacion_electrificados = ventas_electrificados / ventas_totales,
    registros = n(),
    .groups = "drop"
  )

departamentos_mapa <- departamentos %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(DPTO_CNMBR))
  ) %>%
  left_join(ventas_departamento, by = "depto_key") %>%
  mutate(
    ventas_totales = if_else(is.na(ventas_totales), 0, ventas_totales),
    ventas_electricos = if_else(is.na(ventas_electricos), 0, ventas_electricos),
    ventas_electrificados = if_else(is.na(ventas_electrificados), 0, ventas_electrificados),
    participacion_electricos = if_else(is.na(participacion_electricos), 0, participacion_electricos),
    participacion_electrificados = if_else(
      is.na(participacion_electrificados),
      0,
      participacion_electrificados
    ),
    registros = if_else(is.na(registros), 0L, registros)
  )

ventas_no_cruzan_shp <- ventas_departamento %>%
  anti_join(
    departamentos_mapa %>% st_drop_geometry() %>% select(depto_key),
    by = "depto_key"
  )

print("Departamentos de tabla_maestra.csv que no cruzan con shapefile:")
print(ventas_no_cruzan_shp)

tabla_electricos_2025_departamento <- departamentos_mapa %>%
  st_drop_geometry() %>%
  transmute(
    departamento = DPTO_CNMBR,
    ventas_totales,
    ventas_electricos,
    participacion_electricos,
    ventas_electrificados,
    participacion_electrificados,
    registros
  ) %>%
  arrange(desc(ventas_electricos), departamento)

tabla_tecnologia_2025 <- ventas_2025 %>%
  group_by(tecnologia) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    registros = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas))

print(tabla_tecnologia_2025)
print(tabla_electricos_2025_departamento)

# -------------------------------------------------------------------------
# 4. Mapas
# -------------------------------------------------------------------------

puntos_departamentos <- st_point_on_surface(departamentos_mapa)

mapa_electricos_2025_departamento <- ggplot() +
  geom_sf(
    data = departamentos_mapa,
    aes(fill = ventas_electricos),
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf_text(
    data = puntos_departamentos %>% filter(ventas_electricos > 0),
    aes(label = comma(ventas_electricos, big.mark = ".", decimal.mark = ",")),
    size = 2.2,
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt",
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Ventas de carros electricos por departamento",
    subtitle = paste0("Unidades electricas puras vendidas en ", anio_objetivo),
    fill = "Electricos"
  ) +
  theme_void()

mapa_participacion_electricos_2025_departamento <- ggplot() +
  geom_sf(
    data = departamentos_mapa,
    aes(fill = participacion_electricos),
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf_text(
    data = puntos_departamentos %>% filter(ventas_electricos > 0),
    aes(label = percent(participacion_electricos, accuracy = 0.1, decimal.mark = ",")),
    size = 2.2,
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option = "D",
    labels = percent_format(accuracy = 0.1, decimal.mark = ",")
  ) +
  labs(
    title = "Participacion de carros electricos por departamento",
    subtitle = paste0("Electricos puros sobre ventas totales en ", anio_objetivo),
    fill = "% electricos"
  ) +
  theme_void()

print(mapa_electricos_2025_departamento)
print(mapa_participacion_electricos_2025_departamento)

# -------------------------------------------------------------------------
# 5. Guardar salidas
# -------------------------------------------------------------------------

ggsave(
  "mapa_electricos_2025_departamento.png",
  mapa_electricos_2025_departamento,
  width = 9,
  height = 11,
  dpi = 300
)

ggsave(
  "mapa_participacion_electricos_2025_departamento.png",
  mapa_participacion_electricos_2025_departamento,
  width = 9,
  height = 11,
  dpi = 300
)

write_csv(
  tabla_electricos_2025_departamento,
  "tabla_electricos_2025_departamento.csv"
)

write_csv(
  tabla_tecnologia_2025,
  "tabla_tecnologia_2025.csv"
)

write_csv(
  ventas_no_cruzan_shp,
  "departamentos_electricos_2025_no_cruzan_shp.csv"
)
