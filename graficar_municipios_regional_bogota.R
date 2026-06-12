# Grafica automoviles activos por municipio dentro de la regional Bogota.
#
# Regional Bogota segun tabla Allianz:
# - Bogota
# - Cundinamarca
#
# Instrucciones:
# 1. Poner este script en el mismo directorio de los archivos:
#    - CRECIMIENTO_DEL_PARQUE_AUTOMOTOR_RUNT2.0_20260612.csv
#    - municipios_colombia.shp
#    - departamentos_colombia.shp
# 2. En R/RStudio, establecer ese directorio como working directory.
# 3. Ejecutar: source("graficar_municipios_regional_bogota.R")

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(stringi)
library(scales)

# -------------------------------------------------------------------------
# 1. Leer archivos desde el directorio actual
# -------------------------------------------------------------------------

archivo_runt <- "CRECIMIENTO_DEL_PARQUE_AUTOMOTOR_RUNT2.0_20260612.csv"
archivo_municipios <- "municipios_colombia.shp"
archivo_departamentos <- "departamentos_colombia.shp"

archivos_requeridos <- c(
  archivo_runt,
  archivo_municipios,
  archivo_departamentos
)

faltantes <- archivos_requeridos[!file.exists(archivos_requeridos)]

if (length(faltantes) > 0) {
  stop(
    paste0(
      "No se encontraron estos archivos en el directorio actual: ",
      paste(faltantes, collapse = ", ")
    )
  )
}

runt <- read_csv(
  archivo_runt,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

municipios <- st_read(archivo_municipios, quiet = TRUE) %>%
  st_transform(4326)

departamentos <- st_read(archivo_departamentos, quiet = TRUE) %>%
  st_transform(4326)

# -------------------------------------------------------------------------
# 2. Funciones de limpieza y homologacion
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
    TRUE ~ depto
  )
}

normalizar_municipio <- function(depto, municipio) {
  case_when(
    depto == "BOGOTA D C" & municipio == "BOGOTA" ~ "BOGOTA D C",
    depto == "CUNDINAMARCA" & municipio == "UBATE" ~ "VILLA DE SAN DIEGO DE UBATE",
    TRUE ~ municipio
  )
}

# -------------------------------------------------------------------------
# 3. Definir la regional Bogota por departamentos
# -------------------------------------------------------------------------

regional_objetivo <- "Bogota"

departamentos_regional <- tibble(
  departamento = c("Bogota", "Cundinamarca"),
  regional = regional_objetivo
) %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(departamento))
  )

# -------------------------------------------------------------------------
# 4. Filtrar automoviles activos y sumar por municipio
# -------------------------------------------------------------------------

autos_municipio <- runt %>%
  filter(
    limpiar_texto(NOMBRE_DE_LA_CLASE) == "AUTOMOVIL",
    limpiar_texto(ESTADO_DEL_VEHICULO) == "ACTIVO"
  ) %>%
  mutate(
    CANTIDAD = as.numeric(CANTIDAD),
    depto_key = normalizar_depto(limpiar_texto(NOMBRE_DEPARTAMENTO)),
    municipio_key = limpiar_texto(NOMBRE_MUNICIPIO),
    municipio_key = normalizar_municipio(depto_key, municipio_key),
    llave = paste(depto_key, municipio_key, sep = "_")
  ) %>%
  semi_join(departamentos_regional, by = "depto_key") %>%
  group_by(llave, depto_key, NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO) %>%
  summarise(
    automoviles = sum(CANTIDAD, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# 5. Preparar municipios y departamentos de la regional Bogota
# -------------------------------------------------------------------------

municipios_regional <- municipios %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(DPTO_CNMBR)),
    municipio_key = limpiar_texto(MPIO_CNMBR),
    municipio_key = normalizar_municipio(depto_key, municipio_key),
    llave = paste(depto_key, municipio_key, sep = "_")
  ) %>%
  semi_join(departamentos_regional, by = "depto_key") %>%
  left_join(autos_municipio, by = "llave") %>%
  mutate(
    tiene_dato_runt = if_else(is.na(automoviles), "NO", "SI"),
    automoviles = if_else(is.na(automoviles), 0, automoviles)
  )

departamentos_regional_mapa <- departamentos %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(DPTO_CNMBR))
  ) %>%
  semi_join(departamentos_regional, by = "depto_key")

limite_regional <- departamentos_regional_mapa %>%
  summarise(
    regional = regional_objetivo,
    geometry = st_union(geometry),
    .groups = "drop"
  )

# Diagnostico: municipios RUNT de la regional que no cruzan con el shapefile.
no_encontrados <- autos_municipio %>%
  anti_join(st_drop_geometry(municipios_regional), by = "llave") %>%
  transmute(
    departamento_runt = NOMBRE_DEPARTAMENTO,
    municipio_runt = NOMBRE_MUNICIPIO,
    automoviles,
    llave
  ) %>%
  arrange(desc(automoviles), departamento_runt, municipio_runt)

print("Municipios RUNT de la regional Bogota que no cruzaron con el shapefile:")
print(no_encontrados)

# -------------------------------------------------------------------------
# 6. Graficar municipios de la regional Bogota
# -------------------------------------------------------------------------

mapa_municipios_regional_bogota <- ggplot() +
  geom_sf(
    data = municipios_regional,
    aes(fill = automoviles),
    color = "white",
    linewidth = 0.08
  ) +
  geom_sf(
    data = departamentos_regional_mapa,
    fill = NA,
    color = "#2b2b2b",
    linewidth = 0.45
  ) +
  geom_sf(
    data = limite_regional,
    fill = NA,
    color = "#111111",
    linewidth = 0.9
  ) +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt",
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Automoviles activos por municipio",
    subtitle = "Regional Bogota: Bogota y Cundinamarca",
    fill = "Automoviles"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

print(mapa_municipios_regional_bogota)

# -------------------------------------------------------------------------
# 7. Guardar salidas
# -------------------------------------------------------------------------

ggsave(
  "mapa_municipios_regional_bogota.png",
  mapa_municipios_regional_bogota,
  width = 9,
  height = 8,
  dpi = 300
)

tabla_municipios_regional_bogota <- municipios_regional %>%
  st_drop_geometry() %>%
  select(
    departamento = DPTO_CNMBR,
    municipio = MPIO_CNMBR,
    tiene_dato_runt,
    automoviles
  ) %>%
  arrange(desc(automoviles), departamento, municipio)

write_csv(
  tabla_municipios_regional_bogota,
  "tabla_municipios_regional_bogota.csv"
)

write_csv(
  no_encontrados,
  "municipios_runt_no_cruzan_shp_regional_bogota.csv"
)
