# Grafica automoviles activos por municipio en Colombia.
#
# Instrucciones:
# 1. Poner este script en el mismo directorio de los archivos:
#    - CRECIMIENTO_DEL_PARQUE_AUTOMOTOR_RUNT2.0_20260612.csv
#    - municipios_colombia.shp
#    - departamentos_colombia.shp
# 2. En R/RStudio, establecer ese directorio como working directory.
# 3. Ejecutar: source("graficar_automoviles_municipios.R")

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
# 2. Limpiar textos para cruzar RUNT con shapefile municipal
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
    depto == "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA" ~
      "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA",
    TRUE ~ depto
  )
}

normalizar_municipio <- function(depto, municipio) {
  case_when(
    depto == "BOGOTA D C" & municipio == "BOGOTA" ~ "BOGOTA D C",
    depto == "BOLIVAR" & municipio == "CARTAGENA" ~ "CARTAGENA DE INDIAS",
    depto == "NORTE DE SANTANDER" & municipio == "CUCUTA" ~ "SAN JOSE DE CUCUTA",
    depto == "TOLIMA" & municipio == "MARIQUITA" ~ "SAN SEBASTIAN DE MARIQUITA",
    depto == "CUNDINAMARCA" & municipio == "UBATE" ~ "VILLA DE SAN DIEGO DE UBATE",
    depto == "CAUCA" & municipio == "PIENDAMO" ~ "PIENDAMO TUNIA",
    depto == "NARINO" & municipio == "TUMACO" ~ "SAN ANDRES DE TUMACO",
    depto == "ANTIOQUIA" & municipio == "SANTAFE DE ANTIOQUIA" ~ "SANTA FE DE ANTIOQUIA",
    depto == "ANTIOQUIA" & municipio == "DON MATIAS" ~ "DONMATIAS",
    depto == "SUCRE" & municipio == "SINCE" ~ "SAN LUIS DE SINCE",
    TRUE ~ municipio
  )
}

# -------------------------------------------------------------------------
# 3. Filtrar automoviles activos y sumar la cantidad por municipio
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
  group_by(llave, NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO) %>%
  summarise(
    automoviles = sum(CANTIDAD, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# 4. Preparar mapa municipal y unir con datos RUNT
# -------------------------------------------------------------------------

municipios_mapa <- municipios %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(DPTO_CNMBR)),
    municipio_key = limpiar_texto(MPIO_CNMBR),
    municipio_key = normalizar_municipio(depto_key, municipio_key),
    llave = paste(depto_key, municipio_key, sep = "_")
  )

mapa_autos <- municipios_mapa %>%
  left_join(autos_municipio, by = "llave") %>%
  mutate(
    tiene_dato_runt = if_else(is.na(automoviles), "NO", "SI"),
    automoviles = if_else(is.na(automoviles), 0, automoviles)
  )

# Diagnostico opcional: municipios del RUNT que no cruzaron con el shapefile.
no_encontrados <- autos_municipio %>%
  anti_join(st_drop_geometry(municipios_mapa), by = "llave") %>%
  arrange(desc(automoviles))

print("Municipios del RUNT que no cruzaron con el shapefile:")
print(no_encontrados)

# -------------------------------------------------------------------------
# 5. Graficar todos los municipios
# -------------------------------------------------------------------------

mapa_automoviles <- ggplot() +
  geom_sf(
    data = mapa_autos,
    aes(fill = automoviles),
    color = "white",
    linewidth = 0.04
  ) +
  geom_sf(
    data = departamentos,
    fill = NA,
    color = "#222222",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt",
    na.value = "#eeeeee",
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Automoviles activos por municipio",
    subtitle = "Todos los municipios disponibles en el shapefile",
    fill = "Automoviles"
  ) +
  theme_void()

print(mapa_automoviles)

# -------------------------------------------------------------------------
# 6. Guardar salidas
# -------------------------------------------------------------------------

ggsave(
  "mapa_automoviles_municipios.png",
  mapa_automoviles,
  width = 9,
  height = 11,
  dpi = 300
)

tabla_automoviles_municipios <- mapa_autos %>%
  st_drop_geometry() %>%
  select(
    departamento = DPTO_CNMBR,
    municipio = MPIO_CNMBR,
    tiene_dato_runt,
    automoviles
  ) %>%
  arrange(tiene_dato_runt, departamento, municipio)

write_csv(
  tabla_automoviles_municipios,
  "tabla_cobertura_municipios_runt.csv"
)
