# Mapa de automoviles activos por municipio en Colombia
#
# Este script asume que los archivos estan en el mismo directorio:
# - CRECIMIENTO_DEL_PARQUE_AUTOMOTOR_RUNT2.0_20260612.csv
# - municipios_colombia.shp
# - departamentos_colombia.shp

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(stringi)
library(scales)

archivo_runt <- "CRECIMIENTO_DEL_PARQUE_AUTOMOTOR_RUNT2.0_20260612.csv"
archivo_municipios <- "municipios_colombia.shp"
archivo_departamentos <- "departamentos_colombia.shp"

limpiar_texto <- function(x) {
  x %>%
    str_to_upper() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^A-Z0-9]+", " ") %>%
    str_squish()
}

# 1. Leer datos
runt <- read_csv(
  archivo_runt,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

municipios <- st_read(archivo_municipios, quiet = TRUE) %>%
  st_transform(4326)

departamentos <- st_read(archivo_departamentos, quiet = TRUE) %>%
  st_transform(4326)

# 2. Filtrar automoviles activos y sumar por municipio
autos_municipio <- runt %>%
  filter(
    limpiar_texto(NOMBRE_DE_LA_CLASE) == "AUTOMOVIL",
    limpiar_texto(ESTADO_DEL_VEHICULO) == "ACTIVO"
  ) %>%
  mutate(
    CANTIDAD = as.numeric(CANTIDAD),
    depto_key = limpiar_texto(NOMBRE_DEPARTAMENTO),
    municipio_key = limpiar_texto(NOMBRE_MUNICIPIO),
    llave = paste(depto_key, municipio_key, sep = "_")
  ) %>%
  group_by(llave, NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO) %>%
  summarise(
    automoviles = sum(CANTIDAD, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Preparar shapefile municipal para cruce
municipios_mapa <- municipios %>%
  mutate(
    depto_key = limpiar_texto(DPTO_CNMBR),
    municipio_key = limpiar_texto(MPIO_CNMBR),
    llave = paste(depto_key, municipio_key, sep = "_")
  )

# 4. Unir datos RUNT con poligonos municipales
mapa_autos <- municipios_mapa %>%
  left_join(autos_municipio, by = "llave")

# 5. Diagnostico: municipios del RUNT que no cruzaron con el shapefile
no_encontrados <- autos_municipio %>%
  anti_join(st_drop_geometry(municipios_mapa), by = "llave") %>%
  arrange(desc(automoviles))

print("Municipios del RUNT que no cruzaron con el shapefile:")
print(no_encontrados)

# 6. Mapa de todos los municipios segun cantidad de automoviles
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
    subtitle = "Municipios de Colombia segun cantidad de automoviles activos",
    fill = "Automoviles"
  ) +
  theme_void()

print(mapa_automoviles)

# 7. Guardar resultado
ggsave(
  "mapa_automoviles_municipios.png",
  mapa_automoviles,
  width = 9,
  height = 11,
  dpi = 300
)

# 8. Tabla ordenada de todos los municipios con automoviles
tabla_automoviles_municipios <- mapa_autos %>%
  st_drop_geometry() %>%
  filter(!is.na(automoviles)) %>%
  select(DPTO_CNMBR, MPIO_CNMBR, automoviles) %>%
  arrange(desc(automoviles))

print(tabla_automoviles_municipios)

write_csv(
  tabla_automoviles_municipios,
  "automoviles_municipios.csv"
)
