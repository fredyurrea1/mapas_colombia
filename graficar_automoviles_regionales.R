# Grafica automoviles activos por regional Allianz en Colombia.
#
# Instrucciones:
# 1. Poner este script en el mismo directorio de los archivos:
#    - CRECIMIENTO_DEL_PARQUE_AUTOMOTOR_RUNT2.0_20260612.csv
#    - departamentos_colombia.shp
# 2. En R/RStudio, establecer ese directorio como working directory.
# 3. Ejecutar: source("graficar_automoviles_regionales.R")

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
archivo_departamentos <- "departamentos_colombia.shp"

archivos_requeridos <- c(archivo_runt, archivo_departamentos)
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

departamentos <- st_read(archivo_departamentos, quiet = TRUE) %>%
  st_transform(4326)

# -------------------------------------------------------------------------
# 2. Limpiar nombres para cruzar RUNT, shapefile y regionales
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
    depto == "SAN ANDRES" ~
      "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA",
    depto == "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA" ~
      "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA",
    TRUE ~ depto
  )
}

# -------------------------------------------------------------------------
# 3. Tabla de regionales Allianz por departamento
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
  )

# -------------------------------------------------------------------------
# 4. Agregar automoviles activos por departamento y regional
# -------------------------------------------------------------------------

autos_departamento <- runt %>%
  filter(
    limpiar_texto(NOMBRE_DE_LA_CLASE) == "AUTOMOVIL",
    limpiar_texto(ESTADO_DEL_VEHICULO) == "ACTIVO"
  ) %>%
  mutate(
    CANTIDAD = as.numeric(CANTIDAD),
    depto_key = normalizar_depto(limpiar_texto(NOMBRE_DEPARTAMENTO))
  ) %>%
  group_by(depto_key) %>%
  summarise(
    automoviles = sum(CANTIDAD, na.rm = TRUE),
    .groups = "drop"
  )

departamentos_regionales <- departamentos %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(DPTO_CNMBR))
  ) %>%
  left_join(regionales_departamento, by = "depto_key") %>%
  left_join(autos_departamento, by = "depto_key") %>%
  mutate(
    regional = if_else(is.na(regional), "Sin regional", regional),
    automoviles = if_else(is.na(automoviles), 0, automoviles)
  )

departamentos_sin_regional <- departamentos_regionales %>%
  st_drop_geometry() %>%
  filter(regional == "Sin regional") %>%
  select(departamento_shp = DPTO_CNMBR, depto_key)

print("Departamentos del shapefile sin regional asignada:")
print(departamentos_sin_regional)

tabla_automoviles_departamento_regional <- departamentos_regionales %>%
  st_drop_geometry() %>%
  select(
    departamento = DPTO_CNMBR,
    regional,
    automoviles
  ) %>%
  arrange(regional, departamento)

tabla_automoviles_regional <- tabla_automoviles_departamento_regional %>%
  group_by(regional) %>%
  summarise(
    automoviles = sum(automoviles, na.rm = TRUE),
    departamentos = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(automoviles))

print(tabla_automoviles_regional)

# -------------------------------------------------------------------------
# 5. Crear geometria regional agrupando departamentos
# -------------------------------------------------------------------------

regionales_mapa <- departamentos_regionales %>%
  group_by(regional) %>%
  summarise(
    automoviles = sum(automoviles, na.rm = TRUE),
    departamentos = n(),
    geometry = st_union(geometry),
    .groups = "drop"
  )

puntos_regionales <- st_point_on_surface(regionales_mapa)

# -------------------------------------------------------------------------
# 6. Mapa de departamentos coloreados por regional
# -------------------------------------------------------------------------

mapa_departamentos_regional <- ggplot() +
  geom_sf(
    data = departamentos_regionales,
    aes(fill = regional),
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = regionales_mapa,
    fill = NA,
    color = "#222222",
    linewidth = 0.65
  ) +
  labs(
    title = "Regionales Allianz por departamento",
    fill = "Regional"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

print(mapa_departamentos_regional)

# -------------------------------------------------------------------------
# 7. Mapa agregado por regional, segun automoviles activos
# -------------------------------------------------------------------------

mapa_automoviles_regional <- ggplot() +
  geom_sf(
    data = regionales_mapa,
    aes(fill = automoviles),
    color = NA
  ) +
  geom_sf(
    data = departamentos_regionales,
    fill = NA,
    color = "white",
    linewidth = 0.35
  ) +
  geom_sf(
    data = regionales_mapa,
    fill = NA,
    color = "#222222",
    linewidth = 0.75
  ) +
  geom_sf_text(
    data = puntos_regionales,
    aes(
      label = paste0(
        regional,
        "\n",
        comma(automoviles, big.mark = ".", decimal.mark = ",")
      )
    ),
    size = 3
  ) +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt",
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Automoviles activos por regional Allianz",
    subtitle = "Departamentos visibles dentro de cada regional",
    fill = "Automoviles"
  ) +
  theme_void()

print(mapa_automoviles_regional)

# -------------------------------------------------------------------------
# 8. Guardar salidas
# -------------------------------------------------------------------------

ggsave(
  "mapa_departamentos_regional_allianz.png",
  mapa_departamentos_regional,
  width = 9,
  height = 11,
  dpi = 300
)

ggsave(
  "mapa_automoviles_regional_allianz.png",
  mapa_automoviles_regional,
  width = 9,
  height = 11,
  dpi = 300
)

write_csv(
  tabla_automoviles_departamento_regional,
  "tabla_automoviles_departamento_regional.csv"
)

write_csv(
  tabla_automoviles_regional,
  "tabla_automoviles_regional.csv"
)

write_csv(
  departamentos_sin_regional,
  "departamentos_sin_regional.csv"
)
