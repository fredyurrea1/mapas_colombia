# Grafica ventas de carros hasta marzo de 2026 por departamento y regional Allianz.
#
# Instrucciones:
# 1. Poner este script en el mismo directorio de los archivos:
#    - tabla_maestra.csv
#    - departamentos_colombia.shp
# 2. En R/RStudio, establecer ese directorio como working directory.
# 3. Ejecutar: source("graficar_ventas_2025_regionales.R")

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(stringi)
library(scales)

# -------------------------------------------------------------------------
# 1. Parametros y lectura de archivos
# -------------------------------------------------------------------------

archivo_ventas <- "tabla_maestra.csv"
archivo_departamentos <- "departamentos_colombia.shp"
periodo_fin <- 202603
etiqueta_periodo <- "enero 2025 a marzo 2026"

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

ventas <- read_csv(
  archivo_ventas,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

departamentos <- st_read(archivo_departamentos, quiet = TRUE) %>%
  st_transform(4326)

# -------------------------------------------------------------------------
# 2. Limpiar nombres para cruzar tabla maestra, shapefile y regionales
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
# 4. Agregar ventas por departamento y regional hasta marzo de 2026
# -------------------------------------------------------------------------

ventas_2025 <- ventas %>%
  mutate(periodo = as.integer(periodo)) %>%
  filter(periodo <= periodo_fin) %>%
  mutate(
    unidades = as.numeric(unidades),
    depto_key = normalizar_depto(limpiar_texto(departamento))
  )

ventas_departamento <- ventas_2025 %>%
  group_by(depto_key) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    registros = n(),
    .groups = "drop"
  )

departamentos_ventas <- departamentos %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(DPTO_CNMBR))
  ) %>%
  left_join(regionales_departamento, by = "depto_key") %>%
  left_join(ventas_departamento, by = "depto_key") %>%
  mutate(
    regional = if_else(is.na(regional), "Sin regional", regional),
    ventas = if_else(is.na(ventas), 0, ventas),
    registros = if_else(is.na(registros), 0L, registros)
  )

departamentos_sin_regional <- departamentos_ventas %>%
  st_drop_geometry() %>%
  filter(regional == "Sin regional") %>%
  select(departamento_shp = DPTO_CNMBR, depto_key)

departamentos_sin_ventas <- departamentos_ventas %>%
  st_drop_geometry() %>%
  filter(ventas == 0) %>%
  select(departamento_shp = DPTO_CNMBR, depto_key, regional)

departamentos_ventas_no_cruzan_shp <- ventas_departamento %>%
  anti_join(
    departamentos_ventas %>% st_drop_geometry() %>% select(depto_key),
    by = "depto_key"
  )

print("Departamentos del shapefile sin regional asignada:")
print(departamentos_sin_regional)

print("Departamentos del shapefile sin ventas en el periodo:")
print(departamentos_sin_ventas)

print("Departamentos de tabla_maestra.csv que no cruzan con shapefile:")
print(departamentos_ventas_no_cruzan_shp)

tabla_ventas_2025_departamento_regional <- departamentos_ventas %>%
  st_drop_geometry() %>%
  select(
    departamento = DPTO_CNMBR,
    regional,
    ventas,
    registros
  ) %>%
  arrange(desc(ventas), regional, departamento)

tabla_ventas_2025_regional <- tabla_ventas_2025_departamento_regional %>%
  group_by(regional) %>%
  summarise(
    ventas = sum(ventas, na.rm = TRUE),
    departamentos = n(),
    registros = sum(registros, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas))

tabla_ventas_2025_marca <- ventas_2025 %>%
  group_by(marca) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas))

tabla_ventas_2025_tecnologia <- ventas_2025 %>%
  group_by(tecnologia) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas))

tabla_ventas_2025_marca_regional <- ventas_2025 %>%
  left_join(regionales_departamento %>% select(depto_key, regional), by = "depto_key") %>%
  mutate(regional = if_else(is.na(regional), "Sin regional", regional)) %>%
  group_by(regional, marca) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(regional, desc(ventas))

print(tabla_ventas_2025_regional)

# -------------------------------------------------------------------------
# 5. Crear geometria regional agrupando departamentos
# -------------------------------------------------------------------------

regionales_mapa <- departamentos_ventas %>%
  group_by(regional) %>%
  summarise(
    ventas = sum(ventas, na.rm = TRUE),
    departamentos = n(),
    geometry = st_union(geometry),
    .groups = "drop"
  )

puntos_departamentos <- st_point_on_surface(departamentos_ventas)
puntos_regionales <- st_point_on_surface(regionales_mapa)

# -------------------------------------------------------------------------
# 6. Mapa de ventas por departamento
# -------------------------------------------------------------------------

mapa_ventas_2025_departamento <- ggplot() +
  geom_sf(
    data = departamentos_ventas,
    aes(fill = ventas),
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = departamentos_ventas,
    fill = NA,
    color = "#333333",
    linewidth = 0.2
  ) +
  geom_sf_text(
    data = puntos_departamentos %>% filter(ventas > 0),
    aes(label = comma(ventas, big.mark = ".", decimal.mark = ",")),
    size = 2.2,
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt",
    labels = label_number(big.mark = ".", decimal.mark = ","),
    guide = guide_colorbar(
      direction = "vertical",
      barheight = grid::unit(70, "mm"),
      barwidth = grid::unit(5, "mm")
    )
  ) +
  labs(
    title = "Ventas de carros por departamento",
    subtitle = paste0("Unidades vendidas, ", etiqueta_periodo),
    fill = "Ventas"
  ) +
  theme_void() +
  theme(legend.position = "right")

print(mapa_ventas_2025_departamento)

# -------------------------------------------------------------------------
# 7. Mapa de ventas por regional Allianz
# -------------------------------------------------------------------------

mapa_ventas_2025_regional <- ggplot() +
  geom_sf(
    data = regionales_mapa,
    aes(fill = ventas),
    color = NA
  ) +
  geom_sf(
    data = departamentos_ventas,
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
        comma(ventas, big.mark = ".", decimal.mark = ",")
      )
    ),
    size = 3
  ) +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt",
    labels = label_number(big.mark = ".", decimal.mark = ","),
    guide = guide_colorbar(
      direction = "vertical",
      barheight = grid::unit(70, "mm"),
      barwidth = grid::unit(5, "mm")
    )
  ) +
  labs(
    title = "Ventas de carros por regional Allianz",
    subtitle = paste0("Unidades vendidas, ", etiqueta_periodo),
    fill = "Ventas"
  ) +
  theme_void() +
  theme(legend.position = "right")

print(mapa_ventas_2025_regional)

# -------------------------------------------------------------------------
# 8. Guardar salidas
# -------------------------------------------------------------------------

ggsave(
  "mapa_ventas_hasta_marzo_2026_departamento.png",
  mapa_ventas_2025_departamento,
  width = 9,
  height = 11,
  dpi = 300
)

ggsave(
  "mapa_ventas_hasta_marzo_2026_regional_allianz.png",
  mapa_ventas_2025_regional,
  width = 9,
  height = 11,
  dpi = 300
)

write_csv(
  tabla_ventas_2025_departamento_regional,
  "tabla_ventas_hasta_marzo_2026_departamento_regional.csv"
)

write_csv(
  tabla_ventas_2025_regional,
  "tabla_ventas_hasta_marzo_2026_regional.csv"
)

write_csv(
  tabla_ventas_2025_marca,
  "tabla_ventas_hasta_marzo_2026_marca.csv"
)

write_csv(
  tabla_ventas_2025_tecnologia,
  "tabla_ventas_hasta_marzo_2026_tecnologia.csv"
)

write_csv(
  tabla_ventas_2025_marca_regional,
  "tabla_ventas_hasta_marzo_2026_marca_regional.csv"
)

write_csv(
  departamentos_sin_regional,
  "departamentos_sin_regional_ventas_hasta_marzo_2026.csv"
)

write_csv(
  departamentos_sin_ventas,
  "departamentos_sin_ventas_hasta_marzo_2026.csv"
)

write_csv(
  departamentos_ventas_no_cruzan_shp,
  "departamentos_ventas_hasta_marzo_2026_no_cruzan_shp.csv"
)
