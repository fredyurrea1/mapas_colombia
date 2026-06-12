# Script para graficar automoviles activos por municipio en Colombia.
#
# Supuestos:
# 1. La base RUNT ya esta cargada en un objeto llamado `runt`.
# 2. El shapefile municipal ya esta cargado en un objeto `municipios`.
# 3. Opcionalmente, el shapefile departamental puede estar cargado en `departamentos`.
#
# Columnas esperadas en `runt`:
# NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO, NOMBRE_DE_LA_CLASE,
# ESTADO_DEL_VEHICULO, CANTIDAD.
#
# Columnas esperadas en `municipios`:
# DPTO_CNMBR, MPIO_CNMBR, geometry.

library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(scales)

top_n <- 20

limpiar_texto <- function(x) {
  x %>%
    str_to_upper() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^A-Z0-9]+", " ") %>%
    str_squish()
}

# 1. Agregar automoviles activos por municipio
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

# 2. Preparar mapa municipal para emparejar por departamento + municipio
municipios_mapa <- municipios %>%
  st_transform(4326) %>%
  mutate(
    depto_key = limpiar_texto(DPTO_CNMBR),
    municipio_key = limpiar_texto(MPIO_CNMBR),
    llave = paste(depto_key, municipio_key, sep = "_")
  )

# 3. Unir RUNT con poligonos municipales
mapa_autos <- municipios_mapa %>%
  left_join(autos_municipio, by = "llave")

# 4. Revisar posibles municipios sin cruce
no_encontrados <- autos_municipio %>%
  anti_join(st_drop_geometry(municipios_mapa), by = "llave") %>%
  arrange(desc(automoviles))

print("Top municipios del RUNT que no cruzaron con el shapefile:")
print(head(no_encontrados, 20))

# 5. Mapa de intensidad por cantidad de automoviles
mapa_automoviles <- ggplot(mapa_autos) +
  geom_sf(
    aes(fill = automoviles),
    color = "white",
    linewidth = 0.04
  ) +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt",
    na.value = "#eeeeee",
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Automoviles activos por municipio",
    fill = "Automoviles"
  ) +
  theme_void()

# Si tienes `departamentos` cargado, se agregan sus bordes al mapa.
if (exists("departamentos")) {
  departamentos_mapa <- st_transform(departamentos, 4326)

  mapa_automoviles <- mapa_automoviles +
    geom_sf(
      data = departamentos_mapa,
      fill = NA,
      color = "#222222",
      linewidth = 0.25,
      inherit.aes = FALSE
    )
}

print(mapa_automoviles)

# 6. Mapa resaltando los municipios con mayor cantidad de automoviles
top_autos <- mapa_autos %>%
  filter(!is.na(automoviles)) %>%
  arrange(desc(automoviles)) %>%
  slice_head(n = top_n)

colores_top <- setNames(
  c("#e63946", "#e5e7eb"),
  c(paste0("Top ", top_n), "Otros municipios")
)

mapa_top_automoviles <- mapa_autos %>%
  mutate(
    grupo = if_else(
      llave %in% top_autos$llave,
      paste0("Top ", top_n),
      "Otros municipios"
    )
  ) %>%
  ggplot() +
  geom_sf(
    aes(fill = grupo),
    color = "white",
    linewidth = 0.04
  ) +
  scale_fill_manual(
    values = colores_top
  ) +
  labs(
    title = paste0("Top ", top_n, " municipios con mas automoviles activos"),
    fill = NULL
  ) +
  theme_void() +
  theme(legend.position = "bottom")

if (exists("departamentos")) {
  mapa_top_automoviles <- mapa_top_automoviles +
    geom_sf(
      data = departamentos_mapa,
      fill = NA,
      color = "#222222",
      linewidth = 0.25,
      inherit.aes = FALSE
    )
}

print(mapa_top_automoviles)

# 7. Tabla con el top de municipios
top_autos_tabla <- top_autos %>%
  st_drop_geometry() %>%
  select(DPTO_CNMBR, MPIO_CNMBR, automoviles) %>%
  arrange(desc(automoviles))

print(top_autos_tabla)

# Opcional: guardar imagenes
# ggsave("mapa_automoviles_municipios.png", mapa_automoviles, width = 9, height = 11, dpi = 300)
# ggsave("mapa_top_automoviles_municipios.png", mapa_top_automoviles, width = 9, height = 11, dpi = 300)
