# Grafica automoviles activos por municipio para cada regional Allianz.
#
# Crea un mapa por regional, a nivel municipal:
# - municipios con automoviles = 0 quedan en blanco
# - municipios con automoviles > 0 usan barra de color
# - no se escriben numeros sobre el mapa
#
# Instrucciones:
# 1. Poner este script en el mismo directorio de los archivos:
#    - CRECIMIENTO_DEL_PARQUE_AUTOMOTOR_RUNT2.0_20260612.csv
#    - municipios_colombia.shp
#    - departamentos_colombia.shp
# 2. En R/RStudio, establecer ese directorio como working directory.
# 3. Ejecutar: source("graficar_municipios_por_regional.R")

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
# 2. Limpieza y homologacion
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

limpiar_nombre_archivo <- function(x) {
  limpiar_texto(x) %>%
    str_to_lower() %>%
    str_replace_all(" ", "_")
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
# 4. Base de automoviles activos por municipio
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
  group_by(llave, depto_key, NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO) %>%
  summarise(
    automoviles = sum(CANTIDAD, na.rm = TRUE),
    .groups = "drop"
  )

municipios_base <- municipios %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(DPTO_CNMBR)),
    municipio_key = limpiar_texto(MPIO_CNMBR),
    municipio_key = normalizar_municipio(depto_key, municipio_key),
    llave = paste(depto_key, municipio_key, sep = "_")
  ) %>%
  left_join(regionales_departamento, by = "depto_key") %>%
  left_join(autos_municipio, by = "llave") %>%
  mutate(
    regional = if_else(is.na(regional), "Sin regional", regional),
    tiene_dato_runt = if_else(is.na(automoviles), "NO", "SI"),
    automoviles = if_else(is.na(automoviles), 0, automoviles),
    automoviles_mapa = if_else(automoviles == 0, NA_real_, automoviles)
  )

departamentos_base <- departamentos %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(DPTO_CNMBR))
  ) %>%
  left_join(regionales_departamento, by = "depto_key") %>%
  mutate(
    regional = if_else(is.na(regional), "Sin regional", regional)
  )

# -------------------------------------------------------------------------
# 5. Funcion para graficar una regional
# -------------------------------------------------------------------------

graficar_regional <- function(regional_objetivo) {
  nombre_archivo <- limpiar_nombre_archivo(regional_objetivo)

  municipios_regional <- municipios_base %>%
    filter(regional == regional_objetivo)

  departamentos_regional <- departamentos_base %>%
    filter(regional == regional_objetivo)

  limite_regional <- departamentos_regional %>%
    summarise(
      regional = regional_objetivo,
      geometry = st_union(geometry),
      .groups = "drop"
    )

  no_encontrados <- autos_municipio %>%
    semi_join(
      regionales_departamento %>% filter(regional == regional_objetivo),
      by = "depto_key"
    ) %>%
    anti_join(st_drop_geometry(municipios_regional), by = "llave") %>%
    transmute(
      departamento_runt = NOMBRE_DEPARTAMENTO,
      municipio_runt = NOMBRE_MUNICIPIO,
      automoviles,
      llave
    ) %>%
    arrange(desc(automoviles), departamento_runt, municipio_runt)

  mapa <- ggplot() +
    geom_sf(
      data = municipios_regional,
      aes(fill = automoviles_mapa),
      color = "#d9d9d9",
      linewidth = 0.08
    ) +
    geom_sf(
      data = departamentos_regional,
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
      na.value = "white",
      labels = label_number(big.mark = ".", decimal.mark = ",")
    ) +
    labs(
      title = "Automoviles activos por municipio",
      subtitle = paste0("Regional ", regional_objetivo),
      fill = "Automoviles"
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )

  print(mapa)

  ggsave(
    paste0("mapa_municipios_regional_", nombre_archivo, ".png"),
    mapa,
    width = 9,
    height = 8,
    dpi = 300
  )

  tabla_municipios <- municipios_regional %>%
    st_drop_geometry() %>%
    select(
      regional,
      departamento = DPTO_CNMBR,
      municipio = MPIO_CNMBR,
      tiene_dato_runt,
      automoviles
    ) %>%
    arrange(desc(automoviles), departamento, municipio)

  write_csv(
    tabla_municipios,
    paste0("tabla_municipios_regional_", nombre_archivo, ".csv")
  )

  write_csv(
    no_encontrados,
    paste0("municipios_runt_no_cruzan_shp_regional_", nombre_archivo, ".csv")
  )

  invisible(mapa)
}

# -------------------------------------------------------------------------
# 6. Generar mapas para todas las regionales
# -------------------------------------------------------------------------

regionales <- regionales_departamento %>%
  distinct(regional) %>%
  arrange(regional) %>%
  pull(regional)

mapas_regionales <- lapply(regionales, graficar_regional)

tabla_resumen_regionales <- municipios_base %>%
  st_drop_geometry() %>%
  filter(regional %in% regionales) %>%
  group_by(regional) %>%
  summarise(
    municipios = n(),
    municipios_con_dato = sum(tiene_dato_runt == "SI"),
    municipios_sin_dato = sum(tiene_dato_runt == "NO"),
    automoviles = sum(automoviles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(regional)

print(tabla_resumen_regionales)

write_csv(
  tabla_resumen_regionales,
  "tabla_resumen_municipios_regionales.csv"
)
