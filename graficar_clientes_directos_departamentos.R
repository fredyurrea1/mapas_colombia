# Graficos de clientes directos por departamento.
#
# Todo el tratamiento de datos se hace desde el archivo original:
# - clientes_directos.xlsx
# - departamentos_colombia.shp
#
# En R/RStudio:
# source("graficar_clientes_directos_departamentos.R")

library(sf)
library(dplyr)
library(readxl)
library(readr)
library(ggplot2)
library(stringr)
library(stringi)
library(scales)

# -------------------------------------------------------------------------
# 1. Parametros y lectura de archivos originales
# -------------------------------------------------------------------------

archivo_clientes <- "clientes_directos.xlsx"
archivo_departamentos <- "departamentos_colombia.shp"
hoja_clientes <- "Hoja1"

archivos_requeridos <- c(archivo_clientes, archivo_departamentos)
faltantes <- archivos_requeridos[!file.exists(archivos_requeridos)]

if (length(faltantes) > 0) {
  stop(
    paste0(
      "No se encontraron estos archivos en el directorio actual: ",
      paste(faltantes, collapse = ", ")
    )
  )
}

clientes_original <- read_excel(archivo_clientes, sheet = hoja_clientes)

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
    depto == "SAN ANDRES" ~
      "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA",
    depto == "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA" ~
      "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA",
    TRUE ~ depto
  )
}

normalizar_codigo_depto <- function(x) {
  x <- str_remove_all(as.character(x), "[^0-9]")
  x <- if_else(is.na(x) | x == "", NA_character_, x)
  if_else(is.na(x), NA_character_, str_pad(x, width = 2, side = "left", pad = "0"))
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
# 3. Diccionarios de departamento y regional
# -------------------------------------------------------------------------

departamentos_dane <- tribble(
  ~codigo_depto, ~departamento_dane,
  "05", "Antioquia",
  "08", "Atlantico",
  "11", "Bogota",
  "13", "Bolivar",
  "15", "Boyaca",
  "17", "Caldas",
  "18", "Caqueta",
  "19", "Cauca",
  "20", "Cesar",
  "23", "Cordoba",
  "25", "Cundinamarca",
  "27", "Choco",
  "41", "Huila",
  "44", "La Guajira",
  "47", "Magdalena",
  "50", "Meta",
  "52", "Narino",
  "54", "Norte de Santander",
  "63", "Quindio",
  "66", "Risaralda",
  "68", "Santander",
  "70", "Sucre",
  "73", "Tolima",
  "76", "Valle del Cauca",
  "81", "Arauca",
  "85", "Casanare",
  "86", "Putumayo",
  "88", "San Andres",
  "91", "Amazonas",
  "94", "Guainia",
  "95", "Guaviare",
  "97", "Vaupes",
  "99", "Vichada"
) %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(departamento_dane))
  )

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
# 4. Tratamiento desde clientes_directos.xlsx
# -------------------------------------------------------------------------

clientes <- clientes_original %>%
  mutate(
    codigo_depto_codprov = normalizar_codigo_depto(CODPROV),
    codigo_depto_codine = str_sub(normalizar_codigo_depto(CODINE), 1, 2),
    codigo_depto = coalesce(codigo_depto_codprov, codigo_depto_codine)
  ) %>%
  left_join(departamentos_dane, by = "codigo_depto") %>%
  left_join(regionales_departamento, by = "depto_key") %>%
  transmute(
    id_cliente = limpiar_categoria(NUMID),
    poliza = limpiar_categoria(POLIZA),
    producto = limpiar_categoria(PRODUCTO),
    lob = limpiar_categoria(LOB),
    canal = limpiar_categoria(Canal),
    poblacion = str_to_title(limpiar_categoria(POBLACION)),
    codigo_depto,
    departamento = departamento_dane,
    depto_key,
    regional = if_else(is.na(regional), "Sin regional", regional),
    sexo = limpiar_categoria(SEXO),
    edad = as.numeric(Edad),
    antiguedad = as.numeric(Antigüedad),
    valor_cliente = as.numeric(Valor_de_Cliente),
    segmento_vc = limpiar_categoria(SEGMENTO_VC),
    marca = limpiar_categoria(Marca),
    clase = limpiar_categoria(Clase),
    referencia1 = limpiar_categoria(Referencia1),
    referencia2 = limpiar_categoria(Referencia2)
  )

# -------------------------------------------------------------------------
# 5. Tablas de exploracion
# -------------------------------------------------------------------------

resumen_clientes <- tibble(
  registros = nrow(clientes),
  clientes_unicos = n_distinct(clientes$id_cliente),
  polizas = n_distinct(clientes$poliza),
  departamentos = n_distinct(clientes$depto_key, na.rm = TRUE),
  poblaciones = n_distinct(clientes$poblacion, na.rm = TRUE)
)

tabla_departamento <- clientes %>%
  filter(!is.na(depto_key)) %>%
  group_by(depto_key, departamento, regional) %>%
  summarise(
    registros = n(),
    clientes = n_distinct(id_cliente),
    polizas = n_distinct(poliza),
    edad_promedio = mean(edad, na.rm = TRUE),
    valor_cliente_promedio = mean(valor_cliente, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    participacion_clientes = clientes / sum(clientes)
  ) %>%
  arrange(desc(clientes))

tabla_regional <- clientes %>%
  filter(!is.na(depto_key)) %>%
  group_by(regional) %>%
  summarise(
    registros = n(),
    clientes = n_distinct(id_cliente),
    polizas = n_distinct(poliza),
    edad_promedio = mean(edad, na.rm = TRUE),
    valor_cliente_promedio = mean(valor_cliente, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    participacion_clientes = clientes / sum(clientes)
  ) %>%
  arrange(desc(clientes))

tabla_poblacion <- clientes %>%
  filter(!is.na(depto_key)) %>%
  group_by(poblacion, departamento, depto_key, regional) %>%
  summarise(
    registros = n(),
    clientes = n_distinct(id_cliente),
    polizas = n_distinct(poliza),
    .groups = "drop"
  ) %>%
  arrange(desc(clientes))

clientes_sin_departamento <- clientes %>%
  filter(is.na(depto_key)) %>%
  count(poblacion, codigo_depto, name = "registros", sort = TRUE)

print(resumen_clientes)
print(head(tabla_departamento, 15))
print(head(tabla_poblacion, 20))
print(clientes_sin_departamento)

# -------------------------------------------------------------------------
# 6. Mapas por regional y departamento
# -------------------------------------------------------------------------

departamentos_mapa <- departamentos %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(DPTO_CNMBR))
  ) %>%
  left_join(tabla_departamento, by = "depto_key") %>%
  mutate(
    clientes = if_else(is.na(clientes), 0L, clientes),
    registros = if_else(is.na(registros), 0L, registros),
    polizas = if_else(is.na(polizas), 0L, polizas),
    regional = if_else(is.na(regional), "Sin regional", regional)
  )

departamentos_clientes_no_cruzan_shp <- tabla_departamento %>%
  anti_join(
    departamentos_mapa %>% st_drop_geometry() %>% select(depto_key),
    by = "depto_key"
  )

print("Departamentos de clientes que no cruzan con shapefile:")
print(departamentos_clientes_no_cruzan_shp)

regionales_mapa <- departamentos_mapa %>%
  group_by(regional) %>%
  summarise(
    clientes = sum(clientes, na.rm = TRUE),
    registros = sum(registros, na.rm = TRUE),
    polizas = sum(polizas, na.rm = TRUE),
    departamentos = n(),
    geometry = st_union(geometry),
    .groups = "drop"
  )

puntos_departamentos <- st_point_on_surface(departamentos_mapa)
puntos_regionales <- st_point_on_surface(regionales_mapa)

mapa_clientes_regional <- ggplot() +
  geom_sf(
    data = regionales_mapa,
    aes(fill = clientes),
    color = NA
  ) +
  geom_sf(
    data = departamentos_mapa,
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
    data = puntos_regionales %>% filter(clientes > 0),
    aes(
      label = paste0(
        regional,
        "\n",
        comma(clientes, big.mark = ".", decimal.mark = ",")
      )
    ),
    size = 3,
    color = "#1F2933"
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
    title = "Clientes directos por regional",
    subtitle = "Conteo de clientes unicos segun clientes_directos.xlsx",
    fill = "Clientes"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#1F2933"),
    plot.subtitle = element_text(size = 11, color = "#52616B"),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_clientes_departamento <- ggplot() +
  geom_sf(
    data = departamentos_mapa,
    aes(fill = clientes),
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf_text(
    data = puntos_departamentos %>% filter(clientes > 0),
    aes(label = comma(clientes, big.mark = ".", decimal.mark = ",")),
    size = 2.2,
    check_overlap = TRUE,
    color = "#1F2933"
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
    title = "Clientes directos por departamento",
    subtitle = "Conteo de clientes unicos segun clientes_directos.xlsx",
    fill = "Clientes"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#1F2933"),
    plot.subtitle = element_text(size = 11, color = "#52616B"),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

print(mapa_clientes_regional)
print(mapa_clientes_departamento)
guardar_png("mapa_clientes_directos_regional.png", mapa_clientes_regional, width = 9, height = 11)
guardar_png("mapa_clientes_directos_departamento.png", mapa_clientes_departamento, width = 9, height = 11)

# -------------------------------------------------------------------------
# 7. Graficos por regional y poblacion
# -------------------------------------------------------------------------

grafico_clientes_regional <- ggplot(
  tabla_regional,
  aes(x = reorder(regional, clientes), y = clientes, fill = regional)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes(
      label = paste0(
        comma(clientes, big.mark = ".", decimal.mark = ","),
        " (",
        percent(participacion_clientes, accuracy = 0.1, decimal.mark = ","),
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
    title = "Clientes directos por regional",
    subtitle = "Conteo de clientes unicos",
    x = NULL,
    y = "Clientes"
  ) +
  tema_presentacion()

grafico_clientes_departamento <- tabla_departamento %>%
  slice_max(clientes, n = 15) %>%
  ggplot(aes(x = reorder(departamento, clientes), y = clientes, fill = regional)) +
  geom_col(width = 0.68) +
  geom_text(
    aes(label = comma(clientes, big.mark = ".", decimal.mark = ",")),
    hjust = -0.08,
    size = 3.6,
    color = "#1F2933"
  ) +
  coord_flip() +
  scale_fill_manual(values = colores_regional) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Top departamentos con clientes directos",
    subtitle = "Conteo de clientes unicos",
    x = NULL,
    y = "Clientes",
    fill = "Regional"
  ) +
  tema_presentacion()

grafico_clientes_poblacion <- tabla_poblacion %>%
  slice_max(clientes, n = 20) %>%
  mutate(poblacion_label = paste0(poblacion, " - ", departamento)) %>%
  ggplot(aes(x = reorder(poblacion_label, clientes), y = clientes, fill = regional)) +
  geom_col(width = 0.68) +
  geom_text(
    aes(label = comma(clientes, big.mark = ".", decimal.mark = ",")),
    hjust = -0.08,
    size = 3.4,
    color = "#1F2933"
  ) +
  coord_flip() +
  scale_fill_manual(values = colores_regional) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Top poblaciones con clientes directos",
    subtitle = "Variable POBLACION del archivo original",
    x = NULL,
    y = "Clientes",
    fill = "Regional"
  ) +
  tema_presentacion()

print(grafico_clientes_regional)
print(grafico_clientes_departamento)
print(grafico_clientes_poblacion)

guardar_png("grafico_clientes_directos_regional.png", grafico_clientes_regional)
guardar_png("grafico_clientes_directos_departamento.png", grafico_clientes_departamento)
guardar_png("grafico_clientes_directos_poblacion.png", grafico_clientes_poblacion)

# -------------------------------------------------------------------------
# 8. Tablas de salida
# -------------------------------------------------------------------------

write_csv(resumen_clientes, "tabla_clientes_directos_resumen.csv")
write_csv(tabla_departamento, "tabla_clientes_directos_departamento.csv")
write_csv(tabla_regional, "tabla_clientes_directos_regional.csv")
write_csv(tabla_poblacion, "tabla_clientes_directos_poblacion.csv")
write_csv(clientes_sin_departamento, "tabla_clientes_directos_sin_departamento.csv")
write_csv(
  departamentos_clientes_no_cruzan_shp,
  "departamentos_clientes_directos_no_cruzan_shp.csv"
)
