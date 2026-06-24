# Graficos de presentacion: ventas por tecnologia y tendencias.
#
# Todo el tratamiento de datos se hace desde los archivos originales:
# - tabla_maestra.csv
# - departamentos_colombia.shp
#
# En R/RStudio:
# source("graficar_presentacion_tecnologias.R")

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(stringi)
library(scales)
library(tidyr)

# -------------------------------------------------------------------------
# 1. Parametros y lectura de archivos originales
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

ventas_original <- read_csv(
  archivo_ventas,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

departamentos <- st_read(archivo_departamentos, quiet = TRUE) %>%
  st_transform(4326)

# -------------------------------------------------------------------------
# 2. Funciones de limpieza, categorias y estilo
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

clasificar_tecnologia <- function(tecnologia_key) {
  case_when(
    tecnologia_key == "ELECTRICO" ~ "Electricos",
    tecnologia_key %in% c("HIBRIDO ENCHUFABLE", "HIBRIDO NO ENCHUFABLE") ~ "Hibridos",
    tecnologia_key == "DIESEL" ~ "Diesel",
    tecnologia_key == "GASOLINA" ~ "Gasolina",
    tecnologia_key %in% c("GNV", "GAS CONVERTIDO") ~ "Gas",
    TRUE ~ "Otros"
  )
}

colores_tecnologia <- c(
  "Electricos" = "#00A6A6",
  "Hibridos" = "#7CB342",
  "Diesel" = "#5C6670",
  "Gasolina" = "#F28E2B",
  "Gas" = "#8E6CFF",
  "Otros" = "#9E9E9E"
)

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
# 4. Tratamiento desde tabla_maestra.csv
# -------------------------------------------------------------------------

ventas <- ventas_original %>%
  mutate(
    anio = as.integer(anio),
    periodo = as.integer(periodo),
    unidades = as.numeric(unidades),
    mes_num = as.integer(str_sub(as.character(periodo), 5, 6)),
    fecha_mes = as.Date(sprintf("%04d-%02d-01", anio, mes_num)),
    depto_key = normalizar_depto(limpiar_texto(departamento)),
    tecnologia_key = limpiar_texto(tecnologia),
    tecnologia_grupo = clasificar_tecnologia(tecnologia_key)
  ) %>%
  left_join(regionales_departamento, by = "depto_key") %>%
  mutate(
    regional = if_else(is.na(regional), "Sin regional", regional)
  )

ventas_2025 <- ventas %>%
  filter(periodo <= periodo_fin)

# -------------------------------------------------------------------------
# 4. Tablas base para mapas por departamento
# -------------------------------------------------------------------------

ventas_depto_tecnologia <- ventas_2025 %>%
  group_by(depto_key, tecnologia_grupo) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    .groups = "drop"
  )

ventas_depto_total <- ventas_2025 %>%
  group_by(depto_key) %>%
  summarise(
    ventas_totales = sum(unidades, na.rm = TRUE),
    .groups = "drop"
  )

departamentos_base <- departamentos %>%
  mutate(
    depto_key = normalizar_depto(limpiar_texto(DPTO_CNMBR))
  ) %>%
  left_join(ventas_depto_total, by = "depto_key") %>%
  mutate(
    ventas_totales = if_else(is.na(ventas_totales), 0, ventas_totales)
  )

ventas_no_cruzan_shp <- ventas_depto_total %>%
  anti_join(
    departamentos_base %>% st_drop_geometry() %>% select(depto_key),
    by = "depto_key"
  )

print("Departamentos de tabla_maestra.csv que no cruzan con shapefile:")
print(ventas_no_cruzan_shp)

crear_mapa_tecnologia <- function(tecnologia_objetivo, titulo, archivo_salida) {
  datos_tecnologia <- ventas_depto_tecnologia %>%
    filter(tecnologia_grupo == tecnologia_objetivo) %>%
    rename(ventas_tecnologia = ventas)

  mapa_datos <- departamentos_base %>%
    left_join(datos_tecnologia, by = "depto_key") %>%
    mutate(
      ventas_tecnologia = if_else(is.na(ventas_tecnologia), 0, ventas_tecnologia),
      participacion = if_else(
        ventas_totales > 0,
        ventas_tecnologia / ventas_totales,
        0
      )
    )

  puntos <- st_point_on_surface(mapa_datos)

  mapa <- ggplot() +
    geom_sf(
      data = mapa_datos,
      aes(fill = ventas_tecnologia),
      color = "white",
      linewidth = 0.25
    ) +
    geom_sf_text(
      data = puntos %>% filter(ventas_tecnologia > 0),
      aes(label = comma(ventas_tecnologia, big.mark = ".", decimal.mark = ",")),
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
      title = titulo,
      subtitle = paste0("Unidades vendidas por departamento, ", etiqueta_periodo),
      fill = "Unidades"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 18, color = "#1F2933"),
      plot.subtitle = element_text(size = 11, color = "#52616B"),
      legend.position = "right",
      plot.background = element_rect(fill = "white", color = NA)
    )

  guardar_png(archivo_salida, mapa, width = 9, height = 11)
  mapa
}

mapa_electricos <- crear_mapa_tecnologia(
  "Electricos",
  "Ventas de carros electricos",
  "mapa_ventas_hasta_marzo_2026_electricos_departamento.png"
)

mapa_hibridos <- crear_mapa_tecnologia(
  "Hibridos",
  "Ventas de carros hibridos",
  "mapa_ventas_hasta_marzo_2026_hibridos_departamento.png"
)

mapa_diesel <- crear_mapa_tecnologia(
  "Diesel",
  "Ventas de carros diesel",
  "mapa_ventas_hasta_marzo_2026_diesel_departamento.png"
)

mapa_gasolina <- crear_mapa_tecnologia(
  "Gasolina",
  "Ventas de carros gasolina",
  "mapa_ventas_hasta_marzo_2026_gasolina_departamento.png"
)

print(mapa_electricos)
print(mapa_hibridos)
print(mapa_diesel)
print(mapa_gasolina)

# -------------------------------------------------------------------------
# 5. Tendencias mensuales de venta
# -------------------------------------------------------------------------

tendencia_total <- ventas_2025 %>%
  group_by(fecha_mes) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(fecha_mes) %>%
  mutate(
    variacion_mensual = ventas / lag(ventas) - 1
  )

tendencia_tecnologia <- ventas_2025 %>%
  group_by(fecha_mes, tecnologia_grupo) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(fecha_mes) %>%
  mutate(
    ventas_mes = sum(ventas, na.rm = TRUE),
    participacion_mes = ventas / ventas_mes
  ) %>%
  ungroup() %>%
  arrange(fecha_mes, tecnologia_grupo)

tendencia_tecnologia_principal <- tendencia_tecnologia %>%
  filter(tecnologia_grupo %in% c("Electricos", "Hibridos", "Diesel", "Gasolina"))

grafico_tendencia_total <- ggplot(
  tendencia_total %>% filter(!is.na(variacion_mensual)),
  aes(x = fecha_mes, y = variacion_mensual)
) +
  geom_hline(yintercept = 0, color = "#A7B0B8", linewidth = 0.45) +
  geom_line(color = "#005F73", linewidth = 1.4) +
  geom_point(color = "#0A9396", size = 2.8) +
  geom_text(
    aes(label = percent(variacion_mensual, accuracy = 0.1, decimal.mark = ",")),
    vjust = -0.8,
    size = 3.2,
    color = "#1F2933"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1, decimal.mark = ","),
    expand = expansion(mult = c(0.02, 0.14))
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  labs(
    title = "Variacion mensual de ventas de carros",
    subtitle = paste0("Cambio porcentual mes a mes, ", etiqueta_periodo),
    x = NULL,
    y = "Variacion mensual"
  ) +
  tema_presentacion()

grafico_tendencia_tecnologia <- ggplot(
  tendencia_tecnologia_principal,
  aes(x = fecha_mes, y = participacion_mes, color = tecnologia_grupo)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.4) +
  scale_color_manual(values = colores_tecnologia) +
  scale_y_continuous(labels = percent_format(accuracy = 1, decimal.mark = ",")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  labs(
    title = "Tendencia mensual por tecnologia",
    subtitle = "Participacion mensual sobre ventas totales",
    x = NULL,
    y = "Participacion mensual",
    color = "Tecnologia"
  ) +
  tema_presentacion()

grafico_tendencia_tecnologia_facet <- ggplot(
  tendencia_tecnologia_principal,
  aes(x = fecha_mes, y = participacion_mes, color = tecnologia_grupo)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.2) +
  facet_wrap(~tecnologia_grupo, scales = "free_y", ncol = 2) +
  scale_color_manual(values = colores_tecnologia, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1, decimal.mark = ",")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
  labs(
    title = "Tendencia mensual por tecnologia",
    subtitle = "Participacion mensual con escalas independientes",
    x = NULL,
    y = "Participacion mensual"
  ) +
  tema_presentacion() +
  theme(strip.text = element_text(face = "bold", color = "#1F2933"))

mix_tecnologia_2025 <- ventas_2025 %>%
  group_by(tecnologia_grupo) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    participacion = ventas / sum(ventas),
    tecnologia_grupo = factor(
      tecnologia_grupo,
      levels = names(sort(colores_tecnologia))
    )
  ) %>%
  arrange(desc(ventas))

grafico_mix_tecnologia_2025 <- ggplot(
  mix_tecnologia_2025,
  aes(x = reorder(tecnologia_grupo, ventas), y = ventas, fill = tecnologia_grupo)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes(
      label = paste0(
        comma(ventas, big.mark = ".", decimal.mark = ","),
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
  scale_fill_manual(values = colores_tecnologia, guide = "none") +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.22))
  ) +
  labs(
    title = "Mix de ventas por tecnologia",
    subtitle = paste0("Participacion sobre ventas totales, ", etiqueta_periodo),
    x = NULL,
    y = "Unidades"
  ) +
  tema_presentacion()

print(grafico_tendencia_total)
print(grafico_tendencia_tecnologia)
print(grafico_tendencia_tecnologia_facet)
print(grafico_mix_tecnologia_2025)

guardar_png("grafico_tendencia_ventas_total.png", grafico_tendencia_total)
guardar_png("grafico_tendencia_ventas_tecnologia.png", grafico_tendencia_tecnologia)
guardar_png("grafico_tendencia_ventas_tecnologia_facet.png", grafico_tendencia_tecnologia_facet)
guardar_png("grafico_mix_tecnologia_hasta_marzo_2026.png", grafico_mix_tecnologia_2025)

# -------------------------------------------------------------------------
# 6. Ventas por regional
# -------------------------------------------------------------------------

ventas_2025_regional <- ventas_2025 %>%
  group_by(regional) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    participacion = ventas / sum(ventas)
  ) %>%
  arrange(desc(ventas))

cotizaciones_regional <- tribble(
  ~regional, ~cotizaciones,
  "Bogota", 34176,
  "Antioquia y eje", 23924,
  "Occidente y centro", 9401,
  "Oriente", 4234,
  "Costa atlantica", 2988,
  "Resto del pais", 0
)

vendidos_comparables_regional <- tribble(
  ~regional, ~ventas,
  "Antioquia y eje", 72949,
  "Bogota", 58289,
  "Occidente y centro", 42062,
  "Oriente", 28599,
  "Costa atlantica", 8014,
  "Resto del pais", 1908
)

comparacion_ventas_cotizaciones_regional <- vendidos_comparables_regional %>%
  left_join(cotizaciones_regional, by = "regional") %>%
  mutate(
    cotizaciones = if_else(is.na(cotizaciones), 0, cotizaciones),
    tasa_cotizacion = cotizaciones / ventas,
    diferencia_vendidos_cotizados = ventas - cotizaciones
  ) %>%
  arrange(desc(ventas))

ventas_2025_regional_tecnologia <- ventas_2025 %>%
  filter(tecnologia_grupo %in% c("Electricos", "Hibridos", "Diesel", "Gasolina")) %>%
  group_by(regional, tecnologia_grupo) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(regional) %>%
  mutate(
    participacion_regional = ventas / sum(ventas)
  ) %>%
  ungroup()

tendencia_regional <- ventas_2025 %>%
  group_by(fecha_mes, regional) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(fecha_mes) %>%
  mutate(
    ventas_mes = sum(ventas, na.rm = TRUE),
    participacion_mes = ventas / ventas_mes
  ) %>%
  ungroup() %>%
  arrange(fecha_mes, regional)

tendencia_regional_tecnologia <- ventas_2025 %>%
  filter(tecnologia_grupo %in% c("Electricos", "Hibridos", "Diesel", "Gasolina")) %>%
  group_by(fecha_mes, regional, tecnologia_grupo) %>%
  summarise(
    ventas = sum(unidades, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(fecha_mes, regional, tecnologia_grupo)

grafico_ventas_2025_regional <- ggplot(
  ventas_2025_regional,
  aes(x = reorder(regional, ventas), y = ventas, fill = regional)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes(
      label = paste0(
        comma(ventas, big.mark = ".", decimal.mark = ","),
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
    title = "Ventas de carros por regional",
    subtitle = paste0("Unidades vendidas, ", etiqueta_periodo),
    x = NULL,
    y = "Unidades"
  ) +
  tema_presentacion()

grafico_ventas_2025_regional_tecnologia <- ggplot(
  ventas_2025_regional_tecnologia,
  aes(x = reorder(regional, ventas, sum), y = ventas, fill = tecnologia_grupo)
) +
  geom_col(width = 0.68) +
  coord_flip() +
  scale_fill_manual(values = colores_tecnologia) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Ventas por regional y tecnologia",
    subtitle = paste0("Electricos, hibridos, diesel y gasolina, ", etiqueta_periodo),
    x = NULL,
    y = "Unidades",
    fill = "Tecnologia"
  ) +
  tema_presentacion()

grafico_mix_regional_tecnologia_2025 <- ggplot(
  ventas_2025_regional_tecnologia,
  aes(x = regional, y = participacion_regional, fill = tecnologia_grupo)
) +
  geom_col(width = 0.72) +
  coord_flip() +
  scale_fill_manual(values = colores_tecnologia) +
  scale_y_continuous(labels = percent_format(accuracy = 1, decimal.mark = ",")) +
  labs(
    title = "Mix tecnologico por regional",
    subtitle = paste0("Participacion dentro de cada regional, ", etiqueta_periodo),
    x = NULL,
    y = "Participacion",
    fill = "Tecnologia"
  ) +
  tema_presentacion()

grafico_tendencia_regional <- ggplot(
  tendencia_regional,
  aes(x = fecha_mes, y = participacion_mes, color = regional)
) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.1) +
  scale_color_manual(values = colores_regional) +
  scale_y_continuous(labels = percent_format(accuracy = 1, decimal.mark = ",")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  labs(
    title = "Participacion mensual de ventas por regional",
    subtitle = paste0("Peso de cada regional sobre las ventas mensuales, ", etiqueta_periodo),
    x = NULL,
    y = "Participacion mensual",
    color = "Regional"
  ) +
  tema_presentacion()

grafico_tendencia_regional_facet <- ggplot(
  tendencia_regional,
  aes(x = fecha_mes, y = participacion_mes, color = regional)
) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.1) +
  facet_wrap(~regional, scales = "free_y", ncol = 2) +
  scale_color_manual(values = colores_regional, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1, decimal.mark = ",")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
  labs(
    title = "Participacion mensual por regional",
    subtitle = "Escalas independientes para mostrar cambios de peso relativo",
    x = NULL,
    y = "Participacion mensual"
  ) +
  tema_presentacion() +
  theme(strip.text = element_text(face = "bold", color = "#1F2933"))

regionales_comparacion_mapa <- departamentos_base %>%
  group_by(regional) %>%
  summarise(
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>%
  left_join(comparacion_ventas_cotizaciones_regional, by = "regional") %>%
  mutate(
    ventas = if_else(is.na(ventas), 0, ventas),
    cotizaciones = if_else(is.na(cotizaciones), 0, cotizaciones),
    tasa_cotizacion = if_else(is.na(tasa_cotizacion), 0, tasa_cotizacion)
  )

puntos_regionales_comparacion <- st_point_on_surface(regionales_comparacion_mapa)

mapa_vendidos_regional <- ggplot() +
  geom_sf(
    data = regionales_comparacion_mapa,
    aes(fill = ventas),
    color = "#222222",
    linewidth = 0.65
  ) +
  geom_sf(
    data = departamentos_base,
    fill = NA,
    color = "white",
    linewidth = 0.3
  ) +
  geom_sf_text(
    data = puntos_regionales_comparacion %>% filter(ventas > 0),
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
    title = "Vehiculos vendidos por regional",
    subtitle = paste0("Unidades vendidas, ", etiqueta_periodo),
    fill = "Vendidos"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#1F2933"),
    plot.subtitle = element_text(size = 11, color = "#52616B"),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_cotizados_regional <- ggplot() +
  geom_sf(
    data = regionales_comparacion_mapa,
    aes(fill = cotizaciones),
    color = "#222222",
    linewidth = 0.65
  ) +
  geom_sf(
    data = departamentos_base,
    fill = NA,
    color = "white",
    linewidth = 0.3
  ) +
  geom_sf_text(
    data = puntos_regionales_comparacion %>% filter(cotizaciones > 0),
    aes(
      label = paste0(
        regional,
        "\n",
        comma(cotizaciones, big.mark = ".", decimal.mark = ",")
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
    title = "Vehiculos cotizados por regional",
    subtitle = "Cotizaciones entrantes del canal directo",
    fill = "Cotizados"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#1F2933"),
    plot.subtitle = element_text(size = 11, color = "#52616B"),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

mapa_tasa_cotizacion_regional <- ggplot() +
  geom_sf(
    data = regionales_comparacion_mapa,
    aes(fill = tasa_cotizacion),
    color = "#222222",
    linewidth = 0.65
  ) +
  geom_sf(
    data = departamentos_base,
    fill = NA,
    color = "white",
    linewidth = 0.3
  ) +
  geom_sf_text(
    data = puntos_regionales_comparacion %>% filter(ventas > 0),
    aes(
      label = paste0(
        regional,
        "\n",
        percent(tasa_cotizacion, accuracy = 0.1, decimal.mark = ",")
      )
    ),
    size = 3
  ) +
  scale_fill_viridis_c(
    option = "D",
    labels = percent_format(accuracy = 0.1, decimal.mark = ","),
    guide = guide_colorbar(
      direction = "vertical",
      barheight = grid::unit(70, "mm"),
      barwidth = grid::unit(5, "mm")
    )
  ) +
  labs(
    title = "Cotizaciones sobre vehiculos vendidos",
    subtitle = "Cotizados / vendidos por regional",
    fill = "Cotizados /\nvendidos"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#1F2933"),
    plot.subtitle = element_text(size = 11, color = "#52616B"),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

print(grafico_ventas_2025_regional)
print(grafico_ventas_2025_regional_tecnologia)
print(grafico_mix_regional_tecnologia_2025)
print(grafico_tendencia_regional)
print(grafico_tendencia_regional_facet)
print(mapa_vendidos_regional)
print(mapa_cotizados_regional)
print(mapa_tasa_cotizacion_regional)

guardar_png("grafico_ventas_hasta_marzo_2026_regional.png", grafico_ventas_2025_regional)
guardar_png("grafico_ventas_hasta_marzo_2026_regional_tecnologia.png", grafico_ventas_2025_regional_tecnologia)
guardar_png("grafico_mix_regional_tecnologia_hasta_marzo_2026.png", grafico_mix_regional_tecnologia_2025)
guardar_png("grafico_tendencia_ventas_regional.png", grafico_tendencia_regional)
guardar_png("grafico_tendencia_ventas_regional_facet.png", grafico_tendencia_regional_facet)
guardar_png("mapa_comparacion_vendidos_regional.png", mapa_vendidos_regional, width = 9, height = 11)
guardar_png("mapa_comparacion_cotizados_regional.png", mapa_cotizados_regional, width = 9, height = 11)
guardar_png("mapa_comparacion_tasa_cotizacion_regional.png", mapa_tasa_cotizacion_regional, width = 9, height = 11)

# -------------------------------------------------------------------------
# 7. Tablas de salida para revisar cifras
# -------------------------------------------------------------------------

tabla_ventas_2025_departamento_tecnologia <- ventas_depto_tecnologia %>%
  pivot_wider(
    names_from = tecnologia_grupo,
    values_from = ventas,
    values_fill = 0
  ) %>%
  left_join(
    departamentos %>%
      st_drop_geometry() %>%
      transmute(
        departamento_shp = DPTO_CNMBR,
        depto_key = normalizar_depto(limpiar_texto(DPTO_CNMBR))
      ),
    by = "depto_key"
  ) %>%
  relocate(departamento_shp, depto_key) %>%
  arrange(departamento_shp)

tabla_tendencia_mensual_tecnologia <- tendencia_tecnologia %>%
  arrange(fecha_mes, tecnologia_grupo)

tabla_resumen_tecnologia_2025 <- mix_tecnologia_2025 %>%
  mutate(
    participacion = round(participacion, 4)
  ) %>%
  arrange(desc(ventas))

write_csv(
  tabla_ventas_2025_departamento_tecnologia,
  "tabla_ventas_hasta_marzo_2026_departamento_tecnologia.csv"
)

write_csv(
  tabla_tendencia_mensual_tecnologia,
  "tabla_tendencia_mensual_tecnologia.csv"
)

write_csv(
  tabla_resumen_tecnologia_2025,
  "tabla_resumen_tecnologia_hasta_marzo_2026.csv"
)

write_csv(
  ventas_2025_regional,
  "tabla_ventas_hasta_marzo_2026_regional.csv"
)

write_csv(
  comparacion_ventas_cotizaciones_regional,
  "tabla_comparacion_ventas_cotizaciones_regional.csv"
)

write_csv(
  ventas_2025_regional_tecnologia,
  "tabla_ventas_hasta_marzo_2026_regional_tecnologia.csv"
)

write_csv(
  tendencia_regional,
  "tabla_tendencia_mensual_regional.csv"
)

write_csv(
  tendencia_regional_tecnologia,
  "tabla_tendencia_mensual_regional_tecnologia.csv"
)

write_csv(
  ventas_no_cruzan_shp,
  "departamentos_presentacion_no_cruzan_shp.csv"
)
