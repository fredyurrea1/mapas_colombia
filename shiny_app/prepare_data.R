app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(app_dir, "prepare_data.R"))) {
  app_dir <- normalizePath(file.path(getwd(), "shiny_app"), winslash = "/", mustWork = TRUE)
}

user_lib <- path.expand(Sys.getenv("R_LIBS_USER"))
if (dir.exists(user_lib)) .libPaths(c(user_lib, .libPaths()))

required <- c("sf", "dplyr", "readr", "stringi")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Faltan paquetes de R: ", paste(missing, collapse = ", "),
    ". Ejecute install_dependencies.R antes de preparar los datos."
  )
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(stringi)
})

root <- normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(app_dir, "data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

source_path <- function(name) {
  path <- file.path(root, name)
  if (!file.exists(path)) stop("No se encontró la fuente requerida: ", path)
  path
}

clean_text <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[^A-Z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

normalize_dept <- function(x) {
  x <- clean_text(x)
  x[x %in% c("BOGOTA", "BOGOTA D C", "BOGOTA DC")] <- "BOGOTA D C"
  x[x == "NORTE DE SANTADER"] <- "NORTE DE SANTANDER"
  x[x == "CASANERO"] <- "CASANARE"
  x[x == "GUIAJIRA"] <- "LA GUAJIRA"
  x[x %in% c(
    "SAN ANDRES",
    "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA"
  )] <- "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA"
  x
}

normalize_muni <- function(dept, muni) {
  dept <- normalize_dept(dept)
  muni <- clean_text(muni)
  muni[dept == "BOGOTA D C" & muni == "BOGOTA"] <- "BOGOTA D C"
  muni[dept == "BOLIVAR" & muni == "CARTAGENA"] <- "CARTAGENA DE INDIAS"
  muni[dept == "NORTE DE SANTANDER" & muni == "CUCUTA"] <- "SAN JOSE DE CUCUTA"
  muni[dept == "TOLIMA" & muni == "MARIQUITA"] <- "SAN SEBASTIAN DE MARIQUITA"
  muni[dept == "CUNDINAMARCA" & muni == "UBATE"] <- "VILLA DE SAN DIEGO DE UBATE"
  muni[dept == "CAUCA" & muni == "PIENDAMO"] <- "PIENDAMO TUNIA"
  muni[dept == "NARINO" & muni == "TUMACO"] <- "SAN ANDRES DE TUMACO"
  muni[dept == "ANTIOQUIA" & muni == "SANTAFE DE ANTIOQUIA"] <- "SANTA FE DE ANTIOQUIA"
  muni[dept == "ANTIOQUIA" & muni == "DON MATIAS"] <- "DONMATIAS"
  muni[dept == "SUCRE" & muni == "SINCE"] <- "SAN LUIS DE SINCE"
  muni
}

region_for <- function(dept_key) {
  dplyr::case_when(
    dept_key %in% c("ANTIOQUIA", "CALDAS", "QUINDIO", "RISARALDA") ~ "Antioquia y eje",
    dept_key %in% c("BOGOTA D C", "CUNDINAMARCA") ~ "Bogota",
    dept_key %in% c("ATLANTICO", "BOLIVAR", "CESAR", "CORDOBA", "LA GUAJIRA", "MAGDALENA", "SUCRE") ~ "Costa atlantica",
    dept_key %in% c("VALLE DEL CAUCA", "TOLIMA", "HUILA", "NARINO", "CAUCA") ~ "Occidente y centro",
    dept_key %in% c("SANTANDER", "NORTE DE SANTANDER", "BOYACA", "META") ~ "Oriente",
    TRUE ~ "Resto del pais"
  )
}

safe_rate <- function(num, den, multiplier = 1) {
  ifelse(!is.na(den) & den > 0, multiplier * num / den, NA_real_)
}

rank01 <- function(x) {
  out <- rep(0, length(x))
  ok <- is.finite(x)
  if (sum(ok) > 1) out[ok] <- dplyr::percent_rank(x[ok])
  if (sum(ok) == 1) out[ok] <- 1
  out
}

add_metrics <- function(x) {
  technology_columns <- c("Diesel", "Gasolina", "Hibridos", "Electricos", "Gas")
  for (column in technology_columns) {
    if (!column %in% names(x)) x[[column]] <- 0
  }

  x <- x %>%
    mutate(
      automoviles = coalesce(as.numeric(automoviles), 0),
      leads = coalesce(as.numeric(leads), 0),
      emitidos = coalesce(as.numeric(emitidos), 0),
      clientes = coalesce(as.numeric(clientes), 0),
      polizas = coalesce(as.numeric(polizas), 0),
      ventas = coalesce(as.numeric(ventas), 0),
      Diesel = coalesce(as.numeric(Diesel), 0),
      Gasolina = coalesce(as.numeric(Gasolina), 0),
      Hibridos = coalesce(as.numeric(Hibridos), 0),
      Electricos = coalesce(as.numeric(Electricos), 0),
      Gas = coalesce(as.numeric(Gas), 0),
      leads_per_1000 = safe_rate(leads, automoviles, 1000),
      emitidos_per_1000 = safe_rate(emitidos, automoviles, 1000),
      polizas_per_1000 = safe_rate(polizas, automoviles, 1000),
      conversion = safe_rate(emitidos, leads, 1),
      share_diesel = safe_rate(Diesel, ventas, 1),
      share_gasolina = safe_rate(Gasolina, ventas, 1),
      share_hibridos = safe_rate(Hibridos, ventas, 1),
      share_electricos = safe_rate(Electricos, ventas, 1),
      share_gas = safe_rate(Gas, ventas, 1),
      presencia = 100 * (
        0.45 * rank01(leads_per_1000) +
          0.35 * rank01(polizas_per_1000) +
          0.20 * rank01(log1p(leads))
      )
    )

  market_cut <- stats::median(x$automoviles[x$automoviles > 0], na.rm = TRUE)
  presence_cut <- stats::median(x$presencia[x$automoviles > 0], na.rm = TRUE)

  x %>%
    mutate(
      oportunidad = case_when(
        automoviles >= market_cut & presencia >= presence_cut ~ "Defender",
        automoviles >= market_cut & presencia < presence_cut ~ "Prioridad comercial",
        automoviles < market_cut & presencia >= presence_cut ~ "Nicho consolidado",
        TRUE ~ "Menor prioridad"
      )
    )
}

message("Leyendo y simplificando geometría municipal...")
municipal_raw <- st_read(source_path("municipios_colombia.shp"), quiet = TRUE) %>%
  st_transform(4326) %>%
  mutate(
    dane_muni = as.character(MPIO_CCNCT),
    dane_dept = as.character(DPTO_CCDGO),
    dept_key = normalize_dept(DPTO_CNMBR),
    muni_key = normalize_muni(DPTO_CNMBR, MPIO_CNMBR),
    department = as.character(DPTO_CNMBR),
    municipality = as.character(MPIO_CNMBR)
  ) %>%
  group_by(dane_muni, dane_dept, dept_key, muni_key) %>%
  summarise(
    department = first(department),
    municipality = first(municipality),
    .groups = "drop"
  ) %>%
  st_make_valid()

# Simplificar en metros y volver a WGS84. La geometría original queda intacta.
municipal_sf <- municipal_raw %>%
  st_transform(3116) %>%
  st_simplify(dTolerance = 650, preserveTopology = TRUE) %>%
  st_transform(4326) %>%
  mutate(regional = region_for(dept_key))

message("Consolidando parque automotor...")
autos <- read_csv(source_path("tabla_cobertura_municipios_runt.csv"), show_col_types = FALSE) %>%
  transmute(
    dept_key = normalize_dept(departamento),
    muni_key = normalize_muni(departamento, municipio),
    automoviles = as.numeric(automoviles)
  ) %>%
  group_by(dept_key, muni_key) %>%
  summarise(automoviles = max(automoviles, na.rm = TRUE), .groups = "drop")

message("Consolidando cotizaciones y clientes...")
leads_muni <- read_csv(source_path("tabla_leads_ciudad.csv"), show_col_types = FALSE) %>%
  transmute(
    dept_key = normalize_dept(depto_key),
    muni_key = normalize_muni(depto_key, ciudad),
    leads = as.numeric(leads)
  ) %>%
  group_by(dept_key, muni_key) %>%
  summarise(leads = sum(leads, na.rm = TRUE), .groups = "drop")

clients_muni <- read_csv(source_path("tabla_clientes_directos_poblacion.csv"), show_col_types = FALSE) %>%
  transmute(
    dept_key = normalize_dept(depto_key),
    muni_key = normalize_muni(depto_key, poblacion),
    clientes = as.numeric(clientes),
    polizas = as.numeric(polizas)
  ) %>%
  group_by(dept_key, muni_key) %>%
  summarise(
    clientes = sum(clientes, na.rm = TRUE),
    polizas = sum(polizas, na.rm = TRUE),
    .groups = "drop"
  )

municipal_sf <- municipal_sf %>%
  left_join(autos, by = c("dept_key", "muni_key")) %>%
  left_join(leads_muni, by = c("dept_key", "muni_key")) %>%
  left_join(clients_muni, by = c("dept_key", "muni_key")) %>%
  mutate(emitidos = 0, ventas = 0) %>%
  add_metrics()

leads_dept <- read_csv(source_path("tabla_leads_departamento.csv"), show_col_types = FALSE) %>%
  transmute(
    dept_key = normalize_dept(depto_key),
    emitidos = as.numeric(emitidos)
  ) %>%
  group_by(dept_key) %>%
  summarise(emitidos = sum(emitidos, na.rm = TRUE), .groups = "drop")

sales_dept <- read_csv(
  source_path("tabla_ventas_hasta_marzo_2026_departamento_tecnologia.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    dept_key = normalize_dept(depto_key),
    Diesel = as.numeric(Diesel),
    Gasolina = as.numeric(Gasolina),
    Hibridos = as.numeric(Hibridos),
    Electricos = as.numeric(Electricos),
    Gas = as.numeric(Gas),
    ventas = rowSums(across(c(Diesel, Gasolina, Hibridos, Electricos, Gas)), na.rm = TRUE)
  ) %>%
  group_by(dept_key) %>%
  summarise(across(c(Diesel, Gasolina, Hibridos, Electricos, Gas, ventas), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

department_sf <- municipal_sf %>%
  group_by(dane_dept, dept_key, regional) %>%
  summarise(
    department = first(department),
    automoviles = sum(automoviles, na.rm = TRUE),
    leads = sum(leads, na.rm = TRUE),
    clientes = sum(clientes, na.rm = TRUE),
    polizas = sum(polizas, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(leads_dept, by = "dept_key") %>%
  left_join(sales_dept, by = "dept_key") %>%
  mutate(
    emitidos = coalesce(emitidos, 0),
    ventas = coalesce(ventas, 0)
  ) %>%
  add_metrics()

regional_sf <- department_sf %>%
  group_by(regional) %>%
  summarise(
    automoviles = sum(automoviles, na.rm = TRUE),
    leads = sum(leads, na.rm = TRUE),
    emitidos = sum(emitidos, na.rm = TRUE),
    clientes = sum(clientes, na.rm = TRUE),
    polizas = sum(polizas, na.rm = TRUE),
    ventas = sum(ventas, na.rm = TRUE),
    Diesel = sum(Diesel, na.rm = TRUE),
    Gasolina = sum(Gasolina, na.rm = TRUE),
    Hibridos = sum(Hibridos, na.rm = TRUE),
    Electricos = sum(Electricos, na.rm = TRUE),
    Gas = sum(Gas, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  add_metrics()

trend_leads <- read_csv(source_path("tabla_tendencia_leads_regional.csv"), show_col_types = FALSE) %>%
  mutate(
    regional = as.character(regional),
    month = as.Date(mes),
    leads = as.numeric(leads),
    emitidos = as.numeric(emitidos),
    tasa_emision = as.numeric(tasa_emision)
  )

trend_sales <- read_csv(source_path("tabla_tendencia_mensual_regional.csv"), show_col_types = FALSE) %>%
  mutate(
    regional = as.character(regional),
    month = as.Date(fecha_mes),
    unidades = as.numeric(ventas)
  )

utm_sources <- read_csv(source_path("tabla_leads_regional_fuente_utm.csv"), show_col_types = FALSE)

source_files <- c(
  "CRECIMIENTO_DEL_PARQUE_AUTOMOTOR_RUNT2.0_20260612.csv",
  "tabla_cobertura_municipios_runt.csv",
  "tabla_leads_ciudad.csv",
  "tabla_leads_departamento.csv",
  "tabla_clientes_directos_poblacion.csv",
  "tabla_ventas_hasta_marzo_2026_departamento_tecnologia.csv"
)

metadata <- data.frame(
  source = source_files,
  modified = as.POSIXct(vapply(source_files, function(x) {
    as.numeric(file.info(source_path(x))$mtime)
  }, numeric(1)), origin = "1970-01-01"),
  stringsAsFactors = FALSE
)

coverage <- list(
  municipalities = nrow(municipal_sf),
  municipalities_with_quotes = sum(municipal_sf$leads > 0),
  vehicle_park = sum(municipal_sf$automoviles, na.rm = TRUE),
  park_with_quotes = sum(municipal_sf$automoviles[municipal_sf$leads > 0], na.rm = TRUE),
  quotes_matched = sum(municipal_sf$leads, na.rm = TRUE),
  clients_matched = sum(municipal_sf$clientes, na.rm = TRUE)
)

app_data <- list(
  municipal = municipal_sf,
  department = department_sf,
  regional = regional_sf,
  trend_leads = trend_leads,
  trend_sales = trend_sales,
  utm_sources = utm_sources,
  metadata = metadata,
  coverage = coverage,
  prepared_at = Sys.time()
)

output <- file.path(data_dir, "territorial_data.rds")
saveRDS(app_data, output, compress = "xz")

message("Datos preparados en: ", output)
message("Municipios únicos: ", nrow(municipal_sf))
message("Tamaño del archivo: ", round(file.info(output)$size / 1024^2, 2), " MB")
