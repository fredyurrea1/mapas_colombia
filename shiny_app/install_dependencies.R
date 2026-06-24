app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(app_dir, "install_dependencies.R"))) {
  app_dir <- normalizePath(file.path(getwd(), "shiny_app"), winslash = "/", mustWork = TRUE)
}

lib <- path.expand(Sys.getenv("R_LIBS_USER"))
dir.create(lib, recursive = TRUE, showWarnings = FALSE)

# Una instalación interrumpida puede dejar bloqueos 00LOCK-* dentro de esta
# biblioteca de usuario. Son temporales y se pueden retirar de forma segura.
locks <- list.files(lib, pattern = "^00LOCK", full.names = TRUE)
if (length(locks)) unlink(locks, recursive = TRUE, force = TRUE)

options(timeout = 900)

packages <- c(
  "shiny", "bslib", "leaflet", "sf", "dplyr", "readr",
  "plotly", "DT", "scales", "stringi", "htmltools"
)

installed <- rownames(installed.packages(lib.loc = lib))
missing <- setdiff(packages, installed)

if (length(missing)) {
  install.packages(
    missing,
    lib = lib,
    repos = "https://cloud.r-project.org",
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
}

message("Dependencias listas en: ", lib)
