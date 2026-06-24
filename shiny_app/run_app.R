app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(app_dir, "app.R"))) {
  app_dir <- normalizePath(file.path(getwd(), "shiny_app"), winslash = "/", mustWork = TRUE)
}

if (.Platform$OS.type == "windows") {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Spanish_Colombia.1252"), silent = TRUE))
}

user_lib <- path.expand(Sys.getenv("R_LIBS_USER"))
if (dir.exists(user_lib)) .libPaths(c(user_lib, .libPaths()))

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Falta Shiny. Ejecute install_dependencies.R.")
}

shiny::runApp(app_dir, host = "127.0.0.1", port = 3838, launch.browser = FALSE)
