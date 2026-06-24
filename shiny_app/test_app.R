user_lib <- path.expand(Sys.getenv("R_LIBS_USER"))
if (dir.exists(user_lib)) .libPaths(c(user_lib, .libPaths()))

if (.Platform$OS.type == "windows") {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Spanish_Colombia.1252"), silent = TRUE))
}

suppressPackageStartupMessages(library(shiny))

app <- (source("shiny_app/app.R", encoding = "UTF-8"))[["value"]]
stopifnot(inherits(app, "shiny.appobj"))
stopifnot(sum(territorial$department$ventas) == sum(territorial$regional$ventas))
mix_sum <- with(
  territorial$department,
  share_hibridos + share_electricos + share_gasolina + share_diesel + share_gas
)
stopifnot(all(abs(mix_sum[territorial$department$ventas > 0] - 1) < 1e-9))
server_fn <- app$serverFuncSource()

shiny::testServer(server_fn, {
  session$setInputs(
    level = "municipal",
    metric = "presencia",
    region_filter = "Todas",
    department_filter = "Todos",
    trend_regions = c("Bogota", "Antioquia y eje")
  )
  session$flushReact()

  stopifnot(nchar(output$map_kpis) > 0)
  stopifnot(nchar(output$territory_map) > 0)
  stopifnot(nchar(output$opportunity_plot) > 0)
  stopifnot(nchar(output$opportunity_table) > 0)
  stopifnot(nchar(output$correlation_kpis) > 0)
  stopifnot(nchar(output$correlation_plot) > 0)
  stopifnot(nchar(output$funnel_plot) > 0)
  stopifnot(nchar(output$lead_trend) > 0)
  stopifnot(nchar(output$sales_trend) > 0)
  stopifnot(nchar(output$source_plot) > 0)
  stopifnot(nchar(output$method_kpis) > 0)
  stopifnot(nchar(output$source_table) > 0)

  session$setInputs(level = "department", metric = "conversion", region_filter = "Bogota")
  session$flushReact()
  stopifnot(nchar(output$territory_map) > 0)
  stopifnot(nchar(output$sales_mix_summary) > 0)

  for (sales_metric in c("ventas", "share_hibridos", "share_electricos", "share_gasolina", "share_diesel")) {
    session$setInputs(metric = sales_metric)
    session$flushReact()
    stopifnot(nchar(output$territory_map) > 0)
  }

  session$setInputs(level = "regional", metric = "leads_per_1000", region_filter = "Todas")
  session$flushReact()
  stopifnot(nchar(output$territory_map) > 0)
})

cat("SHINY_TESTS_OK\n")
