app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(app_dir, "prepare_data.R"))) {
  app_dir <- normalizePath(file.path(getwd(), "shiny_app"), winslash = "/", mustWork = TRUE)
}

user_lib <- path.expand(Sys.getenv("R_LIBS_USER"))
if (dir.exists(user_lib)) .libPaths(c(user_lib, .libPaths()))

required <- c("shiny", "bslib", "leaflet", "sf", "dplyr", "plotly", "DT", "scales", "htmltools")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Faltan paquetes de R: ", paste(missing, collapse = ", "), ". Ejecute shiny_app/install_dependencies.R.")
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(sf)
  library(dplyr)
  library(plotly)
  library(DT)
  library(scales)
  library(htmltools)
})

data_path <- file.path(app_dir, "data", "territorial_data.rds")
if (!file.exists(data_path)) stop("No existen los datos preparados. Ejecute shiny_app/prepare_data.R.")
territorial <- readRDS(data_path)

metric_labels <- c(
  automoviles = "Parque automotor",
  leads = "Cotizaciones Allianz",
  leads_per_1000 = "Cotizaciones por 1.000 autos",
  polizas = "Pólizas de clientes directos",
  polizas_per_1000 = "Pólizas por 1.000 autos",
  presencia = "Índice exploratorio de presencia",
  conversion = "Conversión cotización a emisión",
  ventas = "Ventas de autos (ene. 2025 a mar. 2026)",
  share_hibridos = "Proporción de híbridos (%)",
  share_electricos = "Proporción de eléctricos (%)",
  share_gasolina = "Proporción de gasolina (%)",
  share_diesel = "Proporción de diésel (%)"
)

metric_choices <- function(labels) stats::setNames(names(labels), unname(labels))

opportunity_colors <- c(
  "Defender" = "#0A9396",
  "Prioridad comercial" = "#D62828",
  "Nicho consolidado" = "#F4A261",
  "Menor prioridad" = "#8D99AE"
)

fmt_int <- function(x) scales::label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)(x)
fmt_dec <- function(x) scales::label_number(big.mark = ".", decimal.mark = ",", accuracy = 0.1)(x)
fmt_pct <- function(x) scales::label_percent(decimal.mark = ",", accuracy = 0.1)(x)

metric_value <- function(x, metric) {
  value <- x[[metric]]
  if (metric == "conversion" || startsWith(metric, "share_")) return(fmt_pct(value))
  if (metric %in% c("leads_per_1000", "polizas_per_1000", "presencia")) return(fmt_dec(value))
  fmt_int(value)
}

territory_name <- function(x, level) {
  if (level == "municipal") x$municipality else if (level == "department") x$department else x$regional
}

territory_id <- function(x, level) {
  if (level == "municipal") as.character(x$dane_muni) else if (level == "department") as.character(x$dane_dept) else as.character(x$regional)
}

theme_allianz <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#003781",
  secondary = "#007AB3",
  success = "#0A9396",
  danger = "#D62828"
)

kpi_card <- function(title, value, note = NULL, accent = "blue") {
  div(
    class = paste("kpi-card", paste0("accent-", accent)),
    div(class = "kpi-title", title),
    div(class = "kpi-value", value),
    if (!is.null(note)) div(class = "kpi-note", note)
  )
}

mix_item <- function(label, value, color) {
  value <- ifelse(is.finite(value), value, 0)
  width <- max(0, min(100, 100 * value))
  div(
    class = "mix-item",
    div(class = "mix-label", span(label), strong(fmt_pct(value))),
    div(class = "mix-track", div(
      class = "mix-fill",
      style = paste0("width:", width, "%;background:", color, ";")
    ))
  )
}

ui <- navbarPage(
  title = div(class = "brand-title", span("ALLIANZ"), tags$small(" Inteligencia territorial")),
  id = "main_nav",
  theme = theme_allianz,
  header = tagList(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    includeCSS(file.path(app_dir, "www", "styles.css"))
  ),

  tabPanel(
    "Mapa territorial",
    div(
      class = "page-shell",
      div(
        class = "page-heading",
        h2("¿Dónde está Allianz frente al mercado automotor?"),
        p("Explore mercado, ventas, tecnologías, demanda, clientes y brechas territoriales.")
      ),
      fluidRow(
        column(
          width = 3,
          div(
            class = "control-panel",
            radioButtons(
              "level", "Nivel territorial",
              choices = c("Municipios" = "municipal", "Departamentos" = "department", "Regionales" = "regional"),
              selected = "municipal", inline = TRUE
            ),
            selectInput(
              "metric", "Indicador del mapa",
              choices = metric_choices(metric_labels[!names(metric_labels) %in% c(
                "conversion", "ventas", "share_hibridos", "share_electricos", "share_gasolina", "share_diesel"
              )]),
              selected = "presencia"
            ),
            selectInput("region_filter", "Regional Allianz", choices = c("Todas", sort(unique(territorial$municipal$regional)))),
            conditionalPanel(
              condition = "input.level == 'municipal'",
              selectInput("department_filter", "Departamento", choices = "Todos")
            ),
            hr(),
            div(
              class = "legend-note",
              strong("Cómo leerlo"),
              p("Los indicadores de ventas y tecnología están disponibles en Departamentos y Regionales. Haga clic en un territorio para ver su ficha.")
            )
          )
        ),
        column(
          width = 9,
          uiOutput("map_kpis"),
          uiOutput("sales_mix_summary"),
          div(class = "map-card", leafletOutput("territory_map", height = "610px")),
          uiOutput("selected_territory")
        )
      )
    )
  ),

  tabPanel(
    "Oportunidades",
    div(
      class = "page-shell",
      div(class = "page-heading", h2("Matriz de oportunidad territorial"), p("Compara el tamaño del mercado con la intensidad de cotización Allianz.")),
      fluidRow(
        column(width = 8, div(class = "content-card", plotlyOutput("opportunity_plot", height = "560px"))),
        column(
          width = 4,
          div(
            class = "insight-panel",
            h4("Lectura de cuadrantes"),
            div(class = "quadrant q-defend", strong("Defender"), p("Mercado grande y presencia relativa alta.")),
            div(class = "quadrant q-priority", strong("Prioridad comercial"), p("Mercado grande y presencia relativa baja.")),
            div(class = "quadrant q-niche", strong("Nicho consolidado"), p("Mercado menor con intensidad Allianz alta.")),
            div(class = "quadrant q-low", strong("Menor prioridad"), p("Mercado e intensidad por debajo de la mediana."))
          )
        )
      ),
      div(class = "content-card", h4("Territorios priorizados"), DTOutput("opportunity_table"))
    )
  ),

  tabPanel(
    "Embudo y correlación",
    div(
      class = "page-shell",
      div(class = "page-heading", h2("Del parque automotor a la póliza"), p("Distingue tamaño del mercado, interés y conversión comercial.")),
      fluidRow(
        column(width = 4, uiOutput("correlation_kpis")),
        column(width = 8, div(class = "content-card", plotlyOutput("correlation_plot", height = "500px")))
      ),
      div(class = "content-card", h4("Embudo por regional"), plotlyOutput("funnel_plot", height = "430px"))
    )
  ),

  tabPanel(
    "Tendencias",
    div(
      class = "page-shell",
      div(class = "page-heading", h2("Evolución y origen de la demanda"), p("Siga cotizaciones, emisiones, ventas de vehículos y fuentes de adquisición.")),
      div(
        class = "control-strip",
        selectizeInput(
          "trend_regions", "Regionales",
          choices = sort(unique(territorial$trend_leads$regional)),
          selected = sort(unique(territorial$trend_leads$regional)),
          multiple = TRUE
        )
      ),
      fluidRow(
        column(width = 6, div(class = "content-card", h4("Cotizaciones y emisiones"), plotlyOutput("lead_trend", height = "420px"))),
        column(width = 6, div(class = "content-card", h4("Ventas de vehículos"), plotlyOutput("sales_trend", height = "420px")))
      ),
      div(class = "content-card", h4("Fuentes de cotización por regional"), plotlyOutput("source_plot", height = "470px"))
    )
  ),

  tabPanel(
    "Metodología",
    div(
      class = "page-shell methodology",
      div(class = "page-heading", h2("Qué mide esta herramienta"), p("Un marco auditable para interpretar presencia territorial.")),
      fluidRow(
        column(
          width = 7,
          div(
            class = "content-card",
            h3("Índice exploratorio de presencia Allianz"),
            p("El índice ordena territorios dentro de cada nivel geográfico. No es una medición de recordación publicitaria ni una inferencia causal."),
            tags$ul(
              tags$li(strong("45%"), " intensidad de cotizaciones por cada 1.000 automóviles."),
              tags$li(strong("35%"), " pólizas de clientes directos por cada 1.000 automóviles."),
              tags$li(strong("20%"), " volumen absoluto de cotizaciones en escala logarítmica.")
            ),
            h4("Ventas y tecnologías"),
            p("Las ventas y el mix de híbridos, eléctricos, gasolina y diésel se muestran en departamentos y regionales, que es la granularidad disponible en la fuente."),
            h4("Llaves geográficas"),
            p("La geometría se consolida con códigos DANE y los nombres comerciales se normalizan antes del cruce.")
          )
        ),
        column(
          width = 5,
          div(class = "content-card", h3("Cobertura del cruce"), uiOutput("method_kpis")),
          div(class = "content-card", h3("Fuentes"), DTOutput("source_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(list(input$level, input$region_filter), {
    allowed <- metric_labels
    if (identical(input$level, "municipal")) {
      department_only <- c("conversion", "ventas", "share_hibridos", "share_electricos", "share_gasolina", "share_diesel")
      allowed <- allowed[!names(allowed) %in% department_only]
    }
    selected <- if (!is.null(input$metric) && input$metric %in% names(allowed)) input$metric else "presencia"
    updateSelectInput(session, "metric", choices = metric_choices(allowed), selected = selected)

    depts <- territorial$municipal %>%
      st_drop_geometry() %>%
      filter(input$region_filter == "Todas" | regional == input$region_filter) %>%
      distinct(department) %>% arrange(department) %>% pull(department)
    updateSelectInput(session, "department_filter", choices = c("Todos", depts), selected = "Todos")
  }, ignoreInit = FALSE)

  map_data <- reactive({
    level <- if (is.null(input$level)) "municipal" else input$level
    x <- switch(level, municipal = territorial$municipal, department = territorial$department, regional = territorial$regional)
    if (level != "regional" && !is.null(input$region_filter) && input$region_filter != "Todas") x <- x %>% filter(regional == input$region_filter)
    if (level == "municipal" && !is.null(input$department_filter) && input$department_filter != "Todos") x <- x %>% filter(department == input$department_filter)
    x
  })

  output$map_kpis <- renderUI({
    x <- map_data()
    total_territories <- nrow(x)
    with_quotes <- sum(x$leads > 0, na.rm = TRUE)
    penetration <- if (sum(x$automoviles, na.rm = TRUE) > 0) 1000 * sum(x$leads, na.rm = TRUE) / sum(x$automoviles, na.rm = TRUE) else NA_real_

    cards <- list(
      kpi_card("Parque automotor", fmt_int(sum(x$automoviles, na.rm = TRUE)), "Automóviles activos", "blue"),
      kpi_card("Cotizaciones", fmt_int(sum(x$leads, na.rm = TRUE)), paste0(fmt_int(with_quotes), " de ", fmt_int(total_territories), " territorios"), "cyan"),
      kpi_card("Intensidad", paste0(fmt_dec(penetration), " / 1.000"), "Cotizaciones por automóvil", "gold"),
      kpi_card("Pólizas directas", fmt_int(sum(x$polizas, na.rm = TRUE)), "Clientes directos georreferenciados", "green")
    )
    if (!identical(input$level, "municipal")) {
      cards <- append(cards, list(kpi_card("Ventas de autos", fmt_int(sum(x$ventas, na.rm = TRUE)), "Enero 2025 a marzo 2026", "purple")))
    }
    div(class = "kpi-grid", cards)
  })

  output$sales_mix_summary <- renderUI({
    if (identical(input$level, "municipal")) return(NULL)
    x <- map_data()
    total <- sum(x$ventas, na.rm = TRUE)
    if (total <= 0) return(NULL)
    shares <- c(
      Hibridos = sum(x$Hibridos, na.rm = TRUE) / total,
      Electricos = sum(x$Electricos, na.rm = TRUE) / total,
      Gasolina = sum(x$Gasolina, na.rm = TRUE) / total,
      Diesel = sum(x$Diesel, na.rm = TRUE) / total,
      Gas = sum(x$Gas, na.rm = TRUE) / total
    )
    div(
      class = "sales-mix-card",
      div(
        class = "sales-mix-heading",
        div(strong("Mix tecnológico de ventas"), span("Enero 2025 a marzo 2026")),
        div(class = "sales-total", span("Total ventas"), strong(fmt_int(total)))
      ),
      div(
        class = "mix-grid",
        mix_item("Híbridos", shares[["Hibridos"]], "#78BE20"),
        mix_item("Eléctricos", shares[["Electricos"]], "#00A6A6"),
        mix_item("Gasolina", shares[["Gasolina"]], "#F28E2B"),
        mix_item("Diésel", shares[["Diesel"]], "#59636E")
      ),
      div(class = "mix-footnote", paste0("Gas y otras categorías: ", fmt_pct(shares[["Gas"]]), "."))
    )
  })

  output$territory_map <- renderLeaflet({
    x <- map_data()
    req(nrow(x) > 0, input$metric)
    metric <- input$metric
    req(metric %in% names(x))
    values <- x[[metric]]
    color_values <- if (startsWith(metric, "share_")) 100 * values else values
    domain <- color_values[is.finite(color_values)]
    if (!length(domain)) domain <- c(0, 1)
    if (length(unique(domain)) == 1) domain <- c(domain, domain + 1)
    palette <- if (metric == "presencia") "YlGnBu" else if (startsWith(metric, "share_")) "YlGn" else "YlOrRd"
    pal <- colorNumeric(palette = palette, domain = domain, na.color = "#E6E9EF")
    names_x <- territory_name(x, input$level)
    ids <- territory_id(x, input$level)

    sales_detail <- if (identical(input$level, "municipal")) {
      rep("", nrow(x))
    } else {
      paste0(
        "<hr>Ventas: ", fmt_int(x$ventas), "<br>",
        "Híbridos: ", fmt_pct(x$share_hibridos), "<br>",
        "Eléctricos: ", fmt_pct(x$share_electricos), "<br>",
        "Gasolina: ", fmt_pct(x$share_gasolina), "<br>",
        "Diésel: ", fmt_pct(x$share_diesel)
      )
    }

    popup <- paste0(
      "<div class='map-popup'><strong>", htmlEscape(names_x), "</strong>",
      "<span>", htmlEscape(metric_labels[[metric]]), "</span>",
      "<b>", metric_value(x, metric), "</b><hr>",
      "Parque: ", fmt_int(x$automoviles), "<br>",
      "Cotizaciones: ", fmt_int(x$leads), "<br>",
      "Cotizaciones/1.000: ", fmt_dec(x$leads_per_1000), "<br>",
      "Pólizas: ", fmt_int(x$polizas), "<br>",
      "Clasificación: ", htmlEscape(x$oportunidad), sales_detail, "</div>"
    )

    map <- leaflet(x, options = leafletOptions(preferCanvas = TRUE, zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(
        layerId = ids,
        fillColor = pal(color_values), fillOpacity = 0.78,
        color = "#FFFFFF", weight = 0.7, opacity = 0.9,
        popup = popup,
        highlightOptions = highlightOptions(weight = 2.4, color = "#003781", fillOpacity = 0.92, bringToFront = TRUE)
      ) %>%
      addLegend(
        position = "bottomright", pal = pal, values = color_values,
        title = metric_labels[[metric]], opacity = 0.85,
        labFormat = labelFormat(big.mark = ".", digits = 1)
      )
    bbox <- st_bbox(x)
    map %>% fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
  })

  selected_row <- reactive({
    click <- input$territory_map_shape_click
    if (is.null(click$id)) return(NULL)
    x <- map_data()
    hit <- which(territory_id(x, input$level) == as.character(click$id))
    if (!length(hit)) return(NULL)
    x[hit[1], ]
  })

  output$selected_territory <- renderUI({
    x <- selected_row()
    if (is.null(x)) return(div(class = "selection-hint", "Seleccione un territorio en el mapa para abrir su ficha."))
    div(
      class = "territory-card",
      div(class = "territory-title", h4(territory_name(x, input$level)), span(x$oportunidad, class = "status-pill")),
      div(
        class = "territory-stats",
        div(span("Parque"), strong(fmt_int(x$automoviles))),
        div(span("Cotizaciones"), strong(fmt_int(x$leads))),
        div(span("Por 1.000 autos"), strong(fmt_dec(x$leads_per_1000))),
        div(span("Pólizas"), strong(fmt_int(x$polizas))),
        div(span("Índice presencia"), strong(fmt_dec(x$presencia))),
        if (!identical(input$level, "municipal")) div(span("Ventas"), strong(fmt_int(x$ventas)))
      ),
      if (!identical(input$level, "municipal")) {
        div(
          class = "territory-mix",
          mix_item("Híbridos", x$share_hibridos, "#78BE20"),
          mix_item("Eléctricos", x$share_electricos, "#00A6A6"),
          mix_item("Gasolina", x$share_gasolina, "#F28E2B"),
          mix_item("Diésel", x$share_diesel, "#59636E")
        )
      }
    )
  })

  opportunity_data <- reactive({
    x <- territorial$municipal %>% st_drop_geometry() %>% filter(automoviles > 0)
    if (!is.null(input$region_filter) && input$region_filter != "Todas") x <- x %>% filter(regional == input$region_filter)
    x
  })

  output$opportunity_plot <- renderPlotly({
    x <- opportunity_data()
    market_cut <- median(x$automoviles, na.rm = TRUE)
    presence_cut <- median(x$leads_per_1000, na.rm = TRUE)
    plot_ly(
      x, x = ~automoviles, y = ~leads_per_1000,
      type = "scatter", mode = "markers",
      color = ~oportunidad, colors = opportunity_colors,
      size = ~pmax(polizas, 1), sizes = c(8, 42),
      text = ~paste0(
        "<b>", municipality, "</b><br>", department,
        "<br>Parque: ", fmt_int(automoviles),
        "<br>Cotizaciones: ", fmt_int(leads),
        "<br>Cotizaciones/1.000: ", fmt_dec(leads_per_1000),
        "<br>Pólizas: ", fmt_int(polizas)
      ),
      hoverinfo = "text", marker = list(opacity = 0.78, line = list(color = "white", width = 0.6))
    ) %>%
      add_segments(x = market_cut, xend = market_cut, y = 0, yend = max(x$leads_per_1000, na.rm = TRUE), inherit = FALSE, line = list(color = "#AAB2BD", dash = "dot"), showlegend = FALSE) %>%
      add_segments(x = min(x$automoviles), xend = max(x$automoviles), y = presence_cut, yend = presence_cut, inherit = FALSE, line = list(color = "#AAB2BD", dash = "dot"), showlegend = FALSE) %>%
      layout(
        xaxis = list(title = "Parque automotor (escala logarítmica)", type = "log", gridcolor = "#EDF0F5"),
        yaxis = list(title = "Cotizaciones por 1.000 automóviles", gridcolor = "#EDF0F5"),
        legend = list(orientation = "h", y = -0.2),
        margin = list(l = 70, r = 20, t = 20, b = 90),
        paper_bgcolor = "transparent", plot_bgcolor = "transparent"
      ) %>% config(displayModeBar = FALSE)
  })

  output$opportunity_table <- renderDT({
    x <- territorial$municipal %>%
      st_drop_geometry() %>% filter(automoviles > 0) %>%
      mutate(
        `Cotizaciones/1.000` = round(leads_per_1000, 2),
        `Índice presencia` = round(presencia, 1)
      ) %>%
      arrange(desc(oportunidad == "Prioridad comercial"), desc(automoviles)) %>%
      select(
        Municipio = municipality, Departamento = department, Regional = regional,
        `Clasificación` = oportunidad, Parque = automoviles, Cotizaciones = leads,
        `Cotizaciones/1.000`, `Pólizas` = polizas, `Índice presencia`
      )
    datatable(x, rownames = FALSE, filter = "top", options = list(pageLength = 12, scrollX = TRUE, dom = "tip")) %>%
      formatRound(c("Parque", "Cotizaciones", "Pólizas"), digits = 0, mark = ".", dec.mark = ",")
  })

  correlation_data <- reactive({
    x <- territorial$municipal %>% st_drop_geometry() %>% filter(automoviles > 0)
    if (!is.null(input$region_filter) && input$region_filter != "Todas") x <- x %>% filter(regional == input$region_filter)
    x
  })

  output$correlation_kpis <- renderUI({
    x <- correlation_data()
    div(
      class = "vertical-kpis",
      kpi_card("Correlación directa", fmt_dec(cor(x$automoviles, x$leads, use = "complete.obs")), "Sensible a las grandes ciudades", "blue"),
      kpi_card("Correlación logarítmica", fmt_dec(cor(log1p(x$automoviles), log1p(x$leads), use = "complete.obs")), "Reduce valores extremos", "cyan"),
      kpi_card("Brechas visibles", fmt_int(sum(x$leads == 0)), "Municipios con parque y sin cotizaciones", "gold")
    )
  })

  output$correlation_plot <- renderPlotly({
    x <- correlation_data() %>% mutate(log_autos = log1p(automoviles), log_leads = log1p(leads))
    fit <- lm(log_leads ~ log_autos, data = x)
    line <- data.frame(log_autos = seq(min(x$log_autos), max(x$log_autos), length.out = 100))
    line$log_leads <- predict(fit, newdata = line)
    plot_ly(
      x, x = ~log_autos, y = ~log_leads, type = "scatter", mode = "markers",
      color = ~regional,
      text = ~paste0("<b>", municipality, "</b><br>", regional, "<br>Parque: ", fmt_int(automoviles), "<br>Cotizaciones: ", fmt_int(leads)),
      hoverinfo = "text", marker = list(size = 9, opacity = 0.7)
    ) %>%
      add_lines(data = line, x = ~log_autos, y = ~log_leads, inherit = FALSE, name = "Tendencia", line = list(color = "#111827", width = 3)) %>%
      layout(
        xaxis = list(title = "log(1 + parque automotor)", gridcolor = "#EDF0F5"),
        yaxis = list(title = "log(1 + cotizaciones)", gridcolor = "#EDF0F5"),
        legend = list(orientation = "h", y = -0.2), margin = list(l = 70, r = 20, t = 15, b = 90),
        paper_bgcolor = "transparent", plot_bgcolor = "transparent"
      ) %>% config(displayModeBar = FALSE)
  })

  output$funnel_plot <- renderPlotly({
    x <- territorial$regional %>% st_drop_geometry() %>% arrange(regional)
    long <- bind_rows(
      transmute(x, regional, etapa = "Cotizaciones", valor = leads),
      transmute(x, regional, etapa = "Emitidas", valor = emitidos),
      transmute(x, regional, etapa = "Pólizas directas", valor = polizas)
    )
    plot_ly(
      long, x = ~regional, y = ~valor, color = ~etapa,
      colors = c("Cotizaciones" = "#007AB3", "Emitidas" = "#0A9396", "Pólizas directas" = "#F4A261"),
      type = "bar", text = ~fmt_int(valor),
      hovertemplate = "%{x}<br>%{fullData.name}: %{text}<extra></extra>"
    ) %>%
      layout(
        barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Registros", gridcolor = "#EDF0F5"),
        legend = list(orientation = "h", y = -0.2), margin = list(l = 70, r = 20, t = 10, b = 100),
        paper_bgcolor = "transparent", plot_bgcolor = "transparent"
      ) %>% config(displayModeBar = FALSE)
  })

  output$lead_trend <- renderPlotly({
    req(input$trend_regions)
    x <- territorial$trend_leads %>% filter(regional %in% input$trend_regions)
    plot_ly(x, x = ~month, y = ~leads, color = ~regional, type = "scatter", mode = "lines+markers") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Cotizaciones", gridcolor = "#EDF0F5"), legend = list(orientation = "h", y = -0.25), margin = list(l = 60, r = 15, t = 10, b = 100), paper_bgcolor = "transparent", plot_bgcolor = "transparent") %>%
      config(displayModeBar = FALSE)
  })

  output$sales_trend <- renderPlotly({
    req(input$trend_regions)
    x <- territorial$trend_sales %>% filter(regional %in% input$trend_regions)
    plot_ly(x, x = ~month, y = ~unidades, color = ~regional, type = "scatter", mode = "lines+markers") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Vehículos vendidos", gridcolor = "#EDF0F5"), showlegend = FALSE, margin = list(l = 60, r = 15, t = 10, b = 55), paper_bgcolor = "transparent", plot_bgcolor = "transparent") %>%
      config(displayModeBar = FALSE)
  })

  output$source_plot <- renderPlotly({
    req(input$trend_regions)
    x <- territorial$utm_sources %>%
      filter(regional %in% input$trend_regions) %>%
      mutate(leads = as.numeric(leads)) %>%
      group_by(fuente_utm, regional) %>% summarise(leads = sum(leads, na.rm = TRUE), .groups = "drop") %>%
      filter(leads > 0)
    plot_ly(x, x = ~leads, y = ~reorder(fuente_utm, leads), color = ~regional, type = "bar", orientation = "h") %>%
      layout(barmode = "stack", xaxis = list(title = "Cotizaciones", gridcolor = "#EDF0F5"), yaxis = list(title = ""), legend = list(orientation = "h", y = -0.18), margin = list(l = 140, r = 20, t = 10, b = 90), paper_bgcolor = "transparent", plot_bgcolor = "transparent") %>%
      config(displayModeBar = FALSE)
  })

  output$method_kpis <- renderUI({
    cvr <- territorial$coverage
    tagList(
      kpi_card("Cobertura territorial", fmt_pct(cvr$municipalities_with_quotes / cvr$municipalities), paste0(fmt_int(cvr$municipalities_with_quotes), " de ", fmt_int(cvr$municipalities), " municipios"), "blue"),
      kpi_card("Cobertura del parque", fmt_pct(cvr$park_with_quotes / cvr$vehicle_park), "Parque en municipios con cotizaciones", "green"),
      kpi_card("Cotizaciones cruzadas", fmt_int(cvr$quotes_matched), "Con municipio reconocido", "cyan")
    )
  })

  output$source_table <- renderDT({
    x <- territorial$metadata %>% transmute(Fuente = source, `Actualización` = format(modified, "%Y-%m-%d %H:%M"))
    datatable(x, rownames = FALSE, options = list(dom = "t", pageLength = 10), class = "compact")
  })
}

shinyApp(ui, server)
