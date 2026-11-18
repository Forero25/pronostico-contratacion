# Pronóstico de la contratación pública en Colombia (SECOP II).
# App Shiny que lee los datos que el observatorio publica cada día y ajusta el modelo en vivo.
# José Nicolás Forero Urrego · https://nicolasforero.pages.dev

library(shiny)
library(bslib)
library(plotly)

# Los textos llevan tildes: se fuerza UTF-8 aunque el servidor arranque con otra configuración regional.
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))

source("R/modelo.R")
source("R/datos.R")

# ---- Estilo ---------------------------------------------------------------------------------
TINTA <- "#1F2430"; TENUE <- "#6B7280"; LINEA <- "#ECE8E0"; FONDO <- "#F6F4EF"
AZUL <- "#7C93E8"; AZUL_SUAVE <- "#DCE3F9"; AZUL_FUERTE <- "#4F6BD8"
AMBAR <- "#F5B482"; LAVANDA <- "#B9A7E8"; MENTA <- "#8CCFB8"; ROSA <- "#EBA3B0"
COLOR_MODELO <- c("Ingenuo estacional" = AMBAR, "Estacional" = LAVANDA,
                  "Estacional con calendario electoral" = AZUL_FUERTE)

tema <- bs_theme(
  version = 5, bg = FONDO, fg = TINTA, primary = AZUL_FUERTE, secondary = TENUE,
  base_font = font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "sans-serif"),
  heading_font = font_collection("Georgia", "serif"),
  "border-color" = LINEA, "card-border-color" = LINEA, "card-bg" = "#FFFFFF"
)

num <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
pct <- function(x, d = 1) paste0(num(100 * x, d), " %")
MESES <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
mes_anio <- function(f) paste(MESES[as.integer(format(f, "%m"))], format(f, "%Y"))

grafico_base <- function(p, titulo_y = "Contratos firmados") {
  p |>
    layout(
      separators = ",.", hovermode = "x unified",
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Inter, system-ui, sans-serif", color = TINTA, size = 12),
      xaxis = list(title = "", gridcolor = LINEA, zeroline = FALSE),
      yaxis = list(title = titulo_y, gridcolor = LINEA, zeroline = FALSE, tickformat = ",d", rangemode = "tozero"),
      legend = list(orientation = "h", x = 0, y = -0.12, yanchor = "top"),
      margin = list(l = 10, r = 10, t = 10, b = 10)
    ) |>
    config(displaylogo = FALSE, locale = "es",
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines"))
}

# Serie observada + pronóstico con banda.
grafico_pronostico <- function(serie, f, nivel) {
  banda <- paste0("Intervalo del ", round(100 * nivel), " %")
  plot_ly() |>
    add_ribbons(data = f, x = ~fecha, ymin = ~inferior, ymax = ~superior, name = banda,
                fillcolor = AZUL_SUAVE, line = list(color = "transparent"), hoverinfo = "skip") |>
    add_lines(data = serie, x = ~fecha, y = ~contratos, name = "Observado",
              line = list(color = TINTA, width = 1.8),
              hovertemplate = "%{y:,d}<extra>Observado</extra>") |>
    add_lines(data = f, x = ~fecha, y = ~pronostico, name = "Pronóstico",
              line = list(color = AZUL_FUERTE, width = 2.4, dash = "dot"),
              customdata = cbind(f$inferior, f$superior),
              hovertemplate = "%{y:,.0f} (entre %{customdata[0]:,.0f} y %{customdata[1]:,.0f})<extra>Pronóstico</extra>") |>
    grafico_base()
}

# ---- Interfaz -------------------------------------------------------------------------------
pie <- tags$footer(
  class = "text-muted small mt-4 mb-2",
  "Fuente: SECOP II – Contratos Electrónicos (Colombia Compra Eficiente, datos.gov.co, CC BY-SA 4.0). ",
  "Datos y código: ",
  tags$a(href = "https://github.com/Forero25/observatorio-secop-datos", "observatorio-secop-datos"), " · ",
  tags$a(href = "https://github.com/Forero25/pronostico-contratacion", "esta app"), " · ",
  tags$a(href = "https://nicolasforero.pages.dev", "José Nicolás Forero Urrego")
)

ui <- page_navbar(
  title = "Contratación pública en Colombia: pronóstico",
  theme = tema,
  fillable = FALSE,
  header = tags$head(tags$style(HTML("
    .navbar { border-bottom: 1px solid #ECE8E0; }
    .navbar-brand { white-space: normal; }
    @media (max-width: 576px) { .navbar-brand { font-size: 1rem; max-width: 75vw; } }
    .bslib-value-box .value-box-title { font-size: .85rem; color: #6B7280; font-family: Inter, system-ui, sans-serif; }
    .bslib-value-box .value-box-value { font-size: 1.9rem; }
    .lead-text { font-size: 1.05rem; max-width: 62rem; }
    .card-header { background: transparent; font-weight: 600; }
    table.tabla { width: 100%; font-size: .92rem; }
    table.tabla td, table.tabla th { padding: .35rem .5rem; border-bottom: 1px solid #ECE8E0; }
    table.tabla td.n, table.tabla th.n { text-align: right; font-variant-numeric: tabular-nums; white-space: nowrap; }
  "))),

  nav_panel(
    "Pronóstico",
    p(class = "lead-text mt-3",
      "¿Cuántos contratos firmará el Estado colombiano en SECOP II el próximo año? ",
      "El modelo aprende la estacionalidad de cada mes y el efecto del calendario electoral: ",
      "el pico de enero antes de una elección presidencial y la caída por la Ley de Garantías."),
    uiOutput("cajas"),
    card(card_header(textOutput("titulo_pronostico", inline = TRUE)),
         plotlyOutput("g_pronostico", height = "420px"),
         card_footer(class = "small text-muted", textOutput("nota_pronostico", inline = TRUE))),
    pie
  ),

  nav_panel(
    "Laboratorio",
    layout_sidebar(
      class = "mt-3",
      sidebar = sidebar(
        width = 300,
        radioButtons("modelo", "Modelo", choices = MODELOS, selected = MODELOS[3]),
        uiOutput("control_inicio"),
        sliderInput("horizonte", "Meses a pronosticar", min = 6, max = 36, value = 18, step = 1),
        radioButtons("nivel", "Intervalo", c("80 %" = 0.8, "90 %" = 0.9, "95 %" = 0.95), inline = TRUE),
        p(class = "small text-muted",
          "Cada cambio vuelve a estimar el modelo en el servidor con R. ",
          "Prueba a quitar el calendario electoral y mira qué pasa con los eneros.")
      ),
      card(card_header("Pronóstico con tu configuración"), plotlyOutput("g_lab", height = "400px")),
      card(card_header("Lo que estimó el modelo"), uiOutput("coeficientes"))
    ),
    pie
  ),

  nav_panel(
    "Evaluación",
    p(class = "lead-text mt-3",
      "Para elegir el modelo no basta con que se ajuste bien al pasado. ",
      "Se simuló haber estado en cada mes desde diciembre de 2022: con lo que se sabía hasta ese momento, ",
      "cada modelo predijo los 12 meses siguientes y se comparó con lo que pasó."),
    layout_columns(
      col_widths = c(4, 8),
      card(card_header("Desde qué mes predecir"),
           uiOutput("control_origen"),
           checkboxGroupInput("modelos_eval", "Modelos", choices = MODELOS, selected = MODELOS),
           p(class = "small text-muted", "Por defecto: lo que habría dicho cada modelo el último diciembre.")),
      card(card_header("Predicho contra observado"), plotlyOutput("g_eval", height = "380px"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Resultados de la evaluación"), uiOutput("tabla_eval")),
      card(card_header("Error según qué tan lejos se predice"), plotlyOutput("g_horizonte", height = "300px"))
    ),
    pie
  ),

  nav_panel(
    "Método",
    div(class = "lead-text mt-3",
      h4("Los datos"),
      p("Contratos firmados por mes en SECOP II desde enero de 2019, sin el mes en curso. ",
        "El ", tags$a(href = "https://github.com/Forero25/observatorio-secop", "pipeline del observatorio"),
        " los descarga cada madrugada de datos.gov.co, los modela con dbt y DuckDB y publica los agregados. ",
        "Esta app los lee al abrirse."),
      h4("Los tres modelos"),
      tags$ul(
        tags$li(tags$b("Ingenuo estacional:"), " cada mes será igual al mismo mes del año anterior. Es la referencia que un modelo debe superar."),
        tags$li(tags$b("Estacional:"), " regresión sobre el logaritmo de los contratos con un efecto por mes del año y errores ARIMA(0,1,1). ",
                "La diferencia deja que el nivel de la serie se mueva, porque la plataforma se adoptó por etapas, y el término de media móvil ",
                "lo suaviza: es equivalente a un suavizamiento exponencial."),
        tags$li(tags$b("Estacional con calendario electoral:"), " el anterior más dos variables: enero de un año de elección presidencial ",
                "y los meses de febrero a junio de ese año, cuando la Ley de Garantías restringe la contratación directa.")
      ),
      h4("Cómo se eligió"),
      p("Evaluación con origen móvil: desde cada mes con al menos cuatro años de historia se ajustan los tres modelos solo con los datos ",
        "anteriores y se predicen hasta 12 meses. Gana el de menor error porcentual absoluto medio (MAPE). ",
        "El intervalo se construye en logaritmos con la desviación del pronóstico, así que es asimétrico al volver a contratos."),
      h4("Lo que no hace"),
      tags$ul(
        tags$li("Cuenta contratos, no pesos."),
        tags$li("Es nacional: no pronostica por municipio ni por entidad."),
        tags$li("No conoce las elecciones territoriales, que tienen su propia restricción."),
        tags$li("Los efectos electorales salen de solo dos elecciones en la serie: el signo es claro, el valor exacto es frágil.")
      )
    ),
    pie
  )
)

# ---- Servidor -------------------------------------------------------------------------------
server <- function(input, output, session) {
  d <- tryCatch(cargar_datos(), error = function(e) e)
  if (inherits(d, "error")) {
    showModal(modalDialog(title = "No se pudieron leer los datos",
                          paste("El origen de datos no respondió:", conditionMessage(d)), easyClose = TRUE))
    return(invisible())
  }
  serie <- d$serie
  ultimo <- serie$fecha[nrow(serie)]

  # Pronóstico principal: el modelo elegido en la evaluación, con toda la serie.
  principal <- pronosticar(d$elegido, serie, 12, d$anios_eleccion, 0.8)

  output$titulo_pronostico <- renderText(
    paste0("Contratos firmados por mes y pronóstico hasta ", mes_anio(principal$tabla$fecha[12])))
  output$nota_pronostico <- renderText(paste0(
    "Modelo: ", d$elegido, ". Datos hasta ", mes_anio(ultimo), ". Banda: intervalo del 80 %. ",
    "Pasa el cursor para ver cada mes."))
  output$g_pronostico <- renderPlotly(grafico_pronostico(serie, principal$tabla, 0.8))

  output$cajas <- renderUI({
    r <- d$resumen
    total <- sum(round(principal$tabla$pronostico))
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Próximos 12 meses", num(total), p(class = "small", "contratos pronosticados"),
                theme = value_box_theme(bg = "#FFFFFF", fg = TINTA)),
      value_box("Error medio del modelo", pct(r[["mape_elegido"]]),
                p(class = "small", paste0("frente a ", pct(r[["mape_ingenuo"]]), " del ingenuo")),
                theme = value_box_theme(bg = "#FFFFFF", fg = TINTA)),
      value_box("Enero de año electoral", paste0("×", num(r[["factor_enero_eleccion"]], 2)),
                p(class = "small", "contratos frente a un enero normal"),
                theme = value_box_theme(bg = "#FFFFFF", fg = TINTA)),
      value_box("Ley de Garantías", paste0("×", num(r[["factor_restriccion"]], 2)),
                p(class = "small", "contratos de febrero a junio"),
                theme = value_box_theme(bg = "#FFFFFF", fg = TINTA))
    )
  })

  # ---- Laboratorio ----
  output$control_inicio <- renderUI({
    inicios <- serie$fecha[1:(nrow(serie) - 48)]   # al menos cuatro años para estimar
    selectInput("inicio", "Entrenar desde", choices = setNames(as.character(inicios), mes_anio(inicios)),
                selected = as.character(inicios[1]))
  })

  lab <- reactive({
    req(input$inicio)
    s <- serie[serie$fecha >= as.Date(input$inicio), ]
    validate(need(nrow(s) >= 25, "Hacen falta al menos dos años de datos."))
    pronosticar(input$modelo, s, input$horizonte, d$anios_eleccion, as.numeric(input$nivel))
  })

  output$g_lab <- renderPlotly({
    s <- serie[serie$fecha >= as.Date(input$inicio), ]
    grafico_pronostico(s, lab()$tabla, as.numeric(input$nivel))
  })

  output$coeficientes <- renderUI({
    l <- lab()
    if (is.null(l$coef)) {
      return(p("El modelo ingenuo no estima nada: copia el mismo mes del año anterior. ",
               "Su incertidumbre sale de cuánto se ha equivocado esa regla en el pasado."))
    }
    k <- l$coef; se <- l$se_coef
    filas <- list()
    etiquetas <- c(enero_eleccion = "Enero de año electoral", restriccion = "Febrero a junio de año electoral")
    for (n in names(etiquetas)) if (n %in% names(k)) {
      filas[[length(filas) + 1]] <- tags$tr(
        tags$td(etiquetas[[n]]), tags$td(class = "n", paste0("×", num(exp(k[[n]]), 2))),
        tags$td(class = "n", paste0("×", num(exp(k[[n]] - 1.96 * se[[n]]), 2), " a ×", num(exp(k[[n]] + 1.96 * se[[n]]), 2))))
    }
    meses <- grep("^mes_", names(k), value = TRUE)
    for (n in meses) {
      m <- as.integer(sub("mes_", "", n))
      filas[[length(filas) + 1]] <- tags$tr(
        tags$td(paste0(toupper(substr(MESES[m], 1, 1)), substr(MESES[m], 2, 3), " frente a enero")), tags$td(class = "n", paste0("×", num(exp(k[[n]]), 2))),
        tags$td(class = "n", paste0("×", num(exp(k[[n]] - 1.96 * se[[n]]), 2), " a ×", num(exp(k[[n]] + 1.96 * se[[n]]), 2))))
    }
    filas[[length(filas) + 1]] <- tags$tr(tags$td("Media móvil del error (θ)"), tags$td(class = "n", num(k[["ma1"]], 2)), tags$td())
    tagList(
      p(class = "small text-muted", "Factores multiplicativos: ×2 quiere decir el doble de contratos. Intervalos de confianza del 95 %."),
      tags$table(class = "tabla",
                 tags$thead(tags$tr(tags$th("Efecto"), tags$th(class = "n", "Estimado"), tags$th(class = "n", "IC 95 %"))),
                 tags$tbody(filas))
    )
  })

  # ---- Evaluación ----
  origenes <- sort(unique(d$retro$origen))
  destacado <- max(d$retro$origen[format(d$retro$origen, "%m") == "12"])
  output$control_origen <- renderUI(
    selectInput("origen", "Origen del pronóstico", choices = setNames(as.character(rev(origenes)), mes_anio(rev(origenes))),
                selected = as.character(destacado)))

  output$g_eval <- renderPlotly({
    req(input$origen)
    o <- as.Date(input$origen)
    r <- d$retro[d$retro$origen == o & d$retro$modelo %in% input$modelos_eval, ]
    ventana <- serie[serie$fecha >= o - 400 & serie$fecha <= max(r$fecha, o), ]
    p <- plot_ly() |>
      add_lines(data = ventana, x = ~fecha, y = ~contratos, name = "Observado",
                line = list(color = TINTA, width = 2), hovertemplate = "%{y:,d}<extra>Observado</extra>")
    for (m in input$modelos_eval) {
      rm <- r[r$modelo == m, ]
      p <- p |> add_lines(data = rm, x = ~fecha, y = ~prediccion, name = m,
                          line = list(color = COLOR_MODELO[[m]], width = 2.2, dash = "dot"),
                          hovertemplate = paste0("%{y:,.0f}<extra>", m, "</extra>"))
    }
    p |> grafico_base() |>
      layout(shapes = list(list(type = "line", x0 = o, x1 = o, yref = "paper", y0 = 0, y1 = 1,
                                line = list(color = TENUE, dash = "dash", width = 1))))
  })

  output$tabla_eval <- renderUI({
    e <- d$evaluacion[order(d$evaluacion$mape), ]
    tagList(
      tags$table(class = "tabla",
        tags$thead(tags$tr(tags$th("Modelo"), tags$th(class = "n", "MAPE"), tags$th(class = "n", "Mediana del error"),
                           tags$th(class = "n", "Dentro del 80 %"))),
        tags$tbody(lapply(seq_len(nrow(e)), function(i) tags$tr(
          tags$td(if (e$elegido[i] == "si") tags$b(e$modelo[i], " (elegido)") else e$modelo[i]),
          tags$td(class = "n", pct(e$mape[i])), tags$td(class = "n", pct(e$mediana_ape[i])),
          tags$td(class = "n", pct(e$cobertura_80[i])))))),
      p(class = "small text-muted mt-2",
        num(e$predicciones[1]), " predicciones por modelo. El ingenuo empata en el mes típico (mediana); ",
        "el modelo electoral gana en el promedio porque evita los errores enormes de los años de elección.")
    )
  })

  output$g_horizonte <- renderPlotly({
    r <- d$retro
    r$ape <- abs(r$prediccion - r$observado) / r$observado
    a <- aggregate(ape ~ modelo + horizonte, data = r, FUN = median)
    p <- plot_ly()
    for (m in MODELOS) {
      am <- a[a$modelo == m, ]
      p <- p |> add_lines(data = am, x = ~horizonte, y = ~ape, name = m, line = list(color = COLOR_MODELO[[m]], width = 2.2),
                          hovertemplate = paste0("%{y:.1%}<extra>", m, "</extra>"))
    }
    p |> grafico_base("Mediana del error") |>
      layout(yaxis = list(tickformat = ".0%", rangemode = "tozero"), xaxis = list(title = "Meses hacia adelante", dtick = 1),
             legend = list(orientation = "h", x = 0, y = -0.3, yanchor = "top")) |>
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)
