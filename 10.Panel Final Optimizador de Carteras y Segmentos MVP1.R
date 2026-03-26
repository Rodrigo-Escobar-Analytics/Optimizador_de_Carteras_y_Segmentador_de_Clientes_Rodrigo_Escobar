library(shiny)

if (!requireNamespace("callr", quietly = TRUE)) {
  install.packages("callr")
}
library(callr)

# =========================
# Rutas de los 3 dashboards
# =========================
app_con_pref <- normalizePath(
  "10a.Panel Final Optimizador de Carteras MVP1 - Con Preferencias.R",
  winslash = "/",
  mustWork = TRUE
)

app_sin_pref <- normalizePath(
  "10b.Panel Final Optimizador de Carteras MVP1 - Sin Preferencias.R",
  winslash = "/",
  mustWork = TRUE
)

app_dap_fm <- normalizePath(
  "10c.Panel Final Optimizador de Carteras MVP1 - DAP-FM.R",
  winslash = "/",
  mustWork = TRUE
)

# =========================
# Guardar procesos hijos
# =========================
child_apps <- new.env(parent = emptyenv())
child_apps$procs <- list()

# =========================
# Revisar si un puerto está abierto
# =========================
is_port_open <- function(port, host = "127.0.0.1") {
  con <- suppressWarnings(
    try(
      socketConnection(
        host = host,
        port = port,
        open = "r+",
        blocking = TRUE,
        timeout = 1
      ),
      silent = TRUE
    )
  )
  
  if (inherits(con, "try-error")) {
    FALSE
  } else {
    close(con)
    TRUE
  }
}

# =========================
# Levantar script shiny en background y mantenerlo vivo
# =========================
start_child_app <- function(app_file, port, key) {
  # Si ya hay proceso vivo, no relanzar
  if (!is.null(child_apps$procs[[key]])) {
    p <- child_apps$procs[[key]]
    if (inherits(p, "r_process") && p$is_alive()) {
      return(invisible(NULL))
    }
  }
  
  # Si el puerto ya está activo, tampoco relanzar
  if (is_port_open(port)) {
    return(invisible(NULL))
  }
  
  proc <- callr::r_bg(
    func = function(app_file, port) {
      library(shiny)
      
      setwd(dirname(app_file))
      
      shiny::runApp(
        appDir = app_file,
        host = "127.0.0.1",
        port = port,
        launch.browser = FALSE
      )
    },
    args = list(app_file = app_file, port = port),
    supervise = TRUE
  )
  
  child_apps$procs[[key]] <- proc
  
  # Esperar a que levante
  for (i in 1:15) {
    Sys.sleep(1)
    if (is_port_open(port)) break
  }
}

# =========================
# Levantar las 3 apps al iniciar
# =========================
start_child_app(app_con_pref, 7441, "conpref")
start_child_app(app_sin_pref, 7442, "sinpref")
start_child_app(app_dap_fm, 7443, "dapfm")

# =========================
# UI
# =========================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f6f8fb;
      }
      .card-wrap {
        display: flex;
        gap: 20px;
        flex-wrap: wrap;
        margin-top: 25px;
      }
      .card-link {
        flex: 1 1 280px;
        min-width: 280px;
        max-width: 360px;
        background: white;
        border: 1px solid #d9e2ec;
        border-radius: 14px;
        padding: 22px 24px;
        text-decoration: none !important;
        color: #1f2937 !important;
        box-shadow: 0 4px 14px rgba(0,0,0,0.05);
      }
      .card-link:hover {
        border-color: #2c7fb8;
        box-shadow: 0 8px 22px rgba(44,127,184,0.12);
      }
      .card-title {
        font-size: 20px;
        font-weight: 700;
        margin-bottom: 10px;
      }
      .card-text {
        font-size: 14px;
        line-height: 1.6;
        color: #4b5563;
      }
      .estado-box {
        background: white;
        border: 1px solid #d9e2ec;
        border-radius: 12px;
        padding: 16px 18px;
        margin-top: 20px;
      }
      .titulo-principal {
        margin-top: 20px;
      }
      .ok {
        color: #15803d;
        font-weight: 600;
      }
      .fail {
        color: #b91c1c;
        font-weight: 600;
      }
    "))
  ),
  
  div(
    class = "titulo-principal",
    h2("Panel Final Optimizador de Carteras MVP1"),
    p("Seleccione el dashboard que desea abrir. Cada panel se ejecuta por separado para evitar conflictos entre filtros, tablas y descargas.")
  ),
  
  div(
    class = "estado-box",
    strong("Estado de servicios"),
    br(), br(),
    uiOutput("estado_apps")
  ),
  
  div(
    class = "card-wrap",
    
    tags$a(
      href = "http://127.0.0.1:7441",
      target = "_blank",
      class = "card-link",
      div(class = "card-title", "Con Preferencias"),
      div(
        class = "card-text",
        "Comparación entre cartera actual y propuesta optimizada incorporando preferencias declaradas, con foco en riesgo, retorno y accionabilidad."
      )
    ),
    
    tags$a(
      href = "http://127.0.0.1:7442",
      target = "_blank",
      class = "card-link",
      div(class = "card-title", "Sin Preferencias"),
      div(
        class = "card-text",
        "Versión cuantitativa base del optimizador, orientada a evaluar mejoras técnicas en la relación riesgo-retorno sin incorporar preferencias."
      )
    ),
    
    tags$a(
      href = "http://127.0.0.1:7443",
      target = "_blank",
      class = "card-link",
      div(class = "card-title", "DAP / FM"),
      div(
        class = "card-text",
        "Visualización comercial de instrumentos DAP y FM, con KPIs, composición, tablas por cliente y capa de segmentación."
      )
    )
  )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
  
  output$estado_apps <- renderUI({
    estados <- list(
      list(nombre = "Con Preferencias", port = 7441, key = "conpref"),
      list(nombre = "Sin Preferencias", port = 7442, key = "sinpref"),
      list(nombre = "DAP/FM", port = 7443, key = "dapfm")
    )
    
    tagList(
      lapply(estados, function(x) {
        proc <- child_apps$procs[[x$key]]
        vivo <- !is.null(proc) && inherits(proc, "r_process") && proc$is_alive()
        puerto <- is_port_open(x$port)
        
        estado_txt <- if (vivo && puerto) {
          span(class = "ok", "Activo")
        } else {
          span(class = "fail", "No disponible")
        }
        
        div(
          paste0(x$nombre, " (", x$port, "): "),
          estado_txt
        )
      })
    )
  })
}

shinyApp(ui, server)
shinyApp(ui, server)