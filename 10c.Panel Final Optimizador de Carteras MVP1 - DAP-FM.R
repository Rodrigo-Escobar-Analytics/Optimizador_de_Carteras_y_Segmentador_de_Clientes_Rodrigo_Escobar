library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(scales)
library(openxlsx)

DATA_PATH <- "FILES/OUTPUT/09_INDICADORES_DAP_FM_SEGMENTO.CSV"

# =========================
# 1) Carga base
# =========================
df <- read_delim(
  DATA_PATH,
  delim = ";",
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE
)

# Limpieza básica
df <- df %>%
  mutate(
    across(c(
      ID_CLIENTE, ID_CUENTA, TIPO, ORIGEN, MONEDA, NOMBRE_FONDO, TIPO_INSTRUMENTO,
      USERNAME, NOMBRE_EJECUTIVO, EQUIPO, JEFE_AREA, NOMBRE_USUARIO_JEFE_AREA,
      CLUSTERNOMBRE, ALERTA_TEMPRANA_CLUSTER
    ), ~ as.character(.x)),
    
    ALERTA_TEMPRANA_CLUSTER = case_when(
      is.na(ALERTA_TEMPRANA_CLUSTER) | trimws(ALERTA_TEMPRANA_CLUSTER) == "" ~ "NO",
      TRUE ~ ALERTA_TEMPRANA_CLUSTER
    )
  )

# =========================
# 2) Dataset por cuenta
# =========================
acct_df <- df %>%
  arrange(ID_CLIENTE, ID_CUENTA) %>%
  distinct(ID_CLIENTE, ID_CUENTA, .keep_all = TRUE) %>%
  select(
    ID_CLIENTE, ID_CUENTA,
    NOMBRE_EJECUTIVO, EQUIPO, JEFE_AREA, NOMBRE_USUARIO_JEFE_AREA,
    CLUSTERNOMBRE, ALERTA_TEMPRANA_CLUSTER,
    Q_CUENTAS, Q_EMPRESAS_INVIERTE,
    CANTIDAD_EN_CUSTODIA_TOTAL,
    MONTO_TOTAL_ACCIONES, MONTO_TOTAL_DAP, MONTO_TOTAL_FM, MONTO_TOTAL_CLIENTE,
    RET_ANUAL_PONDERADO, DE_ANUAL_PONDERADO, SHARPE_ANUAL_PONDERADO,
    NPS, INTERACCIONES_NEGATIVAS_6M, INTERACCIONES_NEUTRAS_6M, INTERACCIONES_POSITIVAS_6M
  )

# =========================
# 3) Función helper filtros
# =========================
keep_or_all <- function(current_value, choices) {
  if (!is.null(current_value) && current_value %in% choices) current_value else "All"
}

# =========================
# 4) UI
# =========================
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .container, .container-fluid { width: 100% !important; max-width: 100% !important; }
      .dataTables_wrapper { width: 100% !important; }
    "))
  ),
  
  titlePanel("Dashboard DAP/FM con Segmentos"),
  
  div(
    style = "
      background-color: #f8f9fa;
      border-left: 5px solid #5b9bd5;
      padding: 18px 22px;
      margin-top: 10px;
      margin-bottom: 20px;
      line-height: 1.6;
    ",
    h3("Descripción DAP/FM"),
    p("Este panel permite visualizar la composición de instrumentos DAP y FM por cliente, cuenta, ejecutivo y segmento. Integra indicadores de tenencia, diversificación, desempeño financiero y experiencia para apoyar el análisis comercial."),
    h4("Cómo leerlo", style = "margin-top: 20px;"),
    tags$ul(
      tags$li("Revise primero los indicadores resumen para entender tamaño de cartera, concentración y composición."),
      tags$li("Use el gráfico de composición para identificar el peso relativo entre DAP y FM."),
      tags$li("Consulte la matriz de segmentos para entender la distribución comercial del universo filtrado."),
      tags$li("Revise la tabla por cliente y luego el detalle por instrumento para bajar al nivel operativo.")
    ),
    h4("Resultado esperado", style = "margin-top: 20px;"),
    p("Una visión integrada de la cartera DAP/FM, útil para priorizar gestión comercial, identificar oportunidades de crecimiento y contextualizar cada cliente dentro de su segmento.")
  ),
  
  fluidRow(
    column(2, selectInput("tipo", "TIPO DAP/FM", choices = "All", selected = "All")),
    column(2, selectInput("jefe", "JEFE AREA", choices = "All", selected = "All")),
    column(2, selectInput("ejecutivo", "NOMBRE EJECUTIVO", choices = "All", selected = "All")),
    column(2, selectInput("equipo", "EQUIPO", choices = "All", selected = "All")),
    column(2, selectInput("cluster", "CLUSTERNOMBRE", choices = "All", selected = "All")),
    column(2, selectInput("cliente", "ID_CLIENTE", choices = "All", selected = "All")),
    column(2, selectInput("cuenta", "ID_CUENTA", choices = "All", selected = "All"))
  ),
  
  hr(),
  h2("Indicadores Clave Resumen"),
  uiOutput("kpis"),
  
  hr(),
  h4("Composición DAP/FM"),
  plotOutput("grafico_composicion", height = 420, width = "100%"),
  
  br(),
  h4("Matriz Segmento vs Alerta Temprana"),
  DTOutput("matriz_segmento_alerta"),
  
  div(
    style = "
      background-color: #f8f9fa;
      border: 1px solid #d9d9d9;
      border-radius: 8px;
      padding: 20px 24px;
      margin-top: 20px;
      margin-bottom: 20px;
      line-height: 1.6;
    ",
    
    h3("Descripción Segmentos"),
    
    p("La segmentación identifica perfiles diferenciados según: tenencia, desempeño financiero y experiencia."),
    p("Se distinguen tres focos estratégicos:"),
    
    tags$ul(
      tags$li(tags$b("Preferentes:"), " alto patrimonio y valor comercial"),
      tags$li(tags$b("Críticos:"), " clientes con fricción en experiencia"),
      tags$li(tags$b("Estándar/Tácticos:"), " base operativa y oportunidades de crecimiento")
    ),
    
    h4("Segmentos y acciones", style = "margin-top: 20px;"),
    
    p(tags$b("Diversificado Tenso - Crítico"),
      style = "margin-top: 28px; margin-bottom: 10px; font-size: 18px;"),
    p(tags$b("Perfil:"), " Patrimonio diversificado, buen rendimiento, alta interacción y baja satisfacción.",
      style = "margin-bottom: 6px;"),
    p(tags$b("Acción:"), " Mejorar experiencia + contacto proactivo para retención",
      style = "margin-bottom: 10px;"),
    
    p(tags$b("Accionario Preferente - Preferente"),
      style = "margin-top: 28px; margin-bottom: 10px; font-size: 18px;"),
    p(tags$b("Perfil:"), " Alto patrimonio en acciones, máxima rentabilidad y alta sofisticación.",
      style = "margin-bottom: 6px;"),
    p(tags$b("Acción:"), " Herramientas avanzadas + retención silenciosa",
      style = "margin-bottom: 10px;"),
    
    p(tags$b("Mixto Exigente - Crítico"),
      style = "margin-top: 28px; margin-bottom: 10px; font-size: 18px;"),
    p(tags$b("Perfil:"), " Portafolio mixto, alta interacción y fricción en experiencia.",
      style = "margin-bottom: 6px;"),
    p(tags$b("Acción:"), " Simplificación + educación + optimización de atención",
      style = "margin-bottom: 10px;"),
    
    p(tags$b("Accionario Tradicional - Estándar"),
      style = "margin-top: 28px; margin-bottom: 10px; font-size: 18px;"),
    p(tags$b("Perfil:"), " Cliente masivo, desempeño medio y estabilidad en experiencia.",
      style = "margin-bottom: 6px;"),
    p(tags$b("Acción:"), " Fidelización + cross-sell básico",
      style = "margin-bottom: 10px;"),
    
    p(tags$b("Accionario Táctico - Táctico"),
      style = "margin-top: 28px; margin-bottom: 10px; font-size: 18px;"),
    p(tags$b("Perfil:"), " Bajo patrimonio, alta volatilidad y comportamiento agresivo.",
      style = "margin-bottom: 6px;"),
    p(tags$b("Acción:"), " Educación en riesgo + contención",
      style = "margin-bottom: 10px;"),
    
    p(tags$b("Patrimonial Preferente - Preferente"),
      style = "margin-top: 28px; margin-bottom: 10px; font-size: 18px;"),
    p(tags$b("Perfil:"), " Alto patrimonio diversificado, buen desempeño, alta interacción y fricción.",
      style = "margin-bottom: 6px;"),
    p(tags$b("Acción:"), " Atención personalizada + ejecutivo senior",
      style = "margin-bottom: 10px;"),
    
    h4("Prioridad", style = "margin-top: 25px;"),
    tags$ol(
      tags$li("Retener críticos de alto valor"),
      tags$li("Monetizar preferentes"),
      tags$li("Escalar eficiencia en base")
    )
  ),
  
  hr(),
  h3("Resumen por Cliente"),
  downloadButton("download_clientes", "Descargar Resumen Clientes (Excel)"),
  br(), br(),
  DTOutput("tabla_cliente"),
  
  hr(),
  h3("Detalle DAP/FM"),
  downloadButton("download_detalle", "Descargar Detalle DAP/FM (Excel)"),
  br(), br(),
  DTOutput("tabla_detalle")
)

# =========================
# 5) Server
# =========================
server <- function(input, output, session) {
  
  # ---- Inicialización filtros
  observe({
    updateSelectInput(session, "tipo",
                      choices = c("All", sort(unique(df$TIPO))),
                      selected = "All")
    updateSelectInput(session, "jefe",
                      choices = c("All", sort(unique(acct_df$JEFE_AREA))),
                      selected = "All")
    updateSelectInput(session, "ejecutivo",
                      choices = c("All", sort(unique(acct_df$NOMBRE_EJECUTIVO))),
                      selected = "All")
    updateSelectInput(session, "equipo",
                      choices = c("All", sort(unique(acct_df$EQUIPO))),
                      selected = "All")
    updateSelectInput(session, "cluster",
                      choices = c("All", sort(unique(acct_df$CLUSTERNOMBRE))),
                      selected = "All")
    updateSelectInput(session, "cliente",
                      choices = c("All", sort(unique(acct_df$ID_CLIENTE))),
                      selected = "All")
    updateSelectInput(session, "cuenta",
                      choices = c("All", sort(unique(acct_df$ID_CUENTA))),
                      selected = "All")
  })
  
  # ---- Dataset filtrado a nivel cuenta
  filtered_accts <- reactive({
    out <- acct_df
    
    if (input$jefe != "All") out <- out %>% filter(JEFE_AREA == input$jefe)
    if (input$ejecutivo != "All") out <- out %>% filter(NOMBRE_EJECUTIVO == input$ejecutivo)
    if (input$equipo != "All") out <- out %>% filter(EQUIPO == input$equipo)
    if (input$cluster != "All") out <- out %>% filter(CLUSTERNOMBRE == input$cluster)
    if (input$cliente != "All") out <- out %>% filter(ID_CLIENTE == input$cliente)
    if (input$cuenta != "All") out <- out %>% filter(ID_CUENTA == input$cuenta)
    
    if (input$tipo != "All") {
      cuentas_tipo <- df %>%
        filter(TIPO == input$tipo) %>%
        distinct(ID_CLIENTE, ID_CUENTA)
      
      out <- out %>%
        semi_join(cuentas_tipo, by = c("ID_CLIENTE", "ID_CUENTA"))
    }
    
    out
  })
  # ---- Base detalle filtrada
  filtered_detail <- reactive({
    out <- df %>%
      semi_join(
        filtered_accts() %>% distinct(ID_CLIENTE, ID_CUENTA),
        by = c("ID_CLIENTE", "ID_CUENTA")
      )
    
    if (input$tipo != "All") {
      out <- out %>% filter(TIPO == input$tipo)
    }
    
    out
  })
  
  
  
  # ---- KPIs
  output$kpis <- renderUI({
    accts <- filtered_accts()
    det <- filtered_detail()
    
    q_clientes <- n_distinct(accts$ID_CLIENTE)
    q_cuentas  <- n_distinct(accts$ID_CUENTA)
    monto_dap  <- sum(det$MONTO_TOTAL_INSTRUMENTO[det$TIPO == "DAP"], na.rm = TRUE)
    monto_fm   <- sum(det$MONTO_TOTAL_INSTRUMENTO[det$TIPO == "FM"], na.rm = TRUE)
    monto_tot  <- sum(det$MONTO_TOTAL_INSTRUMENTO, na.rm = TRUE)
    q_instr    <- nrow(det)
    ticket_cli <- ifelse(q_clientes > 0, monto_tot / q_clientes, 0)
    
    HTML(paste0(
      "<div style='display:flex; justify-content:space-between; gap:30px; flex-wrap:wrap;'>",
      "<div>",
      "<b>Q Clientes:</b> ", format(q_clientes, big.mark = ".", decimal.mark = ","), "<br/>",
      "<b>Q Cuentas:</b> ", format(q_cuentas, big.mark = ".", decimal.mark = ","), "<br/>",
      "<b>Q Instrumentos:</b> ", format(q_instr, big.mark = ".", decimal.mark = ","), "<br/>",
      "</div>",
      "<div>",
      "<b>Monto Total DAP:</b> ", format(round(monto_dap, 0), big.mark = ".", decimal.mark = ","), "<br/>",
      "<b>Monto Total FM:</b> ", format(round(monto_fm, 0), big.mark = ".", decimal.mark = ","), "<br/>",
      "<b>Monto Total DAP/FM:</b> ", format(round(monto_tot, 0), big.mark = ".", decimal.mark = ","), "<br/>",
      "</div>",
      "<div>",
      "<b>Ticket Promedio Cliente:</b> ", format(round(ticket_cli, 0), big.mark = ".", decimal.mark = ","), "<br/>",
      "<b>% DAP:</b> ", percent(ifelse(monto_tot > 0, monto_dap / monto_tot, 0), accuracy = 0.1), "<br/>",
      "<b>% FM:</b> ", percent(ifelse(monto_tot > 0, monto_fm / monto_tot, 0), accuracy = 0.1), "<br/>",
      "</div>",
      "</div>"
    ))
  })
  
  # ---- Gráfico composición
  output$grafico_composicion <- renderPlot({
    det <- filtered_detail() %>%
      mutate(
        NOMBRE_FONDO = iconv(as.character(NOMBRE_FONDO), from = "", to = "UTF-8", sub = "")
      ) %>%
      filter(!is.na(NOMBRE_FONDO), NOMBRE_FONDO != "", trimws(NOMBRE_FONDO) != "") %>%
      group_by(TIPO, NOMBRE_FONDO) %>%
      summarise(MONTO = sum(MONTO_TOTAL_INSTRUMENTO, na.rm = TRUE), .groups = "drop")
    
    top_fondos <- det %>%
      group_by(NOMBRE_FONDO) %>%
      summarise(MONTO_TOTAL = sum(MONTO, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(MONTO_TOTAL)) %>%
      slice_head(n = 10) %>%
      pull(NOMBRE_FONDO)
    
    det <- det %>%
      mutate(
        NOMBRE_FONDO = ifelse(NOMBRE_FONDO %in% top_fondos, NOMBRE_FONDO, "OTROS")
      ) %>%
      group_by(TIPO, NOMBRE_FONDO) %>%
      summarise(MONTO = sum(MONTO, na.rm = TRUE), .groups = "drop")
    
    ggplot(det, aes(x = TIPO, y = MONTO, fill = NOMBRE_FONDO)) +
      geom_col() +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
      labs(
        x = NULL,
        y = "Monto Total",
        title = "Composición por Tipo y Fondo",
        fill = "Nombre Fondo"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "right"
      )
  })
  
  # ---- Matriz Segmento vs Alerta (1 vez por cliente)
  output$matriz_segmento_alerta <- renderDT({
    sub <- filtered_detail() %>%
      group_by(ID_CLIENTE) %>%
      summarise(
        SEGMENTO = dplyr::first(CLUSTERNOMBRE[!is.na(CLUSTERNOMBRE) & trimws(CLUSTERNOMBRE) != ""]),
        ALERTA   = dplyr::first(ALERTA_TEMPRANA_CLUSTER[!is.na(ALERTA_TEMPRANA_CLUSTER) & trimws(ALERTA_TEMPRANA_CLUSTER) != ""]),
        .groups = "drop"
      ) %>%
      mutate(
        SEGMENTO = case_when(
          is.na(SEGMENTO) | trimws(SEGMENTO) == "" ~ "SIN SEGMENTO",
          TRUE ~ SEGMENTO
        ),
        ALERTA = case_when(
          is.na(ALERTA) | trimws(ALERTA) == "" ~ "NO",
          TRUE ~ ALERTA
        )
      ) %>%
      mutate(
        SEGMENTO = factor(
          SEGMENTO,
          levels = c(
            "Accionario Preferente",
            "Accionario Tactico",
            "Accionario Tradicional",
            "Diversificado Tenso",
            "Mixto Exigente",
            "Patrimonial Preferente",
            "SIN SEGMENTO"
          )
        ),
        ALERTA = factor(
          ALERTA,
          levels = c(
            "NO",
            "Potencial Entrada Critico/Tenso",
            "Potencial Salida Critico/Tenso",
            "Potencial Entrada Preferente",
            "Potencial Salida Preferente"
          )
        )
      )
    
    mat <- xtabs(~ SEGMENTO + ALERTA, data = sub)
    mat_total <- addmargins(mat)
    
    out <- as.data.frame.matrix(mat_total)
    out <- cbind(Segmento = rownames(out), out)
    rownames(out) <- NULL
    
    names(out)[names(out) == "Sum"] <- "Total general"
    out$Segmento[out$Segmento == "Sum"] <- "Total general"
    
    datatable(
      out,
      rownames = FALSE,
      options = list(dom = "t", ordering = FALSE, paging = FALSE, scrollX = TRUE),
      class = "compact stripe hover"
    ) %>%
      formatCurrency(
        columns = setdiff(names(out), "Segmento"),
        currency = "",
        interval = 3,
        mark = ".",
        dec.mark = ",",
        digits = 0
      )
  })
  
  # ---- Resumen por cliente
  resumen_cliente <- reactive({
    filtered_detail() %>%
      group_by(ID_CLIENTE) %>%
      summarise(
        NOMBRE_EJECUTIVO           = dplyr::first(NOMBRE_EJECUTIVO[!is.na(NOMBRE_EJECUTIVO) & trimws(NOMBRE_EJECUTIVO) != ""]),
        EQUIPO                     = dplyr::first(EQUIPO[!is.na(EQUIPO) & trimws(EQUIPO) != ""]),
        JEFE_AREA                  = dplyr::first(JEFE_AREA[!is.na(JEFE_AREA) & trimws(JEFE_AREA) != ""]),
        CLUSTERNOMBRE              = dplyr::first(CLUSTERNOMBRE[!is.na(CLUSTERNOMBRE) & trimws(CLUSTERNOMBRE) != ""]),
        ALERTA_TEMPRANA_CLUSTER    = dplyr::first(ALERTA_TEMPRANA_CLUSTER[!is.na(ALERTA_TEMPRANA_CLUSTER) & trimws(ALERTA_TEMPRANA_CLUSTER) != ""]),
        
        Q_CUENTAS                  = max(Q_CUENTAS, na.rm = TRUE),
        Q_EMPRESAS_INVIERTE        = max(Q_EMPRESAS_INVIERTE, na.rm = TRUE),
        CANTIDAD_EN_CUSTODIA_TOTAL = max(CANTIDAD_EN_CUSTODIA_TOTAL, na.rm = TRUE),
        MONTO_TOTAL_ACCIONES       = max(MONTO_TOTAL_ACCIONES, na.rm = TRUE),
        MONTO_TOTAL_DAP            = max(MONTO_TOTAL_DAP, na.rm = TRUE),
        MONTO_TOTAL_FM             = max(MONTO_TOTAL_FM, na.rm = TRUE),
        MONTO_TOTAL_CLIENTE        = max(MONTO_TOTAL_CLIENTE, na.rm = TRUE),
        RET_ANUAL_PONDERADO        = max(RET_ANUAL_PONDERADO, na.rm = TRUE),
        DE_ANUAL_PONDERADO         = max(DE_ANUAL_PONDERADO, na.rm = TRUE),
        SHARPE_ANUAL_PONDERADO     = max(SHARPE_ANUAL_PONDERADO, na.rm = TRUE),
        NPS                        = max(NPS, na.rm = TRUE),
        INTERACCIONES_NEGATIVAS_6M = max(INTERACCIONES_NEGATIVAS_6M, na.rm = TRUE),
        INTERACCIONES_NEUTRAS_6M   = max(INTERACCIONES_NEUTRAS_6M, na.rm = TRUE),
        INTERACCIONES_POSITIVAS_6M = max(INTERACCIONES_POSITIVAS_6M, na.rm = TRUE),
        
        .groups = "drop"
      )
  })
  
  output$tabla_cliente <- renderDT({
    datatable(
      resumen_cliente(),
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 10)
    ) %>%
      formatPercentage(c("RET_ANUAL_PONDERADO", "DE_ANUAL_PONDERADO"), digits = 1) %>%
      formatRound(c("SHARPE_ANUAL_PONDERADO", "NPS"), digits = 2) %>%
      formatCurrency(
        c("MONTO_TOTAL_ACCIONES", "MONTO_TOTAL_DAP", "MONTO_TOTAL_FM", "MONTO_TOTAL_CLIENTE"),
        currency = "",
        interval = 3,
        mark = ".",
        dec.mark = ",",
        digits = 0
      ) %>%
      formatRound(
        c("CANTIDAD_EN_CUSTODIA_TOTAL", "Q_CUENTAS", "Q_EMPRESAS_INVIERTE",
          "INTERACCIONES_NEGATIVAS_6M", "INTERACCIONES_NEUTRAS_6M", "INTERACCIONES_POSITIVAS_6M"),
        digits = 0
      )
  })
  
  # ---- Detalle
  output$tabla_detalle <- renderDT({
    det <- filtered_detail() %>%
      select(
        ID_CLIENTE, ID_CUENTA, TIPO, ORIGEN, MONEDA,
        NOMBRE_FONDO, TIPO_INSTRUMENTO,
        NOMBRE_EJECUTIVO, EQUIPO,
        MONTO_TOTAL_INSTRUMENTO, Q, VALOR_UNITARIO,
        RETORNO_MES, CLUSTERNOMBRE, ALERTA_TEMPRANA_CLUSTER
      )
    
    datatable(
      det,
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 15)
    ) %>%
      formatCurrency(
        c("MONTO_TOTAL_INSTRUMENTO", "VALOR_UNITARIO"),
        currency = "",
        interval = 3,
        mark = ".",
        dec.mark = ",",
        digits = 0
      ) %>%
      formatPercentage("RETORNO_MES", digits = 2) %>%
      formatRound("Q", digits = 0)
  })
  
  # ---- Export resumen cliente
  output$download_clientes <- downloadHandler(
    filename = function() {
      paste0("Resumen_Clientes_DAP_FM_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Resumen Clientes")
      writeData(wb, "Resumen Clientes", resumen_cliente())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---- Export detalle
  output$download_detalle <- downloadHandler(
    filename = function() {
      paste0("Detalle_DAP_FM_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Detalle DAP FM")
      writeData(wb, "Detalle DAP FM", filtered_detail())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)


