library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(scales)
library(openxlsx)
library(ggrepel)

DATA_PATH <- "FILES/OUTPUT/05_PESOS_INDICADORES_CARTERA.csv"

# CARGA DE DATOS
df <- read_delim(
  DATA_PATH,
  delim = ";",
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE
)

# SET DE DATOS CON ID_CLIENTE+ID_CUENTA (SIN PREFERENCIAS)
acct_df <- df %>%
  arrange(ID_CLIENTE, ID_CUENTA) %>%
  distinct(ID_CLIENTE, ID_CUENTA, .keep_all = TRUE) %>%
  select(
    ID_CLIENTE, ID_CUENTA, USERNAME, NOMBRE_EJECUTIVO, JEFE_AREA,
    MONTO_EN_CUSTODIA_TOTAL,
    RET_PROM_ANUAL, DE_ANUAL, SHARPE_ANUAL,
    RET_ESPERADO_ANUAL_OPTM, DESVEST_ANUAL_OPTM, SHARPE_ANUAL_OPTM,
    GAP_RET_ANUAL, GAP_DESV_ANUAL, GAP_SHARPE_ANUAL,
    MSG, FLAG_MEJORA_RIESGO_ANUAL, FLAG_MEJORA_RETORNO_ANUAL
  )

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .container, .container-fluid { width: 100% !important; max-width: 100% !important; }
    .dataTables_wrapper { width: 100% !important; }
  "))),
  titlePanel("Dashboard de Carteras Optimizadas - Sin Preferencias"),
  fluidRow(
    column(2, selectInput("jefe", "JEFE AREA", choices = "All", selected = "All")),
    column(4, selectInput("ejecutivo", "NOMBRE EJECUTIVO", choices = "All", selected = "All")),
    column(2, selectInput("cliente", "ID_CLIENTE", choices = "All", selected = "All")),
    column(2, selectInput("cuenta", "ID_CUENTA", choices = "All", selected = "All")),
    column(2, selectInput("mejora", "MEJORA/EMPEORA", choices = "All", selected = "All"))
  ),
  hr(),
  fluidRow(
    column(
      12,
      uiOutput("kpis"),
      hr(),
      plotOutput("risk_return", height = 420, width = "100%"),
      hr(),
      fluidRow(
        column(
          12,
          downloadButton("dl_resumen", "Descargar Resumen (Excel)"),
          br(), br(),
          DTOutput("tabla_resumen")
        )
      ),
      hr(),
      fluidRow(
        column(
          12,
          downloadButton("dl_actions", "Descargar Acciones (Excel)"),
          br(), br(),
          DTOutput("actions")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  keep_or_all <- function(current_value, choices) {
    if (!is.null(current_value) && current_value %in% choices) current_value else "All"
  }
  
  observe({
    updateSelectInput(session, "jefe",
                      choices = c("All", sort(unique(acct_df$JEFE_AREA))),
                      selected = "All")
    updateSelectInput(session, "ejecutivo",
                      choices = c("All", sort(unique(acct_df$NOMBRE_EJECUTIVO))),
                      selected = "All")
    updateSelectInput(session, "cliente",
                      choices = c("All", sort(unique(acct_df$ID_CLIENTE))),
                      selected = "All")
    updateSelectInput(session, "cuenta",
                      choices = c("All", sort(unique(acct_df$ID_CUENTA))),
                      selected = "All")
    updateSelectInput(session, "mejora",
                      choices = c("All", sort(unique(acct_df$FLAG_MEJORA_RETORNO_ANUAL))),
                      selected = "All")
  })
  
  observeEvent(input$jefe, {
    d <- acct_df
    if (input$jefe != "All") d <- d %>% filter(JEFE_AREA == input$jefe)
    
    exec_choices <- c("All", sort(unique(d$NOMBRE_EJECUTIVO)))
    updateSelectInput(session, "ejecutivo",
                      choices = exec_choices,
                      selected = keep_or_all(input$ejecutivo, exec_choices))
    
    cli_choices <- c("All", sort(unique(d$ID_CLIENTE)))
    updateSelectInput(session, "cliente",
                      choices = cli_choices,
                      selected = keep_or_all(input$cliente, cli_choices))
    
    acc_choices <- c("All", sort(unique(d$ID_CUENTA)))
    updateSelectInput(session, "cuenta",
                      choices = acc_choices,
                      selected = keep_or_all(input$cuenta, acc_choices))
    
    mejora_choices <- c("All", sort(unique(d$FLAG_MEJORA_RETORNO_ANUAL)))
    updateSelectInput(session, "mejora",
                      choices = mejora_choices,
                      selected = keep_or_all(input$mejora, mejora_choices))
  }, ignoreInit = TRUE)
  
  observeEvent(input$ejecutivo, {
    d <- acct_df
    if (input$jefe != "All") d <- d %>% filter(JEFE_AREA == input$jefe)
    if (input$ejecutivo != "All") d <- d %>% filter(NOMBRE_EJECUTIVO == input$ejecutivo)
    
    cli_choices <- c("All", sort(unique(d$ID_CLIENTE)))
    updateSelectInput(session, "cliente",
                      choices = cli_choices,
                      selected = keep_or_all(input$cliente, cli_choices))
    
    acc_choices <- c("All", sort(unique(d$ID_CUENTA)))
    updateSelectInput(session, "cuenta",
                      choices = acc_choices,
                      selected = keep_or_all(input$cuenta, acc_choices))
    
    mejora_choices <- c("All", sort(unique(d$FLAG_MEJORA_RETORNO_ANUAL)))
    updateSelectInput(session, "mejora",
                      choices = mejora_choices,
                      selected = keep_or_all(input$mejora, mejora_choices))
  }, ignoreInit = TRUE)
  
  observeEvent(input$cliente, {
    d <- acct_df
    if (input$jefe != "All") d <- d %>% filter(JEFE_AREA == input$jefe)
    if (input$ejecutivo != "All") d <- d %>% filter(NOMBRE_EJECUTIVO == input$ejecutivo)
    if (input$cliente != "All") d <- d %>% filter(ID_CLIENTE == input$cliente)
    
    acc_choices <- c("All", sort(unique(d$ID_CUENTA)))
    updateSelectInput(session, "cuenta",
                      choices = acc_choices,
                      selected = keep_or_all(input$cuenta, acc_choices))
    
    mejora_choices <- c("All", sort(unique(d$FLAG_MEJORA_RETORNO_ANUAL)))
    updateSelectInput(session, "mejora",
                      choices = mejora_choices,
                      selected = keep_or_all(input$mejora, mejora_choices))
  }, ignoreInit = TRUE)
  
  observeEvent(input$mejora, {
    d <- acct_df
    if (input$jefe != "All") d <- d %>% filter(JEFE_AREA == input$jefe)
    if (input$ejecutivo != "All") d <- d %>% filter(NOMBRE_EJECUTIVO == input$ejecutivo)
    if (input$mejora != "All") d <- d %>% filter(FLAG_MEJORA_RETORNO_ANUAL == input$mejora)
    
    cli_choices <- c("All", sort(unique(d$ID_CLIENTE)))
    updateSelectInput(session, "cliente",
                      choices = cli_choices,
                      selected = keep_or_all(input$cliente, cli_choices))
    
    acc_choices <- c("All", sort(unique(d$ID_CUENTA)))
    updateSelectInput(session, "cuenta",
                      choices = acc_choices,
                      selected = keep_or_all(input$cuenta, acc_choices))
  }, ignoreInit = TRUE)
  
  filtered_accts <- reactive({
    out <- acct_df
    if (input$jefe != "All") out <- out %>% filter(JEFE_AREA == input$jefe)
    if (input$ejecutivo != "All") out <- out %>% filter(NOMBRE_EJECUTIVO == input$ejecutivo)
    if (input$cliente != "All") out <- out %>% filter(ID_CLIENTE == input$cliente)
    if (input$cuenta != "All") out <- out %>% filter(ID_CUENTA == input$cuenta)
    if (input$mejora != "All") out <- out %>% filter(FLAG_MEJORA_RETORNO_ANUAL == input$mejora)
    out
  })
  
  output$kpis <- renderUI({
    accts <- filtered_accts()
    
    n_accts <- nrow(accts)
    aum <- (sum(accts$MONTO_EN_CUSTODIA_TOTAL, na.rm = TRUE))
    
    sharpe_act <- mean(accts$SHARPE_ANUAL, na.rm = TRUE)
    sharpe_opt <- mean(accts$SHARPE_ANUAL_OPTM, na.rm = TRUE)
    gap_sharpe <- mean(accts$GAP_SHARPE_ANUAL, na.rm = TRUE)
    
    ret_act <- mean(accts$RET_PROM_ANUAL, na.rm = TRUE)
    ret_opt <- mean(accts$RET_ESPERADO_ANUAL_OPTM, na.rm = TRUE)
    
    de_act <- mean(accts$DE_ANUAL, na.rm = TRUE)
    de_opt <- mean(accts$DESVEST_ANUAL_OPTM, na.rm = TRUE)
    
    q_mejora  <- sum(accts$FLAG_MEJORA_RETORNO_ANUAL == "MEJORA", na.rm = TRUE)
    q_empeora <- sum(accts$FLAG_MEJORA_RETORNO_ANUAL == "EMPEORA", na.rm = TRUE)
    
    sub <- df %>%
      semi_join(accts %>% transmute(ID_CLIENTE, ID_CUENTA),
                by = c("ID_CLIENTE", "ID_CUENTA"))
    
    q_compra <- sum(sub$FLAG_ACCION_Q == "COMPRAR", na.rm = TRUE)
    q_venta <- sum(sub$FLAG_ACCION_Q == "VENDER", na.rm = TRUE)
    q_mantener <- sum(sub$FLAG_ACCION_Q == "MANTENER", na.rm = TRUE)
    prefs_n <- NA_integer_  # no aplica en sin preferencias
    
    tagList(
      h3("INDICADORES CLAVE RESUMEN"),
      fluidRow(
        column(4, tags$div(tags$b("Q Cuentas: "), n_accts)),
        column(4, tags$div(tags$b("Monto en custodia total (PESOS): "), comma(aum, accuracy = 1)))
      ),
      fluidRow(
        column(4, tags$div(tags$b("Ret Esp Anual (normal): "), percent(ret_act, accuracy = 0.1))),
        column(4, tags$div(tags$b("Ret Esp Anual (óptimo): "), percent(ret_opt, accuracy = 0.1)))
      ),
      fluidRow(
        column(4, tags$div(tags$b("Desv.Est. Anual (normal): "), percent(de_act, accuracy = 0.1))),
        column(4, tags$div(tags$b("Desv.Est. Anual (óptimo): "), percent(de_opt, accuracy = 0.1)))
      ),
      fluidRow(
        column(4, tags$div(tags$b("Avg Sharpe (normal): "), sprintf("%.3f", sharpe_act))),
        column(4, tags$div(tags$b("Avg Sharpe (óptimo): "), sprintf("%.3f", sharpe_opt)))
      ),
      fluidRow(
        column(4, tags$div(tags$b("Q COMPRAR: "), q_compra)),
        column(4, tags$div(tags$b("Q VENDER: "), q_venta)),
        column(4, tags$div(tags$b("Q MANTENER: "), q_mantener))
      ),
      fluidRow(
        column(4, tags$div(tags$b("Q MEJORA / EMPEORA RENTABILIDAD: "), q_mejora, " / ", q_empeora)),
        column(4, tags$div(tags$b("Avg GAP Sharpe: "), sprintf("%.3f", gap_sharpe))),
        column(4, tags$div(""))
      )
    )
  })
  
  output$risk_return <- renderPlot({
    accts <- filtered_accts()
    validate(need(nrow(accts) > 0, ""))
    
    label_limit <- 30
    show_labels <- (nrow(accts) <= label_limit) ||
      (input$cliente != "All") ||
      (input$cuenta  != "All")
    
    plot_df <- dplyr::bind_rows(
      accts %>% transmute(ID_CUENTA, Risk = DE_ANUAL,              Return = RET_PROM_ANUAL,          Tipo = "Actual"),
      accts %>% transmute(ID_CUENTA, Risk = DESVEST_ANUAL_OPTM,    Return = RET_ESPERADO_ANUAL_OPTM, Tipo = "Óptima")
    ) %>%
      filter(is.finite(Risk), is.finite(Return))
    
    p <- ggplot(plot_df, aes(x = Risk, y = Return, color = Tipo)) +
      geom_point(alpha = 0.95, size = 3.6) +
      scale_color_manual(values = c("Actual" = "#D81B60", "Óptima" = "#0072B2")) +
      labs(
        title = "Risk vs Return (Annual): Actual vs Óptima",
        x = "Annual Risk",
        y = "Annual Return",
        color = ""
      ) +
      theme_minimal() +
      theme(
        legend.text = element_text(size = 12),
        plot.title  = element_text(size = 14, face = "bold")
      )
    
    if (show_labels) {
      p <- p + ggrepel::geom_text_repel(
        aes(label = ID_CUENTA),
        size = 4.2,
        fontface = "bold",
        max.overlaps = Inf,
        box.padding = 0.35,
        point.padding = 0.25,
        show.legend = FALSE
      )
    }
    
    p
  })
  
  resumen_cartera <- reactive({
    df %>%
      semi_join(filtered_accts(), by = c("ID_CLIENTE", "ID_CUENTA")) %>%
      group_by(ID_CLIENTE, ID_CUENTA) %>%
      summarise(
        RET_PROM_ANUAL           = first(RET_PROM_ANUAL),
        RET_PROM_ANUAL_OPTM      = first(RET_ESPERADO_ANUAL_OPTM),
        GAP_RET_ANUAL            = first(GAP_RET_ANUAL),
        
        DESVEST_ANUAL            = first(DE_ANUAL),
        DESVEST_ANUAL_OPTM       = first(DESVEST_ANUAL_OPTM),
        GAP_DESV_ANUAL           = first(GAP_DESV_ANUAL),
        
        SHARPE_ANUAL             = first(SHARPE_ANUAL),
        SHARPE_ANUAL_OPTM        = first(SHARPE_ANUAL_OPTM),
        GAP_SHARPE_ANUAL         = first(GAP_SHARPE_ANUAL),
        
        FLAG_MEJORA_RETORNO_ANUAL = first(FLAG_MEJORA_RETORNO_ANUAL),
        FLAG_MEJORA_RIESGO_ANUAL  = first(FLAG_MEJORA_RIESGO_ANUAL),
        
        Q_ACCIONES  = n(),
        Q_COMPRAR   = sum(FLAG_ACCION_Q == "COMPRAR", na.rm = TRUE),
        Q_VENDER    = sum(FLAG_ACCION_Q == "VENDER", na.rm = TRUE),
        Q_MANTENER  = sum(FLAG_ACCION_Q == "MANTENER", na.rm = TRUE),
        
        MONTO_CUSTODIA = sum(MONTO_EN_CUSTODIA, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$tabla_resumen <- renderDT({
    datatable(
      resumen_cartera(),
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 10),
      width = "100%"
    ) %>%
      formatPercentage(
        c("RET_PROM_ANUAL", "RET_PROM_ANUAL_OPTM", "GAP_RET_ANUAL",
          "DESVEST_ANUAL", "DESVEST_ANUAL_OPTM", "GAP_DESV_ANUAL"),
        digits = 1
      ) %>%
      formatCurrency(
        "MONTO_CUSTODIA",
        currency = "",
        interval = 3,
        mark = ".",
        dec.mark = ",",
        digits = 0
      )
  })
  
  actions_data <- reactive({
    accts <- filtered_accts()
    allowed <- accts %>% transmute(ID_CLIENTE, ID_CUENTA)
    sub <- df %>% inner_join(allowed, by = c("ID_CLIENTE", "ID_CUENTA"))
    
    base_cols <- c(
      "NEMO","NAME","FLAG_ACCION_Q",
      "CANTIDAD_EN_CUSTODIA","CANTIDAD_EN_CUSTODIA_OPT",
      "GAP_Q_CUSTODIA","PESO","PESO_OPT",
      "MONTO_EN_CUSTODIA","MONTO_EN_CUSTODIA_OPT",
      "APORTE_ADICIONAL","RET_PROM_ANUAL","RET_ESPERADO_ANUAL_OPTM"
    )
    
    if (input$cliente != "All" && input$cuenta != "All") {
      sub %>%
        filter(ID_CLIENTE == input$cliente, ID_CUENTA == input$cuenta) %>%
        select(any_of(base_cols)) %>%
        arrange(FLAG_ACCION_Q, NEMO)
    } else {
      sub %>%
        filter(FLAG_ACCION_Q %in% c("COMPRAR", "VENDER","MANTENER")) %>%
        select(any_of(c("ID_CLIENTE","ID_CUENTA", base_cols))) %>%
        arrange(FLAG_ACCION_Q, ID_CLIENTE, ID_CUENTA)
    }
  })
  
  output$actions <- renderDT({
    actions <- actions_data()
    
    datatable(
      actions,
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 15),
      width = "100%"
    ) %>%
      formatCurrency(
        columns = intersect(c("MONTO_EN_CUSTODIA","MONTO_EN_CUSTODIA_OPT","APORTE_ADICIONAL"), names(actions)),
        currency = "",
        interval = 3,
        mark = ".",
        dec.mark = ",",
        digits = 0
      ) %>%
      formatPercentage(
        columns = intersect(c("PESO","PESO_OPT","RET_PROM_ANUAL","RET_ESPERADO_ANUAL_OPTM"), names(actions)),
        digits = 1
      )
  })
  
  output$dl_resumen <- downloadHandler(
    filename = function() paste0("resumen_carteras_sin_pref_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Resumen")
      writeDataTable(wb, "Resumen", resumen_cartera())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$dl_actions <- downloadHandler(
    filename = function() paste0("acciones_sin_pref_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Acciones")
      writeDataTable(wb, "Acciones", actions_data())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)

