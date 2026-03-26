# =========================================
# SCRIPT MAESTRO
# Ejecuta varios scripts, deja log y se detiene si hay error
# =========================================

# Carpeta de logs
dir.create("LOGS", showWarnings = FALSE)

# Nombre del log con timestamp
log_file <- file.path(
  "LOGS",
  paste0("log_ejecucion_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
)

# Función para escribir log
log_msg <- function(texto, nivel = "INFO") {
  linea <- paste0(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
    "[", nivel, "] ",
    texto,
    "\n"
  )
  cat(linea)                                  # Consola
  cat(linea, file = log_file, append = TRUE)  # Archivo
}

# Lista de scripts a ejecutar en orden
scripts <- c(
  "07.Segmentación de Clientes - Entrenamiento.R",
  "08.Alerta Temprana Red Neuronal Patrimonial - Entrenamiento.R",
  "08.Alerta Temprana Red Neuronal Tensos y Criticos - Entrenamiento.R",
  "08.Concatenador Alerta Temprana por Red Neuronal - Entrenamiento.R",
  "09.Concatenador de Modelo-Bases Optimizadas.R"
)

log_msg("==== INICIO DE EJECUCION ====")

for (script in scripts) {
  
  log_msg(paste("Iniciando script:", script))
  
  tryCatch(
    {
      source(script, local = FALSE, echo = TRUE)
      log_msg(paste("Script finalizado OK:", script), "OK")
    },
    error = function(e) {
      log_msg(paste("ERROR en script:", script), "ERROR")
      log_msg(paste("Detalle:", conditionMessage(e)), "ERROR")
      log_msg("==== EJECUCION DETENIDA POR ERROR ====", "ERROR")
      stop(e)
    },
    warning = function(w) {
      log_msg(paste("WARNING en script:", script), "WARNING")
      log_msg(paste("Detalle:", conditionMessage(w)), "WARNING")
      invokeRestart("muffleWarning")
    }
  )
}

log_msg("==== FIN DE EJECUCION EXITOSA ====", "OK")