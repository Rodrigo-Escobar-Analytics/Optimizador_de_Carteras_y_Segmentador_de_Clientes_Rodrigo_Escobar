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
  "01.Extraccion Precios Acciones.R",
  "02.Cruce Tabla Principal y Account Manager.R",
  "03. Procesamiento Preferencias Encuesta y WEB con NPS.R",
  "04.Cruce Tabla Principal y Ultimo Precio Cierre.R",
  "05.Calculo Indicadores y Optimizacion de Cartera.R",
  "06.Calculo Indicadores y Optimizacion de Cartera Preferencias.R"
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
    }
  )
}



log_msg("==== FIN DE EJECUCION EXITOSA ====", "OK")