library(readxl)
library(readr)
library(dplyr)
library(openxlsx)
library(keras3)
library(ggplot2)

##### EJECUCIÓN MENSUAL - RED NEURONAL RIESGOSO/TENSO #####


# CARGAR OBJETOS GUARDADOS

escalador_rt <- readRDS("FILES/MODELOS/escalador_red_neuronal_riesgoso_tenso.rds")
model_rt <- load_model("FILES/MODELOS/modelo_red_neuronal_riesgoso_tenso.keras")


# CARGAR BASE DEL MES

# Ajusta la ruta si el archivo mensual cambia
df_mes <- read_excel("FILES/OUTPUT/07_TABLA_RESUMEN_FINAL_KMEANS_CON_SEGMENTO_MENSUAL.xlsx")



# ASEGURAR COLUMNAS DEL MODELO

columnas_x <- escalador_rt$columnas_x

faltantes <- setdiff(columnas_x, names(df_mes))
if (length(faltantes) > 0) {
  stop(paste("Faltan columnas en la base mensual:", paste(faltantes, collapse = ", ")))
}


# PREPARAR Y ESCALAR VARIABLES

X_mes <- df_mes %>%
  select(all_of(columnas_x)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.data.frame()

X_mes_escalado <- scale(
  X_mes,
  center = escalador_rt$medianas,
  scale = escalador_rt$iqr_vals
)

X_mes_escalado <- as.matrix(X_mes_escalado)


# PREDECIR

y_prob_mes <- model_rt %>% predict(X_mes_escalado)
y_pred_mes <- ifelse(y_prob_mes > 0.5, 1, 0)

df_mes$PROB_CLUSTER13 <- as.numeric(y_prob_mes)
df_mes$PRED_CLUSTER13 <- as.numeric(y_pred_mes)


# GENERAR ALERTA

df_mes <- df_mes %>%
  mutate(
    ALERTA_CLUSTER_CRITICO = ifelse(
      PRED_CLUSTER13 == 1 & (is.na(CLUSTER) | !(CLUSTER %in% c("2", "4"))),
      "Potencial Entrada Critico/Tenso",
      "NO"
    )
  )

df_alerta <- df_mes %>%
  filter(PRED_CLUSTER13 == 1)



# CREAR TARGET REAL DESDE CLUSTER

df_mes <- df_mes %>%
  mutate(
    TARGET13 = ifelse(as.character(CLUSTER) %in% c("2", "4"), 1, 0)
  )




# EXPORTAR RESULTADOS
wb <- createWorkbook()

addWorksheet(wb, "base_con_score")
writeData(wb, "base_con_score", df_mes)

addWorksheet(wb, "clientes_alerta")
writeData(wb, "clientes_alerta", df_alerta)



saveWorkbook(
  wb,
  "FILES/OUTPUT/08_RED_NEURONAL_RIESGOSO_TENSO_MENSUAL.xlsx",
  overwrite = TRUE
)


# MATRIZ DE CONFUSIÓN

matriz_mensual <- table(
  Real = factor(df_mes$TARGET13, levels = c(0, 1)),
  Predicho = factor(df_mes$PRED_CLUSTER13, levels = c(0, 1))
)

print(matriz_mensual)


# PASAR A DATA FRAME PARA GRAFICAR

df_matriz_mensual <- as.data.frame(matriz_mensual)
colnames(df_matriz_mensual) <- c("Real", "Predicho", "Frecuencia")


# GRÁFICO

grafico_mc_mensual <- ggplot(df_matriz_mensual, aes(x = Predicho, y = Real, fill = Frecuencia)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frecuencia), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Red neuronal - Clusters Riesgosos/Tensos - Mensual",
    x = "Predicción",
    y = "Valor real"
  ) +
  scale_x_discrete(labels = c("0" = "No cluster Crítico", "1" = "Cluster Crítico")) +
  scale_y_discrete(labels = c("0" = "No cluster Crítico", "1" = "Cluster Crítico")) +
  theme_minimal()

print(grafico_mc_mensual)


# EXPORTAR PNG

ggsave(filename = "FILES/OUTPUT/O8_MATRIZ_RED_NEURONAL_TENSO_RIESGOSO_MES.png",
  plot = grafico_mc_mensual,
  width = 8,
  height = 6,
  dpi = 300)



