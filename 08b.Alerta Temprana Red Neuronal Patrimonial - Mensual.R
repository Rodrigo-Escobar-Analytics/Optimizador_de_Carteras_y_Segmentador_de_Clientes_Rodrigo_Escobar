library(readxl)
library(readr)
library(dplyr)
library(openxlsx)
library(keras3)
library(ggplot2)

##### EJECUCIÓN MENSUAL - RED NEURONAL PATRIMONIAL / PREFERENTE #####


# CARGAR OBJETOS GUARDADOS

escalador_pref <- readRDS("FILES/MODELOS/escalador_red_neuronal_patrimonial.rds")
model_pref <- load_model("FILES/MODELOS/modelo_red_neuronal_patrimonial.keras")


# CARGAR BASE DEL MES

df_mes <- read_excel("FILES/OUTPUT/07_TABLA_RESUMEN_FINAL_KMEANS_CON_SEGMENTO_MENSUAL.xlsx")


# ASEGURAR COLUMNAS DEL MODELO

columnas_x <- escalador_pref$columnas_x

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
  center = escalador_pref$medianas,
  scale = escalador_pref$iqr_vals
)

X_mes_escalado <- as.matrix(X_mes_escalado)


# PREDECIR

y_prob_mes <- model_pref %>% predict(X_mes_escalado)
y_pred_mes <- ifelse(y_prob_mes > 0.5, 1, 0)

df_mes$PROB_CLUSTER26PREF <- as.numeric(y_prob_mes)
df_mes$PRED_CLUSTER26PREF <- as.numeric(y_pred_mes)


# GENERAR ALERTA

df_mes <- df_mes %>%
  mutate(
    ALERTA_CLUSTER_PATRIMONIAL = ifelse(
      PRED_CLUSTER26PREF == 1 & (is.na(CLUSTER) | !(as.character(CLUSTER) %in% c("1", "6", "DAP_PREFERENTE"))),
      "Potencial Entrada Patrimonial/Preferente",
      "NO"
    )
  )

df_alerta <- df_mes %>%
  filter(PRED_CLUSTER26PREF == 1)


# CREAR TARGET REAL DESDE CLUSTER

df_mes <- df_mes %>%
  mutate(
    TARGET26PREF = ifelse(as.character(CLUSTER) %in% c("1", "6", "DAP_PREFERENTE"), 1, 0)
  )


# EXPORTAR RESULTADOS

wb <- createWorkbook()

addWorksheet(wb, "base_con_score")
writeData(wb, "base_con_score", df_mes)

addWorksheet(wb, "clientes_alerta")
writeData(wb, "clientes_alerta", df_alerta)

saveWorkbook(
  wb,
  "FILES/OUTPUT/08_RED_NEURONAL_PATRIMONIAL_MENSUAL.xlsx",
  overwrite = TRUE
)


# MATRIZ DE CONFUSIÓN

matriz_mensual <- table(
  Real = factor(df_mes$TARGET26PREF, levels = c(0, 1)),
  Predicho = factor(df_mes$PRED_CLUSTER26PREF, levels = c(0, 1))
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
    title = "Red neuronal - Cluster Patrimonial/Preferente - Mensual",
    x = "Predicción",
    y = "Valor real"
  ) +
  scale_x_discrete(labels = c("0" = "No cluster Patrimonial", "1" = "Cluster Patrimonial")) +
  scale_y_discrete(labels = c("0" = "No cluster Patrimonial", "1" = "Cluster Patrimonial")) +
  theme_minimal()

print(grafico_mc_mensual)


# EXPORTAR PNG

ggsave(
  filename = "FILES/OUTPUT/08_MATRIZ_RED_NEURONAL_PATRIMONIAL_MES.png",
  plot = grafico_mc_mensual,
  width = 8,
  height = 6,
  dpi = 300
)