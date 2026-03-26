
# PARTE 8 SISTEMA DE ALERTA TEMPRANA - RED NEURONAL PATRIMONIAL PREFERENTE:


# ESTA PARTE SE REALIZA EL ENTRENAMIENTO DE LA RED NEURONAL PARA DETECTAR
# AQUELLOS CLIENTES CON CARACTERISTICAS PARA SALIR O ENTRAR DEL CLUSTER PATRIMONIAL PREFERENTE
# TAMBIEN SE ENTREGAN MATRICES DE CONFUSION Y OUTPUTS

######NO EJECUTAR MENUALMENTE, ESTE CORRESPONDE AL SCRIPT DE ENTRENAMIENTO#############


#TIEMPO ESTIMADO DE EJECUCION: 2 a 3 MINUTOS.

# CUALQUIER DUDA CONSULTAR AL AUTOR: 
# RODRIGO ESCOBAR LANDAETA | RESCOBARL@FEN.UCHILE.CL | LANDAETA77@GMAIL.COM


library(readr)
library(dplyr)
library(caret)
library(robustbase)
library(openxlsx)
library(keras3)
library(ggplot2)

set.seed(42)

# LEER ARCHIVO CSV

df<-read_excel("FILES/MODELOS/TABLA_RESUMEN_FINAL_KMEANS_CON_SEGMENTO.xlsx")



# CREAR LA VARIABLE OBJETIVO (TARGET)

# En este caso:
# 1 = cliente del cluster 1, 6 o DAP_PREFERENTE
# 0 = cualquier otro cluster

df <- df %>%
  mutate(CLUSTER = as.character(CLUSTER),
    TARGET = ifelse(CLUSTER %in% c("1", "6", "DAP_PREFERENTE"), 1, 0))


# DEFINIR VARIABLES DE ENTRADA (X)

columnas_x <- c("Q_CUENTAS",
  "MONTO_TOTAL_ACCIONES",
  "MONTO_TOTAL_DAP",
  "MONTO_TOTAL_FM",
  "RET_ANUAL_PONDERADO",
  "DE_ANUAL_PONDERADO",
  "SHARPE_ANUAL_PONDERADO",
  "INTERACCIONES_NEGATIVAS_6M",
  "INTERACCIONES_NEUTRAS_6M",
  "INTERACCIONES_POSITIVAS_6M",
  "Q_EMPRESAS_INVIERTE",
  "CANTIDAD_EN_CUSTODIA_TOTAL")

# X = variables explicativas
# y = variable objetivo
X <- df %>% select(all_of(columnas_x))
y <- df$TARGET


# SEPARAR EN ENTRENAMIENTO Y PRUEBA



idx_train <- createDataPartition(y, p = 0.8, list = FALSE)

X_train <- X[idx_train, , drop = FALSE]
X_test  <- X[-idx_train, , drop = FALSE]

y_train <- y[idx_train]
y_test  <- y[-idx_train]


# REVISAR DISTRIBUCIÓN DE TARGET

cat("Distribución real de TARGET:\n")
print(table(df$TARGET))

cat("\nDistribución real en y_train:\n")
print(table(y_train))

cat("\nDistribución real en y_test:\n")
print(table(y_test))


# ESCALAR VARIABLES

# Robust scaling = (x - mediana) / IQR

medianas <- sapply(X_train, median, na.rm = TRUE)
iqr_vals <- sapply(X_train, IQR, na.rm = TRUE)

# Evitar divisiones por 0
iqr_vals[iqr_vals == 0] <- 1

X_train_escalado <- as.data.frame(scale(X_train, center = medianas, scale = iqr_vals))
X_test_escalado  <- as.data.frame(scale(X_test, center = medianas, scale = iqr_vals))


# CREAR PESOS POR CLASE

n_0 <- sum(y_train == 0)
n_1 <- sum(y_train == 1)

class_weight <- c("0" = 1,
  "1" = n_0 / n_1)

print(class_weight)


# FORMATO PARA KERAS

X_train_escalado <- as.matrix(X_train_escalado)
X_test_escalado  <- as.matrix(X_test_escalado)

y_train <- as.numeric(y_train)
y_test  <- as.numeric(y_test)

# CREAR LA RED NEURONAL
model <- keras_model_sequential() %>%
  layer_dense(units = 16,
    activation = "relu",
    input_shape = c(ncol(X_train_escalado))) %>%
  layer_dense(units = 8,
    activation = "relu") %>%
  layer_dense(units = 1,
    activation = "sigmoid")


# CONFIGURAR EL MODELO

model %>% compile(optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = list("accuracy",
    metric_precision(name = "precision"),
    metric_recall(name = "recall")))


# EARLY STOPPING

early_stop <- callback_early_stopping(monitor = "val_loss",
  patience = 15,
  restore_best_weights = TRUE)


# ENTRENAR EL MODELO

history <- model %>% fit(x = X_train_escalado,
  y = y_train,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2,
  class_weight = class_weight,
  callbacks = list(early_stop),
  verbose = 1)


# EVALUAR EL MODELO EN TEST

resultado_eval <- model %>% evaluate(x = X_test_escalado,
  y = y_test,
  verbose = 0)

loss <- as.numeric(resultado_eval["loss"])
accuracy <- as.numeric(resultado_eval["accuracy"])
precision_keras <- as.numeric(resultado_eval["precision"])
recall_keras <- as.numeric(resultado_eval["recall"])


# GENERAR PREDICCIONES

y_prob <- model %>% predict(X_test_escalado)

# Convertimos probabilidad a clase final
y_pred <- ifelse(y_prob > 0.5, 1, 0)
y_pred <- as.numeric(y_pred)
y_test_num <- as.numeric(y_test)


# MÉTRICAS CLÁSICAS DE CLASIFICACIÓN

matriz <- table(Real = factor(y_test_num, levels = c(0, 1)),
  Predicho = factor(y_pred, levels = c(0, 1)))

tp <- matriz["1", "1"]
fp <- matriz["0", "1"]
fn <- matriz["1", "0"]
tn <- matriz["0", "0"]

precision <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
recall    <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
f1        <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))

cat("\nMétricas de clasificación:\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1:", f1, "\n")


conf_obj <- confusionMatrix(data = factor(y_pred, levels = c(0, 1)),
  reference = factor(y_test_num, levels = c(0, 1)),
  positive = "1")

cat("\nResumen tipo classification report:\n")
print(conf_obj)


# GRÁFICO MATRIZ DE CONFUSIÓN

df_matriz <- as.data.frame(matriz)
colnames(df_matriz) <- c("Real", "Predicho", "Frecuencia")

grafico_mc <- ggplot(df_matriz, aes(x = Predicho, y = Real, fill = Frecuencia)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frecuencia), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Matriz de confusión - Red neuronal - Cluster Patrimonial",
    x = "Predicción",
    y = "Valor real") +
  scale_x_discrete(labels = c("0" = "No cluster Pref", "1" = "Cluster Pref")) +
  scale_y_discrete(labels = c("0" = "No cluster Pref", "1" = "Cluster Pref")) +
  theme_minimal()

print(grafico_mc)

ggsave(filename = "FILES/MODELOS/matriz_confusion_red_neuronal_patrimonial.png",
  plot = grafico_mc,
  width = 8,
  height = 6,
  dpi = 300)


# PEGAR SCORE A TODA LA BASE


X_total <- df %>% select(all_of(columnas_x))
X_total <- as.data.frame(scale(X_total, center = medianas, scale = iqr_vals))
X_total <- as.matrix(X_total)

y_prob_total <- model %>% predict(X_total)
y_pred_total <- ifelse(y_prob_total > 0.5, 1, 0)

df$PROB_CLUSTER26PREF <- as.numeric(y_prob_total)
df$PRED_CLUSTER26PREF <- as.numeric(y_pred_total)


# SEPARAR TABLA DEL CLUSTER PREDICHO

df_patrimonial <- df %>%
  filter(PRED_CLUSTER26PREF == 1)


# EXPORTAR A EXCEL

wb <- createWorkbook()

addWorksheet(wb, "base_con_score")
writeData(wb, "base_con_score", df)

addWorksheet(wb, "clientes_alerta")
writeData(wb, "clientes_alerta", df_patrimonial)

saveWorkbook(wb, "FILES/MODELOS/RED_NEURONAL_PATRIMONIAL_PREFERENTE.xlsx", overwrite = TRUE)


# GUARDAR ESCALADOR Y MODELO

saveRDS(list(medianas = medianas,
    iqr_vals = iqr_vals,
    columnas_x = columnas_x),
  "FILES/MODELOS/escalador_red_neuronal_patrimonial.rds")


ruta_modelo <- "FILES/MODELOS/modelo_red_neuronal_patrimonial.keras"

if (file.exists(ruta_modelo)) {
  file.remove(ruta_modelo)
}

save_model(model, ruta_modelo)

# GENERAR Y EXPORTAR GRÁFICO BASE COMPLETA



matriz_total <- table(Real = factor(df$TARGET, levels = c(0, 1)),
  Predicho = factor(df$PRED_CLUSTER26PREF, levels = c(0, 1)))

print(matriz_total)

df_matriz_total <- as.data.frame(matriz_total)
colnames(df_matriz_total) <- c("Real", "Predicho", "Frecuencia")

grafico_total <- ggplot(df_matriz_total, aes(x = Predicho, y = Real, fill = Frecuencia)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frecuencia), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Red neuronal - Clusters Patrimonial - Base completa",
    x = "Predicción",
    y = "Valor real") +
  scale_x_discrete(labels = c("0" = "No cluster Patrimonial", "1" = "Cluster Patrimonial")) +
  scale_y_discrete(labels = c("0" = "No cluster Patrimonial", "1" = "Cluster Patrimonial")) +
  theme_minimal()

print(grafico_total)

ggsave(filename = "FILES/MODELOS/matriz_confusion_patrimonial_base_completa.png",
  plot = grafico_total,
  width = 8,
  height = 6,
  dpi = 300)







