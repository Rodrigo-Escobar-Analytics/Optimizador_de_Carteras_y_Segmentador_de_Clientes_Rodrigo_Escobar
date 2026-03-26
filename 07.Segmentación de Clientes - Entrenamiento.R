
# PARTE 7 SEGMENTACION DE CARTERAS:

# ESTA PARTE SE REALIZA EL ENTRENAMIENTO DE LA SEGMENTACION DE CARTERAS EN 3 FASES:

# ETAPA 1: 2 CLUSTERS SIN DAP
# ETAPA 2: 5 CLUSTERS QUE DESPRENDEN DEL CLUSTER MAS GRANDE DE ETAPA 1
# ETAPA DAP: 2 CLUSTERS PARA DAP

# SE EXPORTAN LOS PROMEDIOS, MEDIANAS, CLUSTERS Y SILUETAS DEL ENTRENAMIENTO DEL MODELO

#######NO EJECUTAR MENSUALMENTE, ESTE CORRESPONDE AL SCRIPT DE ENTRENAMIENTO###########

# TIEMPO ESTIMADO DE EJECUCION: 3 a 5 MINUTOS.

# CUALQUIER DUDA CONSULTAR AL AUTOR: 
# RODRIGO ESCOBAR LANDAETA | RESCOBARL@FEN.UCHILE.CL | LANDAETA77@GMAIL.COM


library(readxl)
library(purrr)
library(tidyquant)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(cluster)
library(ggplot2)
library(openxlsx)
library(gt)
library(scales)

set.seed(343)

NPS<-read_excel("FILES/INPUTS/RESULTADOS_ENCUESTA_ACUMULADO_MODELO_NPS.xlsx")
INTERACCIONES<-read_csv2("FILES/INPUTS/TBL_INTERACCIONES_CONSULTA_6M.csv")
DAP_FM<-read_csv2("FILES/INTERMEDIO/02_BASE_CUSTODIA_DAP_FM_PRINCIPAL.CSV")
CARTERA<-read.csv2("FILES/OUTPUT/05_PESOS_INDICADORES_CARTERA.csv")

# mantengo la ultima nota disponible para el NPS

NPS <- NPS %>%
  arrange(ID_CLIENTE, desc(FECHA_ENCUESTA)) %>%
  distinct(ID_CLIENTE, .keep_all = TRUE)

# selecciono las columnas de la tabla NPS

NPS<-NPS%>%select(ID_CLIENTE,NPS)

# separo el tipo de interaccion segun clasificacion

RESUMEN_INTERACCIONES <- INTERACCIONES %>%
  group_by(ID_CLIENTE, SENTIMIENTO) %>%
  summarise(CANTIDAD = n(), .groups = "drop") %>%
  pivot_wider(names_from = SENTIMIENTO,
    values_from = CANTIDAD,
    values_fill = 0) %>%
  rename(INTERACCIONES_POSITIVAS_6M = POSITIVA,
    INTERACCIONES_NEGATIVAS_6M = NEGATIVA,
    INTERACCIONES_NEUTRAS_6M   = NEUTRA)

# Anualizo retorno mensual

DAP_FM <- DAP_FM %>%
  mutate(RETORNO_MES = RETORNO_MES / 100,
    RETORNO_ANUAL = (1 + RETORNO_MES)^12 - 1)

# Asumo una D.E. baja anual para DAP y FM distinta de 0

DAP_FM <- DAP_FM %>%
  mutate(DE_ANUAL = case_when(
      str_starts(ID_CUENTA, "CTD") ~ 0.01,  # DAP
      str_starts(ID_CUENTA, "CTF") ~ 0.02,))   # FM


# Calculo totales, retorno y D.E. ponderados

DAP_FM_RESUMEN <- DAP_FM %>%
  group_by(ID_CLIENTE, ID_CUENTA) %>%
  summarise(MONTO_TOTAL_INSTRUMENTO = sum(MONTO_TOTAL_INSTRUMENTO, na.rm = TRUE),
    
    RETORNO_ANUAL = sum(RETORNO_ANUAL * MONTO_TOTAL_INSTRUMENTO, na.rm = TRUE) / 
      sum(MONTO_TOTAL_INSTRUMENTO, na.rm = TRUE),
    
    DE_ANUAL_PONDERADA = sum(DE_ANUAL * MONTO_TOTAL_INSTRUMENTO, na.rm = TRUE) / 
      sum(MONTO_TOTAL_INSTRUMENTO, na.rm = TRUE),
    
    .groups = "drop")


# selecciono campos de tabla cartera

CARTERA_RESUMEN<-CARTERA%>%select(ID_CLIENTE,ID_CUENTA,MONTO_EN_CUSTODIA_TOTAL,
                                  RET_PROM_ANUAL,DE_ANUAL)

# Dejo tabla con valores unicos

CARTERA_RESUMEN<-unique(CARTERA_RESUMEN)

# Homologo nombres tablas

names(DAP_FM_RESUMEN)<-names(CARTERA_RESUMEN)

# Junto tablas

CARTERA_TOTAL<-bind_rows(CARTERA_RESUMEN,DAP_FM_RESUMEN)

# Calculo varios indicadores: monto acciones, monto dap, monto fm, retornos y da ponderados, ratio sharpe

CARTERA_TOTAL_MONTOS <- CARTERA_TOTAL %>%
  group_by(ID_CLIENTE) %>%
  summarise(
    Q_CUENTAS = n_distinct(ID_CUENTA),
    
    MONTO_TOTAL_ACCIONES = sum(if_else(str_starts(ID_CUENTA, "CTA"), MONTO_EN_CUSTODIA_TOTAL, 0),
      na.rm = TRUE),
    
    MONTO_TOTAL_DAP = sum(if_else(str_starts(ID_CUENTA, "CTD"), MONTO_EN_CUSTODIA_TOTAL, 0),
      na.rm = TRUE),
    
    MONTO_TOTAL_FM = sum(if_else(str_starts(ID_CUENTA, "CTF"), MONTO_EN_CUSTODIA_TOTAL, 0),
      na.rm = TRUE),
    
    MONTO_TOTAL_CLIENTE = sum(MONTO_EN_CUSTODIA_TOTAL, na.rm = TRUE),
    
    RET_ANUAL_PONDERADO = ifelse(sum(MONTO_EN_CUSTODIA_TOTAL, na.rm = TRUE) > 0,
      sum(RET_PROM_ANUAL * MONTO_EN_CUSTODIA_TOTAL, na.rm = TRUE) /
        sum(MONTO_EN_CUSTODIA_TOTAL, na.rm = TRUE),
      NA_real_),
    
    DE_ANUAL_PONDERADO = ifelse(sum(MONTO_EN_CUSTODIA_TOTAL, na.rm = TRUE) > 0,
      sum(DE_ANUAL * MONTO_EN_CUSTODIA_TOTAL, na.rm = TRUE) /
        sum(MONTO_EN_CUSTODIA_TOTAL, na.rm = TRUE),
      NA_real_),
    
    .groups = "drop") %>%
  mutate(SHARPE_ANUAL_PONDERADO = ifelse(!is.na(DE_ANUAL_PONDERADO) & DE_ANUAL_PONDERADO > 0,
      (RET_ANUAL_PONDERADO - 0.03) / DE_ANUAL_PONDERADO,
      NA_real_)) %>%
  select(ID_CLIENTE,
    Q_CUENTAS,
    MONTO_TOTAL_ACCIONES,
    MONTO_TOTAL_DAP,
    MONTO_TOTAL_FM,
    RET_ANUAL_PONDERADO,
    DE_ANUAL_PONDERADO,
    SHARPE_ANUAL_PONDERADO)


# Cruzo el tipo de interaccion y reemplazo si es vacio como 0

CARTERA_TOTAL_MONTOS<-CARTERA_TOTAL_MONTOS%>%left_join(NPS,by="ID_CLIENTE")
CARTERA_TOTAL_MONTOS <- CARTERA_TOTAL_MONTOS %>%
  left_join(RESUMEN_INTERACCIONES, by = "ID_CLIENTE") %>%
  mutate(across(c(INTERACCIONES_NEGATIVAS_6M,
        INTERACCIONES_NEUTRAS_6M,
        INTERACCIONES_POSITIVAS_6M),
      ~coalesce(., 0)))

# Selecciono algunos campos de tabla cartera

CARTERA_EMPRESAS<-CARTERA%>%select(ID_CLIENTE,NEMO,FLAG_ACCION_Q,CANTIDAD_EN_CUSTODIA)

# Cruce con q empresas que invierte y cantidad acciones en custodia

CARTERA_EMPRESAS_RESUMEN <- CARTERA_EMPRESAS %>%
  group_by(ID_CLIENTE) %>%
  summarise(Q_EMPRESAS_INVIERTE = n_distinct(NEMO),
    CANTIDAD_EN_CUSTODIA_TOTAL = sum(CANTIDAD_EN_CUSTODIA, na.rm = TRUE),
    .groups = "drop")


# Cruce montos totales y reemplazo vacios por cero

CARTERA_TOTAL_MONTOS <- CARTERA_TOTAL_MONTOS %>%
  left_join(CARTERA_EMPRESAS_RESUMEN, by = "ID_CLIENTE") %>%
  mutate(Q_EMPRESAS_INVIERTE = replace_na(Q_EMPRESAS_INVIERTE, 0),
    CANTIDAD_EN_CUSTODIA_TOTAL = replace_na(CANTIDAD_EN_CUSTODIA_TOTAL, 0))


# Exporto tabla reumen

write.table(CARTERA_TOTAL_MONTOS, file = "FILES/OUTPUT/07_TABLA_CLUSTERING_E1.CSV", sep = ";",
            na = "", dec = ",", row.names = FALSE,
            col.names = TRUE)



# Selección de variables
columnas <- c("ID_CLIENTE",
  "Q_CUENTAS",
  "MONTO_TOTAL_ACCIONES",
  "MONTO_TOTAL_DAP",
  "MONTO_TOTAL_FM",
  "RET_ANUAL_PONDERADO",
  "DE_ANUAL_PONDERADO",
  "SHARPE_ANUAL_PONDERADO",
  "NPS",
  "INTERACCIONES_NEGATIVAS_6M",
  "INTERACCIONES_NEUTRAS_6M",
  "INTERACCIONES_POSITIVAS_6M",
  "Q_EMPRESAS_INVIERTE",
  "CANTIDAD_EN_CUSTODIA_TOTAL")

CARTERA_TOTAL_MONTOS <- CARTERA_TOTAL_MONTOS %>%
  select(all_of(columnas))

# Eliminar duplicados
CARTERA_TOTAL_MONTOS <- CARTERA_TOTAL_MONTOS %>%
  distinct()

# Separar clientes DAP
df_dap <- CARTERA_TOTAL_MONTOS %>%
  filter(CANTIDAD_EN_CUSTODIA_TOTAL == 0)

# Base para clustering
CARTERA_TOTAL_MONTOS <- CARTERA_TOTAL_MONTOS %>%
  filter(CANTIDAD_EN_CUSTODIA_TOTAL != 0)

# Variables para el modelo
columnas_modelo <- c("Q_CUENTAS",
  "MONTO_TOTAL_ACCIONES",
  "MONTO_TOTAL_DAP",
  "MONTO_TOTAL_FM",
  "RET_ANUAL_PONDERADO",
  "DE_ANUAL_PONDERADO",
  "SHARPE_ANUAL_PONDERADO",
  "NPS",
  "INTERACCIONES_NEGATIVAS_6M",
  "INTERACCIONES_NEUTRAS_6M",
  "INTERACCIONES_POSITIVAS_6M",
  "Q_EMPRESAS_INVIERTE",
  "CANTIDAD_EN_CUSTODIA_TOTAL")




# RobustScaler - Equivalente a sklearn RobustScaler()

X <- CARTERA_TOTAL_MONTOS %>%
  select(all_of(columnas_modelo)) %>%
  as.data.frame()

medianas_m1 <- sapply(X, median, na.rm = TRUE)
iqr_m1 <- sapply(X, IQR, na.rm = TRUE)

# Evitar división por 0 si alguna variable tiene IQR = 0
iqr_m1[iqr_m1 == 0] <- 1

# Objeto tipo "estandarizador"
estandarizador_m1 <- list(center = medianas_m1,
  scale = iqr_m1)


# Transformación de variables


variables_estandarizadas <- sweep(X, 2, estandarizador_m1$center, "-")
variables_estandarizadas <- sweep(variables_estandarizadas, 2, estandarizador_m1$scale, "/")
variables_estandarizadas <- as.matrix(variables_estandarizadas)

# Evaluación de k con silhouette score


scores <- c()
K_range <- 2:10


for (k in K_range) {
  modelo_kmeans <- kmeans(variables_estandarizadas,
    centers = k,
    nstart = 10)
  
  sil <- silhouette(modelo_kmeans$cluster, dist(variables_estandarizadas))
  score <- mean(sil[, "sil_width"])
  
  scores <- c(scores, score)
}


# Tabla de resultados silueta


df_silueta <- data.frame(k = K_range,
  silhouette_score = scores)


# Mejor cantidad de clusters


mejor_k <- df_silueta$k[which.max(df_silueta$silhouette_score)]
mejor_score <- max(df_silueta$silhouette_score)

# Grafico

silueta_1<-ggplot(df_silueta, aes(x = k, y = silhouette_score)) +
  geom_line(color = "darkgreen") +
  geom_point(color = "darkgreen") +
  labs(title = "Coeficiente de Silueta - Etapa 1",
    x = "Número de clusters (k)",
    y = "Silhouette Score") +
  theme_minimal() +
  theme(panel.grid.major = element_line(),
    panel.grid.minor = element_blank())

print(silueta_1)

ggsave(filename = "FILES/MODELOS/kmedias_silueta_etapa_1.png",
       plot = silueta_1,
       width = 8,
       height = 6,
       dpi = 300)


# Modelo final con el mejor k


kmeans_final_m1 <- kmeans(variables_estandarizadas,
  centers = mejor_k,
  nstart = 10)

# Pegar cluster a la base
CARTERA_TOTAL_MONTOS$CLUSTER <- kmeans_final_m1$cluster

# Mostrar cantidad de observaciones por cluster
print(table(CARTERA_TOTAL_MONTOS$CLUSTER))

# Tabla promedio por cluster
promedios_clusters_m1 <- CARTERA_TOTAL_MONTOS %>%
  group_by(CLUSTER) %>%
  summarise(across(all_of(columnas_modelo), mean, na.rm = TRUE),
    .groups = "drop")



# Dejar solo cluster 1

df_1 <- CARTERA_TOTAL_MONTOS %>%
  filter(CLUSTER == 1)


# Robust scaling para modelo 2

X_m2 <- df_1 %>%
  select(all_of(columnas_modelo)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.data.frame()

medianas_m2 <- sapply(X_m2, function(x) median(x, na.rm = TRUE))
iqr_m2 <- sapply(X_m2, function(x) IQR(x, na.rm = TRUE))

iqr_m2[is.na(iqr_m2) | iqr_m2 == 0] <- 1

estandarizador_m2 <- list(center = medianas_m2,
  scale = iqr_m2)

variables_estandarizadas_m2 <- sweep(as.matrix(X_m2), 2, estandarizador_m2$center, "-")
variables_estandarizadas_m2 <- sweep(variables_estandarizadas_m2, 2, estandarizador_m2$scale, "/")

# Silhouette para modelo 2

scores_m2 <- c()
K_range_m2 <- 2:10

set.seed(0)

for (k in K_range_m2) {
  modelo_kmeans_m2 <- kmeans(variables_estandarizadas_m2,
    centers = k,
    nstart = 10)
  
  sil_m2 <- silhouette(modelo_kmeans_m2$cluster, dist(variables_estandarizadas_m2))
  score_m2 <- mean(sil_m2[, "sil_width"])
  
  scores_m2 <- c(scores_m2, score_m2)
}


# Tabla de resultados modelo 2

df_silueta_m2 <- data.frame(k = K_range_m2,
  silhouette_score = scores_m2)

# Mejor k modelo 2
mejor_k_m2 <- df_silueta_m2$k[which.max(df_silueta_m2$silhouette_score)]
mejor_score_m2 <- max(df_silueta_m2$silhouette_score)


# Gráfico modelo 2

silueta_2<-ggplot(df_silueta_m2, aes(x = k, y = silhouette_score)) +
  geom_line(color = "darkred") +
  geom_point(color = "darkred") +
  labs(title = "Coeficiente de Silueta - Etapa 2",
    x = "Número de clusters (k)",
    y = "Silhouette Score") +
  theme_minimal() +
  theme(panel.grid.major = element_line(),
    panel.grid.minor = element_blank())

print(silueta_2)

ggsave(filename = "FILES/MODELOS/kmedias_silueta_etapa_2.png",
       plot = silueta_2,
       width = 8,
       height = 6,
       dpi = 300)

# Modelo final etapa 2 con k = 5

kmeans_final_m2 <- kmeans(variables_estandarizadas_m2,
  centers = 5,
  nstart = 10)

# Pegar cluster al df_1
df_1$CLUSTER <- kmeans_final_m2$cluster

# Mostrar cantidad de observaciones por cluster
print(table(df_1$CLUSTER))

# Tabla promedio por cluster del modelo 2
promedios_clusters_m2 <- df_1 %>%
  group_by(CLUSTER) %>%
  summarise(across(all_of(columnas_modelo), mean, na.rm = TRUE),
    .groups = "drop")


# Crear df con cluster 2 del primer modelo


df_2 <- CARTERA_TOTAL_MONTOS %>%
  filter(CLUSTER == 2)

# Cambiar ese cluster a 6
df_2 <- df_2 %>%
  mutate(CLUSTER = 6)


# Unir resultado final


df_final <- bind_rows(df_1, df_2)

# reset visual del orden
df_final <- df_final %>%
  ungroup()

# Revisar distribución final
print(table(df_final$CLUSTER))



# Variables para modelo DAP

columnas_modelo_dap <- c("Q_CUENTAS",
  "MONTO_TOTAL_DAP",
  "MONTO_TOTAL_FM",
  "RET_ANUAL_PONDERADO",
  "DE_ANUAL_PONDERADO",
  "NPS",
  "INTERACCIONES_NEGATIVAS_6M",
  "INTERACCIONES_NEUTRAS_6M",
  "INTERACCIONES_POSITIVAS_6M")


# Base modelo DAP

X_dap <- df_dap %>%
  select(all_of(columnas_modelo_dap)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.data.frame()


# Robust scaling DAP

medianas_dap <- sapply(X_dap, function(x) median(x, na.rm = TRUE))
iqr_dap <- sapply(X_dap, function(x) IQR(x, na.rm = TRUE))

iqr_dap[is.na(iqr_dap) | iqr_dap == 0] <- 1

estandarizador_mdap <- list(center = medianas_dap,
  scale = iqr_dap)

variables_estandarizadas_dap <- sweep(as.matrix(X_dap), 2, estandarizador_mdap$center, "-")
variables_estandarizadas_dap <- sweep(variables_estandarizadas_dap, 2, estandarizador_mdap$scale, "/")


# Evaluación de k con silhouette score

scores_dap <- c()
K_range_dap <- 2:10


for (k in K_range_dap) {
  modelo_kmeans_dap <- kmeans(variables_estandarizadas_dap,
    centers = k,
    nstart = 10)
  
  sil_dap <- silhouette(modelo_kmeans_dap$cluster, dist(variables_estandarizadas_dap))
  score_dap <- mean(sil_dap[, "sil_width"])
  
  scores_dap <- c(scores_dap, score_dap)
}


# Tabla de resultados silueta

df_silueta_dap <- data.frame(k = K_range_dap,
  silhouette_score = scores_dap)

# Mejor cantidad de clusters
mejor_k_dap <- df_silueta_dap$k[which.max(df_silueta_dap$silhouette_score)]
mejor_score_dap <- max(df_silueta_dap$silhouette_score)


# Gráfico

silueta_dap<-ggplot(df_silueta_dap, aes(x = k, y = silhouette_score)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Coeficiente de Silueta - Etapa DAP",
    x = "Número de clusters (k)",
    y = "Silhouette Score") +
  theme_minimal() +
  theme(panel.grid.major = element_line(),
    panel.grid.minor = element_blank())

print(silueta_dap)

ggsave(filename = "FILES/MODELOS/kmedias_silueta_etapa_dap.png",
       plot = silueta_dap,
       width = 8,
       height = 6,
       dpi = 300)

# Modelo final DAP con k = mejor_k_dap


kmeans_final_mdap <- kmeans(variables_estandarizadas_dap,
  centers = mejor_k_dap,
  nstart = 10)

# Pegar cluster al df_dap

df_dap$CLUSTER <- kmeans_final_mdap$cluster

# Mostrar cantidad de observaciones por cluster
print(table(df_dap$CLUSTER))

# Tabla promedio por cluster
promedios_clusters_mdap <- df_dap %>%
  group_by(CLUSTER) %>%
  summarise(across(all_of(columnas_modelo_dap), mean, na.rm = TRUE),
    .groups = "drop")

# Renombrar clusters DAP
df_dap <- df_dap %>%
  mutate(CLUSTER = recode(
      as.character(CLUSTER),
      "2" = "DAP_PREFERENTE",
      "1" = "DAP"))

# Unir al consolidado final
df_final <- df_final %>%
  mutate(CLUSTER = as.character(CLUSTER))

df_final <- bind_rows(df_final, df_dap)

# Revisar distribución final
print(table(df_final$CLUSTER))


# Variables modelo principal

columnas_modelo <- c("Q_CUENTAS",
  "MONTO_TOTAL_ACCIONES",
  "MONTO_TOTAL_DAP",
  "MONTO_TOTAL_FM",
  "RET_ANUAL_PONDERADO",
  "DE_ANUAL_PONDERADO",
  "SHARPE_ANUAL_PONDERADO",
  "NPS",
  "INTERACCIONES_NEGATIVAS_6M",
  "INTERACCIONES_NEUTRAS_6M",
  "INTERACCIONES_POSITIVAS_6M",
  "Q_EMPRESAS_INVIERTE",
  "CANTIDAD_EN_CUSTODIA_TOTAL")


# Tabla final de promedios

promedios_clusters_finales <- df_final %>%
  group_by(CLUSTER) %>%
  summarise(Q_OBS = n(),
    across(all_of(columnas_modelo), mean, na.rm = TRUE),
    .groups = "drop")


# Tabla final de medianas

medianas_clusters_finales <- df_final %>%
  group_by(CLUSTER) %>%
  summarise(Q_OBS = n(),
    across(all_of(columnas_modelo), median, na.rm = TRUE),
    .groups = "drop")


# Exportar a Excel

wb <- createWorkbook()

addWorksheet(wb, "tabla_final_clusters")
writeData(wb, "tabla_final_clusters", df_final)

addWorksheet(wb, "promedios_clusters")
writeData(wb, "promedios_clusters", promedios_clusters_finales)

addWorksheet(wb, "medianas_clusters")
writeData(wb, "medianas_clusters", medianas_clusters_finales)

saveWorkbook(wb, "FILES/MODELOS/TABLA_RESUMEN_FINAL_KMEANS_CON_SEGMENTO.xlsx", overwrite = TRUE)


# Guardar objetos del modelo

saveRDS(estandarizador_m1, "FILES/MODELOS/estandarizador_m1.rds")
saveRDS(kmeans_final_m1, "FILES/MODELOS/kmeans_final_m1.rds")

saveRDS(estandarizador_m2, "FILES/MODELOS/estandarizador_m2.rds")
saveRDS(kmeans_final_m2, "FILES/MODELOS/kmeans_final_m2.rds")

saveRDS(estandarizador_mdap, "FILES/MODELOS/estandarizador_mdap.rds")
saveRDS(kmeans_final_mdap, "FILES/MODELOS/kmeans_final_mdap.rds")




# Asegurar que CLUSTER quede como primera columna
tabla_grafica <- promedios_clusters_finales %>%
  mutate(CLUSTER = as.character(CLUSTER)) %>%
  relocate(CLUSTER)

# Columnas numéricas a colorear
cols_numericas <- tabla_grafica %>%
  select(-CLUSTER) %>%
  names()

gt_tabla <- tabla_grafica %>%
  gt(rowname_col = "CLUSTER") %>%
  fmt_number(columns = c(Q_OBS, Q_CUENTAS, Q_EMPRESAS_INVIERTE, CANTIDAD_EN_CUSTODIA_TOTAL),
    decimals = 1,
    use_seps = TRUE) %>%
  fmt_number(columns = c(MONTO_TOTAL_ACCIONES, MONTO_TOTAL_DAP, MONTO_TOTAL_FM),
    decimals = 0,
    use_seps = TRUE) %>%
  fmt_percent(columns = c(RET_ANUAL_PONDERADO, DE_ANUAL_PONDERADO),
    decimals = 1,
    scale_values = FALSE) %>%
  fmt_number(columns = c(
      SHARPE_ANUAL_PONDERADO,
      NPS,
      INTERACCIONES_NEGATIVAS_6M,
      INTERACCIONES_NEUTRAS_6M,
      INTERACCIONES_POSITIVAS_6M),
    decimals = 1,
    use_seps = TRUE) %>%
  data_color(
    columns = all_of(cols_numericas),
    fn = col_numeric(
      palette = c("#f8696b", "#ffeb84", "#63be7b"),
      domain = NULL)) %>%
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(4))

gtsave(gt_tabla, "FILES/MODELOS/PROMEDIOS_CLUSTERS_FINALES.html")

