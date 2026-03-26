##### SEGUNDO SCRIPT EJECUCIÓN POSTERIOR #####
##### PARA EJECUTAR EL MODELO ENTRENADO EN EL SIGUIENTE MES #####

library(readr)
library(dplyr)
library(openxlsx)
library(readr)
library(dplyr)
library(openxlsx)
library(tidyr)
library(stringr)
library(readxl)

##### CARGAR OBJETOS GUARDADOS #####

estandarizador_m1   <- readRDS("FILES/MODELOS/estandarizador_m1.rds")
kmeans_final_m1     <- readRDS("FILES/MODELOS/kmeans_final_m1.rds")

estandarizador_m2   <- readRDS("FILES/MODELOS/estandarizador_m2.rds")
kmeans_final_m2     <- readRDS("FILES/MODELOS/kmeans_final_m2.rds")

estandarizador_mdap <- readRDS("FILES/MODELOS/estandarizador_mdap.rds")
kmeans_final_mdap   <- readRDS("FILES/MODELOS/kmeans_final_mdap.rds")

##### CARGAR BASE DEL MES #####


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

df_mes2 <- CARTERA_TOTAL_MONTOS %>%
  left_join(CARTERA_EMPRESAS_RESUMEN, by = "ID_CLIENTE") %>%
  mutate(Q_EMPRESAS_INVIERTE = replace_na(Q_EMPRESAS_INVIERTE, 0),
         CANTIDAD_EN_CUSTODIA_TOTAL = replace_na(CANTIDAD_EN_CUSTODIA_TOTAL, 0))


##### AGREGAR ORDEN ORIGINAL #####

df_mes2 <- df_mes2 %>%
  mutate(ORDEN_ORIGINAL = row_number())

##### VARIABLES MODELO GENERAL #####

columnas_general <- c("ID_CLIENTE",
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

columnas_modelo_general <- c("Q_CUENTAS",
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

##### VARIABLES MODELO DAP #####

columnas_modelo_dap <- c(
  "Q_CUENTAS",
  "MONTO_TOTAL_DAP",
  "MONTO_TOTAL_FM",
  "RET_ANUAL_PONDERADO",
  "DE_ANUAL_PONDERADO",
  "NPS",
  "INTERACCIONES_NEGATIVAS_6M",
  "INTERACCIONES_NEUTRAS_6M",
  "INTERACCIONES_POSITIVAS_6M"
)

##### SELECCIONAR COLUMNAS NECESARIAS #####

columnas_necesarias <- unique(c(
  columnas_general,
  columnas_modelo_dap
))

df_mes2 <- df_mes2 %>%
  select(all_of(columnas_necesarias)) %>%
  distinct()

##### SEPARAR BASE GENERAL Y BASE DAP #####
# Ajusta esta condición si tu lógica DAP real usa otra marca

df_mes2_general <- df_mes2 %>%
  filter(CANTIDAD_EN_CUSTODIA_TOTAL != 0)

df_mes2_dap <- df_mes2 %>%
  filter(CANTIDAD_EN_CUSTODIA_TOTAL == 0)

##### ==============================
##### ETAPA GENERAL - MODELO 1
##### ==============================

X_m1 <- df_mes2_general %>%
  select(all_of(columnas_modelo_general)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

X_m1 <- sweep(X_m1, 2, estandarizador_m1$center, "-")
X_m1 <- sweep(X_m1, 2, estandarizador_m1$scale, "/")

# Asignar cluster por distancia a centroides
asignar_cluster_kmeans <- function(X, centros) {
  distancias <- sapply(1:nrow(centros), function(i) {
    rowSums((X - matrix(centros[i, ], nrow = nrow(X), ncol = ncol(X), byrow = TRUE))^2)
  })
  max.col(-distancias)
}

df_mes2_general$CLUSTER_M1 <- asignar_cluster_kmeans(X_m1, kmeans_final_m1$centers)

##### ETAPA GENERAL - MODELO 2 PARA LOS DEL CLUSTER 1 #####

df_mes2_c1 <- df_mes2_general %>%
  filter(CLUSTER_M1 == 1)

if (nrow(df_mes2_c1) > 0) {
  X_m2 <- df_mes2_c1 %>%
    select(all_of(columnas_modelo_general)) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  
  X_m2 <- sweep(X_m2, 2, estandarizador_m2$center, "-")
  X_m2 <- sweep(X_m2, 2, estandarizador_m2$scale, "/")
  
  df_mes2_c1$CLUSTER <- asignar_cluster_kmeans(X_m2, kmeans_final_m2$centers)
}

##### LOS DEL CLUSTER 2 DEL MODELO 1 QUEDAN COMO 6 #####

df_mes2_c2 <- df_mes2_general %>%
  filter(CLUSTER_M1 == 2) %>%
  mutate(CLUSTER = 6)

##### UNIÓN PARTE GENERAL #####

df_mes2_general_final <- bind_rows(df_mes2_c1, df_mes2_c2)

##### ==============================
##### ETAPA DAP
##### ==============================

if (nrow(df_mes2_dap) > 0) {
  X_dap_mes2 <- df_mes2_dap %>%
    select(all_of(columnas_modelo_dap)) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  
  X_dap_mes2 <- sweep(X_dap_mes2, 2, estandarizador_mdap$center, "-")
  X_dap_mes2 <- sweep(X_dap_mes2, 2, estandarizador_mdap$scale, "/")
  
  df_mes2_dap$CLUSTER <- asignar_cluster_kmeans(X_dap_mes2, kmeans_final_mdap$centers)
  
  df_mes2_dap <- df_mes2_dap %>%
    mutate(
      CLUSTER = recode(
        as.character(CLUSTER),
        "2" = "DAP_PREFERENTE",
        "1" = "DAP"
      )
    )
}

##### ==============================
##### UNIÓN FINAL
##### ==============================

df_mes2_general_final <- df_mes2_general_final %>%
  mutate(CLUSTER = as.character(CLUSTER))

df_mes2_final <- bind_rows(df_mes2_general_final, df_mes2_dap) %>%
  mutate(CLUSTER = as.character(CLUSTER))

##### PROMEDIOS POR CLUSTER #####

columnas_resumen <- c(
  "Q_CUENTAS",
  "RET_ANUAL_PONDERADO",
  "DE_ANUAL_PONDERADO",
  "SHARPE_ANUAL_PONDERADO",
  "NPS",
  "INTERACCIONES_POSITIVAS_6M",
  "INTERACCIONES_NEGATIVAS_6M",
  "INTERACCIONES_NEUTRAS_6M",
  "Q_EMPRESAS_INVIERTE",
  "CANTIDAD_EN_CUSTODIA_TOTAL",
  "MONTO_EN_CUSTODIA_TOTAL",
  "MONTO_TOTAL_DAP",
  "MONTO_TOTAL_FM"
)

columnas_resumen <- columnas_resumen[columnas_resumen %in% names(df_mes2_final)]

promedios_clusters_mes2 <- df_mes2_final %>%
  group_by(CLUSTER) %>%
  summarise(
    Q_OBS = n(),
    across(all_of(columnas_resumen), ~mean(as.numeric(.), na.rm = TRUE)),
    .groups = "drop"
  )

##### MEDIANAS POR CLUSTER #####

medianas_clusters_mes2 <- df_mes2_final %>%
  group_by(CLUSTER) %>%
  summarise(
    Q_OBS = n(),
    across(all_of(columnas_resumen), ~median(as.numeric(.), na.rm = TRUE)),
    .groups = "drop"
  )

##### EXPORTAR A EXCEL #####

wb <- createWorkbook()

addWorksheet(wb, "tabla_final_clusters")
writeData(wb, "tabla_final_clusters", df_mes2_final)

addWorksheet(wb, "promedios_clusters")
writeData(wb, "promedios_clusters", promedios_clusters_mes2)

addWorksheet(wb, "medianas_clusters")
writeData(wb, "medianas_clusters", medianas_clusters_mes2)

saveWorkbook(
  wb,
  "FILES/OUTPUT/07_TABLA_RESUMEN_FINAL_KMEANS_CON_SEGMENTO_MENSUAL.xlsx",
  overwrite = TRUE
)
