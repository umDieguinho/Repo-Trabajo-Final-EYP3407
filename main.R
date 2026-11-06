## main.R ----
# Punto de entrada único. Ejecutar desde la raíz del repositorio:
#   Rscript --vanilla analisis.R
# Entradas: datos/vuelos.csv y datos/diccionario.csv (caso05_bts_vuelos.zip descomprimido en datos/)
# Salidas: carpeta resultados/
#
# Contenido:
#   1. Carga, respuesta, clave y auditoría de duplicados
#   2. Auditoría de predictores y exclusiones (archivo aparte: id_predictores.R)
#   3. Tablas para el informe
#   4. Variables: horarios y tasas históricas sin fuga
#   5. Particiones, preprocesamiento, métricas y referencia
#   (Pendiente: modelos M1, M2, M3, umbral y evaluación en prueba)

library(dplyr)
library(corrplot)
library(glmnet)
library(rpart)
library(ranger)
set.seed(2026)
dir.create("resultados", showWarnings = FALSE)

#### 1-3. PREPARACIÓN ####
## Carga de datos ----
ruta_datos <- if (file.exists(file.path("datos", "vuelos.csv"))) "datos" else "."
vuelos_df      <- read.csv(file.path(ruta_datos, "vuelos.csv"),       stringsAsFactors = FALSE)
diccionario_df <- read.csv(file.path(ruta_datos, "diccionario.csv"),  stringsAsFactors = FALSE)

stopifnot(nrow(vuelos_df) == 130940)   # tamaño indicado en la ficha del caso 05

## Funciones útiles ----
# Descripción oficial de una variable (character(0) si no está en el diccionario)
ver_desc <- function(var_nombre) {
  diccionario_df %>%
    filter(tolower(variable) == tolower(var_nombre)) %>%
    pull(descripcion_oficial)
}

## Variable objetivo ----
# y = 1 si Cancelled == 1, Diverted == 1 o ArrDel15 == 1
# y = 0 si los tres valen 0; NA si el resultado es indeterminado (no se recodifica).
# En estos datos ArrDel15 solo falta cuando el vuelo fue cancelado (y = 1 igualmente),
# por lo que no hay indeterminados; igual se reporta el conteo.
vuelos_df <- vuelos_df %>%
  mutate(
    y = case_when(
      Cancelled == 1 | Diverted == 1 | ArrDel15 == 1 ~ 1L,
      Cancelled == 0 & Diverted == 0 & ArrDel15 == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    Incidencia = factor(y, levels = c(0, 1))
  )

table(vuelos_df$Incidencia, useNA = "always")

## Clave del vuelo y auditoría de duplicados ----
# Se construye ANTES de eliminar columnas (usa número de vuelo y hora programada)
vuelos_df <- vuelos_df %>%
  mutate(
    FlightDate = as.Date(FlightDate),   # el CSV trae yyyy-mm-dd
    clave = paste(FlightDate, Reporting_Airline, Flight_Number_Reporting_Airline,
                  Origin, CRSDepTime, sep = "|")
  )

n_filas      <- nrow(vuelos_df)
n_claves     <- n_distinct(vuelos_df$clave)
n_duplicadas <- n_filas - n_claves

# Si hubiera duplicados, revisarlos antes de decidir (no se borran a ciegas)
duplicados <- vuelos_df %>%
  group_by(clave) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(clave)

## Predictores no válidos ----
source("id_predictores.R")   # define predictoras_drop

# any_of: no falla si algún nombre no existe (id_predictores.R ya avisa de los faltantes)
data_procesada <- vuelos_df %>%
  select(-any_of(predictoras_drop))

# Columnas que se conservan pero NO son predictores
cols_aux <- c("clave", "FlightDate", "y", "Incidencia")

## Tablas para el informe ----
# Tabla 1: construcción de la muestra
tabla_muestra <- tibble(
  paso = c("Paquete original",
           "Claves únicas (sin duplicados)",
           "Respuesta conocida",
           "Respuesta indeterminada (se reporta, no se recodifica)"),
  filas = c(n_filas, n_claves, sum(!is.na(vuelos_df$y)), sum(is.na(vuelos_df$y)))
)
print(tabla_muestra)
write.csv(tabla_muestra, "resultados/tabla_muestra.csv", row.names = FALSE)

# Respuesta por año y tipo de incidencia (los tipos no son excluyentes entre sí)
tabla_respuesta <- vuelos_df %>%
  group_by(Year) %>%
  summarise(
    n              = n(),
    cancelados     = sum(Cancelled == 1, na.rm = TRUE),
    desviados      = sum(Diverted == 1, na.rm = TRUE),
    retraso15      = sum(ArrDel15 == 1, na.rm = TRUE),
    incidencias    = sum(y == 1, na.rm = TRUE),
    indeterminados = sum(is.na(y)),
    prevalencia    = incidencias / sum(!is.na(y)),
    .groups = "drop"
  )
print(tabla_respuesta)
write.csv(tabla_respuesta, "resultados/tabla_respuesta_por_anio.csv", row.names = FALSE)

# Tabla 2: variables (completar a mano 'disponibilidad')
tabla_vars <- tibble(variable = names(vuelos_df)) %>%
  mutate(
    descripcion = vapply(variable, function(v) {
      d <- ver_desc(v)
      if (length(d) == 0) NA_character_ else d[1]
    }, character(1)),
    tipo = vapply(vuelos_df[variable], function(x) class(x)[1], character(1)),
    decision = ifelse(variable %in% predictoras_drop, "Excluida",
                      ifelse(variable %in% cols_aux, "Auxiliar (no predictor)", "Candidata")),
    disponibilidad = NA_character_
  )
write.csv(tabla_vars, "resultados/tabla_variables.csv", row.names = FALSE)

#### 4. VARIABLES ####
stopifnot(!anyDuplicated(data_procesada$clave))   # la clave debe identificar cada vuelo

## Horarios programados ----
# CRSDepTime y CRSArrTime son horas locales HHMM (p. ej. 1530), no minutos.
hhmm_a_min <- function(x) (x %/% 100) * 60 + x %% 100

data_procesada <- data_procesada %>%
  mutate(
    CRSDepMin = hhmm_a_min(CRSDepTime),
    CRSArrMin = hhmm_a_min(CRSArrTime),
    HoraSalida = CRSDepMin %/% 60,
    ruta = paste(Origin, Dest, sep = "-")
  )

## Tasas históricas sin fuga ----
# Para cada vuelo del día t, la tasa usa SOLO vuelos del mismo grupo en días anteriores a t
# (se excluye el día completo, no solo el propio vuelo). Supuesto declarado en el informe:
# el resultado de cada vuelo se conoce al terminar su día.
# Suavizamiento hacia la tasa global acumulada hasta el día anterior:
#   tasa = (incidencias_previas + m * tasa_global) / (vuelos_previos + m)
# Si no hay historia global (primer día), la tasa queda NA y se imputa en entrenamiento.
# m es un hiperparámetro: ajustarlo solo con desarrollo.
tasa_historica <- function(df, grupo, nombre, m = 20) {
  base <- df %>% filter(!is.na(y))

  global <- base %>%
    group_by(FlightDate) %>%
    summarise(n = n(), s = sum(y), .groups = "drop") %>%
    arrange(FlightDate) %>%
    mutate(p_glob = lag(cumsum(s) / cumsum(n))) %>%
    select(FlightDate, p_glob)

  hist <- base %>%
    group_by(across(all_of(grupo)), FlightDate) %>%
    summarise(n = n(), s = sum(y), .groups = "drop") %>%
    arrange(across(all_of(grupo)), FlightDate) %>%
    group_by(across(all_of(grupo))) %>%
    mutate(n_prev = cumsum(n) - n,    # vuelos de días anteriores del grupo
           s_prev = cumsum(s) - s) %>%
    ungroup() %>%
    select(all_of(grupo), FlightDate, n_prev, s_prev)

  res <- df %>%
    select(clave, all_of(grupo), FlightDate) %>%
    left_join(hist,   by = c(grupo, "FlightDate")) %>%
    left_join(global, by = "FlightDate") %>%
    mutate(n_prev = ifelse(is.na(n_prev), 0L, n_prev),
           s_prev = ifelse(is.na(s_prev), 0L, s_prev),
           tasa   = (s_prev + m * p_glob) / (n_prev + m)) %>%
    select(clave, tasa, n_prev)

  names(res) <- c("clave", paste0("tasa_", nombre), paste0("nprev_", nombre))
  res
}

m_suavizamiento <- 20

tasas <- list(
  tasa_historica(data_procesada, "ruta",                          "ruta",        m_suavizamiento),
  tasa_historica(data_procesada, "Reporting_Airline",             "aerolinea",   m_suavizamiento),
  tasa_historica(data_procesada, c("Origin", "Reporting_Airline"), "orig_aerol", m_suavizamiento)
)

datos_modelo <- data_procesada
for (tt in tasas) datos_modelo <- left_join(datos_modelo, tt, by = "clave")
stopifnot(nrow(datos_modelo) == nrow(data_procesada))

## Índice de mes (para bloques cronológicos) ----
datos_modelo <- datos_modelo %>%
  mutate(mes_idx = (Year - min(Year)) * 12 + Month)

## Listas de predictores ----
vars_cat <- c("Origin", "Dest", "DestState", "Reporting_Airline", "Month", "DayOfWeek")
vars_num <- c("Distance", "CRSElapsedTime", "CRSDepMin", "CRSArrMin", "HoraSalida",
              "tasa_ruta", "tasa_aerolinea", "tasa_orig_aerol",
              "nprev_ruta", "nprev_aerolinea", "nprev_orig_aerol")
predictores <- c(vars_cat, vars_num)

# Verificación: ningún predictor es columna auxiliar o excluida
stopifnot(!any(predictores %in% c(cols_aux, predictoras_drop)))
stopifnot(all(predictores %in% names(datos_modelo)))

#### 5. PARTICIONES Y REFERENCIA ####
## Desarrollo y prueba ----
dev  <- datos_modelo %>% filter(Year %in% 2022:2023, !is.na(y))
test <- datos_modelo %>% filter(Year == 2024,        !is.na(y))
# 'test' no se toca hasta fijar modelos, métricas, umbral y regla de selección.

cat("Desarrollo:", nrow(dev), " Prueba:", nrow(test), "\n")

## Validación interna: origen creciente, bloques de 4 meses ----
# Cada bloque contiene los tres aeropuertos juntos (perturbaciones compartidas por fecha).
# Entrenamiento = todos los meses anteriores al bloque. No hace falta brecha: el horizonte
# de la respuesta es el propio vuelo y su resultado se conoce el mismo día.
bloques_validacion <- list(13:16, 17:20, 21:24)   # ene-abr, may-ago, sep-dic de 2023

get_fold <- function(k, datos = dev) {
  val_meses <- bloques_validacion[[k]]
  list(
    train = datos %>% filter(mes_idx <  min(val_meses)),
    val   = datos %>% filter(mes_idx %in% val_meses)
  )
}

tabla_folds <- bind_rows(lapply(seq_along(bloques_validacion), function(k) {
  f <- get_fold(k)
  tibble(fold = k, n_train = nrow(f$train), n_val = nrow(f$val),
         prev_train = mean(f$train$y), prev_val = mean(f$val$y))
}))
print(tabla_folds)

## Preprocesamiento aprendido SOLO en entrenamiento ----
# Categorías raras o nuevas -> "Otra"; numéricas con NA -> media de entrenamiento.
aprender_preproc <- function(train, vars_cat, vars_num, min_n = 30) {
  niveles <- lapply(vars_cat, function(v) {
    tb <- table(as.character(train[[v]]))
    names(tb)[tb >= min_n]
  })
  names(niveles) <- vars_cat
  medias <- vapply(vars_num, function(v) mean(train[[v]], na.rm = TRUE), numeric(1))
  list(niveles = niveles, medias = medias, vars_cat = vars_cat, vars_num = vars_num)
}

aplicar_preproc <- function(df, pp) {
  for (v in pp$vars_cat) {
    x <- as.character(df[[v]])
    x[!(x %in% pp$niveles[[v]])] <- "Otra"
    df[[v]] <- factor(x, levels = c(pp$niveles[[v]], "Otra"))
  }
  for (v in pp$vars_num) {
    x <- df[[v]]
    x[is.na(x)] <- pp$medias[[v]]
    df[[v]] <- x
  }
  df
}

## Métricas ----
logloss <- function(y, p, eps = 1e-15) {
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

brier <- function(y, p) mean((p - y)^2)

# AUC por rangos (con empates promedia: una probabilidad constante da 0.5)
auc <- function(y, p) {
  r  <- rank(p)
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Sensibilidad, VPP y matriz de confusión para un umbral dado
metricas_umbral <- function(y, p, umbral) {
  pred <- as.integer(p >= umbral)
  tp <- sum(pred == 1 & y == 1); fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1); tn <- sum(pred == 0 & y == 0)
  tibble(umbral = umbral, tp = tp, fp = fp, fn = fn, tn = tn,
         sensibilidad = tp / (tp + fn),
         vpp = ifelse(tp + fp == 0, NA_real_, tp / (tp + fp)))
}

evaluar <- function(y, p, modelo) {
  tibble(modelo = modelo, n = length(y), prevalencia = mean(y),
         logloss = logloss(y, p), brier = brier(y, p), auc = auc(y, p))
}

## Referencia: probabilidad constante estimada en entrenamiento ----
ref_folds <- bind_rows(lapply(seq_along(bloques_validacion), function(k) {
  f  <- get_fold(k)
  p0 <- mean(f$train$y)
  evaluar(f$val$y, rep(p0, nrow(f$val)), "Referencia") %>% mutate(fold = k, .before = 1)
}))
print(ref_folds)

write.csv(tabla_folds, "resultados/tabla_folds.csv", row.names = FALSE)
write.csv(ref_folds,   "resultados/referencia_folds.csv", row.names = FALSE)

# Siguiente: modelos M1 (logística), M2 (árbol podado o SVM), M3 (bosque o boosting),
# umbral fijado en desarrollo y evaluación única en prueba.

mm <- function(df, pp) {
  d <- aplicar_preproc(df, pp)
  model.matrix(~ . - 1, data = d[, c(pp$vars_cat, pp$vars_num)])
}
clip <- function(p, e = 1e-3) pmin(pmax(p, e), 1 - e)   # regla fijada a priori

## Cada familia: <= 20 configuraciones x 3 folds. Devuelve logloss por fold y config
cv_familia <- function(grid, ajustar_predecir) {
  bind_rows(lapply(seq_along(bloques_validacion), function(k) {
    f  <- get_fold(k); pp <- aprender_preproc(f$train, vars_cat, vars_num)
    bind_rows(lapply(seq_len(nrow(grid)), function(i) {
      p <- ajustar_predecir(f$train, f$val, pp, grid[i, , drop = FALSE])
      tibble(fold = k, cfg = i, logloss = logloss(f$val$y, clip(p)))
    }))
  }))
}

## M1: logística ridge/lasso (2 alphas x 10 lambdas = 20)
grid1 <- expand.grid(alpha = c(0, 1), lambda = 10^seq(-5, -2, length.out = 10))
fit1 <- function(tr, va, pp, g) {
  m <- glmnet(mm(tr, pp), tr$y, family = "binomial", alpha = g$alpha, lambda = g$lambda)
  as.numeric(predict(m, mm(va, pp), type = "response"))
}

## M2: árbol podado (20 valores de cp)
grid2 <- data.frame(cp = 10^seq(-5, -2.5, length.out = 20))
fit2 <- function(tr, va, pp, g) {
  d <- aplicar_preproc(tr, pp)[, c("Incidencia", pp$vars_cat, pp$vars_num)]
  m <- rpart(Incidencia ~ ., data = d, method = "class", control = rpart.control(cp = g$cp, minbucket = 50))
  predict(m, aplicar_preproc(va, pp), type = "prob")[, "1"]
}

## M3: bosque aleatorio (4 mtry x 5 min.node.size = 20)
grid3 <- expand.grid(mtry = c(3, 5, 8, 11), min.node.size = c(5, 20, 50, 100, 200))
fit3 <- function(tr, va, pp, g) {
  d <- aplicar_preproc(tr, pp)[, c("Incidencia", pp$vars_cat, pp$vars_num)]
  m <- ranger(Incidencia ~ ., data = d, probability = TRUE, num.trees = 300,
              mtry = g$mtry, min.node.size = g$min.node.size, seed = 2026, num.threads = 2)
  predict(m, aplicar_preproc(va, pp))$predictions[, "1"]
}

cv1 <- cv_familia(grid1, fit1); cv2 <- cv_familia(grid2, fit2); cv3 <- cv_familia(grid3, fit3)

mejor <- function(cv, grid) {
  r <- cv %>% group_by(cfg) %>% summarise(ll = mean(logloss), sd = sd(logloss), .groups = "drop") %>% arrange(ll)
  list(tabla = r, cfg = grid[r$cfg[1], , drop = FALSE])
}
b1 <- mejor(cv1, grid1); b2 <- mejor(cv2, grid2); b3 <- mejor(cv3, grid3)
b1