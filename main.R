## main.R ----
# Caso 05 (BTS, vuelos desde BOS/JFK/LGA, 2022-2024) - EYP3407 Tópicos Aplicados en Estadística
# Punto de entrada único. Ejecutar desde la raíz del repositorio:
#   Rscript --vanilla main.R
# Entradas : datos/vuelos.csv y datos/diccionario.csv (caso05_bts_vuelos.zip descomprimido en datos/)
#            id_predictores.R (auditoría y exclusión de predictores; se carga con source())
# Salidas  : carpeta resultados/ (tablas .csv, predicciones, figura de calibración, sessionInfo)
#
# Orden del análisis:
#   0. Configuración
#   1. Carga, respuesta, clave y duplicados
#   2. Auditoría de predictores y tablas descriptivas
#   3. Variables: horarios y tasas históricas sin fuga
#   4. Particiones, preprocesamiento, métricas y referencia
#   5. Selección de hiperparámetros en desarrollo (M1, M2, M3)
#   6. Comparación en desarrollo, elección del modelo y umbral
#   7. Evaluación única en prueba (2024)
#   8. Incertidumbre, diagnósticos y calibración
#   9. Sensibilidades (solo datos de desarrollo)
#  10. Cierre (sessionInfo, tiempo total, informe opcional)

#### 0. CONFIGURACIÓN ####
t_inicio <- Sys.time()
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(glmnet)
  library(rpart)
  library(ranger)
})
SEED <- 2026
set.seed(SEED)
dir.create("resultados", showWarnings = FALSE)

msg     <- function(...) message(format(Sys.time(), "%H:%M:%S"), " | ", ...)
guardar <- function(x, nombre) write.csv(x, file.path("resultados", nombre), row.names = FALSE)

#### 1. CARGA, RESPUESTA, CLAVE Y DUPLICADOS ####
msg("1. Carga de datos")
stopifnot(file.exists("datos/vuelos.csv"), file.exists("datos/diccionario.csv"))
vuelos_df      <- read.csv("datos/vuelos.csv",       stringsAsFactors = FALSE)
diccionario_df <- read.csv("datos/diccionario.csv",  stringsAsFactors = FALSE)
stopifnot(nrow(vuelos_df) == 130940)   # tamaño indicado en la ficha del caso 05

# Descripción oficial de una variable (character(0) si no está en el diccionario).
# La usa id_predictores.R y la tabla de variables.
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
print(table(vuelos_df$Incidencia, useNA = "always"))

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

#### 2. AUDITORÍA DE PREDICTORES Y TABLAS DESCRIPTIVAS ####
msg("2. Auditoría de predictores")
source("id_predictores.R")   # define predictoras_drop
stopifnot(exists("predictoras_drop"))

# any_of: no falla si algún nombre no existe (id_predictores.R ya avisa de los faltantes)
data_procesada <- vuelos_df %>%
  select(-any_of(predictoras_drop))

# Columnas que se conservan pero NO son predictores
cols_aux <- c("clave", "FlightDate", "y", "Incidencia")

## Tabla 1: construcción de la muestra ----
tabla_muestra <- tibble(
  paso = c("Paquete original",
           "Claves únicas (sin duplicados)",
           "Respuesta conocida",
           "Respuesta indeterminada (se reporta, no se recodifica)"),
  filas = c(n_filas, n_claves, sum(!is.na(vuelos_df$y)), sum(is.na(vuelos_df$y)))
)
print(tabla_muestra)
guardar(tabla_muestra, "tabla_muestra.csv")

## Respuesta por año y tipo de incidencia ----
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
guardar(tabla_respuesta, "tabla_respuesta_por_anio.csv")

## Tabla 2: variables (la columna 'disponibilidad' se completa a mano en el informe) ----
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
guardar(tabla_vars, "tabla_variables.csv")

#### 3. VARIABLES: HORARIOS Y TASAS HISTÓRICAS SIN FUGA ####
msg("3. Construcción de variables")
stopifnot(!anyDuplicated(data_procesada$clave))   # la clave debe identificar cada vuelo

## Horarios programados ----
# CRSDepTime y CRSArrTime son horas locales HHMM (p. ej. 1530), no minutos.
hhmm_a_min <- function(x) (x %/% 100) * 60 + x %% 100

data_procesada <- data_procesada %>%
  mutate(
    CRSDepMin  = hhmm_a_min(CRSDepTime),
    CRSArrMin  = hhmm_a_min(CRSArrTime),
    HoraSalida = CRSDepMin %/% 60,
    ruta       = paste(Origin, Dest, sep = "-")
  )

## Tasas históricas sin fuga ----
# Para cada vuelo del día t, la tasa usa SOLO vuelos del mismo grupo en días anteriores a t
# (se excluye el día completo, no solo el propio vuelo). Supuesto declarado en el informe:
# el resultado de cada vuelo se conoce al terminar su día.
# Suavizamiento hacia la tasa global acumulada hasta el día anterior:
#   tasa = (incidencias_previas + m * tasa_global) / (vuelos_previos + m)
# Si no hay historia global (primer día), la tasa queda NA y se imputa con la media de entrenamiento.
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
  tasa_historica(data_procesada, "ruta",                           "ruta",       m_suavizamiento),
  tasa_historica(data_procesada, "Reporting_Airline",              "aerolinea",  m_suavizamiento),
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

#### 4. PARTICIONES, PREPROCESAMIENTO, MÉTRICAS Y REFERENCIA ####
msg("4. Particiones y referencia")
## Desarrollo y prueba ----
dev  <- datos_modelo %>% filter(Year %in% 2022:2023, !is.na(y))
test <- datos_modelo %>% filter(Year == 2024,        !is.na(y))
# 'test' no se usa hasta fijar modelos, métricas, umbral y regla de selección (sección 7).
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
guardar(tabla_folds, "tabla_folds.csv")

## Preprocesamiento aprendido SOLO en entrenamiento ----
# Categorías raras (< min_n en entrenamiento) o nuevas -> "Otra"; numéricas con NA -> media de entrenamiento.
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
guardar(ref_folds, "referencia_folds.csv")

#### 5. SELECCIÓN DE HIPERPARÁMETROS EN DESARROLLO ####
## Funciones comunes ----
# Matriz de diseño (mismas columnas en entrenamiento y validación: los niveles vienen de pp)
mm <- function(df, pp) {
  d <- aplicar_preproc(df, pp)
  model.matrix(~ . - 1, data = d[, c(pp$vars_cat, pp$vars_num)])
}
# Acotado de probabilidades (regla fijada a priori, igual para todos los modelos)
clip <- function(p, e = 1e-3) pmin(pmax(p, e), 1 - e)

# Pérdida logarítmica por bloque y configuración. 'vn' permite cambiar las variables numéricas
# (se usa en la sensibilidad sin tasas históricas).
cv_familia <- function(grid, ajustar_predecir, vn = vars_num) {
  bind_rows(lapply(seq_along(bloques_validacion), function(k) {
    f  <- get_fold(k); pp <- aprender_preproc(f$train, vars_cat, vn)
    bind_rows(lapply(seq_len(nrow(grid)), function(i) {
      p <- ajustar_predecir(f$train, f$val, pp, grid[i, , drop = FALSE])
      tibble(fold = k, cfg = i, logloss = logloss(f$val$y, clip(p)))
    }))
  }))
}

# Mejor configuración por pérdida logarítmica media en los bloques de validación
mejor <- function(cv, grid) {
  r <- cv %>% group_by(cfg) %>%
    summarise(ll = mean(logloss), sd = sd(logloss), .groups = "drop") %>% arrange(ll)
  list(tabla = r, cfg = grid[r$cfg[1], , drop = FALSE])
}

# Tabla de búsqueda para el anexo (todas las configuraciones evaluadas)
resumir_cv <- function(cv, grid, familia, etapa = "1") {
  cv %>% group_by(cfg) %>%
    summarise(ll_media = mean(logloss), ll_sd = sd(logloss), .groups = "drop") %>%
    left_join(cbind(cfg = seq_len(nrow(grid)), grid), by = "cfg") %>%
    mutate(familia = familia, etapa = etapa, .before = 1)
}

## Modelos ----
# M1: regresión logística ridge/lasso (2 alphas x 10 lambdas = 20 configuraciones)
grid1 <- expand.grid(alpha = c(0, 1), lambda = 10^seq(-5, -2, length.out = 10))
fit1 <- function(tr, va, pp, g) {
  m <- glmnet(mm(tr, pp), tr$y, family = "binomial", alpha = g$alpha, lambda = g$lambda)
  as.numeric(predict(m, mm(va, pp), type = "response"))
}

# M2: árbol de clasificación podado (20 valores de cp)
grid2 <- data.frame(cp = 10^seq(-5, -2.5, length.out = 20))
fit2 <- function(tr, va, pp, g) {
  d <- aplicar_preproc(tr, pp)[, c("Incidencia", pp$vars_cat, pp$vars_num)]
  m <- rpart(Incidencia ~ ., data = d, method = "class",
             control = rpart.control(cp = g$cp, minbucket = 50))
  predict(m, aplicar_preproc(va, pp), type = "prob")[, "1"]
}

# M3: bosque aleatorio de probabilidad, 300 árboles. La búsqueda se hizo en tres etapas porque
# el óptimo de las etapas 1 y 2 quedó en el borde de la grilla (nodos mayores mejoraban la
# pérdida); la etapa 3 es la última y muestra una meseta.
grid3  <- expand.grid(mtry = c(3, 5, 8, 11), min.node.size = c(5, 20, 50, 100, 200))      # 20 config.
grid3b <- expand.grid(mtry = c(2, 3, 5),     min.node.size = c(200, 500, 1000, 2000))     # 12 config.
grid3c <- expand.grid(mtry = c(3, 5),        min.node.size = c(4000, 8000))               #  4 config.
fit3 <- function(tr, va, pp, g) {
  d <- aplicar_preproc(tr, pp)[, c("Incidencia", pp$vars_cat, pp$vars_num)]
  m <- ranger(Incidencia ~ ., data = d, probability = TRUE, num.trees = 300,
              mtry = g$mtry, min.node.size = g$min.node.size, seed = SEED, num.threads = 2,
              verbose = FALSE)
  predict(m, aplicar_preproc(va, pp))$predictions[, "1"]
}

## Búsqueda de hiperparámetros ----
msg("5. Búsqueda de hiperparámetros: M1")
cv1 <- cv_familia(grid1, fit1); b1 <- mejor(cv1, grid1)
msg("5. Búsqueda de hiperparámetros: M2")
cv2 <- cv_familia(grid2, fit2); b2 <- mejor(cv2, grid2)
msg("5. Búsqueda de hiperparámetros: M3 (3 etapas)")
etapas_m3 <- list(`1` = grid3, `2` = grid3b, `3` = grid3c)
cv_m3  <- lapply(etapas_m3, function(g) cv_familia(g, fit3))
res_m3 <- Map(function(cv, g) mejor(cv, g), cv_m3, etapas_m3)
b3     <- res_m3[[which.min(sapply(res_m3, function(r) r$tabla$ll[1]))]]   # mejor global de M3

cat("\nM1 elegido:\n"); print(b1$cfg)
cat("M2 elegido:\n");   print(b2$cfg)
cat("M3 elegido:\n");   print(b3$cfg)

tabla_busqueda <- bind_rows(
  resumir_cv(cv1, grid1, "M1"),
  resumir_cv(cv2, grid2, "M2"),
  bind_rows(Map(function(cv, g, e) resumir_cv(cv, g, "M3", e), cv_m3, etapas_m3, names(etapas_m3)))
)
guardar(tabla_busqueda, "busqueda_hiperparametros.csv")

#### 6. COMPARACIÓN EN DESARROLLO, ELECCIÓN DEL MODELO Y UMBRAL ####
msg("6. Predicciones fuera de muestra en desarrollo")
# Predicciones de validación (3 bloques) de la mejor configuración de cada familia
oof <- function(ajustar_predecir, g, modelo) {
  bind_rows(lapply(seq_along(bloques_validacion), function(k) {
    f <- get_fold(k); pp <- aprender_preproc(f$train, vars_cat, vars_num)
    tibble(modelo = modelo, fold = k, clave = f$val$clave, y = f$val$y,
           p = clip(ajustar_predecir(f$train, f$val, pp, g)))
  }))
}
ref_oof <- bind_rows(lapply(seq_along(bloques_validacion), function(k) {
  f <- get_fold(k)
  tibble(modelo = "Referencia", fold = k, clave = f$val$clave, y = f$val$y,
         p = mean(f$train$y))
}))
oof_all <- bind_rows(ref_oof,
                     oof(fit1, b1$cfg, "M1"),
                     oof(fit2, b2$cfg, "M2"),
                     oof(fit3, b3$cfg, "M3"))

## Tablas de desarrollo ----
por_fold <- oof_all %>% group_by(modelo, fold) %>%
  summarise(ll = logloss(y, p), brier = brier(y, p), auc = auc(y, p), .groups = "drop")
tabla_dev <- por_fold %>% group_by(modelo) %>%
  summarise(across(c(ll, brier, auc), mean), .groups = "drop")
print(tabla_dev)
guardar(por_fold,  "dev_por_bloque.csv")
guardar(tabla_dev, "tabla_dev.csv")

# Diferencias pareadas por bloque (negativo: M1 es mejor)
dif_pareada <- por_fold %>% select(modelo, fold, ll) %>%
  pivot_wider(names_from = modelo, values_from = ll) %>%
  mutate(M1_vs_ref = M1 - Referencia, M1_vs_M3 = M1 - M3, M1_vs_M2 = M1 - M2)
print(dif_pareada)
guardar(dif_pareada, "dev_diferencias_pareadas.csv")

## Elección del modelo (regla: menor pérdida logarítmica media; ante empate, el más simple) ----
elegido <- "M1"
mejor_dev <- tabla_dev %>% filter(modelo != "Referencia") %>% slice_min(ll, n = 1, with_ties = FALSE) %>% pull(modelo)
stopifnot("El modelo elegido ya no tiene la menor pérdida media en desarrollo: revisar la regla" =
            elegido == mejor_dev)

## Umbral en desarrollo: mayor umbral con sensibilidad >= piso (predicciones de validación del elegido) ----
piso_sens <- 0.60
o <- oof_all %>% filter(modelo == elegido)

curva <- bind_rows(lapply(seq(0.05, 0.80, by = 0.005),
                          function(u) metricas_umbral(o$y, o$p, u))) %>%
  mutate(f1 = 2 * sensibilidad * vpp / (sensibilidad + vpp))
guardar(curva, "dev_curva_umbral.csv")

cand <- curva %>% filter(sensibilidad >= piso_sens, !is.na(vpp))
stopifnot(nrow(cand) > 0)
umbral     <- max(cand$umbral)                  # regla elegida
umbral_vpp <- cand$umbral[which.max(cand$vpp)]  # solo para comparar
print(c(umbral = umbral, umbral_vpp = umbral_vpp))

print(metricas_umbral(o$y, o$p, umbral))
cat("Proporción de vuelos marcados en desarrollo:", mean(o$p >= umbral), "\n")
por_bloque <- o %>% group_by(fold) %>%
  group_modify(~ metricas_umbral(.x$y, .x$p, umbral)) %>% ungroup()
print(por_bloque)
guardar(por_bloque, "dev_umbral_por_bloque.csv")
guardar(tibble(modelo = elegido, piso_sensibilidad = piso_sens, umbral = umbral), "umbral.csv")

#### 7. EVALUACIÓN ÚNICA EN PRUEBA (2024) ####
# Todo lo que sigue usa decisiones ya fijadas con desarrollo: modelos, hiperparámetros,
# umbral y regla de selección. No se modifica nada después de ver estos resultados.
msg("7. Evaluación en prueba")
umbral <- read.csv("resultados/umbral.csv")$umbral   # fijado en desarrollo

pp_full <- aprender_preproc(dev, vars_cat, vars_num)
t1 <- system.time(p1 <- clip(fit1(dev, test, pp_full, b1$cfg)))
t2 <- system.time(p2 <- clip(fit2(dev, test, pp_full, b2$cfg)))
t3 <- system.time(p3 <- clip(fit3(dev, test, pp_full, b3$cfg)))

pred_test <- bind_rows(
  tibble(modelo = "Referencia", clave = test$clave, y = test$y, p = mean(dev$y)),
  tibble(modelo = "M1", clave = test$clave, y = test$y, p = p1),
  tibble(modelo = "M2", clave = test$clave, y = test$y, p = p2),
  tibble(modelo = "M3", clave = test$clave, y = test$y, p = p3))
guardar(pred_test, "predicciones_test.csv")

tabla_test <- pred_test %>% group_by(modelo) %>%
  summarise(n = n(), prevalencia = mean(y), ll = logloss(y, p),
            brier = brier(y, p), auc = auc(y, p), .groups = "drop")
print(tabla_test)
guardar(tabla_test, "tabla_test.csv")

pt <- pred_test %>% filter(modelo == elegido)
umbral_test <- metricas_umbral(pt$y, pt$p, umbral) %>% mutate(prop_marcados = mean(pt$p >= umbral))
print(umbral_test)
guardar(umbral_test, "test_umbral.csv")

tiempos <- tibble(modelo = c("M1", "M2", "M3"),
                  cpu_s     = c(sum(t1[c("user.self", "sys.self")]),
                                sum(t2[c("user.self", "sys.self")]),
                                sum(t3[c("user.self", "sys.self")])),
                  elapsed_s = c(t1[["elapsed"]], t2[["elapsed"]], t3[["elapsed"]]))
print(tiempos)
guardar(tiempos, "tiempos_ajuste.csv")

#### 8. INCERTIDUMBRE, DIAGNÓSTICOS Y CALIBRACIÓN ####
msg("8. Incertidumbre y diagnósticos")
## Pérdida por vuelo y bloques semanales ----
pw <- pred_test %>% mutate(l = -(y * log(p) + (1 - y) * log(1 - p))) %>%
  select(modelo, clave, l) %>% pivot_wider(names_from = modelo, values_from = l) %>%
  left_join(test %>% select(clave, FlightDate), by = "clave") %>%
  mutate(semana = as.integer(as.numeric(FlightDate - min(FlightDate)) %/% 7))

## Bootstrap por semanas: los tres aeropuertos de cada semana se mantienen juntos.
## Los modelos NO se reajustan: incertidumbre condicional a los ajustes realizados.
bl <- pw %>% group_by(semana) %>%
  summarise(n = n(), d_ref = sum(M1 - Referencia), d_M3 = sum(M1 - M3), .groups = "drop")
set.seed(SEED); B <- 2000
boot <- replicate(B, { i <- sample(nrow(bl), replace = TRUE)
c(sum(bl$d_ref[i]), sum(bl$d_M3[i])) / sum(bl$n[i]) })
est <- c(sum(bl$d_ref), sum(bl$d_M3)) / sum(bl$n)
ic_boot <- tibble(
  comparacion = c("M1 - Referencia", "M1 - M3"),
  estimador   = est,
  p2.5        = unname(c(quantile(boot[1, ], .025), quantile(boot[2, ], .025))),
  p97.5       = unname(c(quantile(boot[1, ], .975), quantile(boot[2, ], .975))),
  bloques     = nrow(bl), replicas = B)
print(ic_boot)
guardar(ic_boot, "test_bootstrap_semanas.csv")

## Diagnóstico por aeropuerto, temporada y tipo de incidencia (elegido, umbral fijado) ----
tipo <- vuelos_df %>% transmute(clave, tipo = case_when(
  Cancelled == 1 ~ "Cancelado", Diverted == 1 ~ "Desviado", ArrDel15 == 1 ~ "Retraso >= 15 min",
  TRUE ~ "Sin incidencia"))
d <- test %>% select(clave, Origin, Month, y) %>%
  left_join(pred_test %>% filter(modelo == elegido) %>% select(clave, p), by = "clave") %>%
  left_join(pw %>% select(clave, l_M1 = M1, l_ref = Referencia), by = "clave") %>%
  left_join(tipo, by = "clave") %>%
  mutate(marcado = p >= umbral,
         temporada = case_when(Month %in% c(12, 1, 2) ~ "Invierno", Month %in% 3:5 ~ "Primavera",
                               Month %in% 6:8 ~ "Verano", TRUE ~ "Otoño"))
resumen <- function(df, g) df %>% group_by(across(all_of(g))) %>%
  summarise(n = n(), prev = mean(y), ll_M1 = mean(l_M1), ll_ref = mean(l_ref),
            delta = ll_M1 - ll_ref,
            sens = sum(marcado & y == 1) / sum(y == 1),
            vpp = sum(marcado & y == 1) / sum(marcado), .groups = "drop")
diag_aeropuerto <- resumen(d, "Origin")
diag_temporada  <- resumen(d, "temporada")
diag_tipo <- d %>% filter(y == 1) %>% group_by(tipo) %>% summarise(n = n(), sens = mean(marcado))
print(diag_aeropuerto); print(diag_temporada); print(diag_tipo)
guardar(diag_aeropuerto, "test_diag_aeropuerto.csv")
guardar(diag_temporada,  "test_diag_temporada.csv")
guardar(diag_tipo,       "test_diag_tipo.csv")

## Calibración en deciles ----
cal <- d %>% mutate(bin = ntile(p, 10)) %>% group_by(bin) %>%
  summarise(p_med = mean(p), obs = mean(y), n = n())
print(cal)
guardar(cal, "test_calibracion_deciles.csv")
png("resultados/calibracion_test.png", width = 900, height = 700, res = 130)
plot(cal$p_med, cal$obs, pch = 19, xlim = c(0, .6), ylim = c(0, .6),
     xlab = "Probabilidad predicha (media por decil)", ylab = "Frecuencia observada de incidencia")
abline(0, 1, lty = 2)
invisible(dev.off())

#### 9. SENSIBILIDADES (SOLO DATOS DE DESARROLLO) ####
## Sensibilidad 1: retirar las tasas históricas y los conteos nprev_* ----
# M1 se vuelve a sintonizar (20 config.); M3 se evalúa con su configuración final.
msg("9. Sensibilidad 1: sin tasas históricas")
vn_sin <- setdiff(vars_num, grep("^(tasa|nprev)_", vars_num, value = TRUE))
print(vn_sin)   # debe quedar: Distance, CRSElapsedTime, CRSDepMin, CRSArrMin, HoraSalida

cv1_sin <- cv_familia(grid1, fit1, vn = vn_sin)
cv3_sin <- cv_familia(b3$cfg, fit3, vn = vn_sin)

comp_base <- por_fold %>% filter(modelo %in% c("Referencia", "M1", "M3")) %>%
  select(modelo, fold, ll) %>% pivot_wider(names_from = modelo, values_from = ll)
mejor1_sin <- cv1_sin %>% group_by(cfg) %>% summarise(ll = mean(logloss), .groups = "drop") %>%
  slice_min(ll, n = 1, with_ties = FALSE)
sens_tasas <- comp_base %>%
  left_join(cv1_sin %>% filter(cfg == mejor1_sin$cfg) %>% select(fold, M1_sin = logloss), by = "fold") %>%
  left_join(cv3_sin %>% select(fold, M3_sin = logloss), by = "fold") %>%
  mutate(d_M1 = M1_sin - M1, d_M3 = M3_sin - M3)   # positivo: sin tasas es peor
print(sens_tasas)
print(colMeans(sens_tasas[, -1]))
guardar(sens_tasas, "sens_sin_tasas.csv")

## Sensibilidad 2: rutas no vistas ----
# Se reserva el 20 % de las rutas de desarrollo (regla reproducible, no usa respuestas), se excluyen
# del entrenamiento de cada bloque y se evalúa el elegido (M1) en rutas vistas y no vistas.
# Para las no vistas la ruta se trata como nueva (arranque en frío: sin historia propia).
msg("9. Sensibilidad 2: rutas no vistas")
set.seed(SEED)
rutas_dev <- sort(unique(dev$ruta))
rutas_out <- sample(rutas_dev, size = round(0.20 * length(rutas_dev)))
cat("Rutas en desarrollo:", length(rutas_dev), "| reservadas:", length(rutas_out),
    "| % de vuelos reservados:", round(100 * mean(dev$ruta %in% rutas_out), 1), "\n")

res_rutas <- bind_rows(lapply(seq_along(bloques_validacion), function(k) {
  f  <- get_fold(k)
  tr <- f$train %>% filter(!ruta %in% rutas_out)
  pp <- aprender_preproc(tr, vars_cat, vars_num)
  va <- f$val %>% mutate(no_vista = ruta %in% rutas_out)
  va <- va %>% mutate(tasa_ruta  = ifelse(no_vista, pp$medias[["tasa_ruta"]], tasa_ruta),
                      nprev_ruta = ifelse(no_vista, 0, nprev_ruta))      # arranque en frío
  p  <- clip(fit1(tr, va, pp, b1$cfg)); p0 <- mean(tr$y)
  va %>% mutate(p = p) %>% group_by(no_vista) %>%
    summarise(n = n(), prev = mean(y), ll_M1 = logloss(y, p),
              ll_ref = logloss(y, rep(p0, length(y))), .groups = "drop") %>%
    mutate(fold = k, delta = ll_M1 - ll_ref, .before = 1)
}))
res_rutas_total <- res_rutas %>% group_by(no_vista) %>%
  summarise(vuelos = sum(n), ll_M1 = weighted.mean(ll_M1, n), ll_ref = weighted.mean(ll_ref, n),
            delta = ll_M1 - ll_ref, .groups = "drop")
print(res_rutas); print(res_rutas_total)
guardar(res_rutas,       "sens_rutas_por_bloque.csv")
guardar(res_rutas_total, "sens_rutas_total.csv")

#### 10. CIERRE ####
writeLines(capture.output(sessionInfo()), "resultados/sessionInfo.txt")
msg("Tiempo total: ", round(as.numeric(difftime(Sys.time(), t_inicio, units = "mins")), 1), " min")




