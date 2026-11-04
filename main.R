## Librerías ----
library(readxl)
library(dplyr)
library(corrplot)
## Carga Datos ----
vuelos_df <- read.csv("vuelos.csv")
diccionario_df <- read.csv("diccionario.csv")

## Funciones útiles ----
# leer descripcion de var_nombre
ver_desc <- function(var_nombre) {
  diccionario_df %>%
    filter(tolower(variable) == tolower(var_nombre)) %>%
    pull(descripcion_oficial)
}

## Variable objetivo ----
# 1 si cancelado, retraso >= 15 minutos o desviado.
# Descripciones:
ver_desc("Cancelled")
ver_desc("Diverted")
ver_desc("ArrDel15")

# Variable objetivo:
vuelos_df <- vuelos_df %>%
  mutate(
    # objetivo
    Incididencia = as.factor(case_when(
      # Si cualquiera de los tres indicadores es 1, la respuesta es 1
      Cancelled == 1 | Diverted == 1 | ArrDel15 == 1 ~ 1,
      
      # si los 3 son 0, entonces 0: no hubo incidencia
      Cancelled == 0 & Diverted == 0 & ArrDel15 == 0 ~ 0,
      
      # Si hay algún NA en medio, se queda como NA
      TRUE ~ NA_real_
    ))
  )

summary(vuelos_df$Incididencia)

## Predictores no válidos ----
source("id_predictores.R")
data_procesada <- vuelos_df %>%
  select(
    -all_of(predictoras_drop),
    -starts_with("Div", ignore.case = TRUE)  # Elimina todas las que empiezan por div/Div
  )



