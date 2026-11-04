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

## Identificar predictores ----
# para estudiar el retraso del vuelo, variables con información durante y después del vuelo no sirven

#### Variables correlacionadas ----
predictoras_drop <- c()
####### Aerolinea ----
vars_id_aerolinea <- vuelos_df %>%
  select(
    Reporting_Airline,
    DOT_ID_Reporting_Airline,
    IATA_CODE_Reporting_Airline,
    Flight_Number_Reporting_Airline,
  )

vars_id_aerolinea <- vars_id_aerolinea %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.))))

matriz_corr <- cor(vars_id_aerolinea, use = "pairwise.complete.obs")

# corrplot
corrplot(
  matriz_corr,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.7,
  addCoef.col = "black",
  number.cex = 0.5,
  title = "\nCorrelación entre Variables de Aerolínea",
  mar = c(0, 0, 2, 0)
)

predictoras_drop <- c(predictoras_drop, "IATA_CODE_Reporting_Airline")

###### Origen ----
vars_origen <- vuelos_df %>%
  select(
    Origin,
    OriginAirportID,
    OriginAirportSeqID,
    OriginCityMarketID,
    OriginCityName,
    OriginState,
    OriginStateFips,
    OriginStateName,
    OriginWac
  )


vars_origen <- vars_origen %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.))))

matriz_corr <- cor(vars_origen, use = "pairwise.complete.obs")

# corrplot
corrplot(
  matriz_corr,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.7,
  addCoef.col = "black",
  number.cex = 0.5,
  title = "\nCorrelación entre Variables de Origen",
  mar = c(0, 0, 2, 0)
)

predictoras_drop <- c(predictoras_drop, "OriginAirportID",
                      "OriginAirportSeqID",
                      "OriginCityMarketID",
                      "OriginCityName",
                      "OriginState",
                      "OriginStateFips",
                      "OriginStateName",
                      "OriginWac")

###### Destino ----
vars_destino <- vuelos_df %>%
  select(
    Dest,
    DestAirportID,
    DestAirportSeqID,
    DestCityMarketID,
    DestCityName,
    DestState,
    DestStateFips,
    DestStateName,
    DestWac
  )

vars_destino <- vars_destino %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.))))

matriz_corr_dest <- cor(vars_destino, use = "pairwise.complete.obs")

# corrplot para Destino
corrplot(
  matriz_corr_dest,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.7,
  addCoef.col = "black",
  number.cex = 0.5,
  title = "\nCorrelación entre Variables de Destino",
  mar = c(0, 0, 2, 0)
)

predictoras_drop <- c(predictoras_drop, "DestAirportID", "DestAirportSeqID", "DestStateFips", "DestStateName")

#### Otras Exclusiones ----
predictoras_drop <- c(
  predictoras_drop,
  "DepTime",
  "WheelsOff",
  "WheelsOn",
  "ArrTime",
  "ActualElapsedTime",
  "AirTime",
  "TaxiOut",
  "TaxiIn",
  "DepDelay",
  "DepDelayMinutes",
  "DepDel15",
  "DepartureDelayGroups",
  "ArrDelay",
  "ArrDelayMinutes",
  "ArrDel15",
  "ArrivalDelayGroups",
  "Cancelled",
  "CancellationCode",
  "Diverted",
  "CarrierDelay",       
  "WeatherDelay",
  "NASDelay",
  "SecurityDelay",
  "LateAircraftDelay",
  "FirstDepTime",
  "TotalAddGTime",
  "LongestAddGTime"
)

# ver la descripción de cada variable de predictoras_drop
for (v in predictoras_drop) {
  cat("Variable:", v, "\n")
  cat("Descripción:", ver_desc(v), "\n")
  cat("--------------------------------------------------\n")
}



