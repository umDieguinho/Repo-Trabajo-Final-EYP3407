## id_predictores.R ----
# Auditoría de predictores y lista de variables a excluir (predictoras_drop).
# Requiere que existan vuelos_df y ver_desc() (se cargan en analisis.R).
# Se ejecuta desde main.R con source("id_predictores.R"),
# DESPUÉS de crear la respuesta y la clave.

suppressPackageStartupMessages(library(dplyr))

## Funciones auxiliares ----
# Relación entre una variable base y otras variables (sin codificar categorías
# como números: los códigos no tienen orden, así que la correlación de Pearson
# entre ellos no significa nada).
#   equivalente a la base         : correspondencia 1 a 1 (redundante)
#   agrupación más gruesa que base: cada valor de base cae en un solo valor de la otra
#   otra                          : sin relación determinista
relacion_con <- function(df, base, otras) {
  otras  <- intersect(otras, names(df))
  n_base <- n_distinct(df[[base]])
  filas <- lapply(otras, function(v) {
    n_v     <- n_distinct(df[[v]])
    n_pares <- n_distinct(df[[base]], df[[v]])
    tibble(
      variable        = v,
      niveles         = n_v,
      base_determina  = n_pares == n_base,
      otra_determina  = n_pares == n_v
    )
  })
  bind_rows(filas) %>%
    mutate(relacion = case_when(
      base_determina & otra_determina ~ "equivalente a la base",
      base_determina                  ~ "agrupación más gruesa que la base",
      TRUE                            ~ "otra"
    ))
}

## Aerolínea ----
vars_aerolinea <- c("DOT_ID_Reporting_Airline", "IATA_CODE_Reporting_Airline")
rel_aerolinea  <- relacion_con(vuelos_df, "Reporting_Airline", vars_aerolinea)
print(rel_aerolinea)

# Reporting_Airline se conserva. Los códigos DOT/IATA son equivalentes (redundantes).
# Flight_Number_Reporting_Airline es un identificador de alta cardinalidad:
# no es predictor, pero se usa en la clave del vuelo (construida antes).
drop_aerolinea <- c("DOT_ID_Reporting_Airline",
                    "IATA_CODE_Reporting_Airline",
                    "Flight_Number_Reporting_Airline")

## Origen ----
vars_origen <- c("OriginAirportID", "OriginAirportSeqID", "OriginCityMarketID",
                 "OriginCityName", "OriginState", "OriginStateFips",
                 "OriginStateName", "OriginWac")
rel_origen <- relacion_con(vuelos_df, "Origin", vars_origen)
print(rel_origen)

# Solo hay tres aeropuertos de origen: Origin basta.
drop_origen <- vars_origen

## Destino ----
vars_destino <- c("DestAirportID", "DestAirportSeqID", "DestCityMarketID",
                  "DestCityName", "DestState", "DestStateFips",
                  "DestStateName", "DestWac")
rel_destino <- relacion_con(vuelos_df, "Dest", vars_destino)
print(rel_destino)

# Se conservan Dest y DestState (agrupación geográfica que ayuda con destinos escasos).
# El resto es redundante con Dest o con DestState.
drop_destino <- setdiff(vars_destino, "DestState")

## Redundantes o sin información ----
# Quarter está determinada por Month; Flights vale 1 siempre; DistanceGroup deriva de Distance.
# Tail_Number: no se usa sin acreditar su disponibilidad en el escenario (ficha del caso).
# "X": columna vacía final típica de los CSV de BTS (no existe en este paquete).
# DepTimeBlk y ArrTimeBlk: bloques de la hora programada, redundantes con CRSDepTime/CRSArrTime
# (que se pasan a minutos en variables.R). Quítalas de aquí si prefieres conservarlas.
drop_redundantes <- c("Quarter", "Flights", "DistanceGroup", "Tail_Number", "X",
                      "DepTimeBlk", "ArrTimeBlk")

## Información posterior al origen de la predicción ----
drop_posteriores <- c(
  "DepTime", "WheelsOff", "WheelsOn", "ArrTime",
  "ActualElapsedTime", "AirTime", "TaxiOut", "TaxiIn",
  "DepDelay", "DepDelayMinutes", "DepDel15", "DepartureDelayGroups",
  "ArrDelay", "ArrDelayMinutes", "ArrDel15", "ArrivalDelayGroups",
  "Cancelled", "CancellationCode", "Diverted",
  "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay",
  "FirstDepTime", "TotalAddGTime", "LongestAddGTime"
)

# Todas las columnas de vuelos desviados (empiezan por "Div", sin distinguir mayúsculas)
drop_div <- grep("^div", names(vuelos_df), ignore.case = TRUE, value = TRUE)

## Lista final ----
predictoras_drop <- unique(c(drop_aerolinea, drop_origen, drop_destino,
                             drop_redundantes, drop_posteriores, drop_div))

# Nombres que no existen en la base (typos o columnas ausentes en este paquete)
faltantes <- setdiff(predictoras_drop, names(vuelos_df))
if (length(faltantes) > 0) {
  message("Aviso: estas variables de predictoras_drop no están en la base: ",
          paste(faltantes, collapse = ", "))
}
predictoras_drop <- intersect(predictoras_drop, names(vuelos_df))

## Descripción de cada variable excluida ----
for (v in predictoras_drop) {
  desc <- ver_desc(v)
  cat("Variable:", v, "\n")
  cat("Descripción:", if (length(desc) == 0) "(no está en el diccionario)" else desc[1], "\n")
  cat("--------------------------------------------------\n")
}

rm(vars_aerolinea, vars_origen, vars_destino,
   drop_aerolinea, drop_origen, drop_destino, drop_redundantes,
   drop_posteriores, drop_div, faltantes)

