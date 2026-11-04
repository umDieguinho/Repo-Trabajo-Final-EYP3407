## Análisis predictoras -----
#### Variables correlacionadas ----
predictoras_drop <- c()
####### Aerolinea ----
vars_id_aerolinea <- vuelos_df %>%
  select(
    Reporting_Airline,
    DOT_ID_Reporting_Airline,
    IATA_CODE_Reporting_Airline,
    Flight_Number_Reporting_Airline
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

# Además, hay que eliminar las que empiezan por div (son vuelos desviados)

rm(vars_destino, vars_id_aerolinea, vars_origen)
