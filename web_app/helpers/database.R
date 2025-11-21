# Gestion des connexions à la base de données avec pool
library(pool)
library(DBI)
library(RPostgres)

# Configuration de la connexion
create_db_pool <- function() {
  pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = "8746146walid_bd",
    host = "postgresql-8746146walid.alwaysdata.net",
    port = 5432,
    user = "8746146walid",
    password = "Djamila78%",
    minSize = 1,
    maxSize = 10,  # Limite le nombre de connexions simultanées
    idleTimeout = 300000  # 5 minutes
  )
}

# Fonctions de requêtes
get_airlines <- function(pool) {
  dbGetQuery(pool, "SELECT * FROM airlines")
}



get_airports <- function(pool) {
  dbGetQuery(pool, "SELECT * FROM airports")
}


get_flights_data <- function(pool, start_date, end_date, airport = NULL, airline = NULL) {
  
  # Normalisation des filtres
  if (is.null(airport) || airport %in% c("", "Tous", "ALL", "NA")) {
    airport <- NULL
  }
  if (is.null(airline) || airline %in% c("", "Toutes", "ALL", "NA")) {
    airline <- NULL
  }
  
  query <- "
    SELECT f.*, a.name as airline_name, ap.name as airport_name, ap.faa,
           w.temp, w.humid, w.wind_speed, w.precip
    FROM flights f
    LEFT JOIN airlines a ON f.carrier = a.carrier
    LEFT JOIN airports ap ON f.origin = ap.faa
    LEFT JOIN weather w ON f.origin = w.origin AND f.time_hour = w.time_hour
    WHERE f.time_hour BETWEEN $1 AND $2
  "
  
  params <- list(start_date, end_date)
  param_index <- 3
  
  # Filtre aéroport
  if (!is.null(airport)) {
    query <- paste0(query, " AND f.origin = $", param_index)
    params[[param_index]] <- airport
    param_index <- param_index + 1
  }
  
  # Filtre airline
  if (!is.null(airline)) {
    query <- paste0(query, " AND f.carrier = $", param_index)
    params[[param_index]] <- airline
    param_index <- param_index + 1
  }
  
  dbGetQuery(pool, query, params = params)
}


get_date_range <- function(pool) {
  query <- "
    SELECT 
      MIN(time_hour) AS start_date,
      MAX(time_hour) AS end_date
    FROM flights
  "
  dbGetQuery(pool, query)
}


