# ============================
# ETL - Script R
# ============================

# ---- Packages ----
library(DBI)
library(RPostgres)
library(readxl)
library(rvest)
library(pdftools)
library(dplyr)
library(lubridate)
library(stringr)
library(jsonlite)

# ---- Configuration ----
data_dir <- "C:/Users/Admin/source/R/data"  

# ---- Connexion PostgreSQL ----
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "8746146walid_bd",
  host     = "postgresql-8746146walid.alwaysdata.net",
  port     = 5432,
  user     = "8746146walid",
  password = "Djamila78%"
)

dbListTables(con)  

# -----------------------------
# 1. AIRLINES (JSON)
# -----------------------------
read_airlines <- function(file_name) {
  path <- file.path(data_dir, file_name)
  if(!file.exists(path)) stop("Fichier JSON introuvable : ", path)
  
  message("Lecture du fichier JSON : ", path)
  airlines <- read_json(path, simplifyVector = TRUE)
  
  str(airlines)
  class(airlines)
  
  message("Airlines chargées : ", nrow(airlines), " lignes")
  return(airlines)
}

airlines <- tryCatch(read_airlines("airlines.json"), error = function(e) { stop(e) })

# -----------------------------
# 2. AIRPORTS (XLSX)
# -----------------------------
read_airports <- function(file_name) {
  path <- file.path(data_dir, file_name)
  message("Lecture du fichier Excel : ", path)
  airports <- read_excel(path) %>%
    select(-1) %>%
    mutate(
      faa = toupper(faa),
      lat = as.numeric(str_replace(lat, ",", ".")),
      lon = as.numeric(str_replace(lon, ",", "."))
    )
  message("Airports chargés : ", nrow(airports), " lignes")
  return(airports)
}

airports <- tryCatch(read_airports("airports.xlsx"), error = function(e) { stop(e) })

# -----------------------------
# 3. PLANES (HTML)
# -----------------------------
read_planes <- function(file_name) {
  path <- file.path(data_dir, file_name)
  message("Lecture du fichier HTML : ", path)
  planes_html <- read_html(path)
  planes <- planes_html %>%
    html_table(fill = TRUE) %>%
    .[[1]] %>%
    select(-1) %>%
    mutate(
      year = as.integer(year),
      engines = as.integer(engines),
      seats  = as.integer(seats),
      speed  = as.integer(speed)
    )
  message("Planes chargés : ", nrow(planes), " lignes")
  return(planes)
}

planes <- tryCatch(read_planes("planes.html"), error = function(e) { stop(e) })

# -----------------------------
# 4. WEATHER (PDF)
# -----------------------------
read_weather <- function(file_name) {
  path <- file.path(data_dir, file_name)
  message("Lecture du fichier PDF : ", path)
  raw_text <- pdf_text(path) %>% paste(collapse = "\n")
  lines <- str_split(raw_text, "\n")[[1]]
  lines <- lines[str_detect(lines, "^(EWR|JFK|LGA),")]
  
  weather <- read.csv(text = lines, header = FALSE)
  colnames(weather) <- c(
    "origin","year","month","day","hour","temp","dewp","humid","wind_dir",
    "wind_speed","wind_gust","precip","pressure","visib","time_hour"
  )
  
  weather <- weather %>%
    mutate(
      year = as.integer(year),
      month = as.integer(month),
      day = as.integer(day),
      hour = as.integer(hour),
      temp = as.numeric(temp),
      dewp = as.numeric(dewp),
      humid = as.numeric(humid),
      wind_dir = as.integer(wind_dir),
      wind_speed = as.numeric(wind_speed),
      wind_gust = as.numeric(wind_gust),
      precip = as.numeric(precip),
      pressure = as.numeric(pressure),
      visib = as.numeric(visib),
      time_hour = ymd_hms(time_hour)
    )
  message("Weather chargées : ", nrow(weather), " lignes")
  return(weather)
}

weather <- tryCatch(read_weather("weather.pdf"), error = function(e) { stop(e) })

# -----------------------------
# 5. FLIGHTS (XLSX)
# -----------------------------
read_flights <- function(file_name) {
  path <- file.path(data_dir, file_name)
  message("Lecture du fichier Excel : ", path)
  flights <- read_excel(path) %>%
    mutate(
      year = as.integer(year),
      month = as.integer(month),
      day = as.integer(day),
      hour = as.integer(hour),
      minute = as.integer(minute),
      time_hour = ymd_h(paste(year, month, day, hour))
    )
  message("Flights chargés : ", nrow(flights), " lignes")
  return(flights)
}

flights <- tryCatch(read_flights("flights.xlsx"), error = function(e) { stop(e) })

# -----------------------------
# 6. INSERT DATA DANS POSTGRES
# -----------------------------
insert_table <- function(con, table_name, df) {
  message("Insertion dans la table ", table_name, " ...")
  dbBegin(con)
  tryCatch({
    dbWriteTable(con, table_name, df, append = TRUE, row.names = FALSE)
    dbCommit(con)
    message("Insertion réussie : ", nrow(df), " lignes")
  }, error = function(e) {
    dbRollback(con)
    stop("Erreur insertion table ", table_name, " : ", e)
  })
}

insert_table(con, "airlines", airlines)
insert_table(con, "airports", airports)
insert_table(con, "planes", planes)
insert_table(con, "weather", weather)
insert_table(con, "flights", flights)

# -----------------------------
# 7. Déconnexion
# -----------------------------
dbDisconnect(con)
message("ETL terminé avec succès ***")

