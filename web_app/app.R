library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(lubridate)

# importation modules et helpers
source("C:/Users/Admin/source/R/web_app/helpers/database.R")
source("C:/Users/Admin/source/R/web_app/modules/traffic_module.R")
source("C:/Users/Admin/source/R/web_app/modules/delays_module.R")
source("C:/Users/Admin/source/R/web_app/modules/cancellations_module.R")
source("C:/Users/Admin/source/R/web_app/modules/forecasts_module.R")
source("C:/Users/Admin/source/R/web_app/modules/alerts_module.R")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Aérien", titleWidth = 250),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Trafic Passé", tabName = "traffic", icon = icon("chart-line")),
      menuItem("Retards", tabName = "delays", icon = icon("clock")),
      menuItem("Annulations", tabName = "cancellations", icon = icon("plane-slash")),
      menuItem("Prévisions", tabName = "forecasts", icon = icon("magic")),
      menuItem("Alertes", tabName = "alerts", icon = icon("bell")),
      
      # Filtres dynamiques
      div(style = "padding: 15px;",
            uiOutput("dynamic_date_filter"),
            selectInput("airport", "Aéroport:", choices = c("Tous" = "")),
            selectInput("airline", "Compagnie:", choices = c("Toutes" = "")),
          actionButton("apply_filters", "Appliquer les Filtres", class = "btn-primary")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "traffic", trafficUI("traffic")),
      tabItem(tabName = "delays", delaysUI("delays")),
      tabItem(tabName = "cancellations", cancellationsUI("cancellations")),
      tabItem(tabName = "forecasts", forecastsUI("forecasts")),
      tabItem(tabName = "alerts", alertsUI("alerts"))
    )
  )
)

# Server
server <- function(input, output, session) {
  pool <- create_db_pool()
  onStop(function() { poolClose(pool) })
  
  # Filtres dynamiques
  output$dynamic_date_filter <- renderUI({
    dr <- get_date_range(pool)
    dateRangeInput("dates", "Période:",
                   start = as.Date(dr$start_date),
                   end   = as.Date(dr$end_date))
  })
  
  # Mise à jour des compagnies
  observe({
    tryCatch({
      airlines <- get_airlines(pool)
      if(!is.null(airlines) && nrow(airlines) > 0) {
        choices <- setNames(airlines$carrier, airlines$name)
        updateSelectInput(session, "airline", choices = c("Toutes" = "", choices))
      }
    }, error = function(e) {
      # Ne pas faire planter l'app si la récupération échoue
      message("Unable to fetch airlines: ", e$message)
    })
  })

  # Mise à jour des aéroports
  observe({
    tryCatch({
      airports <- get_airports(pool)
      # Ne garder que EWR, JFK, LGA (s'ils existent dans la table)
      keep_codes <- c("EWR", "JFK", "LGA")
      if(!is.null(airports) && nrow(airports) > 0) {
        sel <- airports %>% filter(faa %in% keep_codes)
        if(nrow(sel) > 0) {
          ap_choices <- setNames(sel$faa, sel$name)
          updateSelectInput(session, "airport", choices = c("Tous" = "", ap_choices))
        } else {
          # fallback: garder seulement les codes demandés
          updateSelectInput(session, "airport", choices = c("Tous" = "", setNames(keep_codes, keep_codes)))
        }
      } else {
        updateSelectInput(session, "airport", choices = c("Tous" = "", setNames(keep_codes, keep_codes)))
      }
    }, error = function(e) {
      message("Unable to fetch airports: ", e$message)
    })
  })
  
  # Reactive filters
  filters <- reactiveValues(
    dates = reactive({ input$dates }),
    airport = reactive({ input$airport }),
    airline = reactive({ input$airline })
  )
  
  observeEvent(input$apply_filters, {
    filters$dates   <- reactive({ input$dates })
    filters$airport <- reactive({ input$airport })
    filters$airline <- reactive({ input$airline })
  })
  
  # Modules
  trafficServer("traffic", pool, filters)
  delaysServer("delays", pool, filters)
  cancellationsServer("cancellations", pool, filters)
  forecastsServer("forecasts", pool, filters)
  alertsServer("alerts", pool, filters)
}

# Run
shinyApp(ui, server)
