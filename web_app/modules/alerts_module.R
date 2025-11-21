alertsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Alertes en Temps Réel", status = "danger", solidHeader = TRUE,
        uiOutput(ns("alerts_list")), width = 12
      )
    ),
    fluidRow(
      box(
        title = "Seuils d'Alerte", status = "warning",
        numericInput(ns("delay_threshold"), "Seuil de Retard (min):", value = 60, min = 0),
        numericInput(ns("traffic_threshold"), "Seuil de Trafic (vols/jour):", value = 500, min = 0),
        numericInput(ns("cancel_threshold"), "Seuil d'Annulation (%):", value = 10, min = 0, max = 100),
        actionButton(ns("update_thresholds"), "Mettre à jour les Seuils"),
        width = 12
      )
    )
  )
}

alertsServer <- function(id, pool, filters) {
  moduleServer(id, function(input, output, session) {
    
    thresholds <- reactiveValues(
      delay = 60,
      traffic = 500,
      cancel = 10
    )
    
    observeEvent(input$update_thresholds, {
      thresholds$delay <- input$delay_threshold
      thresholds$traffic <- input$traffic_threshold
      thresholds$cancel <- input$cancel_threshold
    })
    
    alerts_data <- reactive({
      req(filters$dates(), filters$airport(), filters$airline())
      
      data <- get_flights_data(
        pool, 
        filters$dates()[1], 
        filters$dates()[2],
        filters$airport(),
        filters$airline()
      )
      
      # Calcul des métriques pour les alertes
      current_data <- data %>%
        filter(time_hour >= max(time_hour) - days(1))  # Dernières 24h
      
      list(
        high_delays = current_data %>%
          filter(pmax(dep_delay, arr_delay, na.rm = TRUE) > thresholds$delay) %>%
          nrow(),
        
        high_traffic = current_data %>%
          group_by(origin) %>%
          summarise(daily_flights = n()) %>%
          filter(daily_flights > thresholds$traffic),
        
        high_cancellations = current_data %>%
          summarise(cancel_rate = sum(cancelled) / n() * 100) %>%
          pull(cancel_rate)
      )
    })
    
    output$alerts_list <- renderUI({
      alerts <- alerts_data()
      alert_items <- list()
      
      # Alerte retards élevés
      if (alerts$high_delays > 0) {
        alert_items[[length(alert_items) + 1]] <- div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          strong("Retards Critiques: "),
          alerts$high_delays, "vols avec retard >", thresholds$delay, "minutes"
        )
      }
      
      # Alerte trafic élevé
      if (nrow(alerts$high_traffic) > 0) {
        for (i in 1:nrow(alerts$high_traffic)) {
          airport <- alerts$high_traffic$origin[i]
          flights <- alerts$high_traffic$daily_flights[i]
          
          alert_items[[length(alert_items) + 1]] <- div(
            class = "alert alert-warning",
            icon("users"),
            strong("Trafic Élevé: "),
            airport, "-", flights, "vols/jour (seuil:", thresholds$traffic, ")"
          )
        }
      }
      
      # Alerte annulations
      if (alerts$high_cancellations > thresholds$cancel) {
        alert_items[[length(alert_items) + 1]] <- div(
          class = "alert alert-info",
          icon("plane-slash"),
          strong("Taux d'Annulation Élevé: "),
          round(alerts$high_cancellations, 1), "% (seuil:", thresholds$cancel, "%)"
        )
      }
      
      # Aucune alerte
      if (length(alert_items) == 0) {
        alert_items[[1]] <- div(
          class = "alert alert-success",
          icon("check-circle"),
          strong("Aucune alerte - Situation normale")
        )
      }
      
      tagList(alert_items)
    })
  })
}