# ============================
# traffic_module.R (FIXÉ)
# ============================

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(lubridate)

trafficUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Séries Temporelles du Trafic",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("traffic_plot")),
        width = 12
      )
    ),
    fluidRow(
      box(
        title = "Statistiques du Trafic",
        status = "info",
        valueBoxOutput(ns("total_flights"), width = 3),
        valueBoxOutput(ns("avg_daily_flights"), width = 3),
        valueBoxOutput(ns("peak_day"), width = 3),
        valueBoxOutput(ns("busiest_airport"), width = 3),
        width = 12
      )
    )
  )
}

trafficServer <- function(id, pool, filters) {
  moduleServer(id, function(input, output, session) {
    
    # ⚠️ IMPORTANT : on ne fait req() QUE sur les dates
    traffic_data <- reactive({
      req(filters$dates())  # pas de req sur airport ni airline
      
      data <- get_flights_data(
        pool,
        filters$dates()[1],
        filters$dates()[2],
        filters$airport(),      # peut être ""
        filters$airline()       # peut être ""
      )
      
      data %>%
        mutate(date = as.Date(time_hour)) %>%
        group_by(date, origin) %>%
        summarise(total_flights = n(), .groups = "drop")
    })
    
    output$traffic_plot <- renderPlotly({
      data <- traffic_data()
      req(nrow(data) > 0)
      
      p <- ggplot(data, aes(x = date, y = total_flights, color = origin, group = origin)) +
        geom_line() + geom_point() +
        labs(
          title = "Évolution du Trafic Aérien",
          x = "Date",
          y = "Nombre de Vols",
          color = "Aéroport"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")

      ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0, y = -0.2))
    })
    
    output$total_flights <- renderValueBox({
      total <- sum(traffic_data()$total_flights)
      valueBox(format(total, big.mark = ","), "Vols Totaux",
               icon = icon("plane"), color = "blue")
    })
    
    output$avg_daily_flights <- renderValueBox({
      avg <- mean(traffic_data()$total_flights)
      valueBox(round(avg, 1), "Moyenne Quotidienne",
               icon = icon("chart-line"), color = "green")
    })
    
    output$peak_day <- renderValueBox({
      peak <- traffic_data()[which.max(traffic_data()$total_flights), ]
      valueBox(
        peak$total_flights,
        paste("Pic le", format(peak$date, "%d/%m")),
        icon = icon("arrow-up"), color = "red"
      )
    })
    
  })
}
