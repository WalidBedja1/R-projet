delaysUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Évolution des Retards", status = "warning", solidHeader = TRUE,
        plotlyOutput(ns("delays_plot")), width = 12
      )
    ),
    fluidRow(
      box(
        title = "Statistiques des Retards", status = "info",
        valueBoxOutput(ns("avg_delay"), width = 3),
        valueBoxOutput(ns("max_delay"), width = 3),
        valueBoxOutput(ns("delayed_flights"), width = 3),
        valueBoxOutput(ns("worst_airline"), width = 3),
        width = 12
      )
    )
  )
}

delaysServer <- function(id, pool, filters) {
  moduleServer(id, function(input, output, session) {
    
    delays_data <- reactive({
      req(filters$dates())

      data <- get_flights_data(
        pool, 
        filters$dates()[1], 
        filters$dates()[2],
        filters$airport(),
        filters$airline()
      )

      data %>%
        mutate(
          date = as.Date(time_hour),
          # has_delay: gérer correctement les NA
          has_delay = as.integer((!is.na(dep_delay) & dep_delay > 15) | (!is.na(arr_delay) & arr_delay > 15)),
          # total_delay: NA si les deux sont NA, sinon le max disponible
          total_delay = dplyr::case_when(
            !is.na(dep_delay) | !is.na(arr_delay) ~ pmax(dep_delay, arr_delay, na.rm = TRUE),
            TRUE ~ as.numeric(NA)
          )
        ) %>%
        group_by(date, origin, carrier) %>%
        summarise(
          avg_delay = if(all(is.na(total_delay))) NA_real_ else mean(total_delay, na.rm = TRUE),
          delayed_flights = sum(has_delay, na.rm = TRUE),
          total_flights = n(),
          .groups = 'drop'
        ) %>%
        mutate(delay_rate = ifelse(total_flights > 0, delayed_flights / total_flights * 100, NA_real_))
    })
    
    output$delays_plot <- renderPlotly({
      data <- delays_data()
      req(nrow(data) > 0)
      
      trend_data <- data %>%
        group_by(date, origin) %>%
        summarise(avg_delay = mean(avg_delay, na.rm = TRUE), .groups = 'drop')

      p <- ggplot(trend_data, aes(x = date, y = avg_delay, color = origin, group = origin)) +
        geom_line() + geom_point() +
        labs(
          title = "Évolution des Retards Moyens (minutes)",
          x = "Date",
          y = "Retard Moyen (min)",
          color = "Aéroport"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")

      ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0, y = -0.2))
    })
    
    output$avg_delay <- renderValueBox({
      data <- delays_data()
      avg <- mean(data$avg_delay, na.rm = TRUE)
      
      valueBox(
        paste(round(avg, 1), "min"),
        "Retard Moyen",
        icon = icon("clock"),
        color = "orange"
      )
    })
    
    output$max_delay <- renderValueBox({
      data <- delays_data()
      max_delay <- max(data$avg_delay, na.rm = TRUE)
      
      valueBox(
        paste(round(max_delay, 1), "min"),
        "Retard Maximum",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    })
    
    output$delayed_flights <- renderValueBox({
      data <- delays_data()
      rate <- mean(data$delay_rate, na.rm = TRUE)
      
      valueBox(
        paste(round(rate, 1), "%"),
        "Taux de Retard",
        icon = icon("percent"),
        color = "yellow"
      )
    })
  })
}