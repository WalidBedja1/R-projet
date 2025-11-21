cancellationsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Évolution des Annulations", status = "danger", solidHeader = TRUE,
        plotlyOutput(ns("cancellations_trend_plot")), width = 6
      ),
      box(
        title = "Taux d'Annulation par Compagnie", status = "info", solidHeader = TRUE,
        plotlyOutput(ns("airline_cancellations_plot")), width = 6
      )
    ),
    fluidRow(
      box(
        title = "Détail des Annulations (Top 20)", status = "info",
        tableOutput(ns("cancellations_table")),
        width = 6
      ),
      box(
        title = "Statistiques des Annulations", status = "primary",
        valueBoxOutput(ns("total_cancellations"), width = 12),
        valueBoxOutput(ns("cancellation_rate"), width = 12),
        width = 6
      )
    )
  )
}

cancellationsServer <- function(id, pool, filters) {
  moduleServer(id, function(input, output, session) {
    
    cancellations_data <- reactive({
      req(filters$dates())

      data <- get_flights_data(
        pool, 
        filters$dates()[1], 
        filters$dates()[2],
        filters$airport(),
        filters$airline()
      )

      # S'assurer que les colonnes attendues existent pour éviter des erreurs
      expected_cols <- c("dep_delay", "arr_delay", "cancelled", "weather_delay", "carrier_delay", "security_delay", "time_hour", "origin", "carrier")
      for(col in expected_cols) if(!(col %in% names(data))) data[[col]] <- NA

      # Normaliser cancelled en 0/1 de façon robuste (numeric/logical/character)
      normalize_cancelled <- function(x) {
        if(is.logical(x)) return(as.integer(x))
        if(is.numeric(x)) return(as.integer(x != 0))
        if(is.character(x)) {
          x_l <- tolower(x)
          return(as.integer(x_l %in% c('t','true','1','y','yes')))
        }
        # fallback
        return(as.integer(ifelse(is.na(x), NA, as.integer(x))))
      }
      data$cancelled <- normalize_cancelled(data$cancelled)

      # If cancelled column is entirely NA or zero, infer cancellations from dep_time/arr_time
      if(all(is.na(data$cancelled)) || sum(data$cancelled, na.rm = TRUE) == 0) {
        data$cancelled <- as.integer(is.na(data$dep_time) | is.na(data$arr_time) | (is.na(data$dep_delay) & is.na(data$arr_delay)))
        message('[cancellations] inferred cancelled from dep_time/arr_time/dep_delay; inferred_count=', sum(data$cancelled, na.rm=TRUE))
      } else {
        message('[cancellations] rows=', nrow(data), ' cols=', paste(names(data), collapse=','), ' cancelled_count=', sum(data$cancelled, na.rm=TRUE))
      }

      # Traitement des données d'annulation
      data %>%
        mutate(
          date = as.Date(time_hour),
          cancellation_reason = case_when(
            cancelled == 1 & is.na(dep_delay) & is.na(arr_delay) ~ "Raison inconnue",
            cancelled == 1 & (!is.na(dep_delay) & dep_delay > 180 | !is.na(arr_delay) & arr_delay > 180) ~ "Retard excessif",
            cancelled == 1 & !is.na(weather_delay) & weather_delay > 60 ~ "Météo",
            cancelled == 1 & !is.na(carrier_delay) & carrier_delay > 60 ~ "Compagnie",
            cancelled == 1 & !is.na(security_delay) & security_delay > 30 ~ "Sécurité",
            cancelled == 1 ~ "Autre raison",
            TRUE ~ "Non annulé"
          )
        ) %>%
        filter(cancelled == 1) %>%  # Garder seulement les annulations
        group_by(date, origin, carrier, cancellation_reason) %>%
        summarise(
          cancellations_count = n(),
          .groups = 'drop'
        )
    })
    
    # Données complètes pour le calcul des taux
    full_data <- reactive({
      req(filters$dates())

      data <- get_flights_data(
        pool, 
        filters$dates()[1], 
        filters$dates()[2],
        filters$airport(),
        filters$airline()
      )

      # garantir colonnes attendues
      expected_cols_full <- c("dep_time","arr_time","dep_delay","arr_delay","cancelled","time_hour","carrier")
      for(col in expected_cols_full) if(!(col %in% names(data))) data[[col]] <- NA

      # normaliser cancelled (reuse small helper)
      normalize_cancelled2 <- function(x) {
        if(is.logical(x)) return(as.integer(x))
        if(is.numeric(x)) return(as.integer(x != 0))
        if(is.character(x)) {
          x_l <- tolower(x)
          return(as.integer(x_l %in% c('t','true','1','y','yes')))
        }
        return(as.integer(ifelse(is.na(x), NA, as.integer(x))))
      }
      data$cancelled <- normalize_cancelled2(data$cancelled)

      # infer cancelled if missing or all zeros
      if(all(is.na(data$cancelled)) || sum(data$cancelled, na.rm = TRUE) == 0) {
        data$cancelled <- as.integer(is.na(data$dep_time) | is.na(data$arr_time) | (is.na(data$dep_delay) & is.na(data$arr_delay)))
        message('[full_data] inferred cancelled_count=', sum(data$cancelled, na.rm = TRUE))
      }

      # carrier safe label
      data$carrier <- as.character(data$carrier)
      data$carrier[is.na(data$carrier) | data$carrier == ""] <- "Unknown"

      data %>% mutate(date = as.Date(time_hour))
    })
    
    # Plot 1: Tendances des annulations dans le temps
    output$cancellations_trend_plot <- renderPlotly({
      data <- cancellations_data()
      req(nrow(data) > 0)
      
      trend_data <- data %>%
        group_by(date, origin) %>%
        summarise(daily_cancellations = sum(cancellations_count), .groups = 'drop')

      p <- ggplot(trend_data, aes(x = date, y = daily_cancellations, color = origin, group = origin)) +
        geom_line(size = 1) +
        geom_point(size = 1.5) +
        labs(
          title = "Évolution Quotidienne des Annulations",
          x = "Date",
          y = "Nombre d'Annulations",
          color = "Aéroport"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")

      ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0, y = -0.2))
    })
    
    # Plot 2: Répartition des raisons d'annulation
    output$cancellations_reasons_plot <- renderPlotly({
      data <- cancellations_data()
      req(nrow(data) > 0)
      reasons_data <- tryCatch({
        data %>%
          group_by(cancellation_reason) %>%
          summarise(total_cancellations = sum(cancellations_count), .groups = 'drop') %>%
          arrange(desc(total_cancellations))
      }, error = function(e) {
        message('[cancellations_reasons_plot] summarise error: ', e$message)
        NULL
      })

      if(is.null(reasons_data) || nrow(reasons_data) == 0) {
        # return an empty safe plotly object to avoid JS errors
        return(plotly::plot_ly() %>% plotly::layout(title = 'Aucune donnée'))
      }

      p <- ggplot(reasons_data, aes(x = reorder(cancellation_reason, total_cancellations), 
                                    y = total_cancellations, 
                                    fill = cancellation_reason)) +
        geom_col() +
        coord_flip() +
        labs(
          title = "Répartition par Raison d'Annulation",
          x = "Raison d'Annulation",
          y = "Nombre d'Annulations"
        ) +
        theme_minimal() +
        theme(legend.position = "none")

      ggplotly(p)
    })
    
    # Plot 3: Taux d'annulation par compagnie
    output$airline_cancellations_plot <- renderPlotly({
      cancel_data <- cancellations_data()
      all_data <- full_data()
      req(nrow(all_data) > 0)
      # Calcul des taux par compagnie (protection contre division par zéro / données manquantes)
      airline_stats <- tryCatch({
        all_data %>%
          group_by(carrier) %>%
          summarise(
            total_flights = n(),
            cancelled_flights = sum(as.integer(cancelled), na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          mutate(
            cancellation_rate = ifelse(total_flights > 0, (cancelled_flights / total_flights) * 100, 0),
            airline_label = paste0(carrier, " (", cancelled_flights, "/", total_flights, ")")
          ) %>%
          arrange(desc(cancellation_rate))
      }, error = function(e) {
        message('[airline_cancellations_plot] summarise error: ', e$message)
        NULL
      })

      if(is.null(airline_stats) || nrow(airline_stats) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = 'Aucune donnée'))
      }

      p <- ggplot(airline_stats, aes(x = reorder(airline_label, cancellation_rate), 
                                     y = cancellation_rate,
                                     fill = cancellation_rate)) +
        geom_col() +
        coord_flip() +
        labs(
          title = "Taux d'Annulation par Compagnie",
          x = "Compagnie (annulés/total)",
          y = "Taux d'Annulation (%)"
        ) +
        scale_fill_gradient(low = "lightblue", high = "red") +
        theme_minimal() +
        theme(legend.position = "none")

      ggplotly(p)
    })
    
    # ValueBox 1: Total des annulations
    output$total_cancellations <- renderValueBox({
      data <- cancellations_data()
      total <- if(nrow(data) > 0) sum(data$cancellations_count) else 0
      
      valueBox(
        format(total, big.mark = ","),
        "Annulations Total",
        icon = icon("plane-slash"),
        color = "red"
      )
    })
    
    # ValueBox 2: Taux d'annulation global
    output$cancellation_rate <- renderValueBox({
      cancel_data <- cancellations_data()
      all_data <- full_data()
      
      total_flights <- nrow(all_data)
      total_cancellations <- if(nrow(cancel_data) > 0) sum(cancel_data$cancellations_count) else 0
      
      rate <- if(total_flights > 0) (total_cancellations / total_flights) * 100 else 0
      
      color <- if(rate > 10) "red" else if(rate > 5) "yellow" else "green"
      
      valueBox(
        paste0(round(rate, 1), "%"),
        "Taux d'Annulation Global",
        icon = icon("percent"),
        color = color
      )
    })
    
    # ValueBox 3: Compagnie avec le plus d'annulations
    output$worst_airline_cancel <- renderValueBox({
      data <- cancellations_data()
      req(nrow(data) > 0)
      
      worst_airline <- data %>%
        group_by(carrier) %>%
        summarise(total_cancellations = sum(cancellations_count), .groups = 'drop') %>%
        arrange(desc(total_cancellations)) %>%
        slice(1)
      
      valueBox(
        worst_airline$carrier,
        paste(worst_airline$total_cancellations, "annulations"),
        icon = icon("exclamation-triangle"),
        color = "maroon"
      )
    })
    
    # ValueBox 4: Raison principale d'annulation
    output$main_reason <- renderValueBox({
      data <- cancellations_data()
      req(nrow(data) > 0)
      
      main_reason <- data %>%
        group_by(cancellation_reason) %>%
        summarise(total = sum(cancellations_count), .groups = 'drop') %>%
        arrange(desc(total)) %>%
        slice(1)
      
      valueBox(
        main_reason$cancellation_reason,
        paste(main_reason$total, "vols"),
        icon = icon("question-circle"),
        color = "orange"
      )
    })
    
    # Table détaillée des annulations - VERSION CORRIGÉE
    output$cancellations_table <- renderTable({  # ← CHANGEMENT ICI : renderTable au lieu de renderDT
      data <- cancellations_data()
      req(nrow(data) > 0)
      
      summary_table <- data %>%
        group_by(Date = as.Date(date), Aéroport = origin, Compagnie = carrier) %>%
        summarise(
          `Nombre d'Annulations` = sum(cancellations_count),
          .groups = 'drop'
        ) %>%
        arrange(desc(`Nombre d'Annulations`), desc(Date)) %>%
        mutate(Date = format(as.Date(Date), "%d/%m/%Y")) %>%
        head(20)  # Limiter à 20 lignes pour l'affichage
      
      summary_table
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  })
}