forecastsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Prévisions des Retards", status = "success", solidHeader = TRUE,
        plotlyOutput(ns("forecast_plot")), width = 6
      ),
      box(
        title = "Importance des Variables", status = "info", solidHeader = TRUE,
        plotOutput(ns("importance_plot")), width = 6
      )
    ),
    fluidRow(
      box(
        title = "Paramètres du Modèle", status = "primary",
        selectInput(ns("model_type"), "Type de Modèle:",
                    choices = c("Régression" = "lm", "Arbre de Décision" = "rpart", "Random Forest" = "rf")),
        actionButton(ns("train_model"), "Entraîner le Modèle"),
        width = 12
      )
    )
  )
}

forecastsServer <- function(id, pool, filters) {
  moduleServer(id, function(input, output, session) {
    
    # Modèle prédictif
    delay_model <- reactiveVal(NULL)
    importance_data <- reactiveVal(NULL)
    
    # Données pour l'entraînement
    training_data <- reactive({
      req(filters$dates(), filters$airport(), filters$airline())
      
      data <- get_flights_data(
        pool, 
        filters$dates()[1], 
        filters$dates()[2],
        filters$airport(),
        filters$airline()
      )
      
      # Préparation des données
      data %>%
        mutate(
          target_delay = pmax(dep_delay, arr_delay, na.rm = TRUE),
          day_of_week = lubridate::wday(time_hour),
          is_weekend = ifelse(day_of_week %in% c(1, 7), 1, 0),
          hour_factor = as.factor(hour)
        ) %>%
        filter(!is.na(target_delay), !is.na(temp)) %>%
        select(target_delay, temp, humid, wind_speed, precip, hour, day_of_week, is_weekend, month)
    })
    
    # Entraînement du modèle
    observeEvent(input$train_model, {
      req(training_data())
      data <- training_data()
      
      withProgress(message = 'Entraînement du modèle...', value = 0.5, {
        if (input$model_type == "lm") {
          # Régression linéaire
          model <- lm(target_delay ~ temp + humid + wind_speed + precip + hour + is_weekend + month, 
                      data = data)
          delay_model(model)
          
          # Importance des variables
          coefs <- summary(model)$coefficients[-1, ]
          importance <- data.frame(
            variable = rownames(coefs),
            importance = abs(coefs[, "Estimate"]),
            stringsAsFactors = FALSE
          )
          importance_data(importance)
          
        } else if (input$model_type == "rpart") {
          # Arbre de décision
          library(rpart)
          model <- rpart(target_delay ~ temp + humid + wind_speed + precip + hour + is_weekend + month, 
                         data = data)
          delay_model(model)
          
          # Importance des variables
          imp <- model$variable.importance
          importance <- data.frame(
            variable = names(imp),
            importance = as.numeric(imp),
            stringsAsFactors = FALSE
          )
          importance_data(importance)
          
        } else if (input$model_type == "rf") {
          # Random Forest (version simplifiée)
          library(randomForest)
          set.seed(123)
          model <- randomForest(target_delay ~ temp + humid + wind_speed + precip + hour + is_weekend + month, 
                                data = data, ntree = 50, importance = TRUE)
          delay_model(model)
          
          # Importance des variables
          imp <- importance(model)
          importance <- data.frame(
            variable = rownames(imp),
            importance = imp[, "%IncMSE"],
            stringsAsFactors = FALSE
          )
          importance_data(importance)
        }
        
        incProgress(1, message = "Modèle entraîné!")
      })
    })
    
    output$forecast_plot <- renderPlotly({
      req(delay_model())
      model <- delay_model()
      data <- training_data()
      
      predictions <- predict(model, newdata = data)
      
      plot_data <- data.frame(
        actual = data$target_delay[1:100],  # Premier 100 points
        predicted = predictions[1:100],
        index = 1:100
      )
      
      p <- ggplot(plot_data, aes(x = index)) +
        geom_line(aes(y = actual, color = "Réel"), size = 1) +
        geom_line(aes(y = predicted, color = "Prédit"), size = 1, linetype = "dashed") +
        labs(
          title = "Comparaison Réel vs Prédit",
          x = "Observation",
          y = "Retard (min)",
          color = "Légende"
        ) +
        scale_color_manual(values = c("Réel" = "blue", "Prédit" = "red")) +
        theme_minimal()
      
      ggplotly(p)
    })
    
    output$importance_plot <- renderPlot({
      req(importance_data())
      importance <- importance_data()
      
      ggplot(importance, aes(x = reorder(variable, importance), y = importance)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(
          title = "Importance des Variables dans la Prédiction",
          x = "Variables",
          y = "Importance"
        ) +
        theme_minimal()
    })
  })
}