library(shiny)
library(DT)           # For interactive tables
library(shinyjs)      # For toggling table visibility
library(bslib)        # For theming (e.g., Minty)
library(ggplot2)      # For plotting
library(dplyr)        # For data wrangling
library(tidyr)        # For data reshaping (pivot_longer etc.)






server <- function(input, output, session) {
  
  # Load the data (or process as needed)
  dat_cleaned <- readRDS("dat_cleaned.rds")
  

  
  # Data preview for introduction (to show the first few rows of the data)
  output$data_preview <- renderPrint({
    head(dat_cleaned)  # Display first few rows of the cleaned data
  })
  
  # Data Table for cleaned data (shown in the Data Cleaning tab)
  output$cleaned_table <- DT::renderDataTable({
    dat_cleaned
  })
  
  # Define models (these are GLMs based on the cleaned data)
  mod_1 <- glm(concentration ~ absorbance, data = dat_cleaned)
  mod_2 <- glm(concentration ~ concentration * absorbance, data = dat_cleaned)
  mod_3 <- glm(cell_count ~ absorbance, data = dat_cleaned)
  
  model_lookup <- list(mod_1 = mod_1, mod_2 = mod_2, mod_3 = mod_3)
  
  # Reactive model selection (based on the input from the UI)
  selected_model <- reactive({
    model_lookup[[input$model_choice]]
  })
  
  # Output for Model Summary (shown in the Model Dashboard tab)
  output$model_summary <- renderPrint({
    summary(selected_model())
  })
  
  # Output for Actual vs Predicted Plot (shown in the Model Dashboard tab)
  output$prediction_plot <- renderPlot({
    model <- selected_model()
    preds <- predict(model)
    actual <- model$y
    
    ggplot(data.frame(actual = actual, predicted = preds),
           aes(x = actual, y = predicted)) +
      geom_point(alpha = 0.6, color = "#2c3e50") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted") +
      theme_minimal()
  })
  
  # Output for Model Metrics (shown in the Model Dashboard tab)
  output$model_metrics <- renderPrint({
    model <- selected_model()
    preds <- predict(model)
    actual <- model$y
    rmse <- sqrt(mean((actual - preds)^2))
    r2 <- cor(actual, preds)^2
    
    cat("RMSE:", round(rmse, 3), "\n")
    cat("R²:", round(r2, 3), "\n")
  })
  
  # Visualization Plot (shown in the Visualization tab)
  output$visualize_plot <- renderPlot({
    dat_cleaned %>% 
      ggplot(aes(x = concentration, y = absorbance, color = reagent)) + 
      geom_point() + 
      facet_wrap(~reagent)
  })
  
  # Visualization Caption (shown in the Visualization tab)
  output$visualize_caption <- renderText({
    "Here is a visual representation of the concentration vs absorbance data, broken down by reagent."
  })
}

