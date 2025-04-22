library(shiny)
library(DT)           # For interactive tables
library(shinyjs)      # For toggling table visibility
library(bslib)        # For theming (e.g., Minty)
library(ggplot2)      # For plotting
library(dplyr)        # For data wrangling
library(tidyr)        # For data reshaping (pivot_longer etc.)


library(shinyjs)


dat_cleaned <- readRDS("dat_cleaned.rds")

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "minty"),
  titlePanel("Final Project: Cell Death Data"),
  
  tabsetPanel(
    tabPanel("Collapsible Table",
             actionButton("toggle_table", "Show/Hide Table"),
             uiOutput("table_output")
    ),
    
    tabPanel("Model Dashboard",
             sidebarLayout(
               sidebarPanel(
                 selectInput("model_choice", "Choose a Model:",
                             choices = c("Model 1: concentration ~ absorbance" = "mod_1",
                                         "Model 2: concentration ~ concentration * absorbance" = "mod_2",
                                         "Model 3: cell_count ~ absorbance" = "mod_3"),
                             selected = "mod_1"),
                 selectInput("reagent_filter", "Filter by Reagent:",
                             choices = c("All", "Water", "Ethanol", "Hydrogen Peroxide"),
                             selected = "All"),
                 checkboxInput("log_trendline", "Add Logarithmic Trendline", value = FALSE)
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Model Summary", verbatimTextOutput("model_summary")),
                   tabPanel("Actual vs Predicted Plot", plotOutput("prediction_plot")),
                   tabPanel("Model Metrics", verbatimTextOutput("model_metrics"))
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Collapsible Table
  output$table_output <- renderUI({
    req(input$toggle_table)
    if (input$toggle_table %% 2 == 1) {
      DT::dataTableOutput("table")
    }
  })
  
  output$table <- DT::renderDataTable({
    dat_cleaned
  })
  
  # Models
  mod_1 <- glm(concentration ~ absorbance, data = dat_cleaned)
  mod_2 <- glm(concentration ~ concentration * absorbance, data = dat_cleaned)
  mod_3 <- glm(cell_count ~ absorbance, data = dat_cleaned)
  
  model_lookup <- list(mod_1 = mod_1, mod_2 = mod_2, mod_3 = mod_3)
  
  selected_model <- reactive({
    model_lookup[[input$model_choice]]
  })
  
  output$model_summary <- renderPrint({
    summary(selected_model())
  })
  
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
  
  output$model_metrics <- renderPrint({
    model <- selected_model()
    preds <- predict(model)
    actual <- model$y
    rmse <- sqrt(mean((actual - preds)^2))
    r2 <- cor(actual, preds)^2
    
    cat("RMSE:", round(rmse, 3), "\n")
    cat("R²:", round(r2, 3), "\n")
  })
}

shinyApp(ui = ui, server = server)
