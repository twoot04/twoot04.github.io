#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
  
library(tidyverse)
library(dplyr)
library(stringr)
library(easystats)
library(equatiomatic)
library(pander)
library(broom)
library(kableExtra)
library(rmdformats)
library(prettydoc)
library(hrbrthemes)
library(tint)
library(tufte)
library(DT)
library(shiny)
library(shinyjs)

# Load your data
dat <- read.csv("plate_reading_2.csv")

# Data cleaning steps
dat_1 <- dat %>%
  pivot_longer(cols = starts_with("cell_count"),
               names_to = "measurement",
               values_to = "cell_count")

dat_2 <- dat_1 %>% 
  pivot_longer(cols = c("standard", "standard.1", "water", "water.1", 
                        "ethanol", "ethanol.1", "hydrogen_peroxide", 
                        "hydrogen_peroxide.1"), 
               names_to = "reagent", 
               values_to = "absorbance")

dat_3 <- dat_2 %>% 
  pivot_longer(cols = c("Concentration", "Concentration.1", 
                        "Concentration.2", "Concentration.3", 
                        "Concentration.4", "Concentration.5"), 
               names_to = "concentration.1", 
               values_to = "concentration")

dat_cleaned <- dat_3 %>% 
  select("Well", 
         "cell_count", 
         "reagent", 
         "absorbance", 
         "concentration")

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs for interactivity
  
  # Button to toggle the table visibility
  actionButton("toggle_table", "Show/Hide Table"),
  
  # Placeholder for the table (will be rendered dynamically)
  uiOutput("table_output")
)

server <- function(input, output, session) {
  
  # Render the table dynamically when the button is clicked
  output$table_output <- renderUI({
    req(input$toggle_table)  # Ensure this triggers only after the button is clicked
    
    # Show the table only if the toggle button has been clicked an odd number of times
    if (input$toggle_table %% 2 == 1) {
      tagList(
        div(id = "table_div", 
            DT::dataTableOutput("table"))  # Interactive data table
      )
    }
  })
  
  # Render the data table
  output$table <- DT::renderDataTable({
    dat_cleaned  # Replace with your actual cleaned dataset
  })
}



ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs for interactivity
  
  # Button to toggle the table visibility
  actionButton("toggle_table", "Show/Hide Table"),
  
  # Placeholder for the table (will be rendered dynamically)
  uiOutput("table_output")
)

server <- function(input, output, session) {
  
  # Render the table dynamically when the button is clicked
  output$table_output <- renderUI({
    req(input$toggle_table)  # Ensure this triggers only after the button is clicked
    
    # Show the table only if the toggle button has been clicked an odd number of times
    if (input$toggle_table %% 2 == 1) {
      tagList(
        div(id = "table_div", 
            DT::dataTableOutput("table"))  # Interactive data table
      )
    }
  })
  
  # Render the data table
  output$table <- DT::renderDataTable({
    dat_cleaned  # Replace with your actual cleaned dataset
  })
}

dat_cleaned %>% 
  ggplot(aes(x = concentration, 
             y = absorbance, 
             color = reagent)) + 
  geom_point() +
  facet_wrap(~reagent)



mod_1 <- dat_cleaned %>% 
  glm(data = . , 
      formula = concentration ~ absorbance)

mod_2 <- dat_cleaned %>% 
  glm(data = . , 
      formula = concentration ~ concentration * absorbance)


mod_3 <-  dat_cleaned %>% 
  glm(data = . , 
      formula = cell_count ~ absorbance)


compare_performance(mod_1, mod_2, mod_3) %>% plot()





library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),  # Modern theme
  titlePanel("📊 Model Dashboard: Predicting from Absorbance"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select model
      selectInput("model_choice", "Choose a Model:",
                  choices = c("Model 1: concentration ~ absorbance" = "mod_1",
                              "Model 2: concentration ~ concentration * absorbance" = "mod_2",
                              "Model 3: cell_count ~ absorbance" = "mod_3"),
                  selected = "mod_1"),
      
      # Reagent filter dropdown
      selectInput("reagent_filter", "Filter by Reagent:",
                  choices = c("All", "Water", "Ethanol", "Hydrogen Peroxide"),
                  selected = "All"),
      
      # Checkboxes for log vs linear trendline toggle
      checkboxInput("log_trendline", "Add Logarithmic Trendline", value = FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Model Summary", 
                 h4("Model Summary"),
                 verbatimTextOutput("model_summary")),
        
        tabPanel("Actual vs Predicted Plot",
                 h4("Actual vs Predicted Plot"),
                 plotOutput("prediction_plot")),
        
        tabPanel("Model Metrics", 
                 h4("Model Metrics"),
                 verbatimTextOutput("model_metrics"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Fit models using dat_cleaned (assumes this is loaded into the environment already)
  mod_1 <- glm(concentration ~ absorbance, data = dat_cleaned)
  mod_2 <- glm(concentration ~ concentration * absorbance, data = dat_cleaned)
  mod_3 <- glm(cell_count ~ absorbance, data = dat_cleaned)
  
  model_lookup <- list(mod_1 = mod_1, mod_2 = mod_2, mod_3 = mod_3)
  
  # Reactive model selection
  selected_model <- reactive({
    model_lookup[[input$model_choice]]
  })
  
  # Summary
  output$model_summary <- renderPrint({
    summary(selected_model())
  })
  
  # Prediction Plot
  output$prediction_plot <- renderPlot({
    model <- selected_model()
    preds <- predict(model)
    actual <- model$y
    
    ggplot(data.frame(actual = actual, predicted = preds),
           aes(x = actual, y = predicted)) +
      geom_point(alpha = 0.6, color = "#2c3e50") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Actual vs Predicted",
           x = "Actual",
           y = "Predicted") +
      theme_minimal()
  })
  
  # Model Metrics
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

