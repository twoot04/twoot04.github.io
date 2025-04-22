library(shiny)
library(shinyjs)
library(ggplot2)
library(DT)
library(bslib)

# Define the UI
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "minty"),
  titlePanel("Final Project: Cell Death Analysis"),
  
  # Introduction Section
  tabsetPanel(
    tabPanel("Introduction",
             h3("MTT Assay Overview"),
             p("This project explores the effects of sterilization reagents on COS-7 cells, which are cancerous monkey kidney cells. The chemicals used were water, ethanol, and hydrogen peroxide."),
             p("Water was used to ensure that it wasn't only the diluting of media that was causing cell death. Diluting media with water causes cells to not have as much nutrients, so some expected cell death occurs."),
             p("Ethanol and hydrogen peroxide are often used in the lab setting as sterilizing reagents, so we were curious to see what their effect on eukaryotic cells was."),
             p("The standard, which consists of the first two wells, is used to create a standard curve of 'cell death' to compare the results to. This is because the cell numbers in the ethanol and hydrogen peroxide conditions were all kept at 12,000 to see what the effect of the reagents was."),
             p("So, let’s take a look at the data!")
    ),
    
    # Cleaned Data Section
    tabPanel("Cleaned Data",
             h4("Cleaned Data"), 
             actionButton("toggle_table", "Show/Hide Table"),
             uiOutput("table_output")
    ),
    
    # Visualization Section
    tabPanel("Visualization",
             h5("Visualization of Cell Death Data"),
             p("Here we visualize the relationship between concentration and absorbance for each reagent. This visualization provides insight into how the different reagents affected the cell count."),
             plotOutput("viz_plot"),
             img(src = "image0 - 2024-11-13T132831.463 (1).jpeg", width = "30%", align = "center")
    ),
    
    # Model Dashboard Section
    tabPanel("Model Dashboard",
             sidebarLayout(
               sidebarPanel(
                 selectInput("model_choice", "Choose a Model:",
                             choices = c("Model 1: concentration ~ absorbance" = "mod_1",
                                         "Model 2: concentration ~ concentration * absorbance" = "mod_2",
                                         "Model 3: cell_count ~ absorbance" = "mod_3")),
                 selectInput("reagent_filter", "Filter by Reagent:",
                             choices = c("All", "Water", "Ethanol", "Hydrogen Peroxide")),
                 checkboxInput("log_trendline", "Add Logarithmic Trendline", FALSE)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Model Summary", verbatimTextOutput("model_summary")),
                   tabPanel("Actual vs Predicted Plot", plotOutput("prediction_plot")),
                   tabPanel("Model Metrics", verbatimTextOutput("model_metrics"))
                 )
               )
             )
    ),
    
    # Conclusion Section
    tabPanel("Conclusion",
             h4("Conclusion and Future Experiments"),
             p("Looking at the image, there is a nice gradient of purple where the less of a reagent was used, the darker it was. This indicates that more cells are present here because the mitochondria is converting formazan crystals to get that purple color. So, as a visual cue, the reagents kill off these cells."),
             p("Looking at these graphs, we can see that hydrogen peroxide will kill anything"))
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Load the data
  dat_cleaned <- read.csv("plate_reading_2_cleaned.csv")  # Adjust the path as needed
  
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
  
  # Visualization Plot
  output$viz_plot <- renderPlot({
    ggplot(dat_cleaned, aes(x = concentration, y = absorbance, color = reagent)) +
      geom_point() +
      facet_wrap(~ reagent)
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






rsconnect::deployApp()

