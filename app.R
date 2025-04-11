# Main application file

# Source all necessary files
source("global.R")
source("utils/helper_utils.R")
source("utils/aws_utils.R")
source("utils/model_utils.R")
source("modules/ui_modules.R")
source("modules/server_modules.R")

# Create the UI
ui <- createDashboardUI()

# Server logic
server <- function(input, output, session) {
  # Shared reactive values across modules
  shared_data <- reactiveValues(
    forecasts = NULL,
    raw_data = NULL,
    ts_data = NULL,
    time_of_day = NULL,
    confidence_level = 95, # Changed from reactive to direct value
    dark_mode = FALSE, # Changed from reactive to direct value
    aws_configured = check_aws_config(),
    region = NULL # Will be updated when region changes
  )
  
  # Update region when it changes
  observe({
    shared_data$region <- input$region
  })
  
  # AWS Configuration status display
  output$aws_status <- renderUI({
    if (shared_data$aws_configured) {
      tags$div(icon("check-circle"), "AWS CLI Configured", style = "color: #00FF00;")
    } else {
      tags$div(icon("exclamation-triangle"), "AWS CLI Not Configured", style = "color: #FF6347;")
    }
  })
  
  # Initialize modules
  predictionTabServer("prediction", shared_data)
  calculatorTabServer("calculator", shared_data)
  # historyTabServer("history", shared_data)
  settingsTabServer("settings", shared_data)
  
  # Export functionality for tables and plots
  output$download_data <- downloadHandler(
    filename = function() {
      paste("spot-prediction-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (is.null(shared_data$forecasts)) {
        return(NULL)
      }
      
      # Get time of day from shared data
      time_of_day <- shared_data$time_of_day
      
      # Create comparison data
      hw_result <- get_forecast_result(shared_data$forecasts$hw, time_of_day)
      ar_result <- get_forecast_result(shared_data$forecasts$ar, time_of_day)
      sn_result <- get_forecast_result(shared_data$forecasts$sn, time_of_day)
      sar_result <- get_forecast_result(shared_data$forecasts$sar, time_of_day)
      
      comparison_data <- data.frame(
        Model = c("Holt-Winters", "ARIMA", "Seasonal Naive", "Seasonal ARIMA"),
        LowerLimit = c(hw_result["Lower Limit"], ar_result["Lower Limit"], 
                     sn_result["Lower Limit"], sar_result["Lower Limit"]),
        Mean = c(hw_result["Mean"], ar_result["Mean"], 
               sn_result["Mean"], sar_result["Mean"]),
        UpperLimit = c(hw_result["Upper Limit"], ar_result["Upper Limit"], 
                      sn_result["Upper Limit"], sar_result["Upper Limit"])
      )
      
      # Add metadata
      metadata <- data.frame(
        Parameter = c("Instance Type", "Availability Zone", "OS", "Time of Day", 
                     "Historical Days", "Confidence Level", "Export Date"),
        Value = c(input$instance, input$zone, input$os, time_of_day, 
                input$hist_days, paste0(shared_data$confidence_level, "%"), Sys.Date())
      )
      
      # Write to file
      write.csv(metadata, file)
      write.csv(comparison_data, file, append = TRUE)
    }
  )
}

# Create and run the Shiny app
shinyApp(ui = ui, server = server)