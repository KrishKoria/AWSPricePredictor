library(shiny)
library(jsonlite)
library(sqldf)
library(forecast)
library(DT)
library(plotly)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(shinyWidgets)

# AWS credentials should be managed securely - options:
# 1. Use environment variables
# 2. Use AWS credential file (~/.aws/credentials)
# 3. Use instance profiles if running on EC2
# 4. Use AWS Secrets Manager

# Function to check if AWS CLI is configured
check_aws_config <- function() {
  result <- try(system("aws configure list", intern = TRUE), silent = TRUE)
  !inherits(result, "try-error")
}

# Date ranges with more flexibility
default_days_back <- 90
startTime <- paste(Sys.Date() - default_days_back, "T00:00:00.001", sep = "")
endTime <- paste(Sys.Date() - 1, "T23:59:59.999", sep = "")

# Improved function to filter spot price history
filterSpotPriceHistory <- function(SpotPriceHistory, zone, instance, os) {
  # Sort by timestamp
  SpotPriceHistory <- SpotPriceHistory[order(SpotPriceHistory$Timestamp), ]
  SpotPriceHistory$Date <- as.Date(SpotPriceHistory$Timestamp)
  
  # Extract hour from timestamp
  SpotPriceHistory$Hour <- as.numeric(format(as.POSIXct(SpotPriceHistory$Timestamp), "%H"))
  
  # Assign part of day (1=Morning, 2=Afternoon, 3=Evening, 4=Night)
  SpotPriceHistory$PartOfDay <- ifelse(5 <= SpotPriceHistory$Hour & SpotPriceHistory$Hour < 12, 1,
                                       ifelse(12 <= SpotPriceHistory$Hour & SpotPriceHistory$Hour < 17, 2,
                                              ifelse(17 <= SpotPriceHistory$Hour & SpotPriceHistory$Hour < 21, 3, 4)))
  
  # Get maximum price for each date and part of day
  result <- sqldf(paste("SELECT '", instance, "' AS InstanceType,
                '", zone, "' AS AvailabilityZone,
                '", os, "' AS ProductDescription,
                MAX(SpotPrice) AS SpotPrice, 
                Date, 
                PartOfDay FROM SpotPriceHistory
                WHERE Timestamp >='", startTime, "' AND Timestamp <='", endTime, "' 
                GROUP BY Date, PartOfDay", sep = ""))
  
  return(result)
}

# Calculate cost for a job duration
calculate_job_cost <- function(price, duration_hours) {
  return(price * duration_hours)
}

# Function to get all available AWS regions
get_aws_regions <- function() {
  result <- try(system("aws ec2 describe-regions --output json --query \"Regions[].RegionName\"", 
                     intern = TRUE), silent = TRUE)
  
  if (inherits(result, "try-error")) {
    # Return default regions if the command fails
    return(c("us-east-1", "us-east-2", "us-west-1", "us-west-2", "eu-west-1"))
  } else {
    # Parse the JSON result
    regions <- tryCatch({
      fromJSON(paste(result, collapse = ""))
    }, error = function(e) {
      return(c("us-east-1", "us-east-2", "us-west-1", "us-west-2", "eu-west-1"))
    })
    
    return(sort(regions))
  }
}

# Function to get availability zones for a region
get_availability_zones <- function(region) {
  # Check if region is a valid string
  if (!is.character(region) || length(region) != 1 || region == "Select") {
    return(c("Select a valid region first"))
  }
  
  # Make sure region is not a JSON query string
  if (grepl("\\[\\]", region)) {
    return(c("Invalid region format"))
  }
  
  result <- try(system(paste("aws ec2 describe-availability-zones --region", region, 
                           "--output json --query \"AvailabilityZones[].ZoneName\""), 
                     intern = TRUE), silent = TRUE)
  
  if (inherits(result, "try-error")) {
    # Return some default zones if the command fails
    return(paste0(region, c("a", "b", "c")))
  } else {
    # Parse the JSON result
    zones <- tryCatch({
      fromJSON(paste(result, collapse = ""))
    }, error = function(e) {
      return(paste0(region, c("a", "b", "c")))
    })
    
    return(sort(zones))
  }
}

# Function to get instance types
get_instance_types <- function() {
  # This is a simplified list - in production, you could fetch this from AWS API
  instance_families <- c("t3", "m5", "m6i", "c5", "c6i", "r5", "r6i", "p3", "p4d", "g4dn")
  sizes <- c("nano", "micro", "small", "medium", "large", "xlarge", "2xlarge", "4xlarge", "8xlarge", "16xlarge", "24xlarge")
  
  # Create combinations
  types <- c()
  for (family in instance_families) {
    for (size in sizes) {
      # Skip invalid combinations
      if ((family %in% c("p3", "p4d", "g4dn") && size %in% c("nano", "micro", "small", "medium"))) {
        next
      }
      types <- c(types, paste0(family, ".", size))
    }
  }
  
  # Add some specialty instances
  types <- c(types, "x1.16xlarge", "x1.32xlarge", "x2gd.metal")
  
  return(sort(types))
}

# UI: Dashboard style with improved layout
ui <- dashboardPage(
  dashboardHeader(title = "Spot The Spot - Enhanced"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Price Prediction", tabName = "prediction", icon = icon("chart-line")),
      menuItem("Cost Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("Spot History", tabName = "history", icon = icon("history")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    ),
    
    # AWS Configuration Status
    div(class = "sidebar-status",
        uiOutput("aws_status"),
        style = "padding: 10px; color: white;"
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tabItems(
      # First tab - Price Prediction
      tabItem(tabName = "prediction",
              fluidRow(
                box(
                  title = "Input Parameters", status = "primary", solidHeader = TRUE,
                  width = 4,
                  selectInput("region", "AWS Region", 
                              choices = c("Select" = "Select", 
                                          setNames(get_aws_regions(), get_aws_regions())), 
                              selected = "us-east-1"),
                  uiOutput("zone_selector"),
                  selectInput("instance", "Instance Type", choices = c("Select", get_instance_types()), selected = "Select"),
                  selectInput("os", "Operating System", choices = c("Select", "Linux/UNIX", "Windows", "SUSE Linux", "Red Hat Enterprise Linux", "Linux/UNIX (Amazon VPC)"), selected = "Linux/UNIX"),
                  prettyRadioButtons("predint", "Time of Day", 
                                     choices = c("Morning (5am-12pm)" = "Morning", 
                                                 "Afternoon (12pm-5pm)" = "Afternoon", 
                                                 "Evening (5pm-9pm)" = "Evening", 
                                                 "Night (9pm-5am)" = "Night"),
                                     selected = "Morning", inline = TRUE),
                  numericInput("hist_days", "Historical Data (days)", value = 90, min = 30, max = 180),
                  actionButton("getData", "Predict Spot Prices", class = "btn-success", icon = icon("play"))
                ),
                
                box(
                  title = "Predicted Price Range", status = "info", solidHeader = TRUE,
                  width = 8, height = 245,
                  tableOutput("comparison_table")
                )
              ),
              
              fluidRow(
                tabBox(
                  title = "Forecasting Models", width = 12,
                  tabPanel("Holt Winters", plotlyOutput("plot_hw"), tableOutput("forecasted_hw")),
                  tabPanel("ARIMA", plotlyOutput("plot_ar"), tableOutput("forecasted_ar")),
                  tabPanel("Seasonal Naive", plotlyOutput("plot_sn"), tableOutput("forecasted_sn")),
                  tabPanel("Seasonal ARIMA", plotlyOutput("plot_sar"), tableOutput("forecasted_sar")),
                  tabPanel("Error Comparison", plotOutput("comparison"))
                )
              )
      ),
      
      # Second tab - Cost Calculatorc
      tabItem(tabName = "calculator",
              fluidRow(
                box(
                  title = "Job Parameters", status = "primary", solidHeader = TRUE,
                  width = 4,
                  numericInput("job_duration", "Job Duration (hours)", value = 1, min = 0.1, max = 1000, step = 0.1),
                  numericInput("instance_count", "Number of Instances", value = 1, min = 1, max = 100),
                  numericInput("interruption_probability", "Interruption Probability (%)", value = 5, min = 0, max = 100)
                ),
                
                box(
                  title = "Cost Estimates", status = "success", solidHeader = TRUE,
                  width = 8,
                  tableOutput("cost_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Price Stability Analysis", status = "warning", solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("price_stability_plot")
                )
              )
      ),
      
      # Third tab - Spot History
      tabItem(tabName = "history",
              fluidRow(
                box(
                  title = "Spot Price History", status = "primary", solidHeader = TRUE,
                  width = 12,
                  dateRangeInput("date_range", "Date Range", 
                                 start = Sys.Date() - 30, end = Sys.Date() - 1),
                  actionButton("getHistory", "Fetch History", class = "btn-info", icon = icon("search")),
                  DTOutput("spot_history_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Historical Price Trends", status = "info", solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("history_plot")
                )
              )
      ),
      
      # Fourth tab - Settings
      tabItem(tabName = "settings",
              fluidRow(
                box(
                  title = "AWS Configuration", status = "danger", solidHeader = TRUE,
                  width = 6,
                  p("AWS credentials should be configured securely outside this application. Options include:"),
                  tags$ul(
                    tags$li("AWS CLI configuration (~/.aws/credentials)"),
                    tags$li("Environment variables (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY)"),
                    tags$li("Instance profiles (if running on EC2)"),
                    tags$li("AWS Secrets Manager")
                  ),
                  p("Avoid hardcoding credentials in your application code!"),
                  actionButton("check_config", "Check AWS Configuration", icon = icon("check"))
                ),
                
                box(
                  title = "Application Settings", status = "warning", solidHeader = TRUE,
                  width = 6,
                  sliderInput("confidence_level", "Prediction Confidence Level", min = 80, max = 99, value = 95, step = 1, post = "%"),
                  checkboxInput("dark_mode", "Dark Mode", value = FALSE),
                  actionButton("reset_settings", "Reset to Defaults", icon = icon("undo"))
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Dynamic UI elements
  output$zone_selector <- renderUI({
    if (input$region != "Select") {
      zones <- get_availability_zones(input$region)
      selectInput("zone", "Availability Zone", choices = c("Select", zones), selected = "Select")
    } else {
      selectInput("zone", "Availability Zone", choices = c("Select"), selected = "Select")
    }
  })
  
  # AWS Configuration check
  aws_configured <- reactiveVal(check_aws_config())
  
  observeEvent(input$check_config, {
    aws_configured(check_aws_config())
  })
  
  output$aws_status <- renderUI({
    if (aws_configured()) {
      tags$div(icon("check-circle"), "AWS CLI Configured", style = "color: #00FF00;")
    } else {
      tags$div(icon("exclamation-triangle"), "AWS CLI Not Configured", style = "color: #FF6347;")
    }
  })
  
  # Update date range when hist_days changes
  observeEvent(input$hist_days, {
    startTime <<- paste(Sys.Date() - input$hist_days, "T00:00:00.001", sep = "")
    endTime <<- paste(Sys.Date() - 1, "T23:59:59.999", sep = "")
  })
  
  # Check if all filters are properly selected
  checkFilters <- eventReactive(input$getData, {
    ifelse(input$instance == "Select" || 
             input$zone == "Select" || 
             input$os == "Select" || 
             input$predint == "Select", FALSE, TRUE)
  })
  
  # Fetch raw spot price data
  createData_1 <- reactive({
  if (!checkFilters()) return(NULL)
  
  # AWS CLI command to get spot price history
  command <- paste("aws ec2 describe-spot-price-history",
                   paste("--region", input$region),
                   paste("--availability-zone", input$zone),
                   paste("--instance-types", input$instance),
                   paste("--product-descriptions", shQuote(input$os)),
                   paste("--start-time", shQuote(startTime)),
                   paste("--end-time", shQuote(endTime)))
  
  result <- try(system(command, intern = TRUE), silent = TRUE)
  
  if (inherits(result, "try-error")) {
    showNotification("Error fetching spot price data. Check AWS configuration.", type = "error")
    return(NULL)
  }
  
  # Parse the JSON result with better error handling
  tryCatch({
    combined_json <- paste(result, collapse = "")
    
    # Check if response contains an error message
    if(grepl("error|usage:", combined_json, ignore.case = TRUE)) {
      showNotification("AWS CLI returned an error. Check your AWS configuration.", type = "error")
      return(NULL)
    }
    
    spot_data <- fromJSON(combined_json)$SpotPriceHistory
    
    # Check if we got meaningful data
    if(length(spot_data) == 0) {
      showNotification("No spot price data found for the selected criteria.", type = "warning")
      return(NULL)
    }
    
    return(spot_data)
  }, error = function(e) {
    showNotification(paste("Error parsing AWS response:", e$message), type = "error")
    return(NULL)
  })
})
  
  # Check if data is available
  checkData <- reactive({
    data <- createData_1()
    ifelse(checkFilters() && !is.null(data) && (length(data) > 0), TRUE, FALSE)
  })
  
  # Process the data for time series analysis
  createData <- reactive({
    if (!checkData()) return(NULL)
    
    # Filter and prepare the data
    SpotPriceHistory <- createData_1()
    SpotPriceHistory_Filtered <- filterSpotPriceHistory(SpotPriceHistory, input$zone, input$instance, input$os)
    
    # Add time index
    SpotPriceHistory_Filtered$Time <- seq.int(nrow(SpotPriceHistory_Filtered))
    
    # Create time series object
    SpotPrice_TS <- as.ts(SpotPriceHistory_Filtered[, -1:-3]) # Remove instance, zone, OS columns
    
    # Create time series with proper frequency (4 parts per day)
    ts(SpotPrice_TS[, -2:-4], start = c(as.numeric(format(SpotPriceHistory_Filtered$Date[1], "%j")), 1), frequency = 4)
  })
  
  # Store models and forecasts
  models <- reactiveValues(
    hw = NULL,
    ar = NULL,
    sn = NULL,
    sar = NULL
  )
  
  forecasts <- reactiveValues(
    hw = NULL,
    ar = NULL,
    sn = NULL,
    sar = NULL
  )
  
  # Build all models and forecasts when user clicks "Get Predicted Price Range"
  observeEvent(input$getData, {
  # Check if all necessary inputs are selected
  if (!checkFilters()) {
    showNotification("Please select all required parameters", type = "warning")
    return()
  }
  
  # Check if data is available
  if (!checkData()) {
    showNotification("No data available for the selected criteria", type = "warning")
    return()
  }
  
  # Set confidence level from input
  conf_level <- as.numeric(input$confidence_level)
  
  withProgress(message = 'Building forecasting models...', value = 0, {
    ts_data <- createData()
    
    # Holt-Winters model
    incProgress(0.1, detail = "Fitting Holt-Winters model")
    models$hw <- tryCatch({
      HoltWinters(ts_data)
    }, error = function(e) {
      showNotification(paste("Holt-Winters error:", e$message), type = "warning")
      NULL
    })
    forecasts$hw <- if (!is.null(models$hw)) forecast(models$hw, h = 4, level = conf_level) else NULL
    
    # ARIMA model
    incProgress(0.2, detail = "Fitting ARIMA model")
    models$ar <- tryCatch({
      auto.arima(ts_data)
    }, error = function(e) {
      showNotification(paste("ARIMA error:", e$message), type = "warning")
      NULL
    })
    forecasts$ar <- if (!is.null(models$ar)) forecast(models$ar, h = 4, level = conf_level) else NULL
    
    # Seasonal Naive model
    incProgress(0.3, detail = "Fitting Seasonal Naive model")
    models$sn <- tryCatch({
      snaive(ts_data, h = 4, level = conf_level)
    }, error = function(e) {
      showNotification(paste("Seasonal Naive error:", e$message), type = "warning")
      NULL
    })
    forecasts$sn <- models$sn  # For snaive, the model is the forecast
    
    # Seasonal ARIMA model
    incProgress(0.4, detail = "Fitting Seasonal ARIMA model")
    models$sar <- tryCatch({
      arima(ts_data, order = c(3, 0, 0), seasonal = c(0, 0, 1))
    }, error = function(e) {
      tryCatch({
        arima(ts_data, order = c(1, 0, 0), seasonal = c(0, 0, 1))
      }, error = function(e) {
        showNotification(paste("Seasonal ARIMA error:", e$message), type = "warning")
        NULL
      })
    })
    forecasts$sar <- if (!is.null(models$sar)) forecast(models$sar, h = 4, level = conf_level) else NULL
    
    incProgress(1, detail = "Done!")
  })
})
  
  # Helper function to get forecast results based on time of day
  get_forecast_result <- function(forecast_obj, time_of_day) {
  if (is.null(forecast_obj)) return(NULL)
  
  index <- switch(time_of_day,
                  "Morning" = 1,
                  "Afternoon" = 2,
                  "Evening" = 3,
                  "Night" = 4)
  
  # Handle both vector and matrix cases for lower/upper
  if (is.matrix(forecast_obj$lower)) {
    CI <- c(
      "Lower Limit" = forecast_obj$lower[index, 1],
      "Mean" = forecast_obj$mean[index],
      "Upper Limit" = forecast_obj$upper[index, 1]
    )
  } else {
    CI <- c(
      "Lower Limit" = forecast_obj$lower[index],
      "Mean" = forecast_obj$mean[index],
      "Upper Limit" = forecast_obj$upper[index]
    )
  }
  
  return(CI)
}
  
  # Extract forecast results for each model
  hw_final_Result <- reactive({
    get_forecast_result(forecasts$hw, input$predint)
  })
  
  ar_final_Result <- reactive({
    get_forecast_result(forecasts$ar, input$predint)
  })
  
  sn_final_Result <- reactive({
    get_forecast_result(forecasts$sn, input$predint)
  })
  
  sar_final_Result <- reactive({
    get_forecast_result(forecasts$sar, input$predint)
  })
  
  # Generate comparison table of all models
  output$comparison_table <- renderTable({
  if (!checkData() || is.null(forecasts$hw)) return(NULL)
  
  # Get results from all models
  hw_result <- hw_final_Result()
  ar_result <- ar_final_Result()
  sn_result <- sn_final_Result()
  sar_result <- sar_final_Result()
  
  # Calculate accuracies - use accuracy on forecast objects, not model objects
  hw_rmse <- if (!is.null(forecasts$hw)) accuracy(forecasts$hw)[1, "RMSE"] else NA
  ar_rmse <- if (!is.null(forecasts$ar)) accuracy(forecasts$ar)[1, "RMSE"] else NA
  sn_rmse <- if (!is.null(forecasts$sn)) accuracy(forecasts$sn)[1, "RMSE"] else NA
  sar_rmse <- if (!is.null(forecasts$sar)) accuracy(forecasts$sar)[1, "RMSE"] else NA
  
  # Calculate weighted average based on inverse RMSE (more weight to more accurate models)
  weights <- c(1/hw_rmse, 1/ar_rmse, 1/sn_rmse, 1/sar_rmse)
  weights <- weights / sum(weights, na.rm = TRUE)  # Add na.rm=TRUE to handle NA values
  
  ensemble_mean <- sum(c(hw_result["Mean"], ar_result["Mean"], sn_result["Mean"], sar_result["Mean"]) * weights, na.rm = TRUE)
  ensemble_lower <- min(c(hw_result["Lower Limit"], ar_result["Lower Limit"], sn_result["Lower Limit"], sar_result["Lower Limit"]), na.rm = TRUE)
  ensemble_upper <- max(c(hw_result["Upper Limit"], ar_result["Upper Limit"], sn_result["Upper Limit"], sar_result["Upper Limit"]), na.rm = TRUE)
  
  # Create comparison table
  comparison_data <- data.frame(
    Model = c("Holt-Winters", "ARIMA", "Seasonal Naive", "Seasonal ARIMA", "Ensemble (Weighted)"),
    "Lower Limit" = c(hw_result["Lower Limit"], ar_result["Lower Limit"], sn_result["Lower Limit"], sar_result["Lower Limit"], ensemble_lower),
    "Mean" = c(hw_result["Mean"], ar_result["Mean"], sn_result["Mean"], sar_result["Mean"], ensemble_mean),
    "Upper Limit" = c(hw_result["Upper Limit"], ar_result["Upper Limit"], sn_result["Upper Limit"], sar_result["Upper Limit"], ensemble_upper),
    "Error (RMSE)" = c(hw_rmse, ar_rmse, sn_rmse, sar_rmse, NA)
  )
  
  comparison_data[, 2:4] <- round(comparison_data[, 2:4], 5)
  comparison_data[, 5] <- round(comparison_data[, 5], 6)
  
  return(comparison_data)
}, digits = 6)
  
  # No data message
  output$nodata <- renderText({
    if (checkFilters() & !checkData()) {
      "No data available for the selected criteria. Cannot predict!"
    } else {
      ""
    }
  })
  
  # Generate interactive plots for each model
  output$plot_hw <- renderPlotly({
  if (!checkData() || is.null(forecasts$hw)) return(NULL)
  
  # Create a basic plotly chart instead of using autoplot
  # Get original data
  orig_data <- createData()
  orig_time <- time(orig_data)
  orig_values <- as.numeric(orig_data)
  
  # Get forecast data
  forecast_time <- time(forecasts$hw$mean)
  mean_values <- as.numeric(forecasts$hw$mean)
  
  # Create lower/upper bound vectors
  lower_values <- if(is.matrix(forecasts$hw$lower)) forecasts$hw$lower[,1] else forecasts$hw$lower
  upper_values <- if(is.matrix(forecasts$hw$upper)) forecasts$hw$upper[,1] else forecasts$hw$upper
  
  # Create the plot
  p <- plot_ly() %>%
    # Add original data
    add_trace(x = orig_time, y = orig_values, 
              type = "scatter", mode = "lines", name = "Historical",
              line = list(color = 'black')) %>%
    # Add forecast
    add_trace(x = forecast_time, y = mean_values, 
              type = "scatter", mode = "lines", name = "Forecast",
              line = list(color = 'blue')) %>%
    # Add confidence interval
    add_ribbons(x = forecast_time, 
                ymin = lower_values, 
                ymax = upper_values,
                name = paste0(input$confidence_level, "% CI"),
                fillcolor = 'rgba(0, 0, 255, 0.2)',
                line = list(color = 'rgba(0, 0, 255, 0)')) %>%
    layout(title = "Holt-Winters Forecast",
           xaxis = list(title = "Time"),
           yaxis = list(title = "Spot Price"),
           showlegend = TRUE)
  
  return(p)
})
  
  output$plot_ar <- renderPlotly({
  if (!checkData() || is.null(forecasts$ar)) return(NULL)
  
  # Get original data
  orig_data <- createData()
  orig_time <- time(orig_data)
  orig_values <- as.numeric(orig_data)
  
  # Get forecast data
  forecast_time <- time(forecasts$ar$mean)
  mean_values <- as.numeric(forecasts$ar$mean)
  
  # Create lower/upper bound vectors
  lower_values <- if(is.matrix(forecasts$ar$lower)) forecasts$ar$lower[,1] else forecasts$ar$lower
  upper_values <- if(is.matrix(forecasts$ar$upper)) forecasts$ar$upper[,1] else forecasts$ar$upper
  
  # Create the plot
  p <- plot_ly() %>%
    add_trace(x = orig_time, y = orig_values, 
              type = "scatter", mode = "lines", name = "Historical",
              line = list(color = 'black')) %>%
    add_trace(x = forecast_time, y = mean_values, 
              type = "scatter", mode = "lines", name = "Forecast",
              line = list(color = 'orange')) %>%
    add_ribbons(x = forecast_time, 
                ymin = lower_values, 
                ymax = upper_values,
                name = paste0(input$confidence_level, "% CI"),
                fillcolor = 'rgba(255, 165, 0, 0.2)',
                line = list(color = 'rgba(255, 165, 0, 0)')) %>%
    layout(title = "ARIMA Forecast",
           xaxis = list(title = "Time"),
           yaxis = list(title = "Spot Price"),
           showlegend = TRUE)
  
  return(p)
})
  
  output$plot_sn <- renderPlotly({
    if (!checkData() || is.null(forecasts$sn)) return(NULL)
    
    p <- autoplot(forecasts$sn) + 
      ggtitle("Seasonal Naive Forecast") +
      xlab("Time") + 
      ylab("Spot Price") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$plot_sar <- renderPlotly({
    if (!checkData() || is.null(forecasts$sar)) return(NULL)
    
    p <- autoplot(forecasts$sar) + 
      ggtitle("Seasonal ARIMA Forecast") +
      xlab("Time") + 
      ylab("Spot Price") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Create tables for each model's forecast
  output$forecasted_hw <- renderTable({
    if (is.null(hw_final_Result())) return(NULL)
    hw_final_Result()
  }, colnames = FALSE, rownames = TRUE, digits = 5)
  
  output$forecasted_ar <- renderTable({
    if (is.null(ar_final_Result())) return(NULL)
    ar_final_Result()
  }, colnames = FALSE, rownames = TRUE, digits = 5)
  
  output$forecasted_sn <- renderTable({
    if (is.null(sn_final_Result())) return(NULL)
    sn_final_Result()
  }, colnames = FALSE, rownames = TRUE, digits = 5)
  
  output$forecasted_sar <- renderTable({
    if (is.null(sar_final_Result())) return(NULL)
    sar_final_Result()
  }, colnames = FALSE, rownames = TRUE, digits = 5)
  
  # Error comparison plot
  output$comparison <- renderPlot({
    if (!checkData() || is.null(forecasts$hw)) return(NULL)
    
    errors <- c(
      accuracy(forecasts$hw)[1, "RMSE"],
      accuracy(forecasts$ar)[1, "RMSE"],
      accuracy(forecasts$sn)[1, "RMSE"],
      accuracy(forecasts$sar)[1, "RMSE"]
    )
    
    barplot(errors, main = "Model Error Comparison", 
            xlab = "Models", ylab = "Root Mean Square Error (RMSE)", 
            names.arg = c("Holt-Winters", "ARIMA", "Seasonal Naive", "Seasonal ARIMA"),
            col = c("#4682B4", "#FF8C00", "#2E8B57", "#B22222"),
            border = "white")
  })
  
  # Cost calculator
  output$cost_table <- renderTable({
    if (!checkData() || is.null(forecasts$hw)) return(NULL)
    
    # Get price predictions
    hw_price <- hw_final_Result()["Mean"]
    ar_price <- ar_final_Result()["Mean"]
    sn_price <- sn_final_Result()["Mean"]
    sar_price <- sar_final_Result()["Mean"]
    
    # Calculate costs
    hw_cost <- calculate_job_cost(hw_price, input$job_duration) * input$instance_count
    ar_cost <- calculate_job_cost(ar_price, input$job_duration) * input$instance_count
    sn_cost <- calculate_job_cost(sn_price, input$job_duration) * input$instance_count
    sar_cost <- calculate_job_cost(sar_price, input$job_duration) * input$instance_count
    
    # Calculate on-demand price (assumed to be ~3x spot price)
    on_demand_price <- max(hw_price, ar_price, sn_price, sar_price) * 3
    on_demand_cost <- calculate_job_cost(on_demand_price, input$job_duration) * input$instance_count
    
    # Expected cost with interruption probability
    interruption_factor <- 1 + (input$interruption_probability / 100)
    hw_expected_cost <- hw_cost * interruption_factor
    ar_expected_cost <- ar_cost * interruption_factor
    sn_expected_cost <- sn_cost * interruption_factor
    sar_expected_cost <- sar_cost * interruption_factor
    
    # Create table
    cost_data <- data.frame(
      "Price Model" = c("Holt-Winters", "ARIMA", "Seasonal Naive", "Seasonal ARIMA", "On-Demand (Est.)"),
      "Hourly Price" = c(hw_price, ar_price, sn_price, sar_price, on_demand_price),
      "Total Cost" = c(hw_cost, ar_cost, sn_cost, sar_cost, on_demand_cost),
      "Est. Cost with Interruptions" = c(hw_expected_cost, ar_expected_cost, sn_expected_cost, sar_expected_cost, on_demand_cost),
      "Savings vs On-Demand" = c(
        (1 - hw_expected_cost/on_demand_cost) * 100,
        (1 - ar_expected_cost/on_demand_cost) * 100,
        (1 - sn_expected_cost/on_demand_cost) * 100,
        (1 - sar_expected_cost/on_demand_cost) * 100,
        0
      )
    )
    
    # Format the savings column
    cost_data$Savings.vs.On.Demand <- paste0(round(cost_data$Savings.vs.On.Demand, 1), "%")
    
    return(cost_data)
  }, digits = 3)
  
  # Price stability plot
  output$price_stability_plot <- renderPlotly({
    if (!checkData() || is.null(forecasts$hw)) return(NULL)
    
    # Get raw data
    raw_data <- createData_1()
    
    # Convert to data frame for plotting
    plot_data <- data.frame(
      Timestamp = as.POSIXct(raw_data$Timestamp),
      SpotPrice = as.numeric(raw_data$SpotPrice)
    )
    
    # Calculate volatility (standard deviation of % changes)
    price_changes <- diff(plot_data$SpotPrice) / plot_data$SpotPrice[-length(plot_data$SpotPrice)] * 100
    volatility <- sd(price_changes, na.rm = TRUE)
    
    # Create plot
    p <- plot_ly(plot_data, x = ~Timestamp, y = ~SpotPrice, type = "scatter", mode = "lines",
                 name = "Spot Price", line = list(color = "#4682B4")) %>%
      layout(title = paste0("Spot Price History and Volatility (", round(volatility, 2), "%)"),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Spot Price ($)"),
             showlegend = TRUE)
    
    # Add trend line
    if (nrow(plot_data) > 10) {
      model <- loess(SpotPrice ~ as.numeric(Timestamp), data = plot_data, span = 0.3)
      smoothed <- predict(model)
      trend_data <- data.frame(
        Timestamp = plot_data$Timestamp,
        SpotPrice = smoothed
      )
      
      p <- p %>% add_trace(data = trend_data, x = ~Timestamp, y = ~SpotPrice, 
                           name = "Trend", line = list(color = "#FF8C00", dash = "dash"))
    }
    
    return(p)
  })
  
  # Spot history functionality
  spot_history_data <- reactiveVal(NULL)
  
  observeEvent(input$getHistory, {
    if (input$instance == "Select" || input$zone == "Select" || input$os == "Select") {
    showNotification("Please select instance type, zone, and OS first", type = "warning")
    return(NULL)
  }
    
    # Format date range for AWS CLI
    start_date <- paste0(input$date_range[1], "T00:00:00.001")
    end_date <- paste0(input$date_range[2], "T23:59:59.999")
    
    # AWS CLI command to get spot price history
    command <- paste("aws ec2 describe-spot-price-history",
                   paste("--region", input$region),  # Add region parameter explicitly
                   paste("--availability-zone", input$zone),
                   paste("--instance-types", input$instance),
                   paste("--product-descriptions", shQuote(input$os)),
                   paste("--start-time", shQuote(start_date)),
                   paste("--end-time", shQuote(end_date)))
    
    withProgress(message = 'Fetching spot price history...', value = 0, {
      result <- try(system(command, intern = TRUE), silent = TRUE)
      
      if (inherits(result, "try-error")) {
        showNotification("Error fetching spot price history. Check AWS configuration.", type = "error")
        return(NULL)
      }
      
      incProgress(0.7, detail = "Processing data...")
      
      # Parse JSON result
      history_data <- fromJSON(paste(result, collapse = ""))$SpotPriceHistory
      
      if (length(history_data) == 0) {
        showNotification("No spot price data found for the selected criteria.", type = "warning")
        return(NULL)
      }
      
      # Convert timestamps to datetime objects
      history_data$Timestamp <- as.POSIXct(history_data$Timestamp)
      
      # Sort by timestamp
      history_data <- history_data[order(history_data$Timestamp), ]
      
      # Store data in reactive value
      spot_history_data(history_data)
      
      incProgress(1, detail = "Done!")
    })
  })
  
  # Render spot history table
  output$spot_history_table <- renderDT({
    data <- spot_history_data()
    if (is.null(data)) return(NULL)
    
    # Format data for display
    display_data <- data.frame(
      Timestamp = data$Timestamp,
      AvailabilityZone = data$AvailabilityZone,
      InstanceType = data$InstanceType,
      SpotPrice = as.numeric(data$SpotPrice),
      ProductDescription = data$ProductDescription
    )
    
    # Create datatable with formatting
    datatable(display_data, 
              options = list(
                pageLength = 10, 
                order = list(list(0, 'desc')),
                searchHighlight = TRUE
              )) %>%
      formatRound('SpotPrice', 5)
  })
  
  # Render history plot
  output$history_plot <- renderPlotly({
  data <- spot_history_data()
  if (is.null(data)) return(NULL)
  
  # Add hour of day and day of week
  data$Hour <- as.numeric(format(data$Timestamp, "%H"))
  data$DayOfWeek <- factor(weekdays(data$Timestamp), 
                          levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  # Convert SpotPrice to numeric explicitly
  data$NumericSpotPrice <- as.numeric(data$SpotPrice)
  
  # Create plot of price over time
  p1 <- plot_ly(data, x = ~Timestamp, y = ~NumericSpotPrice, type = "scatter", 
                mode = "lines", name = "Spot Price", line = list(color = "#4682B4")) %>%
    layout(title = "Spot Price History",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Spot Price ($)"))
  
  # Create heatmap of price by day of week and hour
  daily_pattern <- aggregate(NumericSpotPrice ~ Hour + DayOfWeek, data = data, FUN = mean)
  
  p2 <- plot_ly(daily_pattern, x = ~DayOfWeek, y = ~Hour, z = ~NumericSpotPrice, 
                type = "heatmap", colorscale = "Blues", reversescale = TRUE) %>%
    layout(title = "Average Spot Price by Day and Hour",
           xaxis = list(title = "Day of Week"),
           yaxis = list(title = "Hour of Day", autorange = "reversed"))
  
  # Create subplot with both charts
  subplot(p1, p2, nrows = 2, heights = c(0.7, 0.3)) %>%
    layout(title = "Spot Price Analysis")
})
  
  # Dark mode toggle
  observeEvent(input$dark_mode, {
    if (input$dark_mode) {
      shinyjs::runjs("document.body.className = 'skin-blue sidebar-mini dark-mode';")
    } else {
      shinyjs::runjs("document.body.className = 'skin-blue sidebar-mini';")
    }
  })
  
  # Reset settings
  observeEvent(input$reset_settings, {
    updateSliderInput(session, "confidence_level", value = 95)
    updateCheckboxInput(session, "dark_mode", value = FALSE)
    shinyjs::runjs("document.body.className = 'skin-blue sidebar-mini';")
  })
  
  # Add export functionality for tables and plots
  output$download_data <- downloadHandler(
    filename = function() {
      paste("spot-prediction-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create comparison data
      if (is.null(forecasts$hw)) return(NULL)
      
      hw_result <- hw_final_Result()
      ar_result <- ar_final_Result()
      sn_result <- sn_final_Result()
      sar_result <- sar_final_Result()
      
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
        Value = c(input$instance, input$zone, input$os, input$predint, 
                  input$hist_days, paste0(input$confidence_level, "%"), Sys.Date())
      )
      
      # Combine data
      final_data <- list(Metadata = metadata, Predictions = comparison_data)
      
      # Write to file
      write.csv(final_data$Metadata, file)
      write.csv(final_data$Predictions, file, append = TRUE)
    }
  )
}

# Helper CSS for dark mode
dark_mode_css <- tags$head(
  tags$style(HTML("
    .dark-mode {
      background-color: #2d2d2d;
      color: #f5f5f5;
    }
    .dark-mode .box {
      background-color: #3d3d3d;
      color: #f5f5f5;
    }
    .dark-mode .table {
      background-color: #3d3d3d;
      color: #f5f5f5;
    }
    .dark-mode .dataTables_wrapper {
      background-color: #3d3d3d;
      color: #f5f5f5;
    }
  "))
)

# Add the CSS to the UI
ui$body <- tagList(dark_mode_css, ui$body)

# Create Shiny app
shinyApp(ui = ui, server = server)