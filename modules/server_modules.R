# Server logic as Shiny modules

# Prediction tab server module
predictionTabServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    # Dynamic UI elements
    output$zone_selector <- renderUI({
    if (input$region != "Select") {
      zones <- get_availability_zones(input$region)
      selectInput(session$ns("zone"), "Availability Zone", choices = c("Select", zones), selected = "Select")
    } else {
      selectInput(session$ns("zone"), "Availability Zone", choices = c("Select"), selected = "Select")
    }
  })
    
    # Update date range when hist_days changes
    observeEvent(input$hist_days, {
      startTime <<- paste(Sys.Date() - input$hist_days, "T00:00:00.001", sep = "")
      endTime <<- paste(Sys.Date() - 1, "T23:59:59.999", sep = "")
    })
    
    # Check if all filters are properly selected
    checkFilters <- reactive({
    ifelse(input$instance == "Select" || 
              input$zone == "Select" || 
              input$os == "Select" || 
              input$predint == "Select", FALSE, TRUE)
})
    
    # Fetch raw spot price data
    createData_1 <- reactive({
      if (!checkFilters()) return(NULL)
      
      fetch_spot_price_data(input$region, input$zone, input$instance, input$os, startTime, endTime)
    })
    
    # Check if data is available
    checkData <- reactive({
      data <- createData_1()
      ifelse(checkFilters() && !is.null(data) && (length(data) > 0), TRUE, FALSE)
    })
    
    # Process the data for time series analysis
    createData <- reactive({
      if (!checkData()) return(NULL)
      
      prepare_time_series(createData_1(), input$zone, input$instance, input$os)
    })
    
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
      
      # Set confidence level
      conf_level <- as.numeric(shared_data$confidence_level)
      
      withProgress(message = 'Building forecasting models...', value = 0, {
        ts_data <- createData()
        
        incProgress(0.2, detail = "Fitting models")
        forecasts <- build_all_models(ts_data, conf_level)
        
        # Store in shared data
        shared_data$forecasts <- forecasts
        shared_data$raw_data <- createData_1()
        shared_data$ts_data <- ts_data
        shared_data$time_of_day <- input$predint
        
        incProgress(1, detail = "Done!")
      })
    })
    
    # Generate comparison table of all models
    output$comparison_table <- renderTable({
      if (is.null(shared_data$forecasts)) return(NULL)
      
      generate_comparison_table(shared_data$forecasts, shared_data$time_of_day, 
                                as.numeric(shared_data$confidence_level))
    }, digits = 6)
    
    # Generate interactive plots for each model
    output$plot_hw <- renderPlotly({
      if (is.null(shared_data$forecasts) || is.null(shared_data$forecasts$hw)) return(NULL)
      
      # Get original data and forecast
      forecast_obj <- shared_data$forecasts$hw
      orig_data <- shared_data$ts_data
      
      # Create the plot
      autoplot(forecast_obj) + 
        ggtitle("Holt-Winters Forecast") +
        xlab("Time") + 
        ylab("Spot Price") +
        theme_minimal()
    })
    
    output$plot_ar <- renderPlotly({
      if (is.null(shared_data$forecasts) || is.null(shared_data$forecasts$ar)) return(NULL)
      
      autoplot(shared_data$forecasts$ar) + 
        ggtitle("ARIMA Forecast") +
        xlab("Time") + 
        ylab("Spot Price") +
        theme_minimal()
    })
    
    output$plot_sn <- renderPlotly({
      if (is.null(shared_data$forecasts) || is.null(shared_data$forecasts$sn)) return(NULL)
      
      autoplot(shared_data$forecasts$sn) + 
        ggtitle("Seasonal Naive Forecast") +
        xlab("Time") + 
        ylab("Spot Price") +
        theme_minimal()
    })
    
    output$plot_sar <- renderPlotly({
      if (is.null(shared_data$forecasts) || is.null(shared_data$forecasts$sar)) return(NULL)
      
      autoplot(shared_data$forecasts$sar) + 
        ggtitle("Seasonal ARIMA Forecast") +
        xlab("Time") + 
        ylab("Spot Price") +
        theme_minimal()
    })
    
    # Create tables for each model's forecast
    output$forecasted_hw <- renderTable({
      if (is.null(shared_data$forecasts) || is.null(shared_data$forecasts$hw)) return(NULL)
      get_forecast_result(shared_data$forecasts$hw, shared_data$time_of_day)
    }, colnames = FALSE, rownames = TRUE, digits = 5)
    
    output$forecasted_ar <- renderTable({
      if (is.null(shared_data$forecasts) || is.null(shared_data$forecasts$ar)) return(NULL)
      get_forecast_result(shared_data$forecasts$ar, shared_data$time_of_day)
    }, colnames = FALSE, rownames = TRUE, digits = 5)
    
    output$forecasted_sn <- renderTable({
      if (is.null(shared_data$forecasts) || is.null(shared_data$forecasts$sn)) return(NULL)
      get_forecast_result(shared_data$forecasts$sn, shared_data$time_of_day)
    }, colnames = FALSE, rownames = TRUE, digits = 5)
    
    output$forecasted_sar <- renderTable({
      if (is.null(shared_data$forecasts) || is.null(shared_data$forecasts$sar)) return(NULL)
      get_forecast_result(shared_data$forecasts$sar, shared_data$time_of_day)
    }, colnames = FALSE, rownames = TRUE, digits = 5)
    
    # Error comparison plot
    output$comparison <- renderPlot({
      if (is.null(shared_data$forecasts) || is.null(shared_data$forecasts$hw)) return(NULL)
      
      errors <- c(
        accuracy(shared_data$forecasts$hw)[1, "RMSE"],
        accuracy(shared_data$forecasts$ar)[1, "RMSE"],
        accuracy(shared_data$forecasts$sn)[1, "RMSE"],
        accuracy(shared_data$forecasts$sar)[1, "RMSE"]
      )
      
      barplot(errors, main = "Model Error Comparison", 
              xlab = "Models", ylab = "Root Mean Square Error (RMSE)", 
              names.arg = c("Holt-Winters", "ARIMA", "Seasonal Naive", "Seasonal ARIMA"),
              col = c("#4682B4", "#FF8C00", "#2E8B57", "#B22222"),
              border = "white")
    })
  })
}

customerWillingnessTabServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    observe({
      shared_data$customer_wtp <- estimate_wtp(
        quality = input$quality_of_service,
        usage_time = input$usage_time,
        discount = input$certainty_of_discount,
        sharing_option = input$sharing_option
      )
    })
    # Calculate WTP and display it
    output$willingness_output <- renderText({
      paste("Estimated Willingness to Pay: $", round(shared_data$customer_wtp, 2))
    })
  })
}

# Calculator tab server module
calculatorTabServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {

    # Cost calculator
    output$cost_table <- renderTable({
      if (is.null(shared_data$forecasts) || is.null(shared_data$forecasts$hw)) return(NULL)
      
      time_of_day <- shared_data$time_of_day
      
      # Get price predictions
      hw_price <- get_forecast_result(shared_data$forecasts$hw, time_of_day)["Mean"]
      ar_price <- get_forecast_result(shared_data$forecasts$ar, time_of_day)["Mean"]
      sn_price <- get_forecast_result(shared_data$forecasts$sn, time_of_day)["Mean"]
      sar_price <- get_forecast_result(shared_data$forecasts$sar, time_of_day)["Mean"]
      
      # Calculate WTP-adjusted prices
      wtp_factor <- shared_data$customer_wtp
      hw_price <- hw_price * wtp_factor
      ar_price <- ar_price * wtp_factor
      sn_price <- sn_price * wtp_factor
      sar_price <- sar_price * wtp_factor

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
      if (is.null(shared_data$raw_data) || is.null(shared_data$forecasts$hw)) return(NULL)
      
      # Get raw data
      raw_data <- shared_data$raw_data
      
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
  })
}

# History tab server module
# historyTabServer <- function(id, shared_data) {
#   moduleServer(id, function(input, output, session) {
#     # Spot history data
#     spot_history_data <- reactiveVal(NULL)
    
#     observeEvent(input$getHistory, {
#       if (input$instance == "Select" || input$zone == "Select" || input$os == "Select") {
#         showNotification("Please select instance type, zone, and OS first", type = "warning")
#         return(NULL)
#       }
      
#       # Format date range for AWS CLI
#       start_date <- paste0(input$date_range[1], "T00:00:00.001")
#       end_date <- paste0(input$date_range[2], "T23:59:59.999")
      
#       withProgress(message = 'Fetching spot price history...', value = 0, {
#         result <- fetch_spot_price_data(shared_data$region, input$zone, input$instance, input$os, 
#                                         start_date, end_date)
        
#         if (is.null(result)) {
#           showNotification("No spot price data found or error fetching data.", type = "warning")
#           return(NULL)
#         }
        
#         incProgress(0.7, detail = "Processing data...")
        
#         # Convert timestamps to datetime objects
#         result$Timestamp <- as.POSIXct(result$Timestamp)
        
#         # Sort by timestamp
#         result <- result[order(result$Timestamp), ]
        
#         # Store data in reactive value
#         spot_history_data(result)
        
#         incProgress(1, detail = "Done!")
#       })
#     })
    
#     # Render spot history table
#     output$spot_history_table <- renderDT({
#       data <- spot_history_data()
#       if (is.null(data)) return(NULL)
      
#       # Format data for display
#       display_data <- data.frame(
#         Timestamp = data$Timestamp,
#         AvailabilityZone = data$AvailabilityZone,
#         InstanceType = data$InstanceType,
#         SpotPrice = as.numeric(data$SpotPrice),
#         ProductDescription = data$ProductDescription
#       )
      
#       # Create datatable with formatting
#       datatable(display_data, 
#                 options = list(
#                   pageLength = 10, 
#                   order = list(list(0, 'desc')),
#                   searchHighlight = TRUE
#                 )) %>%
#         formatRound('SpotPrice', 5)
#     })
    
#     # Render history plot
#     output$history_plot <- renderPlotly({
#       data <- spot_history_data()
#       if (is.null(data)) return(NULL)
      
#       # Add hour of day and day of week
#       data$Hour <- as.numeric(format(data$Timestamp, "%H"))
#       data$DayOfWeek <- factor(weekdays(data$Timestamp), 
#                               levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      
#       # Convert SpotPrice to numeric explicitly
#       data$NumericSpotPrice <- as.numeric(data$SpotPrice)
      
#       # Create plot of price over time
#       p1 <- plot_ly(data, x = ~Timestamp, y = ~NumericSpotPrice, type = "scatter", 
#                     mode = "lines", name = "Spot Price", line = list(color = "#4682B4")) %>%
#         layout(title = "Spot Price History",
#                xaxis = list(title = "Date"),
#                yaxis = list(title = "Spot Price ($)"))
      
#       # Create heatmap of price by day of week and hour
#       daily_pattern <- aggregate(NumericSpotPrice ~ Hour + DayOfWeek, data = data, FUN = mean)
      
#       p2 <- plot_ly(daily_pattern, x = ~DayOfWeek, y = ~Hour, z = ~NumericSpotPrice, 
#                     type = "heatmap", colorscale = "Blues", reversescale = TRUE) %>%
#         layout(title = "Average Spot Price by Day and Hour",
#                xaxis = list(title = "Day of Week"),
#                yaxis = list(title = "Hour of Day", autorange = "reversed"))
      
#       # Create subplot with both charts
#       subplot(p1, p2, nrows = 2, heights = c(0.7, 0.3)) %>%
#         layout(title = "Spot Price Analysis")
#     })
#   })
# }

# Settings tab server module
settingsTabServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    # AWS Configuration check
    observeEvent(input$check_config, {
      shared_data$aws_configured <- check_aws_config()
      
      if (shared_data$aws_configured) {
        showNotification("AWS CLI is properly configured.", type = "message")
      } else {
        showNotification("AWS CLI is not configured. Please set up your AWS credentials.", type = "error")
      }
    })
    
    # Update shared confidence level
    observe({
      shared_data$confidence_level <- input$confidence_level
    })
    
    # Dark mode toggle
    observeEvent(input$dark_mode, {
      shared_data$dark_mode <- input$dark_mode
      
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
      
      shared_data$confidence_level <- 95
      shared_data$dark_mode <- FALSE
    })
  })
}