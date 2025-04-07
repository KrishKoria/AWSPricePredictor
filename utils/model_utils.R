# Functions for building and evaluating forecasting models

# Process the data for time series analysis
prepare_time_series <- function(spot_price_data, zone, instance, os) {
  # Filter and prepare the data
  SpotPriceHistory_Filtered <- filterSpotPriceHistory(spot_price_data, zone, instance, os)
  # Add time index
  SpotPriceHistory_Filtered$Time <- seq.int(nrow(SpotPriceHistory_Filtered))
  
  # Create time series object
  SpotPrice_TS <- as.ts(SpotPriceHistory_Filtered[, -1:-3]) # Remove instance, zone, OS columns
  
  # Create time series with proper frequency (4 parts per day)
  ts(SpotPrice_TS[, -2:-4], start = c(as.numeric(format(SpotPriceHistory_Filtered$Date[1], "%j")), 1), frequency = 4)
}

# Build Holt-Winters model
build_hw_model <- function(ts_data, h = 4, conf_level = 95) {
  tryCatch({
    model <- HoltWinters(ts_data)
    forecast(model, h = h, level = conf_level)
  }, error = function(e) {
    NULL
  })
}

# Build ARIMA model
build_arima_model <- function(ts_data, h = 4, conf_level = 95) {
  tryCatch({
    model <- auto.arima(ts_data)
    forecast(model, h = h, level = conf_level)
  }, error = function(e) {
    NULL
  })
}

# Build Seasonal Naive model
build_snaive_model <- function(ts_data, h = 4, conf_level = 95) {
  tryCatch({
    snaive(ts_data, h = h, level = conf_level)
  }, error = function(e) {
    NULL
  })
}

# Build Seasonal ARIMA model
build_sarima_model <- function(ts_data, h = 4, conf_level = 95) {
  tryCatch({
    model <- arima(ts_data, order = c(3, 0, 0), seasonal = c(0, 0, 1))
    forecast(model, h = h, level = conf_level)
  }, error = function(e) {
    tryCatch({
      model <- arima(ts_data, order = c(1, 0, 0), seasonal = c(0, 0, 1))
      forecast(model, h = h, level = conf_level)
    }, error = function(e) {
      NULL
    })
  })
}

# Build all forecasting models
build_all_models <- function(ts_data, conf_level = 95) {
  models <- list()
  forecasts <- list()
  
  # Holt-Winters model
  forecasts$hw <- build_hw_model(ts_data, conf_level = conf_level)
  
  # ARIMA model
  forecasts$ar <- build_arima_model(ts_data, conf_level = conf_level)
  
  # Seasonal Naive model
  forecasts$sn <- build_snaive_model(ts_data, conf_level = conf_level)
  
  # Seasonal ARIMA model
  forecasts$sar <- build_sarima_model(ts_data, conf_level = conf_level)
  
  return(forecasts)
}

# Generate comparison table of all models
generate_comparison_table <- function(forecasts, time_of_day, conf_level = 95) {
  if (is.null(forecasts$hw)) return(NULL)
  
  # Get results from all models
  hw_result <- get_forecast_result(forecasts$hw, time_of_day)
  ar_result <- get_forecast_result(forecasts$ar, time_of_day)
  sn_result <- get_forecast_result(forecasts$sn, time_of_day)
  sar_result <- get_forecast_result(forecasts$sar, time_of_day)
  
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
}