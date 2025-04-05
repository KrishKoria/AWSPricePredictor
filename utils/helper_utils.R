# General utility functions

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