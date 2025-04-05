# AWS-specific utility functions

# Function to check if AWS CLI is configured
check_aws_config <- function() {
  result <- try(system("aws configure list", intern = TRUE), silent = TRUE)
  !inherits(result, "try-error")
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

# Function to fetch spot price data from AWS
fetch_spot_price_data <- function(region, zone, instance, os, startTime, endTime) {
  # AWS CLI command to get spot price history
  command <- paste("aws ec2 describe-spot-price-history",
                   paste("--region", region),
                   paste("--availability-zone", zone),
                   paste("--instance-types", instance),
                   paste("--product-descriptions", shQuote(os)),
                   paste("--start-time", shQuote(startTime)),
                   paste("--end-time", shQuote(endTime)))
  
  result <- try(system(command, intern = TRUE), silent = TRUE)
  
  if (inherits(result, "try-error")) {
    return(NULL)
  }
  
  # Parse the JSON result
  tryCatch({
    combined_json <- paste(result, collapse = "")
    
    # Check if response contains an error message
    if(grepl("error|usage:", combined_json, ignore.case = TRUE)) {
      return(NULL)
    }
    
    spot_data <- fromJSON(combined_json)$SpotPriceHistory
    
    # Check if we got meaningful data
    if(length(spot_data) == 0) {
      return(NULL)
    }
    
    return(spot_data)
  }, error = function(e) {
    return(NULL)
  })
}