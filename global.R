# Load all required libraries
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

# Global variables
default_days_back <- 90
startTime <- paste(Sys.Date() - default_days_back, "T00:00:00.001", sep = "")
endTime <- paste(Sys.Date() - 1, "T23:59:59.999", sep = "")