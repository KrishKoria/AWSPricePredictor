# UI components as Shiny modules

# Prediction tab UI module
predictionTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "prediction",
          fluidRow(
            box(
              title = "Input Parameters", status = "primary", solidHeader = TRUE,
              width = 4,
              selectInput(ns("region"), "AWS Region", 
                          choices = c("Select" = "Select", 
                                      setNames(get_aws_regions(), get_aws_regions())), 
                          selected = "us-east-1"),
              uiOutput(ns("zone_selector")),
              selectInput(ns("instance"), "Instance Type", choices = c("Select", get_instance_types()), selected = "Select"),
              selectInput(ns("os"), "Operating System", choices = c("Select", "Linux/UNIX", "Windows", "SUSE Linux", "Red Hat Enterprise Linux", "Linux/UNIX (Amazon VPC)"), selected = "Linux/UNIX"),
              prettyRadioButtons(ns("predint"), "Time of Day", 
                                 choices = c("Morning (5am-12pm)" = "Morning", 
                                             "Afternoon (12pm-5pm)" = "Afternoon", 
                                             "Evening (5pm-9pm)" = "Evening", 
                                             "Night (9pm-5am)" = "Night"),
                                 selected = "Morning", inline = TRUE),
              numericInput(ns("hist_days"), "Historical Data (days)", value = 90, min = 30, max = 180),
              actionButton(ns("getData"), "Predict Spot Prices", class = "btn-success", icon = icon("play"))
            ),
            
            box(
              title = "Predicted Price Range", status = "info", solidHeader = TRUE,
              width = 8, height = 245,
              tableOutput(ns("comparison_table"))
            )
          ),
          
          fluidRow(
            tabBox(
              title = "Forecasting Models", width = 12,
              tabPanel("Holt Winters", plotlyOutput(ns("plot_hw")), tableOutput(ns("forecasted_hw"))),
              tabPanel("ARIMA", plotlyOutput(ns("plot_ar")), tableOutput(ns("forecasted_ar"))),
              tabPanel("Seasonal Naive", plotlyOutput(ns("plot_sn")), tableOutput(ns("forecasted_sn"))),
              tabPanel("Seasonal ARIMA", plotlyOutput(ns("plot_sar")), tableOutput(ns("forecasted_sar"))),
              tabPanel("Error Comparison", plotOutput(ns("comparison")))
            )
          )
  )
}

# Calculator tab UI module
calculatorTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "calculator",
          fluidRow(
            box(
              title = "Job Parameters", status = "primary", solidHeader = TRUE,
              width = 4,
              numericInput(ns("job_duration"), "Job Duration (hours)", value = 1, min = 0.1, max = 1000, step = 0.1),
              numericInput(ns("instance_count"), "Number of Instances", value = 1, min = 1, max = 100),
              numericInput(ns("interruption_probability"), "Interruption Probability (%)", value = 5, min = 0, max = 100)
            ),
            
            box(
              title = "Cost Estimates", status = "success", solidHeader = TRUE,
              width = 8,
              tableOutput(ns("cost_table"))
            )
          ),
          
          fluidRow(
            box(
              title = "Price Stability Analysis", status = "warning", solidHeader = TRUE,
              width = 12,
              plotlyOutput(ns("price_stability_plot"))
            )
          )
  )
}

# History tab UI module
# historyTabUI <- function(id) {
#   ns <- NS(id)
  
#   tabItem(tabName = "history",
#           fluidRow(
#             box(
#               title = "Spot Price History", status = "primary", solidHeader = TRUE,
#               width = 12,
#               dateRangeInput(ns("date_range"), "Date Range", 
#                              start = Sys.Date() - 30, end = Sys.Date() - 1),
#               actionButton(ns("getHistory"), "Fetch History", class = "btn-info", icon = icon("search")),
#               DTOutput(ns("spot_history_table"))
#             )
#           ),
          
#           fluidRow(
#             box(
#               title = "Historical Price Trends", status = "info", solidHeader = TRUE,
#               width = 12,
#               plotlyOutput(ns("history_plot"))
#             )
#           )
#   )
# }

# Settings tab UI module
settingsTabUI <- function(id) {
  ns <- NS(id)
  
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
              actionButton(ns("check_config"), "Check AWS Configuration", icon = icon("check"))
            ),
            
            box(
              title = "Application Settings", status = "warning", solidHeader = TRUE,
              width = 6,
              sliderInput(ns("confidence_level"), "Prediction Confidence Level", min = 80, max = 99, value = 95, step = 1, post = "%"),
              checkboxInput(ns("dark_mode"), "Dark Mode", value = FALSE),
              actionButton(ns("reset_settings"), "Reset to Defaults", icon = icon("undo"))
            )
          )
  )
}

customerWillingnessTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "willingness",
          fluidRow(
            box(
              title = "Customer Willingness to Pay", status = "primary", solidHeader = TRUE,
              width = 12,
              p("Set parameters to estimate customer willingness to pay based on hedonic values."),
              
              # Dummy selectors for hedonic values
              sliderInput(ns("quality_of_service"), "Quality of Service (0-10)", min = 0, max = 10, value = 5),
              sliderInput(ns("usage_time"), "Usage Time (Weeks to Minutes)", min = 1, max = 10080, value = 1440, step = 60, post = " mins"),
              sliderInput(ns("certainty_of_discount"), "Certainty of Discount (0-100%)", min = 0, max = 100, value = 50, post = "%"),
              selectInput(ns("sharing_option"), "Sharing Option", choices = c("One", "Many"), selected = "One"),
              
              # Display calculated willingness to pay (dummy output)
              h4("Estimated Willingness to Pay:"),
              verbatimTextOutput(ns("willingness_output"))
            )
          )
  )
}

# Combined UI function
createDashboardUI <- function() {
  dashboardPage(
    dashboardHeader(title = "Predict Prices"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Price Prediction", tabName = "prediction", icon = icon("chart-line")),
        menuItem("Customer Willingness", tabName = "willingness", icon = icon("user")),
        menuItem("Cost Calculator", tabName = "calculator", icon = icon("calculator")),
        # menuItem("Spot History", tabName = "history", icon = icon("history")),
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
      
      # Add dark mode CSS
      tags$head(
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
      ),
      
      tabItems(
        predictionTabUI("prediction"),
        calculatorTabUI("calculator"),
        customerWillingnessTabUI("willingness"),
        # historyTabUI("history"),
        settingsTabUI("settings")
      ),
      fluidRow(
        box(
          title = "References", status = "info", solidHeader = TRUE, width = 12,
          p("This application is inspired by the research paper:"),
          tags$blockquote(
            "Hedonic Pricing of Cloud Computing Services",
            tags$footer("Caesar Wu, Adel Nadjaran Toosi, Rajkumar Buyya, and Kotagiri Ramamohanarao")
          )
        )
      )
    )
  )
}