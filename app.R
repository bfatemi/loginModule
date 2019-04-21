#	
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# sendSweetAlert(text = "Unable to login with username/password", type = "error", closeOnClickOutside = TRUE, title = "We can't find you", )


library(shiny)

# source("loginModule.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  
  body = dashboardBody(
    loginModuleUI("login")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  callModule(loginModule, "login")
  # 
  # hpds_encrypt("bobbyf")
  # hpds_encrypt("Newyork1")
  
}

# Run the application
shinyApp(ui = ui, server = server)


