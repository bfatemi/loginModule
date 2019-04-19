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

# Define UI for application that draws a histogram
ui <- dashboardPage(
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


