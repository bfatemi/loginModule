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
library(httr)
library(shinyalert)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)


authy_header <- add_headers(`X-Authy-API-Key` = "4b1xZhqqox7T6yKcrIH9kPBZAY40sNom")
url_req_totp <- "https://api.authy.com/protected/json/sms/"
url_chk_totp <- "https://api.authy.com/protected/json/verify/"



## simulated ocpu ping to verify user 
## and receive authy id needed for sms verification  
hpdsUID <- function(usr, pwd) return("145315361")# 

# Define UI for application that draws a histogram
ui <- dashboardPage(
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    fluidPage(
      theme = shinytheme("cerulean"),
      useShinyalert(),
      
      fluidRow(
        column(width = 10,
               
               box(
                 title = column(12, "HPDS Platform Access"), 
                 status = "primary",
                 footer = br(),
                 
                 column(
                   width = 12,
                   textInput("usr", label = NULL, placeholder = "Enter Username"),
                   passwordInput("pwd", label = NULL, placeholder = "Enter Password"),
                   materialSwitch(
                     inputId = "Id080",
                     label = "Remember me", 
                     value = TRUE,
                     status = "primary"
                   )
                 ),
                 
                 column(
                   width = 12,
                   actionBttn(
                     inputId = "btn_login",
                     label = "Login", 
                     style = "material-flat",
                     color = "danger"
                   ),
                   actionBttn(
                     inputId = "register_new",
                     label = "Guest", 
                     style = "material-flat",
                     color = "primary"
                   )
                 )
               ))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$btn_login, {
    
    if(input$usr == "" || input$pwd == ""){
      
      showNotification(id = "auth_err",
                       ui = paste("Username or password not provided"),
                       type = "error",
                       duration = 0,
                       closeButton = TRUE)
      
    }else{
      
      removeNotification("auth_err")
      
      # begin login by verifying username/pwd with authy api and requesting sms code
      authid <- hpdsUID(input$usr, input$pwd)
      resp <- httr::GET(paste0(url_req_totp, authid), authy_header)
      
      if( httr::status_code(resp) == 200 ){
        
        showModal(modalDialog(
          textInput("smscode", "Enter code sent via sms"),
          title = "SMS with code sent!", 
          "Press Login to Complete...", 
          footer = list(
            actionBttn(
              inputId = "btn_verify",
              label = "Submit", 
              style = "material-flat",
              color = "success"
            ),
            actionBttn(
              inputId = "btn_goto_register",
              label = "Email me", 
              style = "material-flat",
              color = "royal"
            )
          ),
          easyClose = TRUE
        ))
      }else{
        
        shinyalert("Oops!", "Unable to login with username and password", type = "error")
        
      }
      
    }
    
    ## user enters sms code and hits verify
    observeEvent(input$btn_verify, {
      
      ## ensure code is entered
      if(input$smscode == ""){
        
        showNotification(id = "auth_err", 
                         ui = paste("Username or password not provided"), 
                         type = "error", 
                         duration = 0, 
                         closeButton = TRUE)
        
      }else{
        
        url_verify <- paste0(url_chk_totp, input$smscode, "/", authid)
        resp <- httr::GET(url_verify, authy_header)
        # content(resp, "text")
        
        if(httr::status_code(resp)){
          
          showNotification(paste("Authentication Successful!"), type = "message", duration = 0, closeButton = TRUE)
          
        } 
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

