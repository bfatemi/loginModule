library(shiny)
library(httr)
library(shinyalert)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)

library(ocputils)
library(hpdsConnect)

apikey <- "018ec6a3d3927f773a0d1b0adf516fa36b300929"



# Module UI function
loginModuleUI <- function(id, title = "Platform Access") {
  ns <- NS(id)
  tagList(
    fluidPage(
      theme = shinytheme("cerulean"),
      useShinyalert(),
      fluidRow(
        column(width = 11,
               box(
                 title = column(12, title),
                 status = "primary",
                 footer = br(),
                 column(
                   width = 12,
                   textInput(inputId = ns("usr"), 
                             placeholder = "Enter Username", 
                             value = NULL,
                             label = NULL),
                   passwordInput(inputId = ns("pwd"), 
                                 placeholder = "Enter Password",
                                 value = NULL,
                                 label = NULL),
                   conditionalPanel(
                     passwordInput(inputId = ns("pwd_2"), 
                                   placeholder = "Confirm Password", 
                                   value = NULL,
                                   label = NULL),
                     textInput(inputId = ns("firstname"), 
                               placeholder = "First Name", 
                               value = NULL,
                               label = NULL),
                     textInput(inputId = ns("lastname"), 
                               placeholder = "Last Name", 
                               value = NULL,
                               label = NULL),
                     textInput(inputId = ns("usremail"), 
                               placeholder = "Email", 
                               value = NULL,
                               label = NULL),
                     fluidRow(
                       column(3, helpText("Mobile (#):")),
                       column(3, textInput(inputId = ns("phone_area"), 
                                           placeholder = " ### ", 
                                           value = NULL,
                                           label = NULL)),
                       column(3, textInput(inputId = ns("phone_ddd"), 
                                           placeholder = " ### ",
                                           value = NULL, 
                                           label = NULL)),
                       column(3, textInput(inputId = ns("phone_dddd"), 
                                           placeholder = " #### ", 
                                           value = NULL,
                                           label = NULL))
                     ), 
                     condition = "input.btn_skip_login %2 == 1",
                     ns = ns
                   ),
                   materialSwitch(inputId = ns("check_remember"),
                                  label = "Remember me",
                                  value = TRUE,
                                  status = "primary")
                 ),
                 
                 column(
                   width = 12,
                   fluidRow(
                     column(
                       width = 5,
                       actionBttn(inputId = ns("btn_send_login"),
                                  label = "NEXT",
                                  style = "material-flat",
                                  color = "danger",
                                  block = TRUE)
                     ),
                     column(2, ""),
                     column(
                       width = 5,
                       actionBttn(inputId = ns("btn_skip_login"),
                                  label = "GUEST",
                                  style = "material-flat",
                                  color = "primary", 
                                  block = TRUE)
                     )
                   )
                 )
               ))
      )
    )
  )
}


# Module server function
loginModule <- function(input, output, session) {
  
  observeEvent(input$btn_skip_login, {
    if(input$btn_skip_login %% 2 == 1){
      updateActionButton(session, inputId = "btn_skip_login", label = "SKIP")
      updateActionButton(session, inputId = "btn_send_login", label = "NEXT")
    }else{
      updateActionButton(session, inputId = "btn_skip_login", label = "GUEST")
      updateActionButton(session, inputId = "btn_send_login", label = "LOGIN")
    }
  })
  
  ## LOGIN WITH USR/PWD
  observeEvent(input$btn_send_login, {
    usrfound <- hpdsUserSearch(input$usr, apikey)
    
    if( usrfound & length(ull) > 0 ){
      ull <- hpdsUserLogin(input$usr, input$pwd)
  
      if( ull$enabled_2FA ){
        hash <- verify_send_sms(ull$UID)
        
        shinyalert(title = "Welcome", 
                   showCancelButton = TRUE, 
                   showConfirmButton = TRUE, 
                   timer = 30000, 
                   type = "input", 
                   cancelButtonText = "Exit",
                   inputType = "password",
                   text = "Please Enter SMS Code", 
                   callbackR = function(x){
                     if(x == FALSE)
                       stopApp()
                     stopifnot(sodium::password_verify(hash,x))
                   })
      }
      shinyalert(title = "Success!", 
                 type = "success",
                 text = paste0("Welcome Back ", ull$firstname), 
                 showConfirmButton = TRUE, 
                 confirmButtonText = "Continue", 
                 callbackR = stopApp)
    }
    shinyalert(title = "Login Failed", 
               text = "Incorrect Username or Password", 
               type = "error", 
               closeOnClickOutside = TRUE, 
               showCancelButton = TRUE, 
               cancelButtonText = "Exit")
  })
}




ui <- dashboardPage(
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(loginModuleUI("login"))
)

server <- function(input, output, session) {
  callModule(loginModule, "login")
}

shinyApp(ui = ui, server = server)




head_authy <- httr::add_headers(`X-Authy-API-Key` = "4b1xZhqqox7T6yKcrIH9kPBZAY40sNom")
url_rotp <- "https://api.authy.com/protected/json/sms/"
url_veri <- "https://api.authy.com/protected/json/verify/"




# newUserModal <- function(session){
#   ns <- session$ns
#   modalDialog(
#     textInput(inputId = ns("smscode"), 
#               label = "Enter code sent via sms"),
#     
#     title = "SMS with code sent!", 
#     "Press Login to Complete...",
#     footer = list(
#       actionBttn(inputId = ns("btn_verify"),
#                  label = "Submit",
#                  style = "material-flat",
#                  color = "success"),
#       actionBttn(inputId = ns("btn_goto_register"),
#                  label = "Email me",
#                  style = "material-flat",
#                  color = "royal")
#     ),
#     easyClose = TRUE
#   )
# }




