library(shiny)
library(httr)
library(shinyalert)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)

apikey <- ""

hpdsUserLogin <- function(usr, pwd){
  authFailed <- FALSE
  if(authFailed){
    
  }else{
    userll <- list(
      UID = "145315361",
      phone_valid = FALSE,
      email_valid = FALSE,
      enabled_2FA = TRUE
    )
    return(userll)
  }
}

hpdsLoginFailed <- function(usr, apikey){
  
}


hpdsUserFound <- function(usr){
  return(TRUE)
}

hpdsConfirmPhone <- function(uid, apikey){
  
}

hpdsConfirmEmail <- function(uid, apikey){
  
}

send_code_sms <- function(usr){
  sodium::password_store("123456")
}

send_code_email <- function(usr){
  sodium::password_store("123456")
}


# Module UI function
loginModuleUI <- function(id, title = "Platform Access") {
  ns <- NS(id)
  
  tagList(
    fluidPage(
      theme = shinytheme("cerulean"),
      useShinyalert(),
      fluidRow(
        column(
          width = 10,
          box(
            title = column(12, title),
            status = "primary",
            footer = br(),
            column(
              width = 12,
              textInput(ns("usr"), NULL, placeholder = "Enter Username"),
              passwordInput(ns("pwd"), NULL, placeholder = "Enter Password"),
              materialSwitch(inputId = ns("check_remember"),
                             label = "Remember me",
                             value = TRUE,
                             status = "primary")
            ),
            column(
              width = 12,
              fluidRow(
                column(width = 5,
                       actionBttn(inputId = ns("btn_login"),
                                  label = "Login",
                                  style = "material-flat",
                                  color = "danger", 
                                  block = TRUE)),
                column(2, ""),
                column(width = 5,
                       actionBttn(inputId = ns("register_new"),
                                  label = "Guest",
                                  style = "material-flat",
                                  color = "primary", 
                                  block = TRUE))
              )
            )
          )
        )
      )
    ))
}


# Module server function
loginModule <- function(input, output, session) {
  
  # modalTwoFactUI <- function(){
  #   ns <- session$ns
  #   modalDialog(
  #     textInput(ns("smscode"), "Enter code sent via sms"),
  #     title = "SMS with code sent!", "Press Login to Complete...",
  #     footer = list(
  #       actionBttn(ns("btn_verify"),
  #                  label = "Submit",
  #                  style = "material-flat",
  #                  color = "success"),
  #       actionBttn(ns("btn_goto_register"),
  #                  label = "Email me",
  #                  style = "material-flat",
  #                  color = "royal")
  #     ),
  #     easyClose = TRUE
  #   )
  # }
  
  newUserModal <- function(session){
    ns <- session$ns
    modalDialog(
      textInput(ns("smscode"), "Enter code sent via sms"),
      title = "SMS with code sent!", "Press Login to Complete...",
      footer = list(
        actionBttn(ns("btn_verify"),
                   label = "Submit",
                   style = "material-flat",
                   color = "success"),
        actionBttn(ns("btn_goto_register"),
                   label = "Email me",
                   style = "material-flat",
                   color = "royal")
      ),
      easyClose = TRUE
    )
  }
  
  ## LOGIN WITH USR/PWD
  observeEvent(input$btn_login, {
    
 
    
    if( hpdsUserFound(input$usr) ){
      
      userData <- hpdsUserLogin(input$usr, input$pwd)
      
      if(userData == FALSE){
        shinyalert(title = "Login Failed", 
                   type = "error", 
                   callbackR = function(x){
                     hpdsLoginErr( userData$UID, apikey )
                     stopApp()
                   })
      }
        
      if( userData$enabled_2FA ){
        hash <- verify_send_sms(UID)
        shinyalert(title = "Welcome", 
                   showCancelButton = TRUE, 
                   showConfirmButton = TRUE, 
                   timer = 30000, 
                   type = "input", 
                   cancelButtonText = "Cancel",
                   inputType = "password",
                   text = "Please Enter SMS Code", 
                   callbackR = function(x){
                     if(x != FALSE)
                       return(sodium::password_verify(hash,x))
                     message("\nCANCELED\n")
                   })
        
      }
    }else{
      shinyalert(title = "Username not found", 
                 text = "Login Failed", 
                 closeOnClickOutside = TRUE, 
                 type = "error")
    }

    
    # r <- httr::GET(paste0(url_rotp, UID), head_authy)
    # if( httr::status_code(r) != 200 ){
    #   shinyalert("Oops!", "Login Failed", type = "error")
    # }else{
    #   showModal(modalTwoFactUI())
    # }
  })
  
  ## VALIDATE SMS CODE
  observeEvent(input$btn_verify, {
    url <- paste0(url_veri, input$smscode, "/", UID)
    r <- httr::GET(url, head_authy)
    if( httr::status_code(r) != 200 ){
      shinyalert("Oops!", "Login Failed", type = "error")
    }else{
      removeModal()
    }
  })
}

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
}

# Run the application
shinyApp(ui = ui, server = server)

# 
# library(shiny)
# 
# base_url <- "https://icecube.hpds.network"
# auth_api <- "/ocpu/user/bobbyf/library/icecube/R/login"
# 
# 
# 
# ui <- fluidPage(
#   fluidRow(
#     column(
#     width = 4,
#     br(),
#     wellPanel(
#       h4("Login Credentials"), 
#       textInput("usr", "Username", width = "100%"), 
#       passwordInput("pwd", "Password", width = "100%"),
#       actionButton("go", "Submit")
#     ),
#     shiny::u
#   ),
#   ),
#   br(),
#   fluidRow(column(
#     width = 10, 
#     h4("Request Parameters"), 
#     wellPanel(
#       h6("Request Url:"), 
#       verbatimTextOutput("request_url", placeholder = TRUE),
#       br(),
#       h6("Encrypted Username & Password:"),
#       textInput("ciph_usr", label = NULL),
#       textInput("ciph_pwd", label = NULL)
#     )
#   ))
# )
# 
# server <- function(input, output, session) {
#   
#   rUserVerified <- eventReactive(input$go, {
#     
#     hpds_encrypt <- function(msg){
#       hpds_pubx <- "082eba3252b0dea4f85273a2cf0cca31077d57dca155a2c63ead3b3b0b9bf734"
#       pub <- sodium::hex2bin(hpds_pubx)
#       raw <- protolite::serialize_pb(msg)
#       ciph <- sodium::simple_encrypt(raw, pub)
#       sodium::bin2hex(ciph)
#     }
#     
#     usrciph <- hpds_encrypt(input$usr)
#     pwdciph <- hpds_encrypt(input$pwd)
#     
#     updateTextInput(session = session, inputId = "ciph_usr", value = usrciph)
#     updateTextInput(session = session, inputId = "ciph_pwd", value = pwdciph)
#     
#     body <- list("user" = paste0("'", usrciph, "'"), 
#                  "pwd"  = paste0("'", pwdciph, "'"))
#     
#     # resp   <- httr::POST(paste0(base_url, auth_api), body = body)
#     # parsed <- stringr::str_split(httr::content(resp, "text", encoding = "UTF-8"), "\\n")[[1]][1]
#     # 
#     # if(httr::status_code(resp) == 201){
#     #   resp   <- httr::GET(paste0(base_url, parsed))
#     #   parsed <- httr::content(resp, "text", encoding = "UTF-8")
#     # }
#     # return(parsed)
#     
#     return(TRUE)
#   })
#   
#   output$user_verified <- renderUI({
#     if( rUserVerified() )
#       shiny::modalDialog(title = "User Verified")
#   })
#   output$request_url <- renderText(paste0(base_url,auth_api))
#   output$txtoutput   <- renderPrint(reactResult())
#   output$parseoutput <- renderText(deparse(jsonlite::fromJSON(reactResult())))
# }
# 
# 
shinyApp(ui, server)





