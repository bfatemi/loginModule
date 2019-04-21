

library(shiny)
library(httr)
library(shinyalert)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)


authy_header <- add_headers(
  `X-Authy-API-Key` = "4b1xZhqqox7T6yKcrIH9kPBZAY40sNom"
)
url_req_totp <- "https://api.authy.com/protected/json/sms/"
url_chk_totp <- "https://api.authy.com/protected/json/verify/"

## simulated ocpu ping to verify user
## and receive authy id needed for sms verification
hpdsUID <- function(usr, pwd) return("145315361")


# Module UI function
loginModuleUI <- function(id, title = "HPDS Platform Access") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagll <- tagList(fluidPage(
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
            textInput(ns("usr"), label = NULL, placeholder = "Enter Username"),
            passwordInput(ns("pwd"), label = NULL, placeholder = "Enter Password"),
            materialSwitch(inputId = ns("check_remember"),
                           label = "Remember me",
                           value = TRUE,
                           status = "primary")
          ),
          
          column(
            width = 12,
            actionBttn(inputId = ns("btn_login"),
                       label = "Login",
                       style = "material-flat",
                       color = "danger"),
            actionBttn(inputId = ns("register_new"),
                       label = "Guest",
                       style = "material-flat",
                       color = "primary")
          )
          
        )
      )
    )
  ))
  
  return(tagll)
}


# Module server function
loginModule <- function(input, output, session) {
  
  uiLoginFail <- function(msg){
    ns <- session$ns
    shinyalert("Oops!", msg, type = "error")
  }
  
  
  uiTwoFact <- function(){
    ns <- session$ns
    
    ui <- modalDialog( 
      inputId = ns("ui_two_factor"),
      
      textInput(inputId = ns("smscode"), "Enter code sent via sms"),
      
      title = "SMS with code sent!",
      "Press Login to Complete...",
      
      footer = list(
        actionBttn(
          inputId = ns("btn_verify"),
          label = "Submit",
          style = "material-flat",
          color = "success"
        ),
        actionBttn(
          inputId = ns("btn_goto_register"),
          label = "Email me",
          style = "material-flat",
          color = "royal"
        )
      ), 
      easyClose = TRUE
    )
    
    return(ui)
  }

  observeEvent(input$btn_login, {
    
    # if(input$usr == "" || input$pwd == ""){
    #   showNotification(id = "auth_err",
    #                    ui = paste("Username or password not provided"),
    #                    type = "error",
    #                    duration = 0,
    #                    closeButton = TRUE)
    #   removeNotification("auth_err")
    # 
    # }else{
      authid <- hpdsUID(input$usr, input$pwd)
      resp <- httr::GET(paste0(url_req_totp, authid), authy_header)
      
      if( httr::status_code(resp) != 200 ){
        
        uiLoginFail(msg = "Unable to login with username and password")

      }else{
        showModal( uiTwoFact() )
      }
    # }
  })
  
  ## user enters sms code and hits verify
  observeEvent(input$btn_verify, {
    
    url_verify <- paste0(url_chk_totp, input$smscode, "/", authid)
    resp <- httr::GET(url_verify, authy_header)
    # content(resp, "text")
    if( httr::status_code(resp) != 200 ){
      
      uiLoginFail(msg = "Login Failed")
      
    }else{
      
    }
  })
  
}