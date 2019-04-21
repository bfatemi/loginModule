library(shiny)
library(httr)
library(shinyalert)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)

library(sodium)
library(protolite)


authyhead <- httr::add_headers(`X-Authy-API-Key` = "4b1xZhqqox7T6yKcrIH9kPBZAY40sNom")
url_req_totp <- "https://api.authy.com/protected/json/sms/"
url_chk_totp <- "https://api.authy.com/protected/json/verify/"

## 
## HPDS API function that 
##    (1) encrypts creds; 
##    (2) sends to hpds to validate 
##    (3) returns UID returned by HPDS if user has been authenticated
##
hpdsValidUID <- function(user, pwd){
  url <- "https://icecube.hpds.network"
  api <- "/ocpu/user/bobbyf/library/icecube/R/login"
  
  hpds_encrypt <- function(msg){
    hpds_pubx <- "082eba3252b0dea4f85273a2cf0cca31077d57dca155a2c63ead3b3b0b9bf734"
    pub <- sodium::hex2bin(hpds_pubx)
    raw <- protolite::serialize_pb(msg)
    ciph <- sodium::simple_encrypt(raw, pub)
    sodium::bin2hex(ciph)
  }
  
  body <- list(
    "user" = hpds_encrypt(user),
    "pwd" = hpds_encrypt(pwd)
  )
  
  r <- httr::POST(url = paste0(url, api), 
                  body = body, 
                  encode = "json")
  parsed <- content(r, "text", encoding = "UTF-8")
  
  r <- GET(paste0(url, stringr::str_split(parsed, "\\n")[[1]][1]))
  parsed <- content(r, "text", encoding = "UTF-8")
  
  return(parsed)
} 




# Module UI function
loginModuleUI <- function(id, title = "HPDS Platform Access") {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidPage(
      theme = shinytheme("cerulean"),
      useShinyalert(),
      fluidRow(
        column(
          width = 10,
          box( title = column(12, title),
               status = "primary",
               footer = br(),
               column( 
                 width = 12,
                 textInput(ns("usr"), 
                           label = NULL, 
                           placeholder = "Enter Username"),
                 passwordInput(ns("pwd"), 
                               label = NULL, 
                               placeholder = "Enter Password"),
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
               ))
        )
      )
    )
  )
}


# Module server function
loginModule <- function(input, output, session, ...) {
  
  ## function that return UI elements to render to the user
  modalTwoFactUI <- function(){
    
    ns <- session$ns
    modalDialog(
      inputId = ns("ui_two_factor"), 
      
      textInput(inputId = ns("smscode"), "Enter code sent via sms"),
      
      title = "SMS with code sent!", "Press Login to Complete...",
      
      footer = list(
        actionBttn( inputId = ns("btn_verify"),
                    label = "Submit",
                    style = "material-flat",
                    color = "success"),
        
        actionBttn( inputId = ns("btn_goto_register"),
                    label = "Email me",
                    style = "material-flat",
                    color = "royal")
      ), 
      easyClose = TRUE
    )
  }
  
  observeEvent(input$btn_login, {
    
    if(input$usr == "" | input$pwd == ""){
      shinyalert("Oops!", "Enter username/password", type = "error")
    }else{
      
      UID <<- hpdsUID(input$usr, input$pwd)
      url <- paste0(url_req_totp, UID)
      resp <- httr::GET(url, authy_header)
      parsed <- jsonlite::fromJSON(content(resp, "text"))
      msg <- parsed$message
      
      if( httr::status_code(resp) != 200 ){
        shinyalert(title = "Oops!", 
                   text = msg, 
                   type = "error", 
                   showCancelButton = TRUE, 
                   cancelButtonText = "Exit")
      }else{
        showModal( modalTwoFactUI() )
      }
    }
  })
  
  ## user enters sms code and hits verify
  observeEvent(input$btn_verify, {
    
    url <- paste0(url_chk_totp, input$smscode, "/", UID)
    resp <- httr::GET(url, authy_header)
    parsed <- jsonlite::fromJSON(httr::content(resp, "text"))
    msg <- parsed$message
    
    removeModal()
    
    if( httr::status_code(resp) != 200 ){
      shinyalert(title = "Oops!", 
                 text = msg, 
                 type = "error", 
                 showCancelButton = TRUE, 
                 cancelButtonText = "Exit", 
                 callbackR = stopApp)
      
    }else{
      shinyalert("Success!", 
                 "User verified", 
                 type = "success", 
                 showCancelButton = TRUE, 
                 cancelButtonText = "Exit",
                 callbackR = stopApp)
      
    }
  })
}

# 
# uiLoginFail <- function(msg){
#   ns <- session$ns
#   shinyalert("Oops!", msg, type = "error")
# }
# 
# 
# uiLoginFail <- function(msg){
#   ns <- session$ns
#   
# }






