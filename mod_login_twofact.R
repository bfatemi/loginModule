library(shiny)
library(httr)
library(shinyalert)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)

hpdsLogin <- function(usr, pwd){
  burl <- "https://icecube.hpds.network"
  api  <- "/ocpu/user/bobbyf/library/icecube/R/login"
  pubx <- "082eba3252b0dea4f85273a2cf0cca31077d57dca155a2c63ead3b3b0b9bf734"
  
  encrypt <- function(msg){
    bin <- protolite::serialize_pb(msg)
    raw <- sodium::hex2bin(pubx)
    ciph <- sodium::simple_encrypt(bin, raw)
    sodium::bin2hex(ciph)
  }
  
  body <- list(
    user = paste0("'", encrypt(usr), "'"), 
    pwd  = paste0("'", encrypt(pwd), "'")
  )
  r <- httr::POST(url = paste0(burl, api), body = body)
  httr::stop_for_status(r)
  
  rel <- stringr::str_split(httr::content(r, "text", encoding = "UTF-8"),"\\n")[[1]][1]
  r <- httr::GET(url = paste0(burl, rel))
  parsed <- httr::content(r, "text", encoding = "UTF-8")
  ull <- deparse(jsonlite::fromJSON(parsed)) # list(UID = "145315361")
  # return(ull)
  return(list(UID = "145315361"))
}



head_authy <- httr::add_headers(`X-Authy-API-Key` = "4b1xZhqqox7T6yKcrIH9kPBZAY40sNom")
url_rotp <- "https://api.authy.com/protected/json/sms/"
url_veri <- "https://api.authy.com/protected/json/verify/"


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
                             status = "primary")),
            column(
              width = 12,
              actionBttn(inputId = ns("btn_login"),
                         label = "Login",
                         style = "material-flat",
                         color = "danger"),
              actionBttn(inputId = ns("register_new"),
                         label = "Guest",
                         style = "material-flat",
                         color = "primary"))
          )
        )
      )
    ))
}


# Module server function
loginModule <- function(input, output, session) {

  modalTwoFactUI <- function(){
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
    
    UID <<- hpdsLogin(input$usr, input$pwd) 
    
    r <- httr::GET(paste0(url_rotp, UID), head_authy)
    if( httr::status_code(r) != 200 ){
      shinyalert("Oops!", "Login Failed", type = "error")
    }else{
      showModal(modalTwoFactUI())
    } 
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