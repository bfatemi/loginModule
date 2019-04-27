library(shiny)
library(httr)
library(shinyalert)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)


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
              
              conditionalPanel(
                condition = "input.btn_skip_login %2 == 1",
                passwordInput(ns("pwd_2"), NULL, placeholder = "Confirm Password"),
                textInput(ns("usr"), NULL, placeholder = "First Name"),
                textInput(ns("usr"), NULL, placeholder = "Last Name"),
                textInput(ns("usr"), NULL, placeholder = "Email"),
                fluidRow(
                  column(3, helpText("Mobile (#):")),
                  column(3, textInput(ns("phone_area"), NULL, placeholder = " ### ")),
                  column(3, textInput(ns("phone_ddd"), NULL, placeholder = " ### ")),
                  column(3, textInput(ns("phone_dddd"), NULL, placeholder = " #### "))
                ),
                ns = ns
              ),
              
              materialSwitch(inputId = ns("check_remember"),
                             label = "Remember me",
                             value = TRUE,
                             status = "primary")),
            column(
              width = 12,
              actionBttn(
                inputId = ns("btn_send_login"),
                         label = "NEXT",
                         style = "material-flat",
                         color = "danger"
                ),
              actionBttn(
                inputId = ns("btn_skip_login"),
                         label = "GUEST",
                         style = "material-flat",
                         color = "primary"
                )
              )
          )
        )
      )
    ))
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
  observeEvent(input$btn_send_login, {
    
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