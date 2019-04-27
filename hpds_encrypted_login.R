# url_protocol, 
# url_hostname, 
# url_port, 
# url_pathname, 
# url_search, 
# url_hash_initial 
# url_hash 

#can be used to get the components of the URL that was requested by the browser to load the Shiny app page. 
#These values are from the browser's perspective, so neither HTTP proxies nor Shiny Server will affect these values. 
#The url_search value may be used with parseQueryString to access query string parameters.

library(shiny)

base_url <- "https://icecube.hpds.network"
auth_api <- "/ocpu/user/bobbyf/library/icecube/R/login"



ui <- fluidPage(
  fluidRow(column(
    width = 4,
    br(),
    h4("Login Credentials"), 
    wellPanel(
      textInput("usr", "Username", width = "100%"), 
      passwordInput("pwd", "Password", width = "100%"),
      actionButton("go", "Submit")
    )
  )),
  br(),
  fluidRow(column(
    width = 10, 
    h4("Request Parameters"), 
    wellPanel(
      h6("Request Url:"), 
      verbatimTextOutput("request_url", placeholder = TRUE),
      br(),
      h6("Encrypted Username & Password:"),
      textInput("ciph_usr", label = NULL),
      textInput("ciph_pwd", label = NULL)
    )
  ),
  column(
    width = 8, 
    br(),
    h4("Server Response"),
    wellPanel(
      h6("Response Json:"), 
      verbatimTextOutput("txtoutput", placeholder = TRUE),
      h6("Response Parsed:"),
      verbatimTextOutput("parseoutput", placeholder = TRUE),
      br()
    )
  ))
)

server <- function(input, output, session) {
  
  rUserVerified <- eventReactive(input$go, {
    
    hpds_encrypt <- function(msg){
      hpds_pubx <- "082eba3252b0dea4f85273a2cf0cca31077d57dca155a2c63ead3b3b0b9bf734"
      pub <- sodium::hex2bin(hpds_pubx)
      raw <- protolite::serialize_pb(msg)
      ciph <- sodium::simple_encrypt(raw, pub)
      sodium::bin2hex(ciph)
    }

    usrciph <- hpds_encrypt(input$usr)
    pwdciph <- hpds_encrypt(input$pwd)
    
    updateTextInput(session = session, inputId = "ciph_usr", value = usrciph)
    updateTextInput(session = session, inputId = "ciph_pwd", value = pwdciph)
    
    body <- list("user" = paste0("'", usrciph, "'"), 
                 "pwd"  = paste0("'", pwdciph, "'"))
    
    return(TRUE)
  })
  
  output$user_verified <- renderUI({
    if( rUserVerified() )
      shiny::modalDialog(title = "User Verified")
  })
  output$request_url <- renderText(paste0(base_url,auth_api))
  output$txtoutput   <- renderPrint(reactResult())
  output$parseoutput <- renderText(deparse(jsonlite::fromJSON(reactResult())))
}

shinyApp(ui, server)




