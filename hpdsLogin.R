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


hpds_encrypt <- function(msg){
  hpds_pubx <- "082eba3252b0dea4f85273a2cf0cca31077d57dca155a2c63ead3b3b0b9bf734"
  pub <- sodium::hex2bin(hpds_pubx)
  raw <- protolite::serialize_pb(msg)
  ciph <- sodium::simple_encrypt(raw, pub)
  sodium::bin2hex(ciph)
}

ui <- fluidPage(
  fluidRow(
    column(width = 4,
           br(),
           h4("Login Credentials"), 
           wellPanel(
             textInput("usr", "Username", width = "100%"), 
             passwordInput("pwd", "Password", width = "100%"),
             actionButton("go", "Submit")
           ))
  ),
  
  
  br(),
  fluidRow(
    column(width = 10, 
           h4("Request Parameters"), 
           wellPanel(
             h6("Request Url:"), 
             verbatimTextOutput("request_url", placeholder = TRUE),
             
             br(),
             
             h6("Encrypted Username & Password:"),
             textInput("ciph_usr", label = NULL),
             textInput("ciph_pwd", label = NULL)
           )),
    column(width = 8, 
           br(),
           h4("Server Response"),
           wellPanel(
             h6("Response Json:"), 
             verbatimTextOutput("txtoutput", placeholder = TRUE),
             h6("Response Parsed:"),
             verbatimTextOutput("parseoutput", placeholder = TRUE),
             br()
           ))
  )
)

server <- function(input, output, session) {
  
  base_url <- "https://icecube.hpds.network"
  api <- "/ocpu/user/bobbyf/library/icecube/R/login"
  
  reactResult <- eventReactive(input$go, {
    
    usrciph <- hpds_encrypt(input$usr)
    pwdciph <- hpds_encrypt(input$pwd)
    
    updateTextInput(session = session, inputId = "ciph_usr", value = usrciph)
    updateTextInput(session = session, inputId = "ciph_pwd", value = pwdciph)
    
    body <- list(
      user = paste0("'", usrciph, "'"), 
      pwd  = paste0("'", pwdciph, "'")
    )
    
    resp   <- httr::POST(paste0(base_url, api), body = body)
    parsed <- httr::content(resp, "text", encoding = "UTF-8")
    txt    <- stringr::str_split(parsed, "\\n")[[1]][1]
    
    if(httr::status_code(resp) == 201){
      
      resp   <- httr::GET(paste0(base_url, txt))
      parsed <- httr::content(resp, "text", encoding = "UTF-8")
      return(parsed)
    }
    return(txt)
  })
  output$request_url <- renderText(paste0(base_url,api))
  output$txtoutput <- renderPrint(reactResult())
  output$parseoutput <- renderText(deparse(jsonlite::fromJSON(reactResult())))
}

shinyApp(ui, server)