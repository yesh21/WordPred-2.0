library(shiny)
library(dplyr)
library(stringr)
library(shinyWidgets)
# Define UI for application that runs a word predictor

shinyUI(fluidPage(theme = "input.css",
    
                  titlePanel(h1("WORD PREDICTOR", style="position: fixed;top: 20%;text-align: center;  
color: white;
  text-shadow: 12px 12px 24px grey;
  "), windowTitle= "Word Predictor"
),
    fluidRow(
      
      
 
      
                 textInput("inputString",  value = "have a",label = "Text:"),  
                            
                      uiOutput("show"),
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"
                 ),
                 
                 icon("calendar")
                 
                  )





    )
)






