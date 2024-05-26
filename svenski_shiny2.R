library(shiny)
library(reticulate)
library(spacyr)
library(httr)
library(jsonlite)
library(shinyjs) # Add this library

# Setting up Python environment
Sys.setenv(RETICULATE_PYTHON = "/Users/rasools/miniconda3/envs/svenski/bin/python")
spacy_initialize(model = "sv_core_news_md")

ui <- fluidPage(
  shinyjs::useShinyjs(), # Initialize shinyjs
  titlePanel("Svenski: Your Swedish Anki assistant"),
  sidebarLayout(
    sidebarPanel(
      textInput("text", "Enter a Swedish word:", value = ""),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      actionButton("copyBtn", "Copy to Clipboard"),
      verbatimTextOutput("resultText")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$submit, {
    req(input$text)
    # Simulate some processing and generating a response
    word_analysis <- paste("Analysis result for:", input$text)
    output$resultText <- renderText({ word_analysis })
  })
  
  observeEvent(input$copyBtn, {
    shinyjs::runjs('navigator.clipboard.writeText($("#resultText").text())')
  })
}

shinyApp(ui = ui, server = server)