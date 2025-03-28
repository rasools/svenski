#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

Sys.setenv(RETICULATE_PYTHON = "/Users/rasools/miniconda3/envs/svenski/bin/python")
library(reticulate)
library(spacyr)
library(shiny)
library(DT)

# Initialize the spacyr with the Swedish model
spacy_initialize(model = "sv_core_news_md")

# Define the User Interface
ui <- fluidPage(
  titlePanel("Swedish POS Tagging using SpaCy"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text", "Enter Swedish text", "Skriv din text här...", rows = 5),
      actionButton("go", "Analyze Text")
    ),
    mainPanel(
      DTOutput("table")  # DTOutput is part of the DT package
    )
  )
)

# Define the Server Logic
server <- function(input, output, session) {
  results <- eventReactive(input$go, {
    req(input$text)  # ensure there's text input before processing
    spacy_parse(input$text, lemma = TRUE, pos = TRUE)
  })
  
  output$table <- renderDT({
    req(results())  # ensure results are computed before rendering the table
    results()
  }, options = list(pageLength = 10))  # show 10 entries per page by default
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
