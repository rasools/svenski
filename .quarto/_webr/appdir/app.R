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
            DTOutput("table")
        )
    )
)

# Define the Server Logic
server <- function(input, output, session) {
    results <- eventReactive(input$go, {
        req(input$text)  
        spacy_parse(input$text, lemma = TRUE, pos = TRUE)
    })
    
    output$table <- renderDT({
        req(results())
        results()
    }, options = list(pageLength = 10))
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
