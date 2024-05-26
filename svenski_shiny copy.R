library(shiny)
library(reticulate)
library(spacyr)
library(httr)
library(jsonlite)

# Setting up Python environment
Sys.setenv(RETICULATE_PYTHON = "/Users/rasools/miniconda3/envs/svenski/bin/python")
spacy_initialize(model = "sv_core_news_md")

ui <- fluidPage(
  titlePanel("Svenski: Your Swedish Anki assistant"),
  sidebarLayout(
    sidebarPanel(
      textInput("text", "Enter a Swedish word:", value = ""),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      verbatimTextOutput("resultsDisplay")  # Changed from DTOutput to verbatimTextOutput
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(apiKey = NULL)
  
  chatGPT <- function(prompt, modelName = "gpt-3.5-turbo", temperature = 1, apiKey) {
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        model = modelName,
        temperature = temperature,
        messages = list(list(role = "user", content = prompt))
      )
    )
    stop_for_status(response)  # will throw an error if the request failed
    trimws(content(response)$choices[[1]]$message$content)
  }
  
  observeEvent(input$submit, {
    req(input$text)
    txt <- input$text
    
    if (is.null(values$apiKey) || values$apiKey == "") {
      showModal(modalDialog(
        title = "API Key Required",
        textInput("apiKeyInput", "Enter API Key:", value = ""),
        footer = modalButton("Save")
      ))
    } else {
      performAnalysis(txt, values$apiKey)
    }
  })
  
  observeEvent(input$apiKeyInput, {
    req(input$apiKeyInput)
    values$apiKey <- input$apiKeyInput
    performAnalysis(input$text, values$apiKey)
  })
  
  performAnalysis <- function(txt, apiKey) {
    consolidatedtxt <- entity_consolidate(spacy_parse(txt, lemma = FALSE, entity = TRUE, nounphrase = TRUE))
    prompt <- createPrompt(consolidatedtxt$token[1], consolidatedtxt$pos[1], consolidatedtxt)
    response <- chatGPT(prompt, apiKey = apiKey)
    values$results <- parseResults(response)
  }
  
  createPrompt <- function(verb, pos, consolidatedtxt) {
    path_template <- sprintf("temp_tables/%s.txt", tolower(pos))
    if (file.exists(path_template)) {
      lines <- readLines(path_template)
      table_string <- paste(
        lines[which(grepl("Category \\| Details", lines)):which(lines == "End |")],
        collapse = "\n"
      )
      paste(verb, sprintf("is a Swedish %s. Please fill in the blanks:", tolower(pos)),
            table_string, sep = "\n")
    } else { 
      paste("No template found for", pos)
    }
  }
  
  parseResults <- function(response) {
    lines <- strsplit(response, "\n")[[1]]
    lines <- lines[-1]
    lines <- lines[-length(lines)]
    keys <- sub("\\|.*", "", lines)
    values <- sub(".*\\| ", "", lines)
    result_string <- paste(keys, ":", values, collapse = "\n")
    result_string
  }
  
  output$resultsDisplay <- renderText({
    req(values$results)
    values$results
  })
}

shinyApp(ui = ui, server = server)