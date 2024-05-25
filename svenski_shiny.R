library(shiny)
library(reticulate)
library(spacyr)
library(DT)
library(httr)
library(jsonlite)

# Setting up Python environment
Sys.setenv(RETICULATE_PYTHON = "/Users/rasools/miniconda3/envs/svenski/bin/python")
spacy_initialize(model = "sv_core_news_md")

server <- function(input, output, session) {
  values <- reactiveValues(apiKey = NULL)
  
  # Function to query ChatGPT API
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
  
  # Observe the submit button click
  observeEvent(input$submit, {
    req(input$text)
    txt <- input$text
    
    # Check if API key is already saved
    if (is.null(values$apiKey) || values$apiKey == "") {
      # Show modal dialog to ask for API key
      showModal(modalDialog(
        title = "API Key Required",
        textInput("apiKeyInput", "Enter API Key:", value = ""),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save", "Save")
        )
      ))
    } else {
      performAnalysis(txt, values$apiKey)
    }
  })
  
  # Observe save button in modal dialog
  observeEvent(input$save, {
    req(input$apiKeyInput)  # Make sure the API key was entered
    values$apiKey <- input$apiKeyInput
    removeModal()
    performAnalysis(input$text, values$apiKey)  # Call the main analysis function
  })
  
  # Function to perform analysis
  performAnalysis <- function(txt, apiKey) {
    consolidatedtxt <- entity_consolidate(spacy_parse(txt, lemma = FALSE, entity = TRUE, nounphrase = TRUE))
    if (consolidatedtxt$pos[1] == "VERB") {
      prompt <- createPrompt(consolidatedtxt$token[1])
      response <- chatGPT(prompt, apiKey = apiKey)
      values$results <- parseResults(response)
      output$result <- NULL  # clear previous results
    }
  }
  
  createPrompt <- function(verb) {
    # Read supporting file and prepare it for the prompt
    lines <- readLines("temp_tables/verb.txt")
    table_string <- paste(
      lines[which(grepl("Category \\| Details", lines)):which(grepl("Supinum example sentence \\|", lines))],
      collapse = "\n"
    )
    paste(verb, "is a Swedish verb. Please fill in the blanks:",
          table_string, sep = "\n")
  }
  
  parseResults <- function(response) {
    lines <- strsplit(response, "\n")[[1]]
    lines <- lines[-1]
    keys <- sub("\\|.*", "", lines)
    values <- sub(".*\\| ", "", lines)
    data.frame(Property = keys, Details = values, stringsAsFactors = FALSE)
  }
  
  output$resultsTable <- renderDataTable({
    req(values$results)  # ensure there's data to display
    
    datatable(values$results, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'copy',
          text = 'Copy to clipboard',
          exportOptions = list(
            modifier = list(page = 'all'),  # ensure all data is copied, not just what's visible
            columns = 1:(ncol(values$results)),  # allows copying only visible columns if some are toggled off
            stripHtml = FALSE,  # stops the copying from stripping out HTML elements if any
            stripNewlines = FALSE,  # prevents stripping newlines
#            format = list(header = FALSE),
            columns_titles = FALSE  # this option does not exist but explains the conceptual need
          )
        ),
        'csv', 'excel', 'pdf', 'print'  # other buttons
      ),
      pageLength = -1  # show all rows by default
    ))
  })
}

ui <- fluidPage(
  titlePanel("Swedish Verb Anki Card Helper"),
  sidebarLayout(
    sidebarPanel(
      textInput("text", "Enter a Swedish verb:", value = ""),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      DTOutput("resultsTable")
    )
  )
)

shinyApp(ui = ui, server = server)