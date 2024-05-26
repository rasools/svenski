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
#  chatGPT <- function(prompt, modelName = "gpt-4o", temperature = 1, apiKey) {
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
    print(consolidatedtxt)
    prompt <- createPrompt(consolidatedtxt$token[1], consolidatedtxt$pos[1], consolidatedtxt)
    response <- chatGPT(prompt, apiKey = apiKey)
    values$results <- parseResults(response)
    res <- parseResults(response)
    print(res)
    
    # verb
    if (consolidatedtxt$pos[1] == "VERB") {
      prompt <- createPrompt(res$Details[5], consolidatedtxt$pos[1], consolidatedtxt)
      response <- chatGPT(prompt, apiKey = apiKey)
      values$results <- parseResults(response)
    }
    
    # noun
    else if (consolidatedtxt$pos[1] == "NOUN") {
      prompt <- createPrompt(res$Details[5], consolidatedtxt$pos[1], consolidatedtxt)
      response <- chatGPT(prompt, apiKey = apiKey)
      values$results <- parseResults(response)
    }
    
    # adjective
    else if (consolidatedtxt$pos[1] == "ADJ") {
      prompt <- createPrompt(res$Details[5], consolidatedtxt$pos[1], consolidatedtxt)
      response <- chatGPT(prompt, apiKey = apiKey)
      values$results <- parseResults(response)
    }
    
    # pronoun
    else if (consolidatedtxt$pos[1] == "PRON") {
      prompt <- createPrompt(consolidatedtxt$token[1], consolidatedtxt$pos[1], consolidatedtxt)
      response <- chatGPT(prompt, apiKey = apiKey)
      values$results <- parseResults(response)
    }

    output$result <- NULL  # clear previous results
  }
  
  # createPrompt
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
    lines <- lines[-1]  # remove the first line which is the prompt
    lines <- lines[-length(lines)]  # remove the last line which is the user's response
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
  titlePanel("Svenski: Your Swedish Anki assistant"),
  sidebarLayout(
    sidebarPanel(
      textInput("text", "Enter a Swedish word / phrase:", value = ""),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      DTOutput("resultsTable")
    )
  )
)

shinyApp(ui = ui, server = server)