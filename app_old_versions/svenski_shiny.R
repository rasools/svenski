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
    stop_for_status(response) 
    trimws(content(response)$choices[[1]]$message$content)
  }
  
  observeEvent(input$submit, {
    req(input$text)
    if (is.null(values$apiKey) || values$apiKey == "") {
      showModal(modalDialog(
        title = "API Key Required",
        passwordInput("apiKeyInput", "Enter API Key:", value = ""),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save", "Save"),
          tags$p("Your API key will not be saved by the app and will only be used to query the OpenAI API.", style = "font-size: 10px; text-align: left;")
        )
      ))
    } else {
      performAnalysis(input$text, input$modelSelect, values$apiKey)
    }
  })
  
  observeEvent(input$save, {
    req(input$apiKeyInput)
    values$apiKey <- input$apiKeyInput
    removeModal()
    performAnalysis(input$text, input$modelSelect, values$apiKey)
  })
  
  # Function to perform analysis
  performAnalysis <- function(txt, modelName, apiKey) {
    consolidatedtxt <- entity_consolidate(spacy_parse(txt, lemma = FALSE, entity = TRUE, nounphrase = TRUE))
    if (length(consolidatedtxt$token) > 1) {
      prompt <- createPrompt(txt, "phrase", consolidatedtxt)
      response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
      values$results <- parseResults(response)
    }
    else {
      prompt <- createPrompt(consolidatedtxt$token[1], consolidatedtxt$pos[1], consolidatedtxt)
      response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
      values$results <- parseResults(response)
      res <- parseResults(response)

      # verb
      if (consolidatedtxt$pos[1] == "VERB") {
        prompt <- createPrompt(res$Details[5], consolidatedtxt$pos[1], consolidatedtxt)
        response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
        values$results <- parseResults(response)
      }
      
      # noun
      else if (consolidatedtxt$pos[1] == "NOUN") {
        prompt <- createPrompt(res$Details[5], consolidatedtxt$pos[1], consolidatedtxt)
        response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
        values$results <- parseResults(response)
      }
      
      # adjective
      else if (consolidatedtxt$pos[1] == "ADJ") {
        prompt <- createPrompt(res$Details[7], consolidatedtxt$pos[1], consolidatedtxt)
        response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
        values$results <- parseResults(response)
      }
      
      # pronoun
      else if (consolidatedtxt$pos[1] == "PRON") {
        prompt <- createPrompt(consolidatedtxt$token[1], consolidatedtxt$pos[1], consolidatedtxt)
        response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
        values$results <- parseResults(response)
      }
      output$result <- NULL
    }
    print(prompt)
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
            table_string, "For example sentences in the table, please provide sentences with atleast 15 words in each sentence, and put the meaning in English in the end of the sentence." ,sep = "\n")
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
            modifier = list(page = 'all'),
            columns = 1:(ncol(values$results)),
            stripHtml = FALSE,
            stripNewlines = FALSE,
            columns_titles = FALSE
          )
        ),
        'csv', 'excel', 'pdf', 'print'  # other buttons
      ),
      pageLength = -1  # show all rows by default
    ))
  })
}

ui <- fluidPage(
  titlePanel("Svenski: Din Anki-assistent för att Lära Dig Svenska!"),
  sidebarLayout(
    sidebarPanel(
      textInput("text", "Enter a Swedish word / phrase:", value = ""),
      selectInput("modelSelect", "Choose Model:",
                  choices = c("gpt-4o" = "gpt-4o", 
                              "gpt-3.5-turbo" = "gpt-3.5-turbo"),
                  selected = "gpt-3.5-turbo"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      DTOutput("resultsTable")
    )
  ),
  tags$script(HTML("
    $(document).on('keydown', '#text', function(e) {
      if(e.keyCode == 13) {
        $('#submit').click();
      }
    });
  "))
)

shinyApp(ui = ui, server = server)