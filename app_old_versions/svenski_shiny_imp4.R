library(shiny)
library(reticulate)
library(spacyr)
library(DT)
library(httr)
library(jsonlite)
library(base64enc)
library(magick)  # Added for image manipulation
library(shinythemes)  # Added for themes

# Setting up Python environment
Sys.setenv(RETICULATE_PYTHON = "/Users/rasools/miniconda3/envs/svenski/bin/python")
spacy_initialize(model = "sv_core_news_md")

# Create 'tts' and 'pics' directories if they don't exist
if (!dir.exists("tts")) {
  dir.create("tts")
}
if (!dir.exists("pics")) {
  dir.create("pics")
}

server <- function(input, output, session) {
  values <- reactiveValues(apiKey = NULL, results = NULL, image = NULL, audio = list(), pic_url = NULL)
  
  # Function to query ChatGPT API
  chatGPT <- function(prompt, modelName = "gpt-3.5-turbo", temperature = 1, apiKey) {
    tryCatch({
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
    }, error = function(e) {
      print(paste("Error in chatGPT:", e$message))  # Debug: Print the error message
      showNotification("Failed to get response from ChatGPT. Please check the prompt and try again.", type = "error")
      NULL
    })
  }
  
  # Function to query DALL-E API
  dallE <- function(prompt, apiKey) {
    response <- POST(
      url = "https://api.openai.com/v1/images/generations",
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "dall-e-3",
        prompt = prompt,
        n = 1,
        size = "1024x1024"  # Updated size to a supported value
      )
    )
    if (http_status(response)$category != "Success") {
      print(content(response, "text"))
      stop_for_status(response)
    }
    content(response)$data[[1]]$url
  }
  
  # Function to save and compress image locally
  save_image <- function(url, file_path) {
    temp_file <- tempfile(fileext = ".png")
    GET(url, write_disk(temp_file, overwrite = TRUE))
    
    image <- image_read(temp_file)
    image <- image_scale(image, "512x512")  # Resize image to 512x512
    image <- image_convert(image, format = "jpeg")  # Convert to JPEG
    image <- image_write(image, path = file_path, quality = 70)  # Save with quality to reduce size
  }
  
  # Function to call OpenAI TTS API and save the response as an MP3 file
  openai_tts <- function(text, model = "tts-1", voice = "alloy", api_key) {
    url <- "https://api.openai.com/v1/audio/speech"
    
    headers <- add_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    )
    
    body <- list(
      model = model,
      voice = voice,
      input = text
    )
    
    response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
    
    if (status_code(response) != 200) {
      print(content(response, as = "text"))  # Debug: Print the error message from the response
      stop("Request failed: ", content(response, as = "text"))
    }
    
    content(response, as = "raw")
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
  
  observeEvent(input$generateImage, {
    req(values$results)
    example_row <- which(grepl("example sentence", values$results$Property))[1]
    if (!is.na(example_row)) {
      example_sentence <- values$results$Details[example_row]
    } else {
      example_sentence <- NULL
    }
    
    # Remove translation in parentheses for the DALL-E prompt
    example_sentence <- gsub("\\(.*\\)", "", example_sentence)
    example_sentence <- trimws(example_sentence)
    
    if (is.null(example_sentence) || nchar(example_sentence) < 10) {
      showNotification("No valid example sentence found for generating image.", type = "error")
      return()
    }
    
    print(paste("Prompt for DALL-E:", example_sentence))  # Log the final prompt
    
    image_url <- tryCatch({
      dallE(example_sentence, values$apiKey)
    }, error = function(e) {
      showNotification("Failed to generate image. Please check the prompt.", type = "error")
      return(NULL)
    })
    
    if (!is.null(image_url)) {
      values$image <- image_url
      values$pic_url <- image_url  # Store the URL for later saving
    }
  })
  
  observeEvent(input$removePicture, {
    values$image <- NULL
    values$pic_url <- NULL
  })
  
  observeEvent(input$readExamples, {
    req(values$results)
    
    # Generate audio for each row in the table
    audio_files <- lapply(seq_len(nrow(values$results)), function(i) {
      text_to_read <- values$results$Details[i]
      openai_tts(text_to_read, api_key = values$apiKey)
    })
    
    values$audio <- audio_files
    
    # Add audio playbars to the table
    values$results$Audio <- sapply(audio_files, function(audio_content) {
      audio_src <- base64enc::base64encode(audio_content)
      as.character(tags$audio(src = paste0("data:audio/mp3;base64,", audio_src), type = "audio/mp3", controls = TRUE))
    })
    
    showNotification("Audio files for each row have been generated.", type = "message")
  })
  
  # Function to perform analysis
  performAnalysis <- function(txt, modelName, apiKey) {
    values$image <- NULL
    values$audio <- list()
    values$pic_url <- NULL
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
    
    datatable(values$results, escape = FALSE, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'copy',
          text = 'Copy to clipboard',
          exportOptions = list(
            modifier = list(page = 'all'),
            columns = 1:(ncol(values$results) - 1),  # Exclude the audio column from copying
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
  
  output$image <- renderUI({
    req(values$image)
    tagList(
      tags$div(style = "margin-top: 20px;", 
               tags$img(src = values$image, height = "512px", width = "512px")
      ),
      tags$div(style = "margin-top: 20px;",  # Added space between image and button
               actionButton("removePicture", "Remove Picture")
      )
    )
  })
  
  observe({
    req(values$results)
    output$generateButtons <- renderUI({
      tagList(
        div(class = "card", style = "margin-top: 20px; padding: 20px; box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1); border-radius: 10px;",
            h3("Generate Audio"),
            actionButton("readExamples", "Generate Audio")
        ),
        div(class = "card", style = "margin-top: 20px; padding: 20px; box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1); border-radius: 10px;",
            h3("Generate Image"),
            actionButton("generateImage", "Generate Picture"),
            uiOutput("image")
        )
      )
    })
  })
}

ui <- fluidPage(
  theme = shinytheme("flatly"),  # Added theme
  tags$head(tags$style(HTML("
    .shiny-input-container {
      margin-bottom: 15px;
    }
    .btn {
      margin-right: 10px;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      padding: 0;
    }
    .main-panel {
      background-color: #f8f9fa;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
    }
    .sidebar-panel {
      background-color: #343a40;
      color: #fff;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
    }
    .card {
      background-color: #ffffff;
      border-radius: 10px;
      box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
      padding: 20px;
      margin-top: 20px;
    }
  "))),
  titlePanel("Svenski: Din Anki-assistent för att Lära Dig Svenska!"),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      textInput("text", "Enter a Swedish word / phrase:", value = ""),
      selectInput("modelSelect", "Choose Model:",
                  choices = c("gpt-4o" = "gpt-4o", 
                              "gpt-3.5-turbo" = "gpt-3.5-turbo"),
                  selected = "gpt-3.5-turbo"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      class = "main-panel",
      DTOutput("resultsTable"),
      uiOutput("generateButtons")
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
