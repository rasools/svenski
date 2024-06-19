library(shiny)
library(reticulate)
library(spacyr)
library(DT)
library(httr)
library(jsonlite)
library(base64enc)
library(magick)
library(shinythemes)
library(shinyjs)

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
  values <- reactiveValues(apiKey = NULL, results = NULL, image = NULL, audio = list(), pic_url = NULL, gptModel = "gpt-3.5-turbo", dalleModel = "dall-e-3", ttsModel = "tts-1")
  
  # Function to query ChatGPT API
  chatGPT <- function(prompt, modelName, temperature = 1, apiKey) {
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
      content(response)$choices[[1]]$message$content
    }, error = function(e) {
      print(paste("Error in chatGPT:", e$message))
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
        model = values$dalleModel,
        prompt = prompt,
        n = 1,
        size = "1024x1024"
      )
    )
    if (http_status(response)$category != "Success") {
      print(content(response, "text"))
      stop_for_status(response)
    }
    content(response)$data[[1]]$url
  }
  
  # Function to call OpenAI TTS API and save the response as an MP3 file
  openai_tts <- function(text, model, voice = "alloy", api_key) {
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
      print(content(response, as = "text"))
      stop("Request failed: ", content(response, as = "text"))
    }
    
    content(response, as = "raw")
  }
  
  # Function to add a note to Anki using AnkiConnect
  addNoteToAnki <- function(front, back, deck = "Default", model = "Basic") {
    note <- list(
      deckName = deck,
      modelName = model,
      fields = list(
        Front = front,
        Back = back
      ),
      tags = list("auto-generated")
    )
    
    response <- POST(
      url = "http://localhost:8765",
      encode = "json",
      body = toJSON(list(action = "addNote", version = 6, params = list(note = note)))
    )
    
    if (http_status(response)$category != "Success") {
      print(content(response, "text"))
      stop_for_status(response)
    }
    
    content(response)$result
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
      performAnalysis(input$text, values$gptModel, values$apiKey)
    }
  })
  
  observeEvent(input$save, {
    req(input$apiKeyInput)
    values$apiKey <- input$apiKeyInput
    removeModal()
    performAnalysis(input$text, values$gptModel, values$apiKey)
  })
  
  observeEvent(input$generateImage, {
    req(values$results)
    example_row <- which(grepl("example sentence", values$results$Property))[1]
    if (!is.na(example_row)) {
      example_sentence <- values$results$Details[example_row]
    } else {
      example_sentence <- NULL
    }
    
    example_sentence <- gsub("\\(.*\\)", "", example_sentence)
    example_sentence <- trimws(example_sentence)
    
    if (is.null(example_sentence) || nchar(example_sentence) < 10) {
      showNotification("No valid example sentence found for generating image.", type = "error")
      return()
    }
    
    print(paste("Prompt for DALL-E:", example_sentence))
    
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
    
    audio_files <- lapply(seq_len(nrow(values$results)), function(i) {
      text_to_read <- values$results$Details[i]
      openai_tts(text_to_read, values$ttsModel, api_key = values$apiKey)
    })
    
    values$audio <- audio_files
    
    values$results$Audio <- sapply(seq_len(length(audio_files)), function(i) {
      audio_content <- audio_files[[i]]
      audio_src <- base64enc::base64encode(audio_content)
      as.character(tags$div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        tags$audio(src = paste0("data:audio/mp3;base64,", audio_src), type = "audio/mp3", controls = TRUE),
        actionButton(paste0("deleteAudio_", i), "Delete", class = "btn btn-danger btn-sm", onclick = sprintf('Shiny.setInputValue("deleteAudio", %d)', i))
      ))
    })
    
    showNotification("Audio files for each row have been generated.", type = "message")
  })
  
  observeEvent(input$deleteAudio, {
    req(input$deleteAudio)
    index <- as.integer(input$deleteAudio)
    values$audio[[index]] <- NULL
    values$results$Audio[index] <- ""
    
    output$resultsTable <- renderDataTable({
      req(values$results)
      datatable(values$results, escape = FALSE, extensions = 'Buttons', options = list(
        dom = 't',
        paging = FALSE,
        info = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(targets = '_all', className = 'dt-center')
        )
      ))
    })
  })
  
  observeEvent(input$createAnkiCard, {
    req(values$results)
    
    front <- values$results$Property
    back <- values$results$Details
    
    tryCatch({
      addNoteToAnki(front, back)
      showNotification("Anki card created successfully.", type = "message")
    }, error = function(e) {
      showNotification("Failed to create Anki card. Please check Anki and AnkiConnect setup.", type = "error")
    })
  })
  
  performAnalysis <- function(txt, modelName, apiKey) {
    values$image <- NULL
    values$audio <- list()
    values$pic_url <- NULL
    consolidatedtxt <- entity_consolidate(spacy_parse(txt, lemma = FALSE, entity = TRUE, nounphrase = TRUE))
    if (length(consolidatedtxt$token) > 1) {
      prompt <- createPrompt(txt, "phrase", consolidatedtxt)
      response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
      if (is.null(response)) {
        return()
      }
      values$results <- parseResults(response)
    }
    else {
      prompt <- createPrompt(consolidatedtxt$token[1], consolidatedtxt$pos[1], consolidatedtxt)
      response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
      if (is.null(response)) {
        return()
      }
      values$results <- parseResults(response)
      res <- parseResults(response)
      
      if (consolidatedtxt$pos[1] == "VERB") {
        prompt <- createPrompt(res$Details[5], consolidatedtxt$pos[1], consolidatedtxt)
        response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
        if (is.null(response)) {
          return()
        }
        values$results <- parseResults(response)
      }
      
      else if (consolidatedtxt$pos[1] == "NOUN") {
        prompt <- createPrompt(res$Details[5], consolidatedtxt$pos[1], consolidatedtxt)
        response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
        if (is.null(response)) {
          return()
        }
        values$results <- parseResults(response)
      }
      
      else if (consolidatedtxt$pos[1] == "ADJ") {
        prompt <- createPrompt(res$Details[7], consolidatedtxt$pos[1], consolidatedtxt)
        response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
        if (is.null(response)) {
          return()
        }
        values$results <- parseResults(response)
      }
      
      else if (consolidatedtxt$pos[1] == "PRON") {
        prompt <- createPrompt(consolidatedtxt$token[1], consolidatedtxt$pos[1], consolidatedtxt)
        response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
        if (is.null(response)) {
          return()
        }
        values$results <- parseResults(response)
      }
      output$result <- NULL
    }
    print(prompt)
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
            table_string, "For example sentences in the table, please provide sentences with atleast 15 words in each sentence, and put the meaning in English in the end of the sentence." ,sep = "\n")
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
    data.frame(Property = keys, Details = values, stringsAsFactors = FALSE)
  }
  
  output$resultsTable <- renderDataTable({
    req(values$results)
    
    datatable(values$results, escape = FALSE, extensions = 'Buttons', options = list(
      dom = 't',
      paging = FALSE,
      info = FALSE,
      ordering = FALSE,
      columnDefs = list(
        list(targets = '_all', className = 'dt-center')
      )
    ))
  })
  
  output$image <- renderUI({
    req(values$image)
    tagList(
      tags$div(style = "margin-top: 20px;", 
               tags$img(src = values$image, height = "512px", width = "512px")
      ),
      tags$div(style = "margin-top: 20px;",
               actionButton("removePicture", "Remove Picture")
      )
    )
  })
  
  observe({
    req(values$results)
    output$generateButtons <- renderUI({
      tagList(
        actionButton("readExamples", "Generate Audio", style = "margin-bottom: 20px; margin-top: 20px;"),
        actionButton("createAnkiCard", "Create Anki Card", style = "margin-bottom: 20px;"),
        div(class = "card", style = "margin-top: 20px; padding: 20px; box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1); border-radius: 10px;",
            h3("Generate Image"),
            actionButton("generateImage", "Generate Picture"),
            uiOutput("image")
        )
      )
    })
  })
  
  observeEvent(input$settings, {
    showModal(modalDialog(
      title = "Settings",
      passwordInput("apiKeyInput", "Enter API Key:", value = values$apiKey),
      selectInput("gptModelInput", "Choose ChatGPT Model:",
                  choices = c("gpt-4" = "gpt-4", "gpt-3.5-turbo" = "gpt-3.5-turbo"),
                  selected = values$gptModel),
      selectInput("dalleModelInput", "Choose DALL-E Model:",
                  choices = c("dall-e-2" = "dall-e-2", "dall-e-3" = "dall-e-3"),
                  selected = values$dalleModel),
      selectInput("ttsModelInput", "Choose TTS Model:",
                  choices = c("tts-1" = "tts-1", "tts-2" = "tts-2"),
                  selected = values$ttsModel),
      footer = tagList(
        modalButton("Close"),
        actionButton("saveSettings", "Save Settings")
      )
    ))
  })
  
  observeEvent(input$saveSettings, {
    values$apiKey <- input$apiKeyInput
    values$gptModel <- input$gptModelInput
    values$dalleModel <- input$dalleModelInput
    values$ttsModel <- input$ttsModelInput
    removeModal()
  })
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
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
      actionButton("submit", "Submit"),
      tags$div(
        style = "text-align: right; margin-top: 15px;",
        tags$a(
          icon("cog", class = "fa-2x", style = "color: white;"),
          href = "#",
          id = "settings"
        )
      )
    ),
    mainPanel(
      class = "main-panel",
      DTOutput("resultsTable"),
      uiOutput("generateButtons")
    )
  ),
  tags$script(HTML("
    $(document).on('click', '#settings', function() {
      Shiny.setInputValue('settings', Math.random());
    });
  "))
)

shinyApp(ui = ui, server = server)
