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

server <- function(input, output, session) {
  values <- reactiveValues(
    apiKey = NULL,
    results = NULL,
    image = NULL,
    audio = list(),
    pic_url = NULL,
    gptModel = "gpt-3.5-turbo",
    dalleModel = "dall-e-3",
    ttsModel = "tts-1",
    deckName = "Nya ord",
    gptTokens = 0,
    dalleRequests = 0,
    ttsRequests = 0,
    gptCost = 0,
    dalleCost = 0,
    ttsCost = 0
  )
  
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
      result <- content(response)
      values$gptTokens <- values$gptTokens + result$usage$total_tokens
      input_tokens <- result$usage$prompt_tokens
      output_tokens <- result$usage$completion_tokens
      # Updated pricing for ChatGPT
      values$gptCost <- values$gptCost + (input_tokens / 1000) * 0.0005 + (output_tokens / 1000) * 0.0015
      result$choices[[1]]$message$content
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
    result <- content(response)
    values$dalleRequests <- values$dalleRequests + 1
    # Updated pricing for DALL-E
    values$dalleCost <- values$dalleCost + 0.02
    result$data[[1]]$url
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
    
    values$ttsRequests <- values$ttsRequests + 1
    # Calculate duration of audio (in minutes) and updated pricing for Whisper TTS
    audio_duration_minutes <- nchar(text) / 180 # Assuming average speaking rate of 180 characters per minute
    values$ttsCost <- values$ttsCost + audio_duration_minutes * 0.006
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
      # Download and process the image
      temp_image_path <- tempfile(fileext = ".jpg")
      download.file(image_url, temp_image_path, mode = "wb")
      
      img <- image_read(temp_image_path)
      img <- image_scale(img, "512x512")
      img <- image_convert(img, format = "jpeg")
      image_write(img, path = temp_image_path, quality = 70)
      
      # Encode the image in base64
      image_data <- base64enc::base64encode(temp_image_path)
      image_filename <- basename(temp_image_path)
      
      values$image <- list(data = image_data, filename = image_filename)
    }
  })
  
  observeEvent(input$removePicture, {
    values$image <- NULL
  })
  
  observeEvent(input$readExamples, {
    req(values$results)
    
    # Generate audio files only for rows after line 3
    audio_files <- lapply(seq_len(nrow(values$results)), function(i) {
      if (i > 3) {
        text_to_read <- values$results$Details[i]
        # Extract text before parentheses
        text_to_read <- sub("\\s*\\(.*\\)$", "", text_to_read)
        openai_tts(text_to_read, values$ttsModel, api_key = values$apiKey)
      } else {
        NULL
      }
    })
    
    values$audio <- audio_files
    
    values$results$Audio <- sapply(seq_len(length(audio_files)), function(i) {
      if (!is.null(audio_files[[i]])) {
        audio_content <- audio_files[[i]]
        audio_src <- base64enc::base64encode(audio_content)
        as.character(tags$div(
          tags$audio(src = paste0("data:audio/mp3;base64,", audio_src), type = "audio/mp3", controls = TRUE)
        ))
      } else {
        ""
      }
    })
    
    showNotification("Audio files for rows after line 3 have been generated.", type = "message")
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

  performAnalysis <- function(txt, modelName, apiKey) {
    values$image <- NULL
    values$audio <- list()
    values$pic_url <- NULL
    consolidatedtxt <- entity_consolidate(spacy_parse(txt, lemma = FALSE, entity = TRUE, nounphrase = TRUE))
    if (length(consolidatedtxt$token) > 1) {
      prompt <- createPrompt(txt, "phrase", consolidatedtxt)
      response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
      if (is.null(response)) {
        showNotification("Failed to get analysis results. Please try again.", type = "error")
        return()
      }
      values$results <- parseResults(response)
    } else {
      prompt <- createPrompt(consolidatedtxt$token[1], consolidatedtxt$pos[1], consolidatedtxt)
      response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
      if (is.null(response)) {
        showNotification("Failed to get analysis results. Please try again.", type = "error")
        return()
      }
      values$results <- parseResults(response)
      res <- parseResults(response)
      
      if (consolidatedtxt$pos[1] == "VERB") {
        prompt <- createPrompt(res$Details[5], consolidatedtxt$pos[1], consolidatedtxt)
        response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
        if (is.null(response)) {
          showNotification("Failed to get analysis results. Please try again.", type = "error")
          return()
        }
        values$results <- parseResults(response)
      } else if (consolidatedtxt$pos[1] == "NOUN") {
        prompt <- createPrompt(res$Details[5], consolidatedtxt$pos[1], consolidatedtxt)
        response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
        if (is.null(response)) {
          showNotification("Failed to get analysis results. Please try again.", type = "error")
          return()
        }
        values$results <- parseResults(response)
      } else if (consolidatedtxt$pos[1] == "ADJ") {
        prompt <- createPrompt(res$Details[7], consolidatedtxt$pos[1], consolidatedtxt)
        response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
        if (is.null(response)) {
          showNotification("Failed to get analysis results. Please try again.", type = "error")
          return()
        }
        values$results <- parseResults(response)
      } else if (consolidatedtxt$pos[1] == "PRON") {
        prompt <- createPrompt(consolidatedtxt$token[1], consolidatedtxt$pos[1], consolidatedtxt)
        response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
        if (is.null(response)) {
          showNotification("Failed to get analysis results. Please try again.", type = "error")
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
        lines[which(grepl("Category | Details", lines)):which(lines == "End |")],
        collapse = "\n"
      )
      paste(verb, sprintf("is a Swedish %s. Please fill in the blanks:", tolower(pos)),
            table_string, "For example sentences in the table, please provide sentences with atleast 15 words in each sentence, and put the meaning in English in the end of the sentence." ,sep = "\n")
    } else { 
      paste("No template found for", pos)
    }
  }
  
  parseResults <- function(response) {
    # Clean up the response text
    response <- trimws(response)
    lines <- strsplit(response, "\n")[[1]]
    lines <- lines[-1]
    lines <- lines[-length(lines)]
    # remove the first line of the table is empty remove it
    if (lines[1] == "--- | ---") {
      lines <- lines[-1]
    }
    keys <- sub("\\|.*", "", lines)
    values <- sub(".*\\| ", "", lines)
    
    # Create a data frame
    df <- data.frame(Property = keys, Details = values, stringsAsFactors = FALSE)
    
    return(df)
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
               tags$img(src = paste0("data:image/jpeg;base64,", values$image$data), height = "512px", width = "512px")
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
        actionButton("readExamples", "Generate Audio", style = "margin-bottom: 10px;", style = "margin-top: 20px;"),
        div(class = "card", style = "margin-top: 20px; padding: 20px; box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1); border-radius: 10px;",
            h3("Generate Image"),
            actionButton("generateImage", "Generate Picture"),
            uiOutput("image")
        ),
        actionButton("createAnkiCard", "Create Anki Card", class = "btn btn-primary btn-lg", style = "width: 100%; margin-top: 20px; font-weight: bold; font-size: 1.5em;")
      )
    })
  })
  
  observeEvent(input$createAnkiCard, {
    showModal(modalDialog(
      title = "Choose Anki Deck",
      selectInput("deckNameInput", "Choose Deck Name:", choices = values$deckNames, selected = values$deckName),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveDeckName", "Save and Create Card")
      )
    ))
  })
  
  observeEvent(input$saveDeckName, {
    values$deckName <- input$deckNameInput
    removeModal()
    createAnkiCard(input$text, values$results, values$image, values$deckName)
  })
  
  createAnkiCard <- function(word, results, image, deckName) {
    # Extract part of speech
    pos <- results$Details[results$Property == "Part of Speech"]
    pos <- ifelse(length(pos) == 0, "unknown", pos)
    
    # Check if the audio column exists and contains data
    has_audio <- "Audio" %in% names(results) && any(results$Audio != "")
    
    if (has_audio) {
      results$Audio <- gsub("Delete", "", results$Audio)
      audio_elements <- paste0("<br>", results$Audio[4])
      front_content <- paste0(results$Details[4], audio_elements)
    } else {
      front_content <- results$Details[4]
    }
    
    back_content <- paste0(
      if (!is.null(image)) {
        sprintf("<img src='data:image/jpeg;base64,%s' /><br>", image$data)
      } else {
        ""
      },
      paste(
        apply(results, 1, function(row) {
          # Ensure that row[3] exists and is not NA before checking its length
          if (length(row) >= 3 && !is.na(row[3])) {
            row_content <- paste(
              sprintf("<span style='color: lightblue; font-style: italic;'>%s</span>", row[1]),
              row[2],
              sep = "<br>"
            )
            if (nchar(row[3]) > 0) {
              row_content <- paste(row_content, row[3], sep = "<br>")
            }
          } else {
            row_content <- paste(
              sprintf("<span style='color: lightblue; font-style: italic;'>%s</span>", row[1]),
              row[2],
              sep = "<br>"
            )
          }
          row_content
        }),
        collapse = "<br><br>"
      )
    )
    
    fields <- list(
      Front = front_content,
      Back = back_content
    )
    
    note <- list(
      deckName = deckName,
      modelName = "Basic",
      fields = fields,
      tags = list(pos)
    )
    
    response <- httr::POST(
      url = "http://localhost:8765",
      body = toJSON(list(action = "addNote", version = 6, params = list(note = note)), auto_unbox = TRUE),
      encode = "json"
    )
    
    result <- content(response)
    if (is.null(result$error)) {
      showNotification("Anki card created successfully.", type = "message")
    } else {
      showNotification(paste("Failed to create Anki card:", result$error), type = "error")
    }
  }
  
  
  # Retrieve the list of Anki decks
  observe({
    response <- httr::POST(
      url = "http://localhost:8765",
      body = toJSON(list(action = "deckNames", version = 6), auto_unbox = TRUE),
      encode = "json"
    )
    
    result <- content(response)
    if (is.null(result$error)) {
      values$deckNames <- result$result
    } else {
      showNotification(paste("Failed to retrieve Anki decks:", result$error), type = "error")
    }
  })
  
  observeEvent(input$settings, {
    showModal(modalDialog(
      title = "Settings",
      passwordInput("apiKeyInput", "Enter API Key:", value = values$apiKey),
      selectInput("gptModelInput", "Choose ChatGPT Model:",
                  choices = c("gpt-4o" = "gpt-4o", "gpt-4" = "gpt-4", "gpt-3.5-turbo" = "gpt-3.5-turbo"),
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
  
  # Translate API call
  translate <- function(text, source, target) {
    url <- "https://api.mymemory.translated.net/get"
    response <- GET(url, query = list(q = text, langpair = paste(source, "|", target, sep = "")))
    stop_for_status(response)
    content(response, "parsed")$responseData$translatedText
  }
  
  observeEvent(input$translate, {
    req(input$textToTranslate, input$sourceLang, input$targetLang)
    translation <- translate(input$textToTranslate, input$sourceLang, input$targetLang)
    output$translationResult <- renderText({ translation })
  })
  
  output$usageTable <- renderTable({
    total_cost <- values$gptCost + values$dalleCost + values$ttsCost
    data.frame(
      Service = c("ChatGPT", "DALL-E", "TTS", "Total"),
      Tokens_Requests = c(values$gptTokens, values$dalleRequests, values$ttsRequests, values$gptTokens + values$dalleRequests + values$ttsRequests),
      Cost = c(values$gptCost, values$dalleCost, values$ttsCost, total_cost)
    )
  })
  
  # Visualization for usage and cost
  output$usagePlot <- renderPlot({
    total_cost <- values$gptCost + values$dalleCost + values$ttsCost
    costs <- c(values$gptCost, values$dalleCost, values$ttsCost, total_cost)
    barplot(
      height = costs,
      names.arg = c("ChatGPT", "DALL-E", "TTS", "Total"),
      col = c("darkblue", "darkgreen", "darkred", "black"),
      main = "Usage Cost",
      ylab = "Cost (USD)"
    )
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
      textInput("textToTranslate", "First check translation (optional):", value = ""),
      selectInput("sourceLang", "Source Language:", choices = c("sv", "en"), selected = "sv"),
      selectInput("targetLang", "Target Language:", choices = c("sv", "en"), selected = "en"),
      actionButton("translate", "Translate"),
      textOutput("translationResult"),
      tags$hr(),
      textInput("text", "Enter a Swedish word / phrase for Svenski:", value = ""),
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
      tabsetPanel(
        tabPanel("Results",
                 DTOutput("resultsTable"),
                 uiOutput("generateButtons")
        ),
        tabPanel("Usage and Cost",
                 plotOutput("usagePlot"),
                 tableOutput("usageTable")
        )
      )
    )
  ),
  tags$script(HTML("
    $(document).on('click', '#settings', function() {
      Shiny.setInputValue('settings', Math.random());
    });
  "))
)

shinyApp(ui = ui, server = server)
