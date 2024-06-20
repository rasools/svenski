library(shiny)
library(reticulate)
library(spacyr)
library(DT)
library(httr)
library(jsonlite)
library(base64enc)
library(magick)  # Added for image manipulation

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
  values <- reactiveValues(apiKey = NULL, results = NULL, image = NULL, audio = NULL, pic_url = NULL)
  
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
  openai_tts <- function(text, model = "tts-1", voice = "alloy", api_key, output_file) {
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
    
    audio_content <- content(response, as = "raw")
    writeBin(audio_content, output_file)
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
  
  observeEvent(input$savePicture, {
    req(values$pic_url)
    pic_file <- paste0("pics/", gsub(" ", "_", input$text), ".jpeg")
    save_image(values$pic_url, pic_file)
    showNotification(paste("Picture saved as", pic_file), type = "message")
  })
  
  observeEvent(input$readExamples, {
    req(values$results)
    tts_file <- paste0("tts/", gsub(" ", "_", input$text), ".mp3")
    openai_tts(input$text, api_key = values$apiKey, output_file = tts_file)
    values$audio <- tts_file
    
    # Generate audio for example sentences
    example_rows <- which(grepl("example sentence", values$results$Property))
    for (i in example_rows) {
      example_sentence <- values$results$Details[i]
      example_tts_file <- paste0("tts/", gsub(" ", "_", input$text), "_example_", i, ".mp3")
      openai_tts(example_sentence, api_key = values$apiKey, output_file = example_tts_file)
    }
    
    showNotification("Audio files for example sentences have been generated.", type = "message")
  })
  
  # Function to perform analysis
  performAnalysis <- function(txt, modelName, apiKey) {
    values$image <- NULL
    values$audio <- NULL
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
  
  output$image <- renderUI({
    req(values$image)
    tagList(
      tags$div(style = "margin-top: 20px;", 
               tags$img(src = values$image, height = "512px", width = "512px")
      ),
      tags$div(style = "margin-top: 20px;",  # Added space between image and button
               actionButton("savePicture", "Save Picture")
      )
    )
  })
  
  output$audioPlayer <- renderUI({
    req(values$audio)
    tags$div(style = "margin-top: 20px;", 
             tags$audio(src = values$audio, type = "audio/mp3", controls = TRUE)
    )
  })
  
  observe({
    req(values$results)
    output$generateButtons <- renderUI({
      tagList(
        div(style = "margin-top: 20px; margin-bottom: 20px;",
            actionButton("generateImage", "Generate Picture"),
            actionButton("readExamples", "Generate Audio")
        )
      )
    })
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
      DTOutput("resultsTable"),
      uiOutput("generateButtons"),
      uiOutput("image"),
      uiOutput("audioPlayer")
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
