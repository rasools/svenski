
library(httr)
library(jsonlite)

api_key <- Sys.getenv("OPENAI_API_KEY_NBIS")

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
    stop("Request failed: ", content(response, as = "text"))
  }
  
  audio_content <- content(response, as = "raw")
  writeBin(audio_content, output_file)
}

# Usage
text <- "Today is a wonderful day to build something people love!"
output_file <- "speech.mp3"

openai_tts(text, model = "tts-1", voice = "alloy", api_key, output_file)
