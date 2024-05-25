# svenski code
Sys.setenv(RETICULATE_PYTHON = "/Users/rasools/miniconda3/envs/svenski/bin/python")
library(reticulate)
library(spacyr)
library(shiny)
library(DT)
library(httr)
library(jsonlite)

##################################################
chatGPT <- function(prompt, 
                    modelName = "gpt-3.5-turbo",
                    temperature = 1,
                    apiKey = Sys.getenv("chatGPT_API_KEY")) {
  
  if(nchar(apiKey)<1) {
    apiKey <- readline("Paste your API key here: ")
    Sys.setenv(chatGPT_API_KEY = apiKey)
  }
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", apiKey)),
    content_type_json(),
    encode = "json",
    body = list(
      model = modelName,
      temperature = temperature,
      messages = list(list(
        role = "user", 
        content = prompt
      ))
    )
  )
  
  if(status_code(response)>200) {
    stop(content(response))
  }
  
  trimws(content(response)$choices[[1]]$message$content)
}

################################################## get the root of the word

# Initialize the spacyr with the Swedish model
spacy_initialize(model = "sv_core_news_md")
txt <- "körde"
consolidatedtxt <- entity_consolidate(spacy_parse(txt, lemma = FALSE, entity = TRUE, nounphrase = TRUE))

if (consolidatedtxt$pos[1] == "VERB") {
  lines <- readLines("temp_tables/verb.txt", warn = FALSE)  # Disable warning if you're okay ignoring it
  table_string <- paste(lines[which(grepl("Category \\| Details", lines)):which(grepl("Supinum example sentence \\|", lines))], collapse = "\n")
  prompt <- paste(
    consolidatedtxt$token[1],
    "is a Swedish verb. I want to use this information to make an Anki card for learning Swedish. Can you help me with that? Please just fill in the blanks in the following table, without adding any additional information. For the example sentences, Thanks! Here is the table:\n\n",
    table_string, "\n\nIn the following table, please give example sentences with atleast 10 words. Also add their meaning in english in parantesis after the sentence. For example, 'Jag kör bil (I drive a car)'.\n\nExample sentences",
    sep = ""
  )
  ans = chatGPT(prompt)
}
lines <- strsplit(ans, split = "\n")[[1]]
lines <- lines[-1]
key_value_pairs <- lapply(lines, function(x) strsplit(x, split = " | ", fixed = TRUE))
kv_list <- lapply(key_value_pairs, function(x) {
  key <- x[[1]][1]
  value <- if (length(x[[1]]) > 1) x[[1]][2] else NA
  return(c(Key=key, Value=value))
})
kv_df <- do.call(rbind, kv_list)
kv_df <- as.data.frame(kv_df, stringsAsFactors = FALSE)
colnames(kv_df) <- c("Property", "Details")

################################################## make the table for the root of the word
txt <- kv_df$Details[kv_df$Property == "Infinitiv"]
consolidatedtxt <- entity_consolidate(spacy_parse(txt, lemma = FALSE, entity = TRUE, nounphrase = TRUE))

if (consolidatedtxt$pos[1] == "VERB") {
  lines <- readLines("temp_tables/verb.txt", warn = FALSE)  # Disable warning if you're okay ignoring it
  table_string <- paste(lines[which(grepl("Category \\| Details", lines)):which(grepl("Supinum example sentence \\|", lines))], collapse = "\n")
  prompt <- paste(
    consolidatedtxt$token[1],
    "is a Swedish verb. I want to use this information to make an Anki card for learning Swedish. Can you help me with that? Please just fill in the blanks in the following table, without adding any additional information. For the example sentences, Thanks! Here is the table:\n\n",
    table_string,
    sep = ""
  )
  ans = chatGPT(prompt)
}
lines <- strsplit(ans, split = "\n")[[1]]
lines <- lines[-1]
key_value_pairs <- lapply(lines, function(x) strsplit(x, split = " | ", fixed = TRUE))
kv_list <- lapply(key_value_pairs, function(x) {
  key <- x[[1]][1]
  value <- if (length(x[[1]]) > 1) x[[1]][2] else NA
  return(c(Key=key, Value=value))
})
kv_df <- do.call(rbind, kv_list)
kv_df <- as.data.frame(kv_df, stringsAsFactors = FALSE)
colnames(kv_df) <- c("Property", "Details")

################################################## get pronunciations










