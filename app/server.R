#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(tidytext)
library(dplyr)
library(purrr)
library(stringr)
library(digest)
ngram_kneser_dt <- readRDS("ngram_kneser_dt.rds")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  observe({
    sentence <- input$text_input
    prediction <- input$prediction

    if (!is.null(prediction) && prediction != "") {
      # Can also set the label and select items
      updateTextInput(session, "text_input", value = paste(trimws(sentence), prediction))
    }
    
    if (!is.null(sentence)) {
      # Can also set the label and select items
      updateSelectInput(session, "prediction",
                        choices = predict_next_word_dt(sentence))
    }

  })
  
})

prepare_text <- function(text) {
  if (text == "") {
    words <- "__sentence__"
  } else {
    words <- text %>% 
      tibble(text = .) %>% 
      unnest_tokens(sentence, text, token = "sentences") %>% 
      mutate(sentence_number = row_number()) %>% 
      unnest_tokens(word, sentence, token = "words") %>% 
      mutate(has_number = str_detect(word, "\\d")) %>% 
      mutate(word = ifelse(has_number, "__numbers__", word)) %>% 
      group_by(sentence_number) %>% 
      filter(filter_out_consecutive_number(has_number)) %>% 
      select(-has_number) %>% 
      summarise(sentence = paste(c("__sentence__", word), collapse = " ")) %>% 
      unnest_tokens(word, sentence, token = "words") %>% 
      pull("word")
  }
  
  ngram_seq <- min(length(words), 5):1
  
  map_chr(ngram_seq, ~ paste(tail(words, n = .x), collapse = " ")) %>% 
    tibble(ngram = ., ngram_n = ngram_seq) %>% 
    mutate(ngram_hash = map_dbl(ngram, hash_fn))
}

predict_next_word_dt <- function(sentence) {
  sentence_dt <- prepare_text(sentence) %>% 
    as.data.table()
  
  setkey(sentence_dt, ngram_hash)
  
  predictions <- merge(sentence_dt, ngram_kneser_dt, by.x = "ngram_hash", by.y = "last_words_hash")[order(-ngram_n, -Pmkn)] %>% 
    head(10) %>% 
    pull("prediction") %>% 
    unique()
  
  if (length(predictions) == 0) {
    c("and",  "in",   "the",  "to",   "of",  "is",   "for",  "on",   "a",    "that")
  } else {
    predictions
  }
}

filter_out_consecutive_number <- function(is_number) {
  reverse <- rev(is_number)
  next_is_number <- c(tail(rev(is_number), -1), FALSE)
  !rev(reverse & next_is_number)
}

hash_fn <- function(text) {
  digest(text, algo = "xxhash64") %>% 
    paste0("0x", .) %>% 
    parse(text = .) %>% 
    eval()
}
