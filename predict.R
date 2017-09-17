predict_next_word_dt <- function(sentence) {
  sentence_dt <- prepare_text(sentence) %>% 
    as.data.table()
  
  setkey(sentence_dt, ngram_hash)
  
  predictions <- merge(sentence_dt, ngram_kneser_dt, by.x = "ngram_hash", by.y = "last_words_hash")[order(-ngram_n, -Pmkn)] %>% 
    head(10) %>% 
    pull(prediction) %>% 
    unique()
  
  if (length(predictions) == 0) {
    c("and", "in", "the", "to", "is", "of", "for", "a", "on", "that")
  } else {
    predictions
  }
}

prepare_text <- function(text) {
  words <- paste("__start__", text) %>% 
    tibble(text = .) %>% 
    unnest_tokens(word, text, token = "words") %>% 
    mutate(has_number = str_detect(word, "\\d")) %>% 
    mutate(word = ifelse(has_number, "__numbers__", word)) %>% 
    filter(filter_out_consecutive_number(has_number)) %>% 
    select(-has_number) %>% 
    pull(1)
  
  ngram_seq <- min(length(words), 5):1
  
  map_chr(ngram_seq, ~ paste(tail(words, n = .x), collapse = " ")) %>% 
    tibble(ngram = ., ngram_n = ngram_seq) %>% 
    mutate(ngram_hash = map_dbl(ngram, hash_fn))
}