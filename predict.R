katz_backoff <- function(words, k = 2) {
  ngram_count <- length(words) + 1
  
  if(length(words) == 0) {
    words <- ""
    ngram_count <- 1
  }
  
  ngram <- paste(words, collapse = " ")
  
  possible <- corpora_db_optimized %>% 
    tbl("ngram_smooth") %>% 
    filter(n == ngram_count, tail == ngram, r_star > k) %>% 
    select(n, tail, head, r_star) %>% 
    arrange(desc(r_star)) %>% 
    head(20) %>% 
    collect()
  
  if (nrow(possible) == 0) {
    if(ngram_count == 1) {
      return()
    }
    
    words %>% 
      tail(n = -1) %>% 
      katz_backoff() %>% 
      return()
  } else {
    return(possible)
  }
}

predict_next_word <- function(sentence) {
  last_words <- tibble(text = sentence) %>% 
    unnest_tokens(word, text) %>% 
    tail(6) %>% 
    pull(1)
  
  katz_backoff(last_words)
}
