library(tidyverse)
library(tidytext)
library(stringr)
library(RSQLite)
library(glue)
library(digest)
library(data.table)

source("chunking.R")
source("initialize.R")
source("kneser_ney.R")

write_corpora_text <- function(language = rep("en-US", length(types)), 
                             type = types, ...) {
  map2_df(language, type, ~ get_text(.x, .y, ...)) %>% 
    mutate(text_number = row_number()) %>% 
    log(glue("Writing corpora text to db")) %>% 
    dbWriteTable(corpora_db, "corpora", .)
}

write_ngram_raw <- function(ngram_n_max = 6) {
  log(msg = "Writing raw ngrams")
  
  corpora <- tbl(corpora_db, "corpora") %>% 
    select(text_number, text)
    
  if ("ngram_raw" %in% dbListTables(corpora_db)) {
    last_processed <- tbl(corpora_db, "ngram_raw") %>% 
      summarise(max(text_number)) %>%
      pull(1)
    
    total <- corpora %>% count() %>% pull(1)
    progress <- (last_processed * 100) %/% total
    log(msg = glue("Resuming from {progress} %"))
    
    corpora <- filter(corpora, text_number > last_processed)
  }
  
  corpora %>% 
    chunk_apply_lazy(chunk_size = 10000, fn = function(chunk) {
      sentence_df <- chunk %>% 
        collect() %>% 
        unnest_tokens(sentence, text, token = "sentences") %>% 
        filter(!str_detect(sentence, "_")) %>% 
        filter(!str_detect(sentence, "[^\\u0000-\\u007F]+")) %>% 
        mutate(sentence_number = row_number()) %>% 
        unnest_tokens(word, sentence, token = "words") %>% 
        mutate(has_number = str_detect(word, "\\d")) %>% 
        mutate(word = ifelse(has_number, "__numbers__", word)) %>% 
        group_by(text_number, sentence_number) %>% 
        filter(filter_out_consecutive_number(has_number)) %>% 
        select(-has_number) %>% 
        summarise(sentence = paste(c("__sentence__", word), collapse = " ")) %>% 
        group_by(text_number) %>% 
        summarise(text = paste(c("__text__", sentence), collapse = " ")) %>% 
        ungroup() 
      
      ngrams <- map_df(1:ngram_n_max, ~ {
        sentence_df %>% 
          unnest_tokens(ngram, text, "ngrams", n = .x) %>% 
          mutate(ngram_n = .x) 
      })
      
      ngrams %>% 
        select(ngram, ngram_n) %>% 
        dbWriteTable(corpora_db, "ngram_raw", ., append = T)
    }) %>% 
    invisible()
}

write_ngram <- function() {
  log(msg = "Writing unique keys")
  db_create_table(corpora_db, "ngram", 
                  c("ngram_n" = "INTEGER", "ngram_count" = "INTEGER", 
                    "ngram" = "TEXT"))
  
  dbSendQuery(corpora_db, "
    INSERT INTO `ngram` (`ngram_n`, `ngram_count`, `ngram`)
    SELECT `ngram_n`, COUNT() AS `ngram_count`, `ngram`
    FROM `ngram_raw`
    GROUP BY `ngram`, `ngram_n`
")
}

write_ngram_key <- function() {
  log(msg = "Writing ngram keys")
  tbl(corpora_db, "ngram") %>% 
    filter(ngram_count != 1) %>% 
    chunk_apply_lazy(chunk_size = 10000, function(chunk) {
      collect(chunk) %>% 
        mutate(space = gregexpr(" ", ngram)) %>% 
        mutate(keys = map2_chr(ngram, space, function(full, space_pos) {
          if (space_pos[1] == -1) {
            "||"
          } else {
            first <- head(space_pos, 1)
            last <- tail(space_pos, 1)
            paste(
              substr(full, 1, last - 1),
              substr(full, first + 1, last - 1),
              substr(full, first + 1, 1000000L)
              , sep = "|")
          }
        })) %>% 
        separate(keys, c("except_last_word", "middle", "except_first_word"), "\\|") %>% 
        select(-space) %>% 
        dbWriteTable(corpora_db, "ngram_key", ., append = TRUE)
    }) %>% 
    invisible()
}

ngram_key_index <- function() {
  log(msg = "Making index for ngram_key")
  list("ngram_count", c("ngram_n", "ngram_count"),
       "except_last_word", "middle", "except_first_word", "ngram") %>% 
    map(~ db_create_index(corpora_db, "ngram_key", .))
  log(msg = "Index completed")
}

prepare_all <- function(n_max = 100000) {
  start <- Sys.time()
  log(msg = "Preparing Pmkn for corpur")
  write_corpora_text(n_max = n_max, sampled = TRUE)
  write_ngram_raw()
  write_ngram()
  write_ngram_key()
  ngram_key_index()
  modified_discount(write = T)
  kneser_ney_full()
  kneser_compact_ngram()
  duration <- as.integer(difftime(Sys.time(), start, units = "mins"))
  log(msg = glue("Completed. Took {duration} minutes."))
}