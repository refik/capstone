library(tidyverse)
library(tidytext)
library(stringr)
library(RSQLite)
library(edgeR)
library(glue)

source("chunking.R")
set.seed(2017)

corpora_url <- paste0("https://d396qusza40orc.cloudfront.net", 
                      "/dsscapstone/dataset/Coursera-SwiftKey.zip")
corpora_zip <- basename(corpora_url)
corpora_folder <- tools::file_path_sans_ext(corpora_zip)
corpora_db_path <- glue("{corpora_folder}.sqlite")
corpora_db_path_optimized <- glue("{corpora_folder}.optimized.sqlite")
corpora_db <- DBI::dbConnect(RSQLite::SQLite(), corpora_db_path)
corpora_db_optimized <- DBI::dbConnect(RSQLite::SQLite(), corpora_db_path_optimized)

log <- function(.data = NULL, msg) {
  time <- format(Sys.time(), "%X")
  message(glue("{time} | {msg}"))
  .data
}

if (!file.exists(corpora_zip)) {
  log(msg = "Downloading corpora zip")
  download.file(corpora_url, corpora_zip)
}

if (!dir.exists(corpora_folder)) {
  log(msg = "Unzipping corpora")
  unzip(corpora_zip, exdir = corpora_folder)
  message(glue("Corpora unzipped in \"{corpora_folder}\" folder."))
}

languages <- c("en-US", "de-DE", "ru-RU", "fi-FI")
types <- c("twitter", "blogs", "news")

get_path <- function(language, type) {
  language_file <- str_replace(language, "-", "_")
  paste(language_file, type, "txt", sep = ".") %>% 
    file.path(corpora_folder, "final", language_file, .)
}

get_text <- function(language, type, n_max = 500) {
  log(msg = glue("Reading text {language} {type}"))
  read_lines(get_path(language, type), n_max = n_max) %>% 
    tibble(language = language, type = type, text = .) %>% 
    mutate(line = row_number())
}

corpora_write_db <- function(language = rep("en-US", length(types)), 
                             type = types, ...) {
  map2(language, type, function(language, type) {
    get_text(language, type, ...) %>% 
      log(glue("Writing text {language} {type}")) %>% 
      chunk_apply(function(chunk) {
        dbWriteTable(corpora_db$con, "corpora", chunk, 
                     append = T, row.names = F)
      }, chunk_size = 10000)
  }) %>% 
    invisible()
}

corpora_ngram_write_db <- function(ngram_n = 1:6) {
  lng_typ <- corpora_db %>%
    tbl("corpora") %>% 
    count(language, type) %>% 
    collect()
  
  ngram_count <- length(ngram_n)
  
  map2(lng_typ$language, lng_typ$type, function(language, type) {
    corpora_db %>% 
      tbl("corpora") %>% 
      filter(language == !!language, type == !!type) %>% 
      arrange(line) %>% 
      log(glue("Tokenizing {language} {type} to {ngram_count} kinds of ngrams")) %>% 
      chunk_apply(chunk_size = 5000, fn = function(chunk) {
        ngram_n %>% 
          map_df(function(n) {
            chunk %>% 
              unnest_tokens(ngram, text, "ngrams", n = n) %>% 
              mutate(n = n)
          }) %>% 
          dbWriteTable(corpora_db$con, "ngram_raw", ., 
                       append = T, row.names = F)
      })
  }) %>% 
    invisible()
}

corpora_write_distinct_ngram <- function() {
  # Creating schema
  # dbWriteTable(corpora_db$con, "ngram_frequency", tibble(ngram = character(), n = integer(), r = integer()), row.names = F)
  log(msg = "Counting distinct ngrams")
  dbSendQuery(
    corpora_db$con,
    "INSERT INTO `ngram_frequency` (`ngram`, `n`, `r`)
     SELECT `ngram`, `n`, COUNT() AS `r`
     FROM `ngram_raw`
     GROUP BY `ngram`, `n`"
  )
  log(msg = "Writing distinct ngrams to db completed")
}

corpora_write_ngram_optimized <- function() {
  log(msg = "Writing optimized ngram to db")
  
  smooth <- corpora_db %>% 
    tbl("ngram_goodturing") %>% 
    select(n, r, r_star) %>% 
    collect()
  
  corpora_db %>%
    tbl("ngram_frequency") %>% 
    chunk_apply_lazy(function(chunk) {
      smoothed <- chunk %>% 
        collect() %>% 
        left_join(smooth, by = c("n", "r")) %>% 
        mutate(ngram = ifelse(n == 1, paste0(" ", ngram), ngram)) %>% 
        separate(ngram, c("tail", "head"), " (?=[^ ]*$)") %>% 
        select(n, tail, head, r_star)
      
      dbWriteTable(corpora_db_optimized$con, "ngram_smooth", smoothed, append = T,
                   row.names = F, field.types = c("n" = "INTEGER", "tail" = "VARCHAR(255)",
                                                  "head" = "VARCHAR(255)", "r_star" = "REAL"))
    }, chunk_size = 100000)
  
  log(msg = "Writing optimized ngram to db completed")
}

