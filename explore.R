library(kableExtra)
library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)
library(memoise)
library(forcats)
library(knitr)
library(glue)
library(tm)

set.seed(2017)

corpora_zip <- "Coursera-SwiftKey.zip"
corpora_folder <- tools::file_path_sans_ext(corpora_zip)
cache_fs <- cache_filesystem(glue("{corpora_folder}.cache"))

if (!dir.exists(corpora_folder)) {
  message("Unzipping corpora...")
  unzip(corpora_zip, exdir = corpora_folder)
  message(glue("Corpora unzipped in \"{corpora_folder}\" folder."))
}

log <- function(action, data = NULL) {
  current_env <- environment()
  parent.env(current_env) <- parent.frame()
  time <- format(Sys.time(), "%X")
  message(glue("{time} | {action}, {language} {type}"))
  data
}

language <- c("en-US", "de-DE", "ru-RU", "fi-FI")
type <- c("twitter", "blogs", "news")
all_lang <- rep(language, each = 3); all_type <- rep(type, 4)
l <- language[1]; t <- type[1]

get_path <- function(language, type) {
  language_file <- str_replace(language, "-", "_")
  paste(language_file, type, "txt", sep = ".") %>% 
    file.path(corpora_folder, "final", language_file, .)
}

get_text <- function(language, type, max_line = 500) {
  log("Reading text")
  read_lines(get_path(language, type), n_max = max_line) %>% 
    tibble(language = language, type = type, text = .) %>% 
    mutate(line_num = row_number())
}

get_word <- memoise(function(language, type, ...) {
  get_text(language, type, ...) %>% 
    log("Tokenizing words", .) %>% 
    unnest_tokens(word, text)
}, cache = cache_fs)

basic_stat <- memoise(function(language, type, ...) {
  map_df(list(list("word", get_word), list("line", get_text)), function(fn) {
    fn[[2]](language, type, ...) %>% 
      count(language, type) %>% 
      mutate(count = fn[[1]])
  })
}, cache = cache_fs)

word_count <- memoise(function(language, type, ...) {
  get_word(language, type, ...) %>% 
    log("Counting words", .) %>% 
    count(language, type, line_num) %>% 
    select(-line_num)
}, cache = cache_fs)

frequent_word <- memoise(function(language, type, n = 10, ...) {
  get_word(language, type, ...) %>% 
    log("Getting most frequent words", .) %>% 
    anti_join(stop_words) %>% 
    select(-line_num) %>% 
    filter(!str_detect(word, "[^a-z]+|^rt$")) %>% # Filter out numbers and rt 
    count(language, type, word, sort = TRUE) %>%
    mutate(frequency = n / sum(n)) %>% 
    head(n) 
}, cache = cache_fs)

frequent_word_en <- function(...) {
  map2_df(rep("en-US", length(type)), type, ~ frequent_word(.x, .y, ...))
}

word_count_all <- function(...) {
  map2_df(all_lang, all_type, ~ word_count(.x, .y, ...))
}

basic_stat_all <- function(...) {
  map2_df(all_lang, all_type, ~ basic_stat(.x, .y, ...)) %>% 
    spread(count, n) %>% 
    mutate(`word per line` = as.integer(word / line)) %>% 
    gather(count, n, word, line, `word per line`) %>% 
    arrange(language, type, count)
}

basic_stat_kable <- function(basic_stat) {
  basic_stat %>% 
    spread(type, n) %>% 
    kable("html", booktabs = TRUE, format.args = list(big.mark = ",")) %>% 
    kable_styling(full_width = FALSE) %>% 
    column_spec(1, bold = TRUE) %>% 
    column_spec(2, italic = TRUE) %>% 
    collapse_rows(1) %>% 
    add_header_above(c(" ", " ", "Document Types" = 3)) 
}

word_count_ggplot <- function(word_count) {
  word_count %>% 
    group_by(language, type) %>% 
    filter(n < quantile(n, 0.95)) %>% # Filtering values outsite 95th quantile
    ggplot(aes(x = n)) +
    geom_histogram(aes(y = ..density.., fill = language), binwidth = 5, 
                   show.legend = FALSE) +
    facet_grid(language ~ type) +
    xlab("Number of words") +
    ylab("Frequency of observation")
}

frequent_word_ggplot <- function(frequent_word) {
  frequent_word %>% 
    mutate(word = reorder(word, n, sum)) %>% 
    ggplot(aes(x = word, y = frequency)) +
    geom_col(aes(fill = type), show.legend = FALSE) +
    coord_flip() +
    facet_wrap(~ type, scales = "free_y") +
    xlab("Word") +
    ylab("Frequency")
}
