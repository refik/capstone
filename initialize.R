set.seed(20170915)

corpora_url <- paste0("https://d396qusza40orc.cloudfront.net", 
                      "/dsscapstone/dataset/Coursera-SwiftKey.zip")
corpora_zip <- basename(corpora_url)
corpora_folder <- tools::file_path_sans_ext(corpora_zip)
corpora_db_path <- glue("{corpora_folder}.test2.sqlite")
corpora_db <- DBI::dbConnect(RSQLite::SQLite(), corpora_db_path)
corpora_db_compact_path <- glue("{corpora_folder}.compact.sqlite")
corpora_db_compact <- DBI::dbConnect(RSQLite::SQLite(), corpora_db_compact_path)

corpora_db_compact_path2 <- glue("{corpora_folder}.compact2.sqlite")
corpora_db_compact2 <- DBI::dbConnect(RSQLite::SQLite(), corpora_db_compact_path2)

log <- function(.data = NULL, msg) {
  time <- format(Sys.time(), "%X")
  message(glue("{time} | {msg}"))
  invisible(.data)
}

initialize <- function() {
  if (!file.exists(corpora_zip)) {
    log(msg = "Downloading corpora zip")
    download.file(corpora_url, corpora_zip)
  }
  
  if (!dir.exists(corpora_folder)) {
    log(msg = "Unzipping corpora")
    unzip(corpora_zip, exdir = corpora_folder)
    message(glue("Corpora unzipped in \"{corpora_folder}\" folder."))
  }
}

languages <- c("en-US", "de-DE", "ru-RU", "fi-FI")
types <- c("blogs", "news", "twitter")

get_path <- function(language, type) {
  language_file <- str_replace(language, "-", "_")
  paste(language_file, type, "txt", sep = ".") %>% 
    file.path(corpora_folder, "final", language_file, .)
}

get_text <- function(language, type, n_max = 500, sampled = FALSE) {
  log(msg = glue("Reading text {language} {type}"))
  path <- get_path(language, type)
  
  if (sampled == TRUE) {
    lines <- read_lines(path, n_max = -1) %>% 
      sample(n_max)
  } else {
    lines <- read_lines(path, n_max = n_max) 
  }
  
  tibble(language = language, type = type, text = lines) %>% 
    mutate(line_num = row_number())
}

filter_out_consecutive_number <- function(is_number) {
  reverse <- rev(is_number)
  next_is_number <- c(tail(rev(is_number), -1), FALSE)
  !rev(reverse & next_is_number)
}