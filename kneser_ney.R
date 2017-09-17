kneser_ney <- function(ngram_order, highest_order = FALSE, write = FALSE) {
  if (ngram_order == 1) {
    `N1+(••)` <- tbl(corpora_db, "ngram") %>% 
      filter(ngram_n == 2) %>% 
      count() %>% pull(1) %>% as.double()
    
    result <- tbl(corpora_db, "ngram_key") %>% 
      filter(ngram_n == 2) %>% 
      group_by(`Wi` = except_first_word) %>% 
      summarise(`N1+(•Wi)` = n()) %>% 
      mutate(`Pmkn` = `N1+(•Wi)` / `N1+(••)`, ngram_n = 1) %>% 
      select(ngram_n, ngram = Wi, Pmkn)
  } else {
    if (highest_order == FALSE) {
      `N1+(•Wi,i-n+1)` <- tbl(corpora_db, "ngram_key") %>% 
        filter(ngram_n == ngram_order + 1) %>% 
        group_by(`Wi,i-n+1` = except_first_word, `Wi-1,i-n+1` = middle) %>% 
        summarise(`N1+(•Wi,i-n+1)` = n())
      
      `N1+(•Wi-1,i-n+1•)` <- tbl(corpora_db, "ngram_key") %>% 
        filter(ngram_n == ngram_order + 1) %>% 
        group_by(`Wi-1,i-n+1` = middle) %>% 
        summarise(`N1+(•Wi-1,i-n+1•)` = n()) %>% 
        compute()
      
      `c(Wi,i-n+1)` <- tbl(corpora_db, "ngram_key") %>% 
        filter(ngram_n == ngram_order) %>% 
        select(`Wi,i-n+1` = ngram, `c(Wi,i-n+1)` = ngram_count)
    } else {
      `c(Wi,i-n+1)` <- tbl(corpora_db, "ngram_key") %>% 
        filter(ngram_n == ngram_order) %>% 
        select(`Wi,i-n+1` = ngram, `Wi-1,i-n+1` = except_last_word, 
               `c(Wi,i-n+1)` = ngram_count) %>% 
        compute()
      
      `c(Wi-1,i-n+1)` <- tbl(corpora_db, "ngram_key") %>% 
        filter(ngram_n == ngram_order - 1) %>% 
        select(`Wi-1,i-n+1` = ngram, `c(Wi-1,i-n+1)` = ngram_count) %>% 
        compute()
    }
    
    `Dc` <- tbl(corpora_db, "discount") %>% 
      filter(ngram_n == ngram_order) %>% 
      select(`Dc` = discount, `c1,2,3+` = count_group)
    
    `D(c(Wi,i-n+1))` <- `c(Wi,i-n+1)` %>% 
      mutate(`c1,2,3+` = ifelse(`c(Wi,i-n+1)` > 3, 3, `c(Wi,i-n+1)`)) %>% 
      select(`Wi,i-n+1`, `c1,2,3+`) %>% 
      inner_join(`Dc`, by = "c1,2,3+") %>% 
      select(`Wi,i-n+1`, `D(c(Wi,i-n+1))` = `Dc`)
    
    if (highest_order == FALSE) {
      novelty_nominator <- `N1+(•Wi,i-n+1)` %>% 
        inner_join(`D(c(Wi,i-n+1))`, by = "Wi,i-n+1") %>% 
        mutate(nominator = ifelse(`D(c(Wi,i-n+1))` > `N1+(•Wi,i-n+1)`, 
                                  0, `N1+(•Wi,i-n+1)` - `D(c(Wi,i-n+1))`)) %>% 
        select(`Wi,i-n+1`, `Wi-1,i-n+1`, nominator)
      
      novelty <- novelty_nominator %>% 
        inner_join(`N1+(•Wi-1,i-n+1•)`, by = "Wi-1,i-n+1") %>% 
        mutate(novelty = nominator / `N1+(•Wi-1,i-n+1•)`) %>% 
        select(`Wi,i-n+1`, `Wi-1,i-n+1`, novelty) %>% 
        compute()
    } else {
      novelty_nominator <- `c(Wi,i-n+1)` %>% 
        inner_join(`D(c(Wi,i-n+1))`, by = "Wi,i-n+1") %>% 
        mutate(nominator = ifelse(`D(c(Wi,i-n+1))` > `c(Wi,i-n+1)`, 
                                  0, `c(Wi,i-n+1)` - `D(c(Wi,i-n+1))`)) %>% 
        select(`Wi,i-n+1`, `Wi-1,i-n+1`, nominator)
      
      novelty <- novelty_nominator %>% 
        inner_join(`c(Wi-1,i-n+1)`, by = "Wi-1,i-n+1") %>% 
        mutate(novelty = nominator / `c(Wi-1,i-n+1)`) %>% 
        select(`Wi,i-n+1`, `Wi-1,i-n+1`, novelty) %>% 
        compute()
    }
    
    `Pmkn(Wi|Wi-1,i-n+2)` <- tbl(corpora_db, "ngram_kneser") %>% 
      filter(ngram_n == ngram_order - 1) %>% 
      select(`Wi,i-n+2` = ngram, Pmkn_lower = Pmkn)
    
    `γ(Wi-1,i-n+1)_nominator` <- tbl(corpora_db, "ngram_key") %>% 
      filter(ngram_n == ngram_order) %>% 
      group_by(`Wi-1,i-n+1` = except_last_word, 
               `c1,2,3+` = ifelse(ngram_count > 3, 3, ngram_count)) %>% 
      summarise(`Nc(Wi-1,i-n+1•)` = n()) %>% 
      inner_join(`Dc`, by = "c1,2,3+") %>% 
      group_by(`Wi-1,i-n+1`) %>% 
      summarise(nominator = sum(`Dc` * `Nc(Wi-1,i-n+1•)`))
    
    if (highest_order == FALSE) {
      `γ(Wi-1,i-n+1)` <- `γ(Wi-1,i-n+1)_nominator` %>% 
        inner_join(`N1+(•Wi-1,i-n+1•)`, by = "Wi-1,i-n+1") %>% 
        mutate(`γ(Wi-1,i-n+1)` = nominator / `N1+(•Wi-1,i-n+1•)`) %>% 
        select(`Wi-1,i-n+1`, `γ(Wi-1,i-n+1)`)
    } else {
      `γ(Wi-1,i-n+1)` <- `γ(Wi-1,i-n+1)_nominator` %>% 
        inner_join(`c(Wi-1,i-n+1)`, by = "Wi-1,i-n+1") %>% 
        mutate(`γ(Wi-1,i-n+1)` = nominator / `c(Wi-1,i-n+1)`) %>% 
        select(`Wi-1,i-n+1`, `γ(Wi-1,i-n+1)`)
    }
    
    result <- novelty %>% 
      inner_join(`γ(Wi-1,i-n+1)`, by = "Wi-1,i-n+1") %>% 
      mutate(`Wi,i-n+2` = sql("substr(`Wi,i-n+1`, instr(`Wi,i-n+1`, ' ') + 1)")) %>% 
      inner_join(`Pmkn(Wi|Wi-1,i-n+2)`, by = "Wi,i-n+2") %>% 
      mutate(continuity = `γ(Wi-1,i-n+1)` * Pmkn_lower) %>%
      compute() %>% # because of parser stack overflow
      mutate(Pmkn = continuity + novelty, ngram_n = ngram_order) %>% 
      select(ngram_n, ngram = `Wi,i-n+1`, Pmkn)
  }
  
  if (write == TRUE) {
    if (!("ngram_kneser" %in% dbListTables(corpora_db))) {
      db_create_table(
        corpora_db, "ngram_kneser", 
        c("ngram_n" = "INTEGER", "ngram" = "TEXT", "Pmkn" = "REAL"))
    }
    
    result %>% sql_build(corpora_db) %>% sql_render(corpora_db) %>% as.character() %>% 
      paste("INSERT INTO `ngram_kneser` (`ngram_n`, `ngram`, `pmkn`)", .) %>% 
      dbSendQuery(corpora_db, .)
  }
  
  result
}

modified_discount <- function(write = FALSE) {
  count_group <- tbl(corpora_db, "ngram") %>% 
    filter(ngram_count <= 4, ngram_n > 1) %>% 
    group_by(ngram_n, ngram_count) %>% 
    count() %>% 
    collect()
  
  D <- count_group %>% 
    group_by(ngram_n) %>% 
    filter(ngram_count %in% c(1, 2)) %>% 
    summarise(discount = n[1] / (n[1] + 2 * n[2]))
  
  result <- count_group %>% 
    left_join(D, "ngram_n") %>% 
    group_by(ngram_n) %>% 
    summarise(`1` = 1 - 2 * discount[1] * n[2] / n[1],
              `2` = 2 - 3 * discount[1] * n[3] / n[2],
              `3` = 3 - 4 * discount[1] * n[4] / n[3]) %>% 
    gather(count_group, discount, -ngram_n) %>% 
    mutate(count_group = as.integer(count_group))
  
  if (write == TRUE) dbWriteTable(corpora_db, "discount", result)
  
  result
}

kneser_ney_full <- function(ngram_order_max = 6) {
  log(msg = "Smoothing kneser ney")
  map2(1:ngram_order_max, c(rep(F, ngram_order_max - 1), T),
       ~ kneser_ney(.x, highest_order = .y, write = T)) %>% 
    invisible()
}

hash_fn <- function(text) {
  digest(text, algo = "xxhash64") %>% 
    paste0("0x", .) %>% 
    parse(text = .) %>% 
    eval()
}

kneser_top_word <- function() {
  top_words <- tbl(corpora_db, "ngram_kneser") %>% 
    filter(ngram_n == 1) %>% 
    select(word = ngram, Pmkn) %>% 
    arrange(desc(Pmkn)) %>% 
    head(15) %>% 
    collect() %>% 
    pull("word") %>% 
    unique()
  
  top_words[!str_detect(top_words, "__$")] %>% 
    head(10)
}

kneser_compact_word <- function() {
  tbl(corpora_db, "ngram_kneser") %>% 
    filter(ngram_n == 1) %>% 
    select(word = ngram, Pmkn) %>% 
    collect() %>% 
    mutate(id = row_number()) %>% 
    select(word_id, word, Pmkn)
    dbWriteTable(corpora_db_compact, "word_id", .)
  
  map(c("word", "word_id"), ~ db_create_index(corpora_db_compact, 
                                              "word_id", .))
}

kneser_compact_ngram <- function() {
  log(msg = "Hashing keys for fast access")
  
  ngram <- tbl(corpora_db, "ngram_kneser") %>% 
    filter(ngram_n > 1) %>% 
    select(ngram, Pmkn) %>% 
    collect() %>% 
    separate(ngram, c("last_words", "prediction"), " (?=[^ ]*$)") %>% 
    mutate(last_words_hash = map_dbl(last_words, hash_fn)) %>% 
    select(last_words_hash, prediction, Pmkn) %>% 
    filter(!str_detect(prediction, "__")) %>% 
    as.data.table()
  
  setkey(ngram, last_words_hash)
  write_rds(ngram, "ngram_kneser_dt.rds")
    
  #map(c("last_words_hash", "Pmkn"), ~ db_create_index(corpora_db_compact, 
  #                                                    "ngram_kneser", .))
}