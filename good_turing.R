corpora_write_good_turing <- function(ngram_n = 1:6) {
  ngram_count <- length(ngram_n)
  log(msg = glue("Smoothing frequencies with good-turing for {ngram_count} types of ngrams"))
  map(ngram_n, function(n) {
    freq <- corpora_db %>% 
      tbl("ngram_frequency") %>% 
      filter(n == !!n) %>% 
      log(glue("Collecting {n}grams")) %>% 
      pull("r")
    
    N <- sum(freq)
    
    log(msg = glue("Calculating good-turing for {n}grams"))
    gt <- goodTuring(freq)
    
    gt_table <- tibble(
      n = n, 
      r = gt$count, 
      n_r = gt$n, 
      p = gt$proportion, 
      p0 = gt$P0, 
      n_total = N, 
      r_star = N * gt$proportion
    )
    
    log(msg = glue("Writing good-turing coefficients for {n}grams to db"))
    dbWriteTable(corpora_db$con, "ngram_goodturing", gt_table, append = T, row.names = F)
  })
  log(msg = "Smoothing frequencies done")
}