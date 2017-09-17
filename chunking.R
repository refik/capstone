library(dbplyr)

chunk_apply_lazy <- function(.data, fn, chunk_size = 1000) {
  row_count <- .data %>% 
    count() %>% 
    pull(1)
  
  chunk_amount <- ceiling(row_count / chunk_size)
  progress <- progress_estimated(chunk_amount)
  
  map(1:chunk_amount, ~ {
    offset <- (.x - 1) * chunk_size
    con <- .data$src$con
    
    .data %>% 
      head(chunk_size) %>% 
      sql_build(con) %>% 
      sql_render(con) %>% 
      as.character() %>% 
      paste(glue("OFFSET {offset}")) %>% 
      dbGetQuery(con, .) %>% 
      fn()
    
    progress$tick()$print()
  })
}