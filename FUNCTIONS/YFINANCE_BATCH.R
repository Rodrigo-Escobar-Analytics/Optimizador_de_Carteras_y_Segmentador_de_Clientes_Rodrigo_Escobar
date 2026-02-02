library(purrr)
library(tidyquant)

tq_get_batch <- function(
    TICKERS,
    from,
    batch_size = 25,
    sleep_sec = 10
) {
  
  if (!is.character(TICKERS)) {
    stop("TICKERS must be a character vector")
  }
  
  TICKERS <- unique(TICKERS)
  
  batches <- split(
    TICKERS,
    ceiling(seq_along(TICKERS) / batch_size)
  )
  
  total_batches <- length(batches)
  
  map_dfr(seq_along(batches), function(i) {
    
    batch <- batches[[i]]
    
    message(sprintf(
      "Batch %s/%s (%s tickers)",
      i, total_batches, length(batch)
    ))
    
    Sys.sleep(sleep_sec)
    
    tryCatch(
      tq_get(
        batch,
        get  = "stock.prices",
        from = from
      ),
      error = function(e) {
        message(
          sprintf(
            "[[ E R R O R ]] Batch %s/%s failed: %s",
            i, total_batches, e$message
          )
        )
        NULL
      }
    )
  })
}
