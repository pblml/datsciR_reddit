library(progress)

assign_tickers <- function(dat, deepness = 4){
  pb <- progress_bar$new(
    format = " [:bar] :percent in :eta",
    total = nrow(dat), clear = FALSE, width= 60)
  for (i in seq(nrow(dat), 1)){
    d <- 0
    tmp_link <- dat[i, ] %>% pull(link)
    tmp_structure <- dat[i, ] %>% pull(structure)
    tmp_ticker <- dat[i, ] %>% pull(ticker) %>% unlist()
    tmp_dat <- dat %>% filter(link == tmp_link)
    
    if (length(tmp_ticker) != 0){
      next
    }
    
    while (length(unlist(tmp_ticker)) == 0 & d <= deepness & nchar(tmp_structure)!=0) {
      tmp_structure <- gsub("^(.*)_\\d+$", "\\1", tmp_structure) 
      tmp_ticker <- tmp_dat %>% filter(structure==tmp_structure) %>% pull(ticker)
      d <- d + 1
    }
    dat[i, "ticker"] <- ifelse(is.character(tmp_ticker), tmp_ticker, list(tmp_ticker))
    pb$tick()
  }
  return(dat)
} 


wsb2 <- wsb %>% assign_tickers()
stocks2 <- stocks %>% assign_tickers()
