library(stringr)
library(tidyverse)
library(sentimentr)

add_tickers_sentiment <- function(dat, ticker_list, chunk_size = 100, polarity_dt = lexicon::hash_sentiment_loughran_mcdonald){
  tmp_list <- list()
  ticker_list <- ticker_list %>%
    mutate(joined = paste0("[^A-Za-z0-9]*(", symbol, ")[^A-Za-z0-9]*|[^A-Za-z0-9]*(", name, ")[^A-Za-z0-9]*"))
  recode_lst <- ticker_list$symbol
  names(recode_lst) <- toupper(ticker_list$name)
  chunks <- ggplot2::cut_interval(1:nrow(dat), length=chunk_size, labels=FALSE)
  i <- 1
  for (c in unique(chunks)){
    print(i)
    tmp_df <- dat[which(chunks==c),] %>%
      rowwise() %>%
      mutate(ticker = str_extract_all(comment, regex(paste0(ticker_list$joined, collapse = "|"), ignore_case = T))) %>%
      mutate(ticker = list(gsub("[^[:alnum:]]", "", ticker))) %>%
      mutate(ticker = list(unique(toupper(ticker)))) %>%
      mutate(ticker = list(recode(ticker, !!!recode_lst))) %>%
      mutate(ticker = list(unique(toupper(ticker)))) %>%
      mutate(sentiment = sentiment_by(comment, polarity_dt=polarity_dt))
    tmp_list[[i]] <- tmp_df
    i <- i + 1
  }
  return(dplyr::bind_rows(tmp_list))
}