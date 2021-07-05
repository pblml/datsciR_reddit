library(tidyverse)
library(ggplot2)
library(BatchGetSymbols)
library(forecast)
library(plotly)
library(imputeTS)

stocks <- readRDS("stocks.Rds")
#stocks <- loadData("reddit", "wallstreetbets")
prepare_analysis <- function(df, topn=10, blacklist=NULL){
  mentioned_tickers <- df$ticker %>%
    unlist() %>%
    paste0(collapse = " ") %>%
    str_squish() %>%
    strsplit(" ") %>%
    table() %>%
    as.data.frame() %>%
    arrange(desc(Freq)) %>% 
    head(10) %>%
    pull(1)
  
  l.out <- BatchGetSymbols(tickers = mentioned_tickers, 
                           first.date = min(df$comm_date, na.rm = T),
                           last.date = max(df$comm_date, na.rm = T), 
                           freq.data = "daily",
                           cache.folder = file.path(tempdir(), 
                                                    'BGS_Cache'))
  res_list <- list()
  for (t in mentioned_tickers) {
    print(t)
    tmp_reddit <- df %>%
      filter(t %in% ticker) %>%
      mutate(sentiment= case_when(sentiment < 0 ~ -1, sentiment > 0 ~ 1)) %>%
      group_by("date"=lubridate::date(comm_date)) %>%
      summarise(sentiment = mean(sentiment, na.rm = T), sent_vol = n())
    tmp_joined <- l.out$df.tickers %>%
      filter(ticker==t) %>%
      left_join(tmp_reddit, by=c("ref.date"="date"))
    if (nrow(tmp_joined)!=0){
      res_list[[t]] <- tmp_joined
    }
  }
  
  return(res_list)
}

analysis <- function(df_lst){
  res_lst <- list()
  for (ticker in names(df_lst)){
    print(ticker)
    df_stock <- df_lst[[ticker]] %>%
      arrange(ref.date) %>%
      mutate(lag_sentiment = lag(sentiment), (sentiment-lag_sentiment)/lag_sentiment)
    
    sentiment_ts <- df_stock$sentiment %>% na_seadec()
    price_ts <- df_stock$ret.adjusted.prices %>% na_seadec()
    volume_ts <- df_stock$volume %>% na_seadec()
    res_lst[[ticker]][["price"]] <- ccf(sentiment_ts, price_ts, lag.max = 10, plot=F)
    res_lst[[ticker]][["volume"]] <- ccf(sentiment_ts, volume_ts, lag.max = 10, plot=F)
  }
  return(res_lst)
}  

plot_analysis <- function(ccf_lst){
  plot_lst <- list()
  for (ticker in names(ccf_lst)){
    print(ticker)
    plot_lst[[paste0(ticker, "_price")]] <- (ccf_lst[[ticker]][["price"]] %>% 
      autoplot() +
      scale_x_continuous(breaks = seq(-10, 10))) %>% 
      plotly::ggplotly() %>%
      layout(annotations = list(x = 0.2 , y = 1.2, text = paste0(ticker, " Sentiment/Price"), showarrow = F,
                                xref='paper', yref='paper'))

    plot_lst[[paste0(ticker, "_volume")]] <- (ccf_lst[[ticker]][["volume"]] %>% 
      autoplot() +
      scale_x_continuous(breaks = seq(-10, 10))) %>%
      plotly::ggplotly() %>%
      layout(annotations = list(x = 0.8 , y = 1.2, text = paste0(ticker, " Sentiment/Volume"), showarrow = F,
                                xref='paper', yref='paper'))
    
  }
  return(plot_lst)
}

prep_stocks <- prepare_analysis(stocks)
ccf_stocks <- analysis(prep_stocks)
plot_list <- plot_analysis(ccf_stocks)
subplot(plot_list, nrows = length(plot_list)/2)

prepare_analysis(wsb_mod) %>% analysis() %>% plot_analysis %>% subplot(., nrows = length(.)/2)
lapply(names(ccf_stocks), function(x) {ccf_stocks[[x]]$price[[1]]}) %>%
purrr::reduce(cbind) %>%
magrittr::set_colnames(names(ccf_stocks)) %>%
as.data.frame() %>%
rowwise() %>%
mutate(rowMean = mean(c_across())) %>%
ungroup() %>%
mutate(lag=seq(-10, 10))


