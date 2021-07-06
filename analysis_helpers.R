library(tidyverse)
library(ggplot2)
library(BatchGetSymbols)
library(forecast)
library(plotly)

#stocks <- readRDS("stocks.Rds")
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
    head(topn) %>%
    pull(1)
  
  l.out <- BatchGetSymbols(tickers = mentioned_tickers, 
                           first.date = min(df$comm_date, na.rm = T),
                           last.date = max(df$comm_date, na.rm = T), 
                           freq.data = "daily",
                           cache.folder = file.path(tempdir(), 
                                                    'BGS_Cache'))
  res_list <- list()
  for (t in mentioned_tickers) {
    tmp_reddit <- df %>%
      filter(t %in% ticker) %>%
      group_by("date"=lubridate::date(comm_date)) %>%
      mutate(sentiment = case_when(sentiment > 0 ~ 1,
                                   sentiment < 0 ~ -1)) %>%
      summarise(sentiment = mean(sentiment, na.rm = T), sent_vol = n())
    tmp_joined <- l.out$df.tickers %>%
      filter(ticker==t) %>%
      left_join(tmp_reddit, by=c("ref.date"="date")) %>%
      arrange(ref.date) %>%
      mutate(roll_sentiment=zoo::rollmeanr(sentiment, 3, na.pad=T, fill=0))
    
    if (nrow(tmp_joined)!=0){
      res_list[[t]] <- tmp_joined
    }
  }
  
  return(res_list)
}

ccf_by_price_vol <- function(df_lst, na.action = na.pass){
  res_lst <- list()

  for (ticker in names(df_lst)){
    print(ticker)
    df_stock <- df_lst[[ticker]]
    
    sentiment_ts <- df_stock %>%
      select(ref.date, sentiment) %>%
      zoo::read.zoo() %>% 
      as.ts(frequency = 1) %>% 
      {if (ndiffs(.) > 0) diff(., ndiffs(.)) else .}
    price_ts <- df_stock %>% select(ref.date, ret.closing.prices) %>% zoo::read.zoo() %>% as.ts(frequency = 1) %>% {if (ndiffs(.) > 0) diff(., ndiffs(.)) else .}
    volume_ts <- df_stock %>% select(ref.date, volume) %>% zoo::read.zoo() %>% as.ts(frequency = 1) %>% {if (ndiffs(.) > 0) diff(., ndiffs(.)) else .}
    
    res_lst[[ticker]][["price"]] <- ccf(sentiment_ts, price_ts, lag.max = 5, plot=F, na.action = na.action)
    res_lst[[ticker]][["volume"]] <- ccf(sentiment_ts, volume_ts, lag.max = 5, plot=F, na.action = na.action)
  }
  return(res_lst)
} 

ccf_plots <- function(ccf_lst){
  plot_lst <- list()
  for (ticker in names(ccf_lst)){
    plot_lst[[paste0(ticker, "_price")]] <- (ccf_lst[[ticker]][["price"]] %>% 
      autoplot() +
      scale_x_continuous(breaks = seq(-5, 5))) %>% 
      plotly::ggplotly() %>%
      layout(annotations = list(x = 0.3 , y = 1.2, text = paste0(ticker, " Sentiment/Price"), showarrow = F,
                                xref='paper', yref='paper'))

    plot_lst[[paste0(ticker, "_volume")]] <- (ccf_lst[[ticker]][["volume"]] %>% 
      autoplot() +
      scale_x_continuous(breaks = seq(-5, 5))) %>%
      plotly::ggplotly() %>%
      layout(annotations = list(x = 0.8 , y = 1.2, text = paste0(ticker, " Sentiment/Volume"), showarrow = F,
                                xref='paper', yref='paper'))
    
  }
  return(plot_lst)
}

ccf_table <- function(cff_output) {
  tmp_table <- lapply(names(cff_output), function(x) {cff_output[[x]]$price[[1]]}) %>%
    purrr::reduce(cbind) %>%
    magrittr::set_colnames(names(cff_output)) %>%
    as.data.frame() %>%
    rowwise() %>%
    mutate(rowMean = mean(c_across())) %>%
    ungroup() %>%
    mutate(lag=seq(-5, 5)) %>%
    round(2) %>%
    select(lag, everything(), rowMean) %>%
    t() %>% as.data.frame()
  return(tmp_table)
}

plot_ts <- function(dat, symbol) {
  dat <- prepare_analysis(dat)
  
  fig1 <- dat[[symbol]] %>%
    plot_ly(x = ~ref.date, type="candlestick",
            open = ~price.open, close = ~price.close,
            high = ~price.high, low = ~price.low) %>%
    layout(xaxis = list(rangeslider = list(visible = F)), title=paste0(symbol))
  
  ay <- list(
    overlaying = "y2",
    side = "right"
  )
  
  fig2 <- dat[[symbol]] %>%
    plot_ly() %>%
    add_trace(x=~ref.date, y=~roll_sentiment, name="sentiment", type="scatter", mode="line")

  fig3 <- dat[[symbol]] %>%
    plot_ly() %>%
    add_trace(x=~ref.date, y=~sent_vol, name="vol_sentiment", type="bar")


  
  return(subplot(fig1, fig2, fig3, nrows=3, shareX = T, margin = 0.02))
}
