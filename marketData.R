library(BatchGetSymbols)
library(mongolite)
library(tidyverse)
source("config.R")


saveData <- function(databaseName,collectionName,data) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName
              ),
              options = ssl_options(weak_cert_validation = TRUE))
  # Insert the data into the mongo collection
  db$insert(data)
}

#upload the symbols and name of tickers 
df_tickers_nasdaq <- read.csv('files/nasdaq_screener_1623692663224.csv',
                       header=TRUE) %>%
  subset(select=c(Symbol,Name,Country,Industry))

saveData("yahooFinance","ticker_names",df_tickers_nasdaq)
#amex
df_tickers_amex <- read.csv('files/amex_screener_1623693617044.csv',
                       header=TRUE) %>%
  subset(select=c(Symbol,Name,Country,Industry))

saveData("yahooFinance","ticker_names",df_tickers_amex)

#nyse
df_tickers_nyse <- read.csv('files/nyse_screener_1623694365867.csv',
                            header=TRUE) %>%
  subset(select=c(Symbol,Name,Country,Industry))

saveData("yahooFinance","ticker_names",df_tickers_nyse)

# set dates
first.date <- Sys.Date() - 60
last.date <- Sys.Date()
freq.data <- 'daily'
# set tickers
tickers <- c('FB','lol', 'AAPL')

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') )

databaseName <- "yahooFinance"
collectionName <- "ticker_prices"

saveData(databaseName,collectionName,l.out$df.tickers)