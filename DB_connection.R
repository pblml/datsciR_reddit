library(mongolite)
source("config.R")

loadData <- function(databaseName,collectionName) {
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
  # Read all the entries
  data <- db$find()
  data
}


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


databaseName <- "reddit"
collectionName <- "wallstreetbets"

#read data from test data
#finance <- read.csv(file = '/datasets/reddit_finance/finance/submissions_reddit.csv')
#store information from csv in database
#saveData(databaseName,collectionName,raw_data)

exportAll <- function(reddit_list) {
  res_lst <- list()
  for (sub in reddit_list) {
    print(sub)
    res_lst[[sub]] <- get_reddit(subreddit = sub, cn_threshold = 10)
  }
  return(res_lst)
}

reddits <- c("finance", "stocks")

cmts_df_list <- exportAll(reddits)
#load data from database 
#financedb <- loadData(databaseName,collectionName)