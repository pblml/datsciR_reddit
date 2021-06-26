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

loadDataDates <- function(databaseName,collectionName,initial_date, final_date) {
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
  data <- db$find(sprintf('{"post_date" : { "$gte" : { "$date" : "%s" }},
                    "post_date" : { "$lte" : { "$date" : "%s" }}}',format(as.Date(initial_date), "%Y-%m-%dT%H:%M:%SZ"),
                          format(as.Date(final_date), "%Y-%m-%dT%H:%M:%SZ")))
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


getDeletedPost <- function(databaseName,collectionName) {
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
  # Filter data
  #data <- db$find('{"$or": [{"user": "[deleted]"},{"author": "[deleted]"}]}')
  data <- db$aggregate('[{"$match":{"$or": [{"user": "[deleted]"},{"author": "[deleted]"}]}},
                       {
                            "$group": {
                                "_id": {
                                    "$substr": [
                                        "$comm_date", 0, 10
                                    ]
                                }, 
                                "total": {
                                    "$sum": 1
                                }
                            }
                        }
                       ]')
  #data <- db$find('{"user": "[delete]"}')
  data
}

getPostPerDay <- function(databaseName,collectionName){
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName
              ),
              options = ssl_options(weak_cert_validation = TRUE))
  data <- db$aggregate('[
          {
              "$group": {
                  "_id": {
                      "$substr": [
                          "$comm_date", 0, 10
                      ]
                  }, 
                  "total": {
                      "$sum": 1
                  }
              }
          }
      ]')
}

databaseName <- "reddit"
collectionName <- "finance"

#read data from test data
#finance <- read.csv(file = '/datasets/reddit_finance/finance/submissions_reddit.csv')
#store information from csv in database
#saveData(databaseName,collectionName,raw_data)


#load data from database 
financedb <- loadData(databaseName,collectionName)
