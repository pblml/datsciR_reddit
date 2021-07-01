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

#get aggregated information of posts that have deleted user or author
getNumberPostDeletedUsers <- function(databaseName,collectionName) {
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
  #data <- db$aggregate('[{"$match":{"$or": [{"user": "[deleted]"},{"author": "[deleted]"}]}},
  data <- db$aggregate('[{"$match":{"user": "[deleted]"}},
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
  data
}

#get aggregated information of posts that have deleted user as authors
getNumberPostDeletedAuthors <- function(databaseName,collectionName) {
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
  data <- db$aggregate('[{"$match":{"author": "[deleted]"}},
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
                          "data":{"$substr": [
                              "$comm_date", 0, 10
                          ]},
                          "link":"$link"
                      }, 
                      "total": {
                          "$sum": 1
                      }
                  }
              }
          ]')
  data <- do.call(data.frame,data) %>%
    group_by(X_id.data) %>%
    summarise(nr_post = n())
}

#get number of comments per day
getCommentsPerDay <- function(databaseName,collectionName){
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

# function that updates a column.
# Parameters: 
# - columnFilter: Name of the column to filter from
# - valueFilter: Value search on the columnFilter to find one row
# - newColumnName: name of the column to update information. If the column already exists in the document. The information is updated. Otherwise the column is created
# - valueNewColumn: Value to include in the new column
updateDocuments<-function(databaseName,collectionName,columnFilter, valueFilter,newColumnName, valueNewColumn){
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName
              ),
              options = ssl_options(weak_cert_validation = TRUE))
  
  db$update(query=sprintf('{"%s":"%s"}',columnFilter,valueFilter),
            update=sprintf('{"$set":{"%s":"%s"}}',newColumnName,valueNewColumn))
}



databaseName <- "reddit"
collectionName <- "stocks"

#read data from test data
#finance <- read.csv(file = '/datasets/reddit_finance/finance/submissions_reddit.csv')
#store information from csv in database
#saveData(databaseName,collectionName,raw_data)
#updateDocuments("yahooFinance","ticker_names","Symbol","AACQW","add_col","change")

#load data from database 
#financedb <- loadData(databaseName,collectionName)
