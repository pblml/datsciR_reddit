library(mongolite)
#source("config.R")
source("configStocks.R")

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



loadDataCol <- function(databaseName,collectionName) {
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
  data <- db$find(fields='{"comm_date":1,
                            "subreddit":1,
                  "sentiment":1,
                  "ticker":1,
                  "author":1,
                  "user":1,
                  "link":1, 
                  "structure":1}')
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
  data <- db$find(sprintf('{"comm_date" : { "$gte" : { "$date" : "%s" }},
                    "comm_date" : { "$lte" : { "$date" : "%s" }}}',format(as.Date(initial_date), "%Y-%m-%dT%H:%M:%SZ"),
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
  data <- db$aggregate('[{"$match":{"user": "[deleted]", "comm_date": { "$lte" : { "$date" : "2021-05-01T00:00:00Z" }}}},
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
  data <- db$aggregate('[{"$match":{"author": "[deleted]", "comm_date": { "$lte" : { "$date" : "2021-05-01T00:00:00Z" }}}},
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
                              "$post_date", 0, 10
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
  data <- db$aggregate('[{"$match":{"comm_date": { "$lte" : { "$date" : "2021-05-01T00:00:00Z" }}}},
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

#get active users in a subreddit
getActiveUsers<- function(collectionName){
  databaseName <- "reddit"
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName
              ),
              options = ssl_options(weak_cert_validation = TRUE))
  active_users <- db$aggregate('[
          {
              "$group": {
                  "_id": "$user", 
                  "comments": {"$sum": 1}
              }
          }
      ]')
}


