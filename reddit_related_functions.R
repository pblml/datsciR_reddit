source("DB_connection.R")

#get all the interactions with 
get_info_deleted_users <- function(collectionName){
  databaseName <- "reddit"
  comments_deleted_users <-getNumberPostDeletedUsers(databaseName,collectionName)
  colnames(comments_deleted_users) <- c('dates','comments_deleted_users')
  comments_deleted_authors <-getNumberPostDeletedAuthors(databaseName,collectionName)
  colnames(comments_deleted_authors) <- c('dates','comments_deleted_auth')
  post_per_day <-getCommentsPerDay(databaseName,collectionName)
  colnames(post_per_day) <- c('dates','total_comments')
  
  merged_data <- merge(x = post_per_day, y = comments_deleted_users, 
                       by = "dates", all = TRUE) %>%
    merge(.,comments_deleted_authors, by="dates",all=TRUE) 
  
  merged_data[is.na(merged_data)] <- 0
  merged_data$percentage_comments_del_users <- merged_data$comments_deleted_users / merged_data$total_comments
  merged_data$percentage_comments_del_authors <- merged_data$comments_deleted_auth / merged_data$total_comments
  merged_data$dates <- ymd(merged_data$dates)
  merged_data
}

