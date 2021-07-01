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

#create communities in reddits
#create_communities_subreddits("stocks","clusters_stocks")
create_communities_subreddits <- function(collectionName, collectionClusters){
  databaseName <- "reddit"
  raw_data <-loadData(databaseName,collectionName)%>%
    filter(user!="[deleted]")
  
  new_authors_names <-raw_data %>%
    filter(author=="[deleted]") %>%
    distinct(link) %>%
    mutate(author_name = paste0("author_",row_number(.)))
  
  raw_data <-raw_data %>%
    left_join(., new_authors_names, by=c( "link"="link")) %>%
    mutate(author = ifelse(author == "[deleted]", author_name, author))
  
  #create dataframe with information about direct comments to posts and nested comments
  comments_posts <- raw_data %>%
    filter(!grepl("_",structure)) %>%
    rename(
      from = user,
      to = author
    )%>%
    subset( select = c(from,to))
  
  nested_comments <-raw_data %>% 
    mutate(
      from = structure,
      to = gsub("^(.*)_\\d+$", "\\1", structure) 
    )%>%
    subset( select = c(user,from,to,link))%>%
    left_join(., ., by=c( "link"="link","from"="to")) %>% 
    drop_na("user.y") %>%
    select("from"=user.y, "to"=user.x) 
  
  #combination of the connections between reddit users
  connections <-rbind(comments_posts, nested_comments) %>% 
    filter(from!=to) %>% 
    group_by(from, to) %>%
    summarise(weight = n()) %>% 
    ungroup() %>%
    mutate(width = weight+5)
  
  #create igraph object
  g <- graph_from_data_frame(connections,directed=FALSE)
  
  lc <- cluster_louvain(g)
  users <- do.call(rbind.data.frame, as.list(V(g)$name)) %>%
    mutate(group = membership(lc)) %>%
    rename("user" = 1) 
  
  #store cluster information in DB
  saveData(databaseName,collectionClusters,users)
  users
}