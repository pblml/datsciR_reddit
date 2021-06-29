library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
source("DB_connection.R")

clustering_evaluation <- function(databaseName,collectionName,initial_time,end_time){

  raw_data <- loadDataDates(databaseName,collectionName,initial_time,end_time) %>%
    filter(user!="[deleted]") 
  
  #for authors that have name [deleted] a new name is assigned with the structure "author_number"
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
  
  connections <-rbind(comments_posts, nested_comments) %>% 
    filter(from!=to) %>% 
    group_by(from, to) %>%
    summarise(weight = n()) %>% 
    ungroup() %>%
    mutate(width = weight+5)
  
  #create igraph object
  #First two columns work as edge list and the others as 
  g <- graph_from_data_frame(connections,directed=FALSE)
  
  #execute clustering algorithms
  #louvain
  start_time <- Sys.time()
  lc <- cluster_louvain(g)
  communities(lc)
  end_time <- Sys.time()
  time_louvain <-end_time - start_time
  
  #label propagation 
  start_time <- Sys.time()
  lpc <- cluster_label_prop(g)
  end_time <- Sys.time()
  time_lpc <-end_time - start_time
  
  #infomap
  start_time <- Sys.time()
  imc <- cluster_infomap(g)
  end_time <- Sys.time()
  time_imc <-end_time - start_time
  
  result <- data.frame(number_comments = nrow(raw_data), 
                       number_posts = length(unique(raw_data$link)),
                       clusters_louvain = length(communities(lc)),
                       clusters_infomap = length(communities(imc)),
                       clusters_lp = length(communities(lpc)),
                       clusters_1_louvain = do.call(sum, lapply(communities(lc), function(x) length(x)==1)),
                       clusters_1_infomap = do.call(sum, lapply(communities(imc), function(x) length(x)==1)),
                       clusters_1_lp = do.call(sum, lapply(communities(lpc), function(x) length(x)==1)),
                       exec_time_louvain = time_louvain,
                       exec_time_infomap = time_imc,
                       exec_time_lp = time_lpc, 
                       modularity_louvain = modularity(lc),
                       modularity_infomap = modularity(imc),
                       modularity_lp = modularity(lpc))
}


#data selected for evaluation of clusters
evaluations <- data.frame(collectionName = c("stocks","stocks","wallstreetbets","wallstreetbets"),
                         initial_time = c("2020-11-03","2020-11-03","2021-01-03","2021-02-03"),
                         end_time = c("2020-11-07","2020-11-12","2021-01-07","2021-02-05"))

result <-data.frame()
for(i in 1:nrow(evaluations)) {
  row <- evaluations[i,]
  result_evaluation <- clustering_evaluation("reddit",row$collectionName,row$initial_time,row$end_time)
  result <-rbind(result, result_evaluation)
}
