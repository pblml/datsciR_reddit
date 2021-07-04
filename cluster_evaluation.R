library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(tm)
library(wordcloud)
library(visNetwork)
source("DB_connection.R")

#create an igraph representation of a subreddit in a timeframe
create_subreddit_graph <- function(databaseName,collectionName,initial_time,end_time){
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
  g
}


#evaluate different clustering algorithms
clustering_evaluation <- function(id_test,databaseName,collectionName,initial_time,end_time){

  g <-create_subreddit_graph(databaseName,collectionName,initial_time,end_time)
  
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
  
  modularities <- data.frame(id_test = rep(id_test,3),
                             measure = rep("modularity",3),
                             algorithm = c("Louvain","Infomap","Label Propagation"),
                             value = c(modularity(lc),modularity(imc),modularity(lpc)),
                             clusters_one_user = rep(NA, 3))
  clusters <- data.frame(id_test = rep(id_test,3),
                         measure = rep("clusters",3),
                         algorithm = c("Louvain","Infomap","Label Propagation"),
                         value = c(length(communities(lc)),length(communities(imc)),length(communities(lpc))),
                         clusters_one_user = c(do.call(sum, lapply(communities(lc), function(x) length(x)==1)),
                                               do.call(sum, lapply(communities(imc), function(x) length(x)==1)),
                                               do.call(sum, lapply(communities(lpc), function(x) length(x)==1))))
  time <- data.frame(id_test = rep(id_test,3),
                     measure = rep("time",3),
                     algorithm = c("Louvain","Infomap","Label Propagation"),
                     value = c(as.numeric(time_louvain),as.numeric(time_imc),as.numeric(time_lpc)),
                     clusters_one_user = rep(NA, 3))
  result <- list(df=rbind(modularities,clusters,time),number_comments =nrow(raw_data))
}


#data selected for evaluation of clusters
evaluations <- data.frame(test = 1:4,
                         collectionName = c("wallstreetbets","stocks","wallstreetbets","stocks"),
                         initial_time = c("2021-02-03","2020-11-03","2021-01-03","2020-11-03"),
                         end_time = c("2021-02-05","2020-11-07","2021-01-07","2020-11-12"),
                         number_comments = rep(NA, 4))

result <-data.frame()
for(i in 1:nrow(evaluations)) {
  print(i)
  row <- evaluations[i,]
  result_evaluation <- clustering_evaluation(row$test,"reddit",row$collectionName,row$initial_time,row$end_time)
  #update number of comments per test
  evaluations[[i,"number_comments"]]<-result_evaluation$number_comments
  result <-rbind(result, result_evaluation$df)
}


#Additional functions over generated communities
generate_term_doc_matrix_community <-function(raw_data,communities,community) {
  posts_community <- subset(raw_data,user %in% as.list(communities[[community]])|author %in%
                              as.list(communities[[community]]))
  single_posts <- unique(posts_community$post_text)
  TextDoc <- Corpus(VectorSource(c(posts_community$comment,single_posts)))
  # pre processing of data
  #Replacing "/", "@" and "|" with space
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  TextDoc <- tm_map(TextDoc, toSpace, "/")
  TextDoc <- tm_map(TextDoc, toSpace, "@")
  TextDoc <- tm_map(TextDoc, toSpace, "\\|")
  # Convert the text to lower case
  TextDoc <- tm_map(TextDoc, content_transformer(tolower))
  # Remove numbers
  TextDoc <- tm_map(TextDoc, removeNumbers)
  # Remove english common stopwords
  TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
  # Remove punctuations
  TextDoc <- tm_map(TextDoc, removePunctuation)
  # Eliminate extra white spaces
  TextDoc <- tm_map(TextDoc, stripWhitespace)
  
  # Build a term-document matrix
  TextDoc_dtm <- TermDocumentMatrix(TextDoc)
  dtm_m <- as.matrix(TextDoc_dtm)
  # Sort by descearing value of frequency
  dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
  dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
  return(dtm_d)
}


get_word_cloud_community <- function(raw_data,communities,community) {
  dtm_d <-generate_term_doc_matrix_community(raw_data,communities,community)
  set.seed(1234)
  wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 1,           
             max.words=50, random.order=FALSE, rot.per=0.35,
             colors=brewer.pal(8, "Dark2"),scale=c(5, .2))
  #wc <-wordcloud2(dtm_d)
  #wc
}

#retrieve the 3 more common words in a community. As input it expects the raw data that generated the communities, 
#the communities object and the number of the community
get_relevant_words_community <- function(raw_data,communities,community) {
  dtm_d <-generate_term_doc_matrix_community(raw_data,communities,community)
  relevant_words <- paste(head(dtm_d, 3)$word,collapse=', ')
  return(relevant_words)
}


get_related_tickers_community <-function(comments, communities,community){
  tryCatch({
    tickers_comments <- comments %>%
      filter(user %in% as.list(communities[[community]])|author %in%
               as.list(communities[[community]])) %>%
      select(ticker) 
    #turn list of tickers into string of tickers separated by ,
    aux_tickers <-paste(lapply(tickers_comments$ticker, function(x) paste(x,collapse=',')),
                        collapse=",")
    # get frequency of tickers and select top 3 and frequency
    tickers_community <- as.data.frame(table(strsplit(aux_tickers, ","),exclude = "")) %>%
      arrange(desc(Freq)) %>%
      slice_head( n=3)%>%
      mutate(ticker_info = paste(Var1,"(",Freq,")",sep=""))
      
    tickers_community <-paste(tickers_community$ticker_info,collapse=",")
  },
  error=function(cond) {
    return(c(""))
  }
  )
}


tickers <- get_related_tickers_community(raw_data,communities_cl,5)

get_related_tickers_user <-function(comments, nameUser){
  tryCatch({
    tickers_comments <- comments %>%
      filter(user == nameUser|author == nameUser) %>%
      select(ticker) 
    aux_tickers <-paste(lapply(tickers_comments$ticker, function(x) paste(x,collapse=',')),
                        collapse=",")
    unique_tickers <-unique(unlist(strsplit(aux_tickers, ",")))
    unlist(paste(unique_tickers,collapse=","))
  },
  error=function(cond){
    return("")
  })
}


#create interactive communities
create_communities_visualization <- function(collectionName, initial_date, final_date){
  
  databaseName <- "reddit"
  #get related information DB
  raw_data <- loadDataDates(databaseName,collectionName,initial_date,final_date) %>%
    filter(user!="[deleted]") 
  #create graph representation
  g <-create_subreddit_graph(databaseName,collectionName,initial_date,final_date)
  #create communities
  cl <- cluster_louvain(g)
  communities_cl <- communities(cl)
  #get relevant tickers for each community
  tickers_communities <- lapply(1:length(communities_cl),
                                function(x)
                                  get_related_tickers_community(raw_data,communities_cl,x))
  
  #get nodes and edges
  nodes <-do.call(rbind.data.frame, as.list(V(g)$name)) %>%
    rename_at(1,~"id") %>%
    mutate(id_group = membership(cl), label = id) %>%
    mutate(description = unlist(tickers_communities[id_group])) %>%
    unite('group',c(id_group,description), remove=FALSE, sep=": ") %>%
    select(id,group,label)
  
  #get tickers per node
  nodes$tickers = lapply(nodes$id,
                         function(x)
                           get_related_tickers_user(raw_data,x))
  #add title to nodes
  nodes <- nodes %>%
    mutate(title = paste0("<p><b>Tickers ",id,":</b><br>",tickers,"</p>"))
  
  #get nodes
  edges <- get.data.frame(g, what= c("edges") )

  #create VisNetwork representation
  visualization <- visNetwork(nodes, edges)%>%
    visClusteringByGroup(groups = unique(nodes$group), label="Cluster ")%>% 
    #visNodes(title = "<p><b>Cluster</b></p>")%>%
    visInteraction(navigationButtons = TRUE) %>%
    visPhysics(maxVelocity = 1,repulsion = list(centralGravity=-0.5,springLength=500)) 
} 

# collectionName <- "stocks"
# initial_date <-"2021-01-20"
# final_date <-"2021-02-04"
# vis <-create_communities_visualization(collectionName,initial_time,end_time)
# vis