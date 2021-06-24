library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
source("DB_connection.R")

databaseName <- "reddit"
collectionName <- "stocks"

raw_data <- loadDataDates(databaseName,collectionName,"2020-12-23","2020-12-25") %>%
  filter(user!="[deleted]",author!="[deleted]")

raw_data <-head(raw_data,500)

#Select the direct comments to a post
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

# louvain community detection 
start_time <- Sys.time()
lc <- cluster_louvain(g)
end_time <- Sys.time()
total_time <-end_time - start_time

membership(lc)
communities(lc)
plot(lc, g )
tkplot(g)
rglplot(g)

#create dataframe with nodes and group for visualization
nodes <-do.call(rbind.data.frame, as.list(V(g)$name))
nodes$group =membership(lc)
colnames(nodes)<-c('id','group')
nodes$label =nodes$id
edges <- get.data.frame(g, what= c("edges") )
visNetwork(nodes, edges)%>%
 # visOptions(selectedBy = "group", 
 #             highlightNearest = TRUE) %>%
  visClusteringByGroup(groups = unique(nodes$group), label="cluster: ")

 

# infomap 
start_time <- Sys.time()
imc <- cluster_infomap(g)
end_time <- Sys.time()
membership(imc)
communities(imc)
plot(imc, g)

#get all of the post related to users in a community
communities_imc <- communities(imc)
posts_first_community <- subset(clean_data,from %in% as.list(communities_imc$'1')|to %in% as.list(communities_imc$'1'))
single_posts <- unique(posts_first_community$post_text)

#sentiment analysis
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(c(posts_first_community$comment,single_posts)))

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
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)


# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 1,           
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))

# Find associations 
findAssocs(TextDoc_dtm, terms = c("fuck"), corlimit = 0.5)			

# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
posts_first_community$sentiment <- get_sentiment(posts_first_community$comment, method="syuzhet")
syuzhet_vector <- get_sentiment(posts_first_community$comment, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)
#interpratation -> as meadian is 0. It can be interpreted as the overall average sentiment across the all the responses is positive



##sentiment analysis --> Emotion classification

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
d<-get_nrc_sentiment(posts_first_community$comment)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)
## for each column it returns the number of ocurrencies for each of the emotions

td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:40]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

library(ggplot2)
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")


##-------------------------metrics ------------------------------------------------

modularity(lc)
modularity(imc)


compare(lc, imc)


##------------------------end metrics ---------------------------------------------




##-------------------------other algorithms ---------------------------------------
#greedy
greedy <- cluster_fast_greedy(g) # problem with multiple edges

start_time <- Sys.time()
cluster_edge_betweenness <-cluster_edge_betweenness(g)
end_time <- Sys.time()
communities(cluster_edge_betweenness)
modularity(cluster_edge_betweenness)

start_time <- Sys.time()
cl_lp <- cluster_label_prop(g)
end_time <- Sys.time()
communities(cl_lp)
modularity(cl_lp)

#cluster optimal 
start_time <- Sys.time()
optimal <-cluster_optimal(g)
end_time <- Sys.time()
modularity(optimal)
compare(imc,optimal)
#cluster edge betweenness
cluster_eb <-cluster_edge_betweenness(g)
modularity(cluster_eb)
compare(optimal,cluster_eb)

closeness(g, mode="all")
##-------------------------end other algorithms -----------------------------------


