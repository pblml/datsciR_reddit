library(RedditExtractoR)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)

raw_data <- get_reddit(subreddit = "wallstreetbets", cn_threshold = 10) %>%
  filter(user!="[deleted]") 

#Select the columns needed
clean_data <- raw_data %>%
  subset( select = c(author,user,id,structure,title,comment,URL,comm_date))%>%#,
  rename(
    from = author,
    to = user
  )
#create igraph object
#First two columns work as edge list and the others as 
g <- graph_from_data_frame(clean_data,directed=FALSE)

# louvain community detection 
lc <- cluster_louvain(g)
membership(lc)
communities(lc)
plot(lc, g)

# infomap 
imc <- cluster_infomap(g)
membership(imc)
communities(imc)
plot(imc, g)

#get all of the post related to users in a community
communities_imc <- communities(imc)
posts_first_community <- subset(clean_data,from %in% as.list(communities_imc$'1')|to %in% as.list(communities_imc$'1'))
#sentiment analysis
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(posts_first_community$comment))

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
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 2,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# Find associations 
findAssocs(TextDoc_dtm, terms = c("fuck"), corlimit = 0.5)			

# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
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
td_new <- data.frame(rowSums(td[2:59]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

library(ggplot2)
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

##-------------------------other algorithms ---------------------------------------
#greedy
greedy <- cluster_fast_greedy(g)

#cluster optimal 
optimal <-cluster_optimal(g)

closeness(g, mode="all")
##-------------------------end other algorithms -----------------------------------


##------------------multinet library-----------------------------------------------
library(multinet)
net <- ml_empty()
add_igraph_layer_ml(net, g, "comments")

#community algorithms for multinet
community <-glouvain_ml(net, gamma=1, omega=1, limit=0)
plot(net, vertex.labels.cex=.5, com=community)

community2 <-flat_ec_ml(net)
plot(net, vertex.labels.cex=.5, com=community2)
 
community3 <- abacus_ml(net, min.actors=3, min.layers=1)
plot(net, vertex.labels.cex=.5, com=community3)

#evaluation 
modularity_ml(net, community)
modularity_ml(net, community2)

nmi_ml(net, community, community2)
omega_index_ml(net, community, community2)


##------------------end multinet library-------------------------------------------