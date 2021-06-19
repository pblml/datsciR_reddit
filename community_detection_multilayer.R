##------------------multinet library-----------------------------------------------
library(igraph)
library(multinet)
library(tidyverse)
source("DB_connection.R")

databaseName <- "reddit"
collectionName <- "stocks"

financedb <- loadDataDates(databaseName,collectionName,"2020-12-01","2020-12-03")

clean_data <- financedb %>%
  subset( select = c(author,user,id,structure,title,comment,URL,comm_date,post_text))%>%#,
  rename(
    from = author,
    to = user
  )


net <- ml_empty()

#create igraph object for first layer
#First two columns work as edge list and the others as 
first_layer <- clean_data %>% 
  filter(from!="[deleted]") %>%
  filter(!grepl("_",structure))
g <- graph_from_data_frame(first_layer,directed=FALSE)
#add layer to the multinet
add_igraph_layer_ml(net, g, "first_comments")

#second layer
content <- financedb %>% 
  filter(user!="[deleted]") %>%
  mutate(
    from = structure,
    to = gsub("^(.*)_\\d+$", "\\1", structure) 
  )

edges <- content %>%
  select(user, from, to, link) %>% 
  left_join(., ., by=c( "link"="link","from"="to")) %>% 
  select("from"=user.x, "to"=user.y) %>% 
  filter(from!=to) %>% 
  group_by(from, to) %>%
  summarise(weight = n()) %>% 
  ungroup() %>%
  mutate(width = weight+5)

nodes <- c(edges$from, edges$to) %>%
  unique() %>%
  data.frame("id" = .)

g2 <- graph_from_data_frame(edges,directed=FALSE)
add_igraph_layer_ml(net, g2, "nested_comments")

#community algorithms for multinet
community <-glouvain_ml(net, gamma=1, omega=1, limit=0)
plot(net, vertex.labels.cex=.2, com=community)

community2 <-flat_ec_ml(net)
plot(net, vertex.labels.cex=.5, com=community2)

community3 <- abacus_ml(net, min.actors=3, min.layers=1)
plot(net, vertex.labels.cex=.5, com=community3)

#evaluation 
modularity_ml(net, community)
modularity_ml(net, community2)

nmi_ml(net, community, community2)
omega_index_ml(net, community, community2)


#get information layers and actors for analysis
comm_list <-get_community_list_ml(community, net)
actors <-actors_ml(net)

# All community detection algorithms return a data frame where each row contains actor name, layer
# name and community identifier.
# The evaluation functions return a number between -1 and 1. For the comparison functions, 1 indicates that the two community structures are equivalent. The maximum possible value of modularity
# is <= 1 and depends on the network, so modularity results should not be compared across different
# networks. Also, notice that modularity is only defined for partitioning community structures.
# get_community_list_ml transforms the output of a community detection function into a list by
# grouping all the nodes having the same community identifier and the same layer.

##------------------end multinet library-------------------------------------------