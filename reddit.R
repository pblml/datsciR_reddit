library(RedditExtractoR)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(visNetwork)

raw_data <- get_reddit(subreddit = "wallstreetbets", cn_threshold = 20, page_threshold = 999,)

content <- raw_data %>% 
  mutate(
    from = paste0(author, "_", structure),
    to = gsub("^(.*)_\\d+$", "\\1", structure) %>%
      paste0(getcontent$author, "_", .)
    )

edges <- content %>%
  select(user, from, to) %>% 
  left_join(., ., by=c("from"="to")) %>% 
  select(-starts_with("from"), -starts_with("to"), "from"=user.x, "to"=user.y) %>% 
  group_by(from, to) %>%
  summarise(weight = n()) %>% 
  ungroup() %>%
  mutate(width = weight+5)

nodes <- c(edges$from, edges$to) %>%
  unique() %>%
  data.frame("id" = .)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

ggraph(routes_tidy, layout = "linear") + 
  geom_node_point() +
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 8)) +
  theme_graph()

visNetwork(nodes, edges) %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visNodes(size = 10) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T)
