library(tidyverse)
library(progress)
source("DB_connection.R")

reddit_urls <- function(subreddit, start_date, end_date, num_urls=5) {
  #extracts the top 100 submissions by comments from a subreddit
  #subreddit: string, subreddit to get URLs from
  #start_date: PosixCt datetime to start searching from
  #end_date: PosixCt datetime to search to
  #one day ~ 3 secs
  
  url_list <- list()
  seq_days <- seq(start_date, end_date, by="1 day")
  pb <- progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = length(seq_days), clear = FALSE, width= 60)
  for (day in seq_days) {
    skip_to_next <- FALSE
    
    tryCatch({
      pb$tick()
      url_list <- sprintf("https://api.pushshift.io/reddit/search/submission/?q=&after=%s&before=%s&subreddit=%s&author=&aggs=&metadata=true&frequency=hour&advanced=false&sort=desc&domain=&sort_type=num_comments&size=%s",
                          as.integer(day), as.integer(day)+86399, subreddit, num_urls) %>%
        URLencode() %>%
        jsonlite::fromJSON() %>% 
        .$data %>%
        as.data.frame() %>%
        select(full_link)
      
      Sys.sleep(1)}, error = function(e) {
        print(e)
        skip_to_next <<- TRUE
      })
    
    if(skip_to_next) {next}
    
  }
  return(url_list)
}

reddit_content2 <- function (URL, wait_time = 2) {
  if (is.null(URL) | length(URL) == 0 | !is.character(URL)) {
    stop("invalid URL parameter")
  }
  GetAttribute = function(node, feature) {
    Attribute = node$data[[feature]]
    replies = node$data$replies
    reply.nodes = if (is.list(replies)) 
      replies$data$children
    else NULL
    return(list(Attribute, lapply(reply.nodes, function(x) {
      GetAttribute(x, feature)
    })))
  }
  get.structure = function(node, depth = 0) {
    if (is.null(node)) {
      return(list())
    }
    filter = is.null(node$data$author)
    replies = node$data$replies
    reply.nodes = if (is.list(replies)) 
      replies$data$children
    else NULL
    return(list(paste0(filter, " ", depth), lapply(1:length(reply.nodes), 
                                                   function(x) get.structure(reply.nodes[[x]], paste0(depth, 
                                                                                                      "_", x)))))
  }
  data_extract = data.frame(id = numeric(), structure = character(), 
                            post_date = as.Date(character()), comm_date = as.Date(character()), 
                            num_comments = numeric(), subreddit = character(), upvote_prop = numeric(), 
                            post_score = numeric(), author = character(), user = character(), 
                            comment_score = numeric(), controversiality = numeric(), 
                            comment = character(), title = character(), post_text = character(), 
                            link = character(), domain = character(), URL = character())
  pb <- progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = length(seq(URL)), clear = FALSE, width= 60)
  
  for (i in seq(URL)) {
    if (!grepl("^https?://(.*)", URL[i])) 
      URL[i] = paste0("https://www.", gsub("^.*(reddit\\..*$)", 
                                           "\\1", URL[i]))
    if (!grepl("\\?ref=search_posts$", URL[i])) 
      URL[i] = paste0(gsub("/$", "", URL[i]), 
                      "/?ref=search_posts")
    X = paste0(gsub("\\?ref=search_posts$", "", 
                    URL[i]), ".json?limit=100")
    raw_data = tryCatch(RJSONIO::fromJSON(readLines(X, warn = FALSE)), 
                        error = function(e) NULL)
    if (is.null(raw_data)) {
      Sys.sleep(min(1, wait_time))
      raw_data = tryCatch(RJSONIO::fromJSON(readLines(X, 
                                                      warn = FALSE)), error = function(e) NULL)
    }
    if (is.null(raw_data) == FALSE) {
      meta.node = raw_data[[1]]$data$children[[1]]$data
      main.node = raw_data[[2]]$data$children
      if (min(length(meta.node), length(main.node)) > 0) {
        structure = unlist(lapply(1:length(main.node), 
                                  function(x) get.structure(main.node[[x]], x)))
        TEMP = data.frame(id = NA, structure = gsub("FALSE ", 
                                                    "", structure[!grepl("TRUE", structure)]), 
                          post_date = as.POSIXct(meta.node$created_utc, 
                                                 origin = "1970-01-01", tz = "UTC"), 
                          comm_date = as.POSIXct(unlist(lapply(main.node,
                                                               function(x) {
                                                                 GetAttribute(x, "created_utc")
                                                               })), origin = "1970-01-01", tz = "UTC"), 
                          num_comments = meta.node$num_comments, subreddit = ifelse(is.null(meta.node$subreddit), 
                                                                                    "UNKNOWN", meta.node$subreddit), upvote_prop = meta.node$upvote_ratio, 
                          post_score = meta.node$score, author = meta.node$author, 
                          user = unlist(lapply(main.node, function(x) {
                            GetAttribute(x, "author")
                          })), comment_score = unlist(lapply(main.node, 
                                                             function(x) {
                                                               GetAttribute(x, "score")
                                                             })), controversiality = unlist(lapply(main.node, 
                                                                                                   function(x) {
                                                                                                     GetAttribute(x, "controversiality")
                                                                                                   })), comment = unlist(lapply(main.node, function(x) {
                                                                                                     GetAttribute(x, "body")
                                                                                                   })), title = meta.node$title, post_text = meta.node$selftext, 
                          link = meta.node$url, domain = meta.node$domain, 
                          URL = URL[i], stringsAsFactors = FALSE)
        TEMP$id = 1:nrow(TEMP)
        if (dim(TEMP)[1] > 0 & dim(TEMP)[2] > 0) 
          data_extract = rbind(TEMP, data_extract)
        else print(paste("missed", i, ":", 
                         URL[i]))
      }
    }
    pb$tick()
    Sys.sleep(min(2, wait_time))
  }
  return(data_extract)
}

ETL <- function(subreddit_vec, start_date, end_date, date_list=NULL) {
  if (is.null(date_list)){
    seq_days <- seq(start_date, end_date, by="1 day")  
  } else {
    seq_days <- date_list
  }
  res_lst <- list()
  
  tryCatch({
  for (sub in subreddit_vec){
    tmp_df <- data.frame()
    for (d in seq_days) {
      print(as.POSIXct(d, origin = "1970-01-01", tz = "UTC"))
      urls <- reddit_urls(sub, 
                          as.POSIXct(d, origin = "1970-01-01", tz = "UTC"),
                          as.POSIXct(d, origin = "1970-01-01", tz = "UTC"))
      for (url in urls$full_link){
        if (length(urls)!=0){
          content <- reddit_content2(URLencode(url))
          tmp_df <- rbind(tmp_df, content)
          print(nrow(tmp_df))
        }
      }
    }
    res_lst[[paste0(sub)]] <- tmp_df
  }}, error=function(cond) {print(paste0(cond, " ", d))
    return(res_lst)})
  return(res_lst)
}

# mask <- (seq(min(stocks$date), max(stocks$date), by="1 days") %in% stocks$date)
# seq(min(stocks$date), max(stocks$date), by="1 days")[!mask]
