library(tidyverse)

reddit_urls <- function(subreddit, start_date, end_date) {
  #subreddit: string, subreddit to get URLs from
  #start_date: PosixCt datetime to start searching from
  #end_date: PosixCt datetime to search to
  
  url_list <- list()
  for (day in seq(start_date, end_date, by="1 day")) {
    print(day)
    url_list[[paste0(day)]] <- sprintf("https://api.pushshift.io/reddit/search/submission/?q=&after=%s&before=%s&subreddit=%s&author=&aggs=&metadata=true&frequency=hour&advanced=false&sort=desc&domain=&sort_type=num_comments&size=100",
            as.numeric(day), as.numeric(day)+86399, subreddit) %>%
      jsonlite::fromJSON()
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
  pb = utils::txtProgressBar(min = 0, max = length(URL), style = 3)
  for (i in seq(URL)) {
    if (!grepl("^https?://(.*)", URL[i])) 
      URL[i] = paste0("https://www.", gsub("^.*(reddit\\..*$)", 
                                           "\\1", URL[i]))
    if (!grepl("\\?ref=search_posts$", URL[i])) 
      URL[i] = paste0(gsub("/$", "", URL[i]), 
                      "/?ref=search_posts")
    X = paste0(gsub("\\?ref=search_posts$", "", 
                    URL[i]), ".json?limit=500")
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
                          post_date = format(as.Date(as.POSIXct(meta.node$created_utc, 
                                                                origin = "1970-01-01")), "%d-%m-%y"), 
                          comm_date = format(as.Date(as.POSIXct(unlist(lapply(main.node, 
                                                                              function(x) {
                                                                                GetAttribute(x, "created_utc")
                                                                              })), origin = "1970-01-01")), "%d-%m-%y %H:%M:%S"), 
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
    utils::setTxtProgressBar(pb, i)
    Sys.sleep(min(2, wait_time))
  }
  close(pb)
  return(data_extract)
}