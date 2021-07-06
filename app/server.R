#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("../cluster_evaluation.R", chdir = T)
source("../DB_connection.R", chdir = T)
source("../analysis_helpers.R", chdir = T)

#dat <- rbind(loadDataCol("reddit", "wallstreetbets"), loadDataCol("reddit", "stocks"))
#dat <- rbind(loadDataDates("reddit","wallstreetbets","2021-01-01","2021-01-15"), loadDataDates("reddit", "stocks","2021-01-01","2021-01-15"))
#dat <- readRDS("../stocks.Rds")
dat <- readRDS("../stocks_January.Rds")
#wsb <- readRDS("../wsb.Rds")
#dat<- readRDS("../full_data.Rds")#rbind(stocks,wsb)

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {

  output$clusterVisualizarion <- renderVisNetwork({
    initial_date <- input$daterange1[1]
    end_date <- input$daterange1[2]
    subreddit <- input$subreddit
    #create_communities_visualization(subreddit,initial_date,end_date)
    validate(
      need(difftime(input$daterange1[2], input$daterange1[1], "days") <= 14, "Please select a range smaller than 14 days"
      )
    )
    vis_data <- dat %>%
      mutate(date=lubridate::date(comm_date)) %>%
      filter(subreddit == input$subreddit, date>=input$daterange1[1], date<=input$daterange1[2])
    create_communities_visualization_from_df(vis_data)
  })
  observeEvent(input$daterange1,{
    mentioned_tickers <- dat %>%
      mutate(date=lubridate::date(comm_date)) %>%
      filter(subreddit == input$subreddit, date>=input$daterange1[1], date<=input$daterange1[2]) %>%
      select(ticker) %>%
      unlist() %>%
      paste0(collapse = " ") %>%
      str_squish() %>%
      strsplit(" ") %>%
      table() %>%
      as.data.frame() %>%
      arrange(desc(Freq)) %>% 
      head(10) %>%
      pull(1)
    
    updateSelectInput(session, "symbol", choices = mentioned_tickers)  
  })
  
  
  output$tableReddit <- DT::renderDataTable(
    head(dat %>%
      subset(select = -c(ticker,sentiment)),3),
    options = list(
      dom = 't',
      ordering = FALSE,
      rowCallback = JS("function(r,d) {$(r).attr('height', '100px')}"))
      
  )
  
  output$tableStockData <- DT::renderDataTable({
    tickers <- c("TSLA")
    l.out <- BatchGetSymbols(tickers = tickers, 
                             first.date = lubridate::ymd_hms("2021-06-07 00:00:00"),
                             last.date = lubridate::ymd_hms("2021-06-09 00:00:00"), 
                             freq.data = "daily",
                             cache.folder = file.path(tempdir(), 
                                                      'BGS_Cache'))
    head(l.out$df.tickers, 5)
    }
  )
 
  observeEvent(input$symbol, {
    symbol <- input$symbol
    dat_tmp <- dat %>% 
      mutate(date=lubridate::date(comm_date)) %>%
      rowwise() %>%
      filter(subreddit == input$subreddit, date>=input$daterange1[1], date<=input$daterange1[2], symbol %in% ticker) %>%
      ungroup()
    validate(
      need(nrow(dat_tmp)!=0, paste0("No mention of ", symbol))
    )
    output$nodes_data_from_shiny <- renderPlotly({
      
      
      plot_ts(dat_tmp, symbol)
      
    })
  })
})
