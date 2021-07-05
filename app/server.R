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

dat <- rbind(loadDataCol("reddit", "wallstreetbets"), loadDataCol("reddit", "stocks"))
#dat <- readRDS("../stocks.Rds",chdir=T)
#wsb <- readRDS("../wsb.Rds")
#dat<- readRDS("../full_data.Rds")#rbind(stocks,wsb)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$clusterVisualizarion <- renderVisNetwork({
    initial_date <- input$daterange1[1]
    end_date <- input$daterange1[2]
    subreddit <- input$subreddit
    create_communities_visualization(subreddit,initial_date,end_date)
  })
 
  observeEvent(input$symbol, {
    symbol <- input$symbol
    dat_tmp <- dat %>% 
      mutate(date=lubridate::date(comm_date)) %>%
      filter(subreddit == input$subreddit, date>=input$daterange1[1], date<=input$daterange1[2], ticker==symbol)
    validate(
      need(nrow(dat_tmp)!=0, paste0("No mention of ", symbol))
    )
    output$nodes_data_from_shiny <- renderPlotly({
      
      
      plot_ts(dat_tmp, symbol)
      
    })
  })
})
