require(shiny)
require(visNetwork)
require(plotly)
dat_tmp <- readRDS("dat_tmp.Rds")
server <- function(input, output, session) {
  nodes <- data.frame(id = 1:3,
                      label = c("1", "2", "3"), 
                      name = c("first", "second", "third"), 
                      extra = c("info1", "info2", "info3"),
                      ticker = c("PLTR", "AMZN", "AMZN"))
  nodes <- nodes %>% mutate(title = paste0('<button id=', id,'_',ticker, '>', ticker, '</button>',
                                           '<br>',
                                           '<button id=', id,'_',ticker, '>', ticker, '</button>'))
  edges <- data.frame(from = c(1,2), to = c(1,3), id = 1:2)
  
  output$network_proxy <- renderVisNetwork({
    visNetwork(nodes, edges)
  })
  
  
  
  observeEvent(input$js.button_clicked, {
    output$nodes_data_from_shiny <- renderPlotly({
      symbol <- NULL
      
      
      uid = str_split(input$js.button_clicked, "_")
      button = uid[[1]][1]
      symbol <- uid[[1]][2]
      # for debugging...
      print(paste0("ID:", button,  " Ticker: ", symbol))
      
      l.out <- BatchGetSymbols(tickers = symbol,
                               first.date = min(dat_tmp$date, na.rm = T),
                               last.date = max(dat_tmp$date, na.rm = T),
                               freq.data = "daily",
                               cache.folder = file.path(tempdir(),
                                                        'BGS_Cache'))
      fig1 <- l.out$df.tickers %>%
        filter(ticker==symbol) %>%
        plot_ly(x = ~ref.date, type="candlestick",
                open = ~price.open, close = ~price.close,
                high = ~price.high, low = ~price.low) %>%
        layout(xaxis = list(rangeslider = list(visible = F)), title=paste0(symbol))
      
      ay <- list(
        overlaying = "y2",
        side = "right"
      )
      
      fig2 <- dat_tmp %>%
        filter(ticker==symbol) %>%
        plot_ly() %>%
        add_trace(x=~date, y=~sentiment, name="sentiment", type="scatter", line=list(color="#00000"), mode="line")
      
      fig3 <- dat_tmp %>%
        filter(ticker==symbol) %>%
        plot_ly() %>%
        add_trace(x=~date, y=~vol_sentiment, name="vol_sentiment", type="bar")
      
      
      
      subplot(fig1, fig2, fig3, nrows=3, shareX = T)
    })
  })
  
  observeEvent(input$current_node_id, {
    visNetworkProxy("network_proxy") %>%
      visGetNodes() 
  })
  
}

ui <- fluidPage(
  tags$head(tags$script(src="buttonclicks.js")),
  visNetworkOutput("network_proxy", height = "400px"),
  plotlyOutput("nodes_data_from_shiny"),
  actionButton("getNodes", "Nodes")
)

shinyApp(ui = ui, server = server)