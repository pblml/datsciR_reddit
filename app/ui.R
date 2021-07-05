
library(shiny)
library(shinythemes)
require(visNetwork)
require(plotly)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        # tags$head(
        #     tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
        # ),
        theme = shinytheme("cerulean"),
        title = "Reddit communities",
        tags$div(class = "title",
                 tags$h1("Analysis Reddit communities"),),
        tabsetPanel(
            tabPanel(title = "Home",
                    tags$h2("Overview"),
                    tags$p("In January of 2021, a major short squeeze of the stock of video game retailer GameStop and other securities took place[@RePEc]. A major driving force behind this event was the subreddit wallstreetbets, where users discussed the market situation around GameStop, focussing on the amount of short positions major hedge funds held for this stock and a “David versus Goliath”-situation unfolded. What followed was a large influx of users to the subreddit, who were convinced the stock had to go up and hedge funds had to pay for it."),
                    tags$p("Several interesting questions regarding the influence of these subreddits on the stock market emerge from this event. In this project, we want to detect communities in finance related subreddits and mine their sentiment towards discussed stocks to measure if they either have significant influence on or can predict market movements."),
                    tags$h2("Research questions"),
                    tags$h4("1. How can we identify subcommunities inside subreddit and which measures can be used to evaluate the subcommunities?"),
                    tags$p("A subreddit is already a big community inside Reddit, which allows the interaction between users. It is important to represent the interaction from the users in order to find relations and smaller clusters from users which we can analyze afterwards."),
                    tags$h4("2. Is it possible to identify stocks that are discussed in each subcommunity and the sentiment related to the stocks?"),
                    tags$p("For this question we would like to find strategies that allows us to identify the stocks that are mentioned inside each community and the sentiment (positive, negative or neutral) towards the stocks."),
                    tags$h4("3. Based on the stocks and sentiments towards them inside the communities, is it possible to find a correlation with the stock prices?"),
                    tags$p("Considering the Gamestop event we would like to find out if there are relations between the sentiment that the users express in the subreddit towards a stock with the real stock prices.")
                    ),
            tabPanel(title = "Reddit Data",
                     ""),
            tabPanel(title = "Community Detection",""),
            tabPanel(title = "Interactive Community creation",
                     sidebarLayout(
                         sidebarPanel(
                             dateRangeInput("daterange1", "Date range:",
                                            start = "2021-01-21",
                                            end   = "2021-01-23",
                                            min    = "2020-11-01",
                                            max    = "2021-06-20"),
                             selectInput("subreddit", "Subreddit:",
                                         c("Stocks" = "stocks",
                                           "Wallstreetbets" = "wallstreetbets")),
                             selectInput("symbol", "Ticker Symbol", choices=c("AMC", "TSLA", "PLTR"))
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                             visNetworkOutput("clusterVisualizarion"),
                             plotlyOutput("nodes_data_from_shiny")
                         )
                     )),
            tabPanel(title = "Stock Analysis",""),
            tabPanel(title = "Resources",
                     "")
        ),

    )
)
