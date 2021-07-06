
library(shiny)
library(shinythemes)
require(visNetwork)
require(plotly)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        useShinyjs(),
        # tags$head(
        #     tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
        # ),
        theme = shinytheme("united"),
        title = "Reddit communities",
        tags$div(class = "title",
                 tags$h1("Analysis Reddit communities"),),
        tabsetPanel(
            tabPanel(title = "Home",
                    tags$h2("Overview"),
                    tags$p("In January of 2021, a major short squeeze of the stock of video game retailer GameStop and other securities took place. A major driving force behind this event was the subreddit wallstreetbets, where users discussed the market situation around GameStop, focussing on the amount of short positions major hedge funds held for this stock and a “David versus Goliath”-situation unfolded. What followed was a large influx of users to the subreddit, who were convinced the stock had to go up and hedge funds had to pay for it."),
                    tags$p("Several interesting questions regarding the influence of these subreddits on the stock market emerge from this event. In this project, we want to detect communities in finance related subreddits and mine their sentiment towards discussed stocks to measure if they either have significant influence on or can predict market movements."),
                    tags$h2("Research questions"),
                    tags$h4("1. How can we identify subcommunities inside subreddit and which measures can be used to evaluate the subcommunities?"),
                    tags$p("A subreddit is already a big community inside Reddit, which allows the interaction between users. It is important to represent the interaction from the users in order to find relations and smaller clusters from users which we can analyze afterwards."),
                    tags$h4("2. How can we identify stocks that are discussed in each subcommunity and the sentiment related to the stocks?"),
                    tags$p("For this question we would like to find strategies that allows us to identify the stocks that are mentioned inside each community and the sentiment (positive, negative or neutral) towards the stocks."),
                    tags$h4("3. Based on the stocks and sentiments towards them inside the communities, is it possible to find a correlation with the stock prices?"),
                    tags$p("Considering the Gamestop event we would like to find out if there are relations between the sentiment that the users express in the subreddit towards a stock with the real stock prices.")
                    ),
            tabPanel(title = "Data",
                     navlistPanel(
                         tabPanel(title = "Reddit",
                                  tags$p("The data that we use from the subreddits are comments on posts and the post content. The structure of the raw data is shown in the following table: "),
                                  dataTableOutput("tableReddit")),
                         tabPanel(title="Yahoo Finance",
                                  tags$p("To get the market data for the relevant tickers, the package 'quantmod' is used. It takes a list of ticker symbols, a start and an end date and returns the market
data in a clean format."),
                                  dataTableOutput("tableStockData"))
                     )
                     ),
            tabPanel(title = "Community Detection",
                     tags$h2("Graph structure from data"),
                     tags$p("For the community detection we used the library igraph. For this purpose the subreddit's data was transformed into a graph representation. Nodes represent the users of the subreddit, while interaction between the users is represented by edges. The interactions are classified into:"),
                     tags$p(tags$b("1. Answers to a post:")," direct comments to a post. For this case an edge is created between the user and the author of the post."),
                     tags$p(tags$b("2. Answer to a comment:")," comments made as answer to other comments. For this case an edge is created between two users and it requires to use the hierarchy of comments to find the proper connection."),
                     tags$p("The weight of the edges is given by the number of interactions between the two users."),
                     tags$h2("Community detection algorithms"),
                     tags$p("For the community detection we considered the algorithms Louvain, Infomap and Label Propagation from the igraph library. To compare them we used as measure the modularity in 4 different samples of the data.
                            The algorithms were compared in terms of the Modularity of the generated communities and the execution time. The results are shown below:"),
                     tags$img( id = "modularity", src="modularity_clusters.png"),
                     tags$img( id = "executionTime", src="execution_time_clusters.png"),
                     tags$p("According to our results, the best algorithm for the datasets is Louvain. An interactive example of the creation of the communities with this algorithm can be found in the Interactive Community Analysis Tab")
                     ),
            tabPanel(title = "Stock Analysis",
                     tags$p("To answer the the question, if the sentiment towards a specific ticker has an influence on market data, cross-correlation between the sentiment and the percentage change of the price is used."),
                     tags$p("During the cross-correlation time series are lagged against each other, so that the  ",tags$b("t - lag"), "values of one time series are correlated against the values of the other time series at time ",tags$b("t"),"."),
                     tags$p("Since the mentioned tickers are quite sparse, only the top 10 mentioned tickers are used and missing sentiment values are handled by `na.pass`, so they are not considered in the calculation, but simply passed through. Imputation of the missing values is also not feasable, since sentiment values are often not available for longer time spans."),
                     tags$p("The results are visualized using the ACF plots which are returned by the ",tags$b("ccf")," function. It shows the correlation of the two time series depending on the lag, including 95% confidence intervals."),
                     tags$p("Since applying a cross correlation function assumes that the both time series are stationary, this is ensured de-trending the timeseries before the calculation using ",tags$b("ndiffs")," and ",tags$b("diff"),"."),
                     tags$p("The ACF Plots and summary tables for the top 10 tickers mentioned by the subreddits are shown below."),
                     tags$img(id = "acf_top10", src="acf_top10tickers.png"),
                     tags$p("The mean correlation at lag - 2 is 0.22, so the mean sentiment of wallstreetbets on a day could be used to predict market movement two days later."),
                     tags$img(id = "acf_top10_stocks", src="acf_top10tickers_stocks.png"),
                     tags$p("For the stocks subreddit, we get the mean correlation for every lag is close to 0, so the sentiment could not be used to predict market movements in general."),
                     tags$p("These different outcomes probably result from the sparsity of the ticker mentions, so these conclusions should be taking with a grain of salt. Because of this, an analysis of a specific community is not feasable, the data is just too sparse. To remedy this, the extraction of the data has to be done different. Only posts which mention a specific ticker in a given time frame should be extracted to combat this problem of sparse data.")),
                    
            tabPanel(title = "Interactive Community Analysis",
                     sidebarLayout(
                         sidebarPanel(
                             dateRangeInput("daterange1", "Date range:",
                                            start = "2021-01-19",
                                            end   = "2021-01-22",
                                            min    = "2020-11-01",
                                            max    = "2021-05-01"),
                             selectInput("subreddit", "Subreddit:",
                                         c("Stocks" = "stocks",
                                           "Wallstreetbets" = "wallstreetbets")),
                             selectInput("symbol", "Ticker Symbol", choices=c("AMC", "TSLA", "PLTR"))
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                             shinycssloaders::withSpinner(
                                 visNetworkOutput("clusterVisualizarion")),
                             shinycssloaders::withSpinner(
                                 plotlyOutput("nodes_data_from_shiny"))
                             
                         )
                     )),
            tabPanel(title = "Resources",
                     navlistPanel(
                         tabPanel(title = "Rmarkdown Notebook",
                                  tags$a( href = "https://rpubs.com/dianacgr/redditFinanceAnalysis", target="_blank",
                                          tags$img( id = "rmarkdown-logo", src="logo-rmarkdown.jpg",height = 200, width = 200)
                                  )),
                         tabPanel(title = "Github",
                                  tags$a( href = "https://github.com/pblml/datsciR_reddit", target="_blank",
                                          tags$img( id = "github-logo", src="logo_github.jpg",height = 200, width = 200)
                                  )),
                         tabPanel(title = "Screencast video",
                                  tags$a( href = "https://youtu.be/AtRUgU-btCs", target="_blank",
                                          tags$img( id = "rmarkdown-logo", src="yt_logo_rgb_light.png",height = 150, width =350)
                                  )),
                         tabPanel(title = "Data",
                                  tags$a( href = "https://github.com/pblml/datsciR_reddit/blob/main/stocks.Rds", target="_blank",
                                          tags$img( id = "rmarkdown-logo", src="stocks.png",height = 200, width = 200)
                                  ),
                                  tags$a( href = "https://github.com/pblml/datsciR_reddit/blob/main/wsb.Rds", target="_blank",
                                          tags$img( id = "rmarkdown-logo", src="WallStreetBets.png",height = 200, width = 400)
                                  )
                                  )
                         ))
        ),

    )
)
