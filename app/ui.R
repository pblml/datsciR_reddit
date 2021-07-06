
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
                     tags$p("For the community detection we considered the algorithms Louvain, Infomap and Label Propagation from the igraph library. To compare them we used as measure the modularity in 4 different samples of the data"),
                     
                     tags$p("An example of the creation of the communities with the Louvain algorithm can be found in the Interactive Community Analysis Tab")
                     ),
            tabPanel(title = "Stock Analysis",""),
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
                                  tags$a( href = "", target="_blank",
                                          tags$img( id = "rmarkdown-logo", src="logo-rmarkdown.jpg",height = 200, width = 200)
                                  )),
                         tabPanel(title = "Github",
                                  tags$a( href = "", target="_blank",
                                          tags$img( id = "github-logo", src="logo_github.jpg",height = 200, width = 200)
                                  )),
                         tabPanel(title = "Screencast video",
                                  tags$a( href = "", target="_blank",
                                          tags$img( id = "rmarkdown-logo", src="logo-rmarkdown.jpg",height = 200, width = 200)
                                  ))
                         ))
        ),

    )
)
