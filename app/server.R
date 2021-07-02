#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("cluster_evaluation.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$clusterVisualizarion <- renderVisNetwork({
    initial_date <- input$daterange1[1]
    end_date <- input$daterange1[2]
    subreddit <- input$subreddit
    create_communities_visualization(subreddit,initial_date,end_date)
  })
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
})
