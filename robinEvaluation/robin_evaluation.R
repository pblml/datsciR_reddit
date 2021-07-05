library(robin)
source("cluster_evaluation.R")

#### subreddit stocks

graph_stocks <- create_subreddit_graph("reddit","stocks","2020-11-01","2021-05-02")

##Finds if the community structure found is statistically significant by compare to a random curve
graphRandom <- random(graph=graph_stocks)
#comparison random with louvain
robust_stocks_louvain <- robinRobust(graph=graph_stocks, graphRandom=graphRandom, method="louvain",
                                     measure="vi",type="independent")
#plot comparison Louvain and random
plotRobin_random_louvain <- plotRobin(graph=graph_stocks, 
                                      model1=robust_stocks_louvain$Mean, 
                                      model2=robust_stocks_louvain$MeanRandom,
                                     measure="vi", legend=c("Real Data", "Null Model"))

#store plot
jpeg(filename="Stocks_robust_louvain.jpeg")
plotRobin(graph=graph_stocks, 
          model1=robust_stocks_louvain$Mean, 
          model2=robust_stocks_louvain$MeanRandom,
          measure="vi", legend=c("Real Data", "Null Model"),
          title = "Robin plot Stocks")
dev.off()


#get the AUC
robinAUC_random_louvain <-robinAUC(graph=graph_stocks, model1=robust_stocks_louvain$Mean, 
                                   model2=robust_stocks_louvain$MeanRandom)

###Results: 
###$area1
###[1] 0.2631427
###
###$area2
###[1] 0.2494959

#get Bayes Factor
BFLouvain <- robinGPTest(model1=robust_stocks_louvain$Mean,
                         model2=robust_stocks_louvain$MeanRandom)
###Results: 
###282.8668

#return the fitted curves and the adjusted p-values
robinFDATest(graph=graph_stocks, model1=robust_stocks_louvain$Mean, 
             model2=robust_stocks_louvain$MeanRandom,
             measure="vi",legend=c("Real Data", "Null Model"))

#store plot
jpeg(filename="Stocks_adjust_pvalues_louvain.jpeg")
robinFDATest(graph=graph_stocks, model1=robust_stocks_louvain$Mean, 
             model2=robust_stocks_louvain$MeanRandom,
             measure="vi",legend=c("Real Data", "Null Model"))
dev.off()

###Results
###$adj.pvalue
###[1] 0.1331 0.0108 0.0361 0.0045 0.0045 0.0036 0.0038 0.0045 0.0086
###$pvalues
###[1] 0.1331 0.0024 0.0361 0.0024 0.0024 0.0024 0.0024 0.0024 0.0024



#compare two algorithms in the same initial graph

#compare louvain with label propagation 
compareLouvainLP <- robinCompare(graph=graph_stocks, method1="louvain",
                                 method2="labelProp", measure="vi", type="independent")


plotRobin_louvain_lp <- plotRobin(graph=graph_stocks, 
          model1=compareLouvainLP$Mean1, 
          model2=compareLouvainLP$Mean2,
          measure="vi", legend=c("Louvain", "LabelPropagation"))

jpeg(filename="Stocks_louvain_vs_lp.jpeg")
plotRobin(graph=graph_stocks, 
          model1=compareLouvainLP$Mean1, 
          model2=compareLouvainLP$Mean2,
          measure="vi", legend=c("Louvain", "LabelPropagation"))
dev.off()

robinAUC_louvain_lp <- robinAUC(graph=graph_stocks, 
        model1=compareLouvainLP$Mean1, 
         model2=compareLouvainLP$Mean2)

###Results: 
###$area1
###[1] 0.2638162
###
###$area2
###[1] 0.05335504



# #Comparison Louvain with Infomap -- after more than 12 hours it doesn't finish
# plotRobin_louvain_infomap <- robinCompare(graph=graph_stocks, method1="louvain",
#              method2="infomap", measure="vi", type="independent")
# 
# plotRobin(graph=g, model1=compare$Mean1, model2=compare$Mean2,
#           measure="vi", legend=c("Louvain", "Infomap"))
# robinAUC(graph=g, model1=compare$Mean1, model2=compare$Mean2)






###################Subreddit Wallstreetbets

graph_wsb <- create_subreddit_graph("reddit","wallstreetbets","2020-11-01","2021-05-02")

##Finds if the community structure found is statistically significant by compare to a random curve
graphRandom <- random(graph=graph_wsb)
#comparison random with louvain
Sys.time()
robust_wsb_louvain <- robinRobust(graph=graph_wsb, graphRandom=graphRandom, method="louvain",
                                     measure="vi",type="independent")
Sys.time()
#plot comparison Louvain and random
plotRobin_random_louvain <- plotRobin(graph=graph_wsb, 
                                      model1=robust_wsb_louvain$Mean, 
                                      model2=robust_wsb_louvain$MeanRandom,
                                      measure="vi", legend=c("Real Data", "Null Model"))

#store plot
jpeg(filename="WSB_robust_louvain.jpeg")
plotRobin(graph=graph_wsb, 
          model1=robust_wsb_louvain$Mean, 
          model2=robust_wsb_louvain$MeanRandom,
          measure="vi", legend=c("Real Data", "Null Model"),
          title = "Robin plot Wallstreetbets")
dev.off()


#get the AUC
robinAUC_random_louvain <-robinAUC(graph=graph_wsb, model1=robust_wsb_louvain$Mean, 
                                   model2=robust_wsb_louvain$MeanRandom)

###Results: 
###$area1
###[1] 0.2357264
###
###$area2
###[1] 0.2471575

#get Bayes Factor
BFLouvain <- robinGPTest(model1=robust_wsb_louvain$Mean,
                         model2=robust_wsb_louvain$MeanRandom)
###Results: 
###327.2399

#return the fitted curves and the adjusted p-values
robinFDATest(graph=graph_wsb, model1=robust_wsb_louvain$Mean, 
             model2=robust_wsb_louvain$MeanRandom,
             measure="vi",legend=c("Real Data", "Null Model"))

#store plot
jpeg(filename="Stocks_adjust_pvalues_louvain.jpeg")
robinFDATest(graph=graph_wsb, model1=robust_wsb_louvain$Mean, 
             model2=robust_wsb_louvain$MeanRandom,
             measure="vi",legend=c("Real Data", "Null Model"))
dev.off()

###Results
###$adj.pvalue
###[1] 0.0194 0.0043 0.0027 0.0028 0.0055 0.0481 0.0873 0.8813 0.0645
###$pvalues
###[1] 0.0194 0.0022 0.0022 0.0022 0.0055 0.0262 0.0150 0.8813 0.0143



#compare two algorithms in the same initial graph

#compare louvain with label propagation 
compareLouvainLP <- robinCompare(graph=graph_wsb, method1="louvain",
                                 method2="labelProp", measure="vi", type="independent")


plotRobin_louvain_lp <- plotRobin(graph=graph_wsb, 
                                  model1=compareLouvainLP$Mean1, 
                                  model2=compareLouvainLP$Mean2,
                                  measure="vi", legend=c("Louvain", "LabelPropagation"))

jpeg(filename="Stocks_louvain_vs_lp.jpeg")
plotRobin(graph=graph_wsb, 
          model1=compareLouvainLP$Mean1, 
          model2=compareLouvainLP$Mean2,
          measure="vi", legend=c("Louvain", "LabelPropagation"))
dev.off()

robinAUC_louvain_lp <- robinAUC(graph=graph_wsb, 
                                model1=compareLouvainLP$Mean1, 
                                model2=compareLouvainLP$Mean2)

###Results: 
###$area1
###[1] 0.2638162
###
###$area2
###[1] 0.05335504
