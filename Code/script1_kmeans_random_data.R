######################   KMEANS CLUSTERING ALGORITHM - V2    #######################


# CLEAR NAME SPACE
rm(list=ls())

# IMPORT LIBRARIES
library(ggplot2)
library(dplyr)
setwd('C:\\Users\\Chris.Cirelli\\Desktop\\Programming_Repositories\\Clustering_Algorithms\\Code')
source('module1_kmeans_algorithm.R')


# DRIVER FUNCTION - K MEANS CLUSTER--------------------------------------------------
k.means <- function(num.data.points, num.centroids, num.iters){
  
  # Step 1:  Generate Random Dataset
  set.seed(7)
  p.x = runif(num.data.points, min = 0, max = 10)
  p.y = runif(num.data.points, min = 0, max = 10)
  
  # Step 2:  Create DataFrame with X & Y Values
  df.d = data.frame(p.x, p.y)  
  p.1 <- ggplot(df.d, aes(x=p.x, y=p.y)) + geom_point() + ggtitle('RAW DATA')
  print(p.1)
  Sys.sleep(4)
  
  # Step 3:  Generate Random Data Points For K Centroids
  df.c <- f.centroids(num.centroids)
  
  # Step4:   Iterate N times each time recalculating the centroids
  for (i in seq(1, num.iters)){
    # a.  Print User Log
    print(paste('Starting Iteration =>', i))
    
    # b.  Calculate Euclidean Distance 2 Each Centroid
    df.d.c.dist <- get.euclid.dist(df.d, df.c)
    
    # c.  Create Assignments Based on Distance to Nearest Centroid
    df.c.assignments <- c.assignments(df.d.c.dist)
    
    # d.  Plot The Assignment of Each Point & Centroid
    plot.c.assignments(df.c.assignments, df.c, i)
    
    # e.  Calculate the within and between cluster mse
    sys.mse <- round(calc.mse(df.c.assignments),2)
    print(paste('MSE => ', sys.mse))
    
    # f.  Set Delay between iterations
    Sys.sleep(4)
    
    # g.  Recalculate Centroids
    df.c <- re.calc.centroids(df.c.assignments)
    
  }
}


k.means(num.data.points = 300, 
        num.centroids   = 2, 
        num.iters       = 6)







