######################   KMEANS CLUSTERING ALGORITHM - V2    #######################


# Clear Namespace
rm(list=ls())

# Import Libraries
library(ggplot2)
setwd('C:\\Users\\Chris.Cirelli\\Desktop\\Programming_Repositories\\Clustering_Algorithms')
source('module1_kmeans_algorithm.R')

# Step 1:  Generate A Random Dataset
set.seed(7)
p.x = runif(100, min = 0, max = 10)
p.y = runif(100, min = 0, max = 10)
plot(p.x, p.y)

# Create DataFrame with X & Y Values
df.pxy = data.frame(p.x, p.y)
plot(df.pxy)
head(df.pxy)

# Generate Data Points For K Centroids
c.1 <- f.centroids(4)

# Calculate Euclidean Distance 2 Each Centroid
df.pxy.c.dist <- get.euclid.dist(df.pxy, c.1)

# Create Assignments Based on Distance to Nearest Centroid





# K.MEANS ALGORITHM

k.means <- function(df.pxy, num.centroids, plot.data){
  'df.pxy:           Randomly generated dataset
   num.centroids:    Number of centroids'
  # Generate Centroids
  df.centroids <- centroids(num.centroids)
  # Plot Data Cloud + Centroids
  if (plot.data == TRUE){
    p.1 <- ggplot(data=df.pxy, aes(x=p.x,y=p.y)) + geom_point() + geom_point(data=c.1, aes(x=x, y=y), colour='red', size=5)}
    print(p.1)
    
  # For Loop Fitting Process
  
      
  return(df.centroids)
}


test.1 <- k.means(df.pxy, 2, TRUE)






