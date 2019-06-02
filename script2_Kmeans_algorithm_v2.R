######################   KMEANS CLUSTERING ALGORITHM - V2    #######################


# Clear Namespace
rm(list=ls())

# Import Libraries
library(ggplot2)
library(dplyr)
setwd('C:\\Users\\Chris.Cirelli\\Desktop\\Programming_Repositories\\Clustering_Algorithms')
source('module1_kmeans_algorithm.R')

# Step 1:  Generate A Random Dataset
set.seed(7)
p.x = runif(100, min = 0, max = 10)
p.y = runif(100, min = 0, max = 10)

# Create DataFrame with X & Y Values
df.d = data.frame(p.x, p.y)

# Generate Data Points For K Centroids
df.c <- f.centroids(4)

# Initial Scatter Plot w/ Centroids
p.1 <- ggplot(df.d, aes(x = p.x, y = p.y)) + geom_point() + 
  geom_point(data=df.c, aes(x=df.c$x, y=df.c$y), colour='red', size=5) + 
  ggtitle('Initial Plot - Data Cloud + Centroids')


# Calculate Euclidean Distance 2 Each Centroid
df.d.c.dist <- get.euclid.dist(df.d, df.c)

# Create Assignments Based on Distance to Nearest Centroid
'Steps:
  1.) Iterate each row
  2.) Determine which dist is the min
  3.) Add a single col with the name of the centroid that represnts the min dist
  4.) Return data frame with new col w/ centroid name
'
df.c.assignments <- c.assignments(df.d.c.dist)

# Plot Function
plot.c.assignments <- function(df.c.assignments, df.c){
  'Inputs:      df.c.assignments - dataframe that includes our data cloud plus assignments
                df.c             - original coordinates for centroids.  
   Output:      tbd.  Plot
  '
  # Step1:  Determine how many centroid groups we have
  names.c <- as.vector(unique(df.c.assignments$centroid.min.dist))
  # Step2:  Limit dataframe to only x,y values & centroid assignments (dplyr has select f)
  df.xy.c <- select(df.c.assignments, p.x, p.y, centroid.min.dist) 
  # Step3:  Isolate x,y values for centroids
  df.c    <- select(df.c.assignments, names.c)
  # Generate Plots
  p <- ggplot(df.xy.c, aes(x = p.x, y = p.y, color = centroid.min.dist)) + geom_point()
  
  # Generate Plot
  p
}


plot.c.assignments(df.c.assignments, df.c)






'So now we have a way of identifying which col has the min euclidean dist
 We can also use this to assign a value to each row (add name to a vector, then convert that vector to a col in the df)
 
 After we have the assignment for each row, we need to subset the data for the plot. 
 
 After we plot the data cloud plus cluster assignments we need to recalculate the x,y coordinates for each cluster. 
 This value is the mean of the x and y coordinates for each cluster assignment. 
 Once recalculated, we use these new cluster values as the input for the second iteration of our algorithm. 
 
 This process repeats until we hit a threshold or max count of iterations.
 
 '





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






