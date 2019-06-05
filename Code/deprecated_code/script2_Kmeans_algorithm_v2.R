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
df.c

# Initial Scatter Plot w/ Centroids
p.1 <- ggplot(df.d, aes(x = p.x, y = p.y)) + geom_point() + 
  geom_point(data=df.c, aes(x=df.c$c.x, y=df.c$c.y), colour='red', size=5) + 
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

# Plot The Assignment of Each Point & Centroid
plot.c.assignments(df.c.assignments, df.c)

# Recalculate Centroids
df.c <- re.calc.centroids(df.c.assignments)
df.c






