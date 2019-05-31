# Create Your Own Kmeans Algorithm

# Clear Namespace
rm(list=ls())

# Import Libraries
library(ggplot2)

# Step 1:  Generate two random datasets (R2)
set.seed(7)
p.x = runif(100, min = 0, max = 10)
p.y = runif(100, min = 0, max = 10)
plot(p.x, p.y)

# Create DataFrame with X & Y Values
df.pxy = data.frame(p.x, p.y)
plot(df.pxy)

# Centroid 1
c1.x = runif(1, min = 5, max = 10)
c1.y = runif(1, min = 5, max = 10)
df.c1 = data.frame(c1.x, c1.y)
df.c1

# Centroid 2
c2.x = runif(1, min = 0, max = 5)
c2.y = runif(1, min = 0, max = 5)
df.c2 = data.frame(c2.x, c2.y)
df.c2


# Plot Random Data + Centroids
p.1 <- ggplot(df.pxy, aes(x = p.x, y = p.y)) + geom_point() + 
     geom_point(data=df.c1, aes(x=c1.x, y=c1.y), colour='red', size=5) +
     geom_point(data=df.c2, aes(x=c2.x, y=c2.y), colour='blue', size=5)

# Add Centroids to df.pxy
df.pxy$c1.x <- df.c1$c1.x
df.pxy$c1.y <- df.c1$c1.y
df.pxy$c2.x <- df.c2$c2.x
df.pxy$c2.y <- df.c2$c2.y
head(df.pxy)
setwd("C:\\Users\\Chris.Cirelli\\Desktop\\Programming_Repositories\\Clustering_Algorithms")
write.csv(df.pxy, 'plot_data.csv')

# Calculate Euclidean Distance to Each Centroid
e.distance <- function(p.x, c.x, p.y, c.y){
  'p.x/p.y:  Randomly generated x/y points
   c.x/c.y:  Centroid x & y coordinates'
  a = (p.x - c.x)^2
  b = (p.y - c.y)^2
  c = sqrt(a + b)  
  return(c)  
}


e.dist.c1 = e.distance(df.pxy$p.x, df.pxy$c1.x, df.pxy$p.y, df.pxy$c1.y)
e.dist.c2 = e.distance(df.pxy$p.x, df.pxy$c2.x, df.pxy$p.y, df.pxy$c2.y)

# Add Distances to Data Frame & Write to CSV
df.pxy$dist.2.c1 <- e.dist.c1
df.pxy$dist.2.c2 <- e.dist.c2
head(df.pxy)
write.csv(df.pxy, 'dataset_w_dist_2_centroids.csv')


# Create Columns to Label Points as C1 or C2
df.pxy$comp.dist <- df.pxy$dist.2.c1 < df.pxy$dist.2.c2
df.pxy$comp.dist[df.pxy$comp.dist == TRUE] <- 'C1'
df.pxy$comp.dist[df.pxy$comp.dist == FALSE] <- 'C2'
head(df.pxy)

# Plot Data Points Coloured By Assignment to C1 & C2
p.2 <- ggplot(    data=df.pxy[which(df.pxy$comp.dist=='C1'), ], aes(x=p.x, y=p.y)) + geom_point(colour='red', size=2) + 
       geom_point(data=df.pxy[which(df.pxy$comp.dist=='C1'), ], aes(x=c1.x, y=c1.y), colour='red', size=5) +
       geom_point(data=df.pxy[which(df.pxy$comp.dist=='C2'), ], aes(x=p.x, y=p.y), colour='blue', size=2)  +
       geom_point(data=df.pxy[which(df.pxy$comp.dist=='C2'), ], aes(x=c2.x, y=c2.y), colour='blue', size=5) + 
       ggtitle('Kmean Cluster Example')


##########################  ITER 2 ##############################################


# Re-Calculate C1 & C2 As the Mean of The Datapoints Assigned to C1 & C2
df.pxy.c1 <- df.pxy[which(df.pxy$comp.dist=='C1'), ]
df.pxy.c2 <- df.pxy[which(df.pxy$comp.dist=='C2'), ]

df.c1.x <- mean(df.pxy.c1$p.x)
df.c1.y <- mean(df.pxy.c1$p.y)
df.c2.x <- mean(df.pxy.c2$p.x)
df.c2.y <- mean(df.pxy.c2$p.x)

# Add Centroids to df.pxy
head(df.pxy)
df.pxy$c1.x <- df.c1.x
df.pxy$c1.y <- df.c1.y
df.pxy$c2.x <- df.c2.x
df.pxy$c2.y <- df.c2.y

# Calculate Distance To Each Centroid
e.dist.c1 = e.distance(df.pxy$p.x, df.pxy$c1.x, df.pxy$p.y, df.pxy$c1.y)
e.dist.c2 = e.distance(df.pxy$p.x, df.pxy$c2.x, df.pxy$p.y, df.pxy$c2.y)

# Add Distances to Data Frame & Write to CSV
df.pxy$dist.2.c1 <- e.dist.c1
df.pxy$dist.2.c2 <- e.dist.c2
head(df.pxy)

# Create Columns to Label Points as C1 or C2
df.pxy$comp.dist <- df.pxy$dist.2.c1 < df.pxy$dist.2.c2
df.pxy$comp.dist[df.pxy$comp.dist == TRUE] <- 'C1'
df.pxy$comp.dist[df.pxy$comp.dist == FALSE] <- 'C2'
head(df.pxy)

# Plot Data Points Coloured By Assignment to C1 & C2
p.3 <- ggplot(    data=df.pxy[which(df.pxy$comp.dist=='C1'), ], aes(x=p.x, y=p.y)) + geom_point(colour='red', size=2) + 
  geom_point(data=df.pxy[which(df.pxy$comp.dist=='C1'), ], aes(x=c1.x, y=c1.y), colour='red', size=5) +
  geom_point(data=df.pxy[which(df.pxy$comp.dist=='C2'), ], aes(x=p.x, y=p.y), colour='blue', size=2)  +
  geom_point(data=df.pxy[which(df.pxy$comp.dist=='C2'), ], aes(x=c2.x, y=c2.y), colour='blue', size=5) + 
  ggtitle('Kmean Cluster Example - Iter 2')

p.3




