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

# Randomly Generate K Centroids
K.1 = 2
c.x = runif(K.1, min = 0, max = 10)
c.y = runif(K.1, min = 0, max = 10)

# Centroid 1
c1.x = c.x[1]
c1.y = c.y[1]
df.c1 = data.frame(c1.x, c1.y)

# Centroid 2
c2.x = c.x[2]
c2.y = c.y[2]
df.c2 = data.frame(c2.x, c2.y)

# Plot Random Data + Centroids
p <- ggplot(df.pxy, aes(x = p.x, y = p.y)) + geom_point()
p + geom_point(data = df.c1, aes(x = c1.x, y = c2.y), colour = 'red', size =5) + 
    geom_point(data = df.c2, aes(x = c2.x, y = c2.y), colour = 'blue', size =5)

# Calculate Euclidean Distance to Each Centroid
e.distance <- function(p.x, p.y, c.x, c.y){
  'p.x/p.y:  Randomly generated x/y points
   c.x/c.y:  Centroid x & y coordinates'
  a = (p.x - c.x)^2
  b = (p.y - c.y)^2
  c = sqrt(a + b)  
  return(c)  
}

e.dist.c1 = e.distance(p.x, p.y, c1.x, c1.y)
e.dist.c2 = e.distance(p.x, p.y, c2.x, c2.y)


# Create DataFrame w/ x/y points, distance to c1, c2
comp.dist = e.dist.c1 > e.dist.c2
df.comp = data.frame(p.x, p.y, e.dist.c1, e.dist.c2, comp.dist)

df.comp
'If True:    dist.c1 > c2, then assign c2, 
 Otherwise:  assign c1'

df.comp$comp.dist[df.comp$comp.dist == TRUE] <- 'C2'
df.comp$comp.dist[df.comp$comp.dist == FALSE] <- 'C1'


# Create New Plot - Distinguish C1 and C2 Point Assignments By Color Or Shape

p <- ggplot(df.comp[df.comp$comp.dist == 'C1'], aes(x = p.x, y = p.y)) + geom_point()


#p + geom_point(data = df.c1, aes(x = c1.x, y = c2.y), colour = 'red', size =5) + 
#  geom_point(data = df.c2, aes(x = c2.x, y = c2.y), colour = 'blue', size =5)














