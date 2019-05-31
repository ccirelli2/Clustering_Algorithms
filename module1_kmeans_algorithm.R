# KMEANS CLUSTER MODULE

# Function - Generate Centroid Datapoints
'input:        number of centroids to create
 output:       Data frame that includes the x/y points for the data cloud +
               data points for each centroid + 
               Euclidean distance of each datapoint to each centroid'
f.centroids <- function(num){
  # Set Seed
  set.seed(7)
  # Create Vectors to Capture X & Y Values
  x <- c()
  y <- c()
  
  # For Loop - Generate N-Centroids
  for (i in seq(1, num)){
    x <- c(x, runif(1, min = 0, max = 10))
    y <- c(y, runif(1, min = 0, max = 10))
  }
  # Create Dataframe
  df <- data.frame(x,y)
  return(df)
}


# Iterate Centroids X & Y Columns - Calculate Dist 4 Each - Add to DF

get.euclid.dist <- function(df.d, df.c){
  'Inputs:      df.c = data frame containing centroids. 
                df.d = data frame containing randomly generated data points. 
   Output:      dataframe that includes data points & dist to centroids'
  
  # For Loop (Iters = num centroids)
  for (i in seq(1,length(df.c[,1]))){
    # Count Object - Convert to character 
    c.count <- as.character(i)
    # Create Col Name For Each Centroid 
    c.n <- paste('c.', c.count, sep='')
    # Calculate the Distance From Each Data Point to Each Centroid
    'df.c$x[i]:  Each row in our dataframe represents a diff centroid.  So we use the count of the for loop
                 to iterate each row.  c^2 = a^2 + b^2'
    df.d$c.n <- sqrt((df.d$p.x - df.c$x[i])^2 + (df.d$p.y - df.c$y[i])^2)
    # Rename Col w/ Centroid Number (i+2 because we start w/ two cols x & y)
    colnames(df.d)[i+2] <- c.n
  }
  # Return Data frame
  return(df.d)
}





