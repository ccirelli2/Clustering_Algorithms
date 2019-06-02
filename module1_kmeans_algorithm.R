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
   Output:      dataframe that includes data points & dist to centroids
   Process:     df.c$x: Returns col 1 for all centroids.  We use this in the seq as 
                the code needs to know how many centroids exist.  
   calc dis     So now we have a for loop that will loop over n number of centroids. 
                We can calculate the dist to each in one line of code, returning a single col
                that can be added to our data frame, w/ name c.# and the number of the centroid. 
  '
  # For Loop (Iters = num centroids)
  for (i in seq(1,length(df.c$x))){
    
    # Count Object - Convert to character 
    c.count <- as.character(i)
    # Create Col Name For Each Centroid 
    c.n <- paste('c.', c.count, sep='')
    
    # Calculate the Distance From Each Data Point to Each Centroid
    'Process:   df.d$p.x      returns all my x values for the scatter data. 
                - df.c$x[i]:  returns only the x values for centroid = i in our sequence. 
                Output:       should output a single column with the euclidean dist for our data 
                              vs centroid i in our seq. '
    df.d$c.n <- sqrt((df.d$p.x - df.c$x[i])^2 + (df.d$p.y - df.c$y[i])^2)
    
    # Something is wrong with the assignment of the values.  The same value is being assigned to every row
    
    # Rename Col w/ Centroid Number (i+2 because we start w/ two cols x & y)
    colnames(df.d)[i+2] <- c.n
 
  }
  # Return Data frame
  return(df.d)
}


# Get Col Name Function 
'This function is an input to the c.assignments function. 
 All we do is iterate the vector and find which one is true. 
 Then we return c. + the count value, the latter of which coincides with our col names. 
'

get.col.name <- function(min.value.matrix){
  'Input:    min.value is a matrix.  Convert to df
   Output:   col name w/ min e.dist value'
  df.min.value <- data.frame(min.value.matrix)
  count = 0
  col.num = ''
  
  for (i in seq(1, length(df.min.value))){
    count = count+1
    # Identify Col == True
    if (df.min.value[i] == TRUE){
      # Assign i value to col.num
      col.num = count
    }
  }  
  return(paste('c.', col.num, sep=''))
}


# Centroid Assignment

'Purpose:    The purpose of this function is to create a column within our existing data
             data frame w/ the assignment of the centroid whose distance is shortest to 
             data point in question (each row in our df represents one of n datapoints)
             
 Input:      Our data frame that contains the data set + distance to each centroid
             ****Note:  We need to recode the input to only receive a single row / vector. 
             
 Process     In order to find which column has the min dist, 
             i.) First find the min value using the min function, 
            ii.) Second we convert the vector to bollean values based on whcih == our min value. 
           iii.) Next, we use our count object to tells us which col contained the min value
                 and return c. plus the count value, which coincides with our column names'


c.assignments <- function(df.d){
  'Input:     data frame that includes data + dist to centroids
   Output:    same data frame w/ a new col with centroid assignments'
  # Create Empty Vector to House Our Assignments
  c.assignment.vector <- c()
  # Get Number of Cols (necessary as we need to tell the program how many centroids were gen)
  num.cols <- length(df.d[1,])
  
  # Run For Loop Over Every Row in Data Frame
  for (i in seq(1, length(df.d[, 1]))){
    # Isolate col/vector w/ distance measure to each centroid
    e.dist.vector <- df.d[i, 3: num.cols]
    print(e.dist.vector)
    # Get the Min Distance (essentially the min value of this vector)
    min.e.dist <- min(e.dist.vector)
    # Generate Bolean Values based on which col == min value
    min.value <- min.e.dist == e.dist.vector
    # Iterate vector to identify which centroid has the min dist
    col.name.min.value <- get.col.name(min.value)
    # Add Column Name (Centroid Name) to our assignment vector
    c.assignment.vector <- c(c.assignment.vector, col.name.min.value)
    }
  
  # Add Assignment Vector as a new column to our dataframe
  df.d$centroid.min.dist <- c.assignment.vector
  # Return our data frame
  return(df.d)
}




