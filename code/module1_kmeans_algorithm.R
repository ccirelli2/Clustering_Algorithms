# KMEANS CLUSTER MODULE



# FUNCTION -  RANDOMLY GENERATE K CENTROID DATA POINTS------------------------------------------
'input:        number of centroids to create
 output:       Data frame that includes the x/y points for the data cloud +
               data points for each centroid + 
               Euclidean distance of each datapoint to each centroid'
f.centroids <- function(num){
  # Set Seed
  set.seed(7)
  # Create Vectors to Capture X & Y Values
  c.x <- c()
  c.y <- c()
  
  # For Loop - Generate N-Centroids
  for (i in seq(1, num)){
    c.x <- c(c.x, runif(1, min = 0, max = 10))
    c.y <- c(c.y, runif(1, min = 0, max = 10))
  }
  # Create Dataframe
  centroid <- c('c.1', 'c.2', 'c.3', 'c.4')
  df <- data.frame(c.x,c.y, centroid)
  return(df)
}


# ITERATE CENTROIDS X & Y COLUMNS - CALCULATE DISTANCE TO EACH CENTROID----------------------------

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
  for (i in seq(1,length(df.c$c.x))){
    
    # Count Object - Convert to character 
    c.count <- as.character(i)
    # Create Col Name For Each Centroid 
    c.n <- paste('c.', c.count, sep='')
    
    # Calculate the Distance From Each Data Point to Each Centroid
    'Process:   df.d$p.x      returns all my x values for the scatter data. 
                - df.c$x[i]:  returns only the x values for centroid = i in our sequence. 
                Output:       should output a single column with the euclidean dist for our data 
                              vs centroid i in our seq. '
    df.d$c.n <- sqrt((df.d$p.x - df.c$c.x[i])^2 + (df.d$p.y - df.c$c.y[i])^2)
    
    # Something is wrong with the assignment of the values.  The same value is being assigned to every row
    
    # Rename Col w/ Centroid Number (i+2 because we start w/ two cols x & y)
    colnames(df.d)[i+2] <- c.n
 
  }
  # Return Data frame
  return(df.d)
}


# GET COLUMN NAME----------------------------------------------------------------------
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


# CENTROID ASSIGNMENTS -----------------------------------------------------------------------

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



# PLOT FUNCTION------------------------------------------------------------------------
'Purpose:  The purpose of this function is to plot, at each iteration of the algorithm, the centroids and 
           assignments based on the min euclidean distance. '

plot.c.assignments <- function(df.c.assignments, df.c, n.iteration){
  'Inputs:      df.c.assignments - dataframe that includes our data cloud plus assignments
                df.c             - original coordinates for centroids.  
   Output:      tbd.  Plot
  '
  # Limit data frame to x/y points and centroid assignments
  df.xy.c <- select(df.c.assignments, p.x, p.y, centroid.min.dist) 
  
  # Generate Plots
  p <- ggplot(data=df.xy.c, aes(x=p.x, y=p.y, colour=centroid.min.dist)) + 
    geom_point(aes(shape=centroid.min.dist)) +
    geom_point(data=df.c, aes(x=c.x, y=c.y, colour=centroid, size=3)) +
    ggtitle(paste('Kmeans Plot Iteration =>', n.iteration))
  
  # Generate Plot
  print(p)
}


# RECALCULATE CENTROIDS-----------------------------------------------------------------
'Calculation:  Take each group of datapoints for each centroid.  
               Calculate the mean x,y for each group. 
               Use these valueas as the new centroids
               Calculate the distance of all of the datapoints to the new centroids
               Redue assignments
'
re.calc.centroids <- function(df.c.assignments){
  centroid.list <- unique(df.c.assignments$centroid.min.dist)
  centroid      <- c()
  c.x           <- c()
  c.y           <- c()
  
  # Iterate Over Length of Num Centroids
  for (i in seq(1, length(centroid.list))){
    # Obtain ith Centroid From Centroid List
    i.centroid  <- centroid.list[i]
    # Find Rows where assignment centroid
    subset.i    <- df.c.assignments$centroid.min.dist == i.centroid
    # Subset Data Frame Where Assignment == centroid.i
    df.sub      <- df.c.assignments[subset.i,]
    # Calculate Mean X, Y Coordinates / Assign to Vectors
    c.x         <- c(c.x, mean(df.sub$p.x))
    c.y         <- c(c.y, mean(df.sub$p.y))
    centroid    <- c(centroid, i.centroid)
  }
  # Create New df.c (data frame containing centroid coordinates)
  df.c        <- data.frame(c.x, c.y, centroid)
  # Return New df.c Data Frame
  return(df.c)
}



# CALCULATE SUM OF WITHIN & BETWEEN CLUSTER MSE----------------------------------------------------
'1.) Sum the MSE within each cluster
 2.) Sum the MSE for each cluster.'

calc.mse <- function(df.c.assignments){
  centroid.list <- sort(unique(df.c.assignments$centroid.min.dist))
  inner.mse     <- c()
  
  # Iterate Over Length of Num Centroids
  for (i in seq(1, length(centroid.list))){
    # Obtain ith Centroid From Centroid List
    i.centroid  <- centroid.list[i]
    # Find Rows where assignment centroid
    subset.i    <- df.c.assignments$centroid.min.dist == i.centroid
    # Subset Data Frame Where Assignment == centroid.i
    df.sub      <- df.c.assignments[subset.i,]
    # Calculate MSE Mean X, Y Coordinates / Assign to Vectors
    'Lets try to use the num of iteration to identify the correct column. 
     Since we now have a sorted list of centroids we know that col 3 will always
     be c.1 and after that c.1 + n'
    inner.mse <- c(inner.mse, sum(df.sub[ , 2 + i]))
    }
  
  # Calculate Total MSE
  outer.mse <- sum(inner.mse)
  # Return Outer MSE
  return(outer.mse)
  
}


