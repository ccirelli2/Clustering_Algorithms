# KMEANS - SALESFORCE DATA
'References:
   %>%        https://www.datacamp.com/community/tutorials/pipe-r-tutorial
              ""chaining" means that you invoke multiple method calls. 
              As each method returns an object, you can actually allow the calls 
              to be chained together in a single statement, without needing variables 
              to store the intermediate results.
              
   $<>%       Compound operator.  Super cool.  Applies pipe forward to functions and
              then updates the object x. 
              x <- rnorm(100)
              x %<>% abs() %>% sort()
   


   F#         google where this forward pipe comes from and what languages have similar
              functions
   diff       look up in R library what this function does. 
'

# Clear Namespace
rm(list=ls())

  # Load Libraries
library(tidyverse)
library(dplyr)
library(magrittr)

# Set Working Directory 
getwd()
setwd('C:\\Users\\Chris.Cirelli\\Desktop\\Programming_Repositories\\Clustering_Algorithms\\Data')

# Load CSV FIle
list.files()
sf_data <- read.csv('sf_data_06052019.csv')

head(sf_data)

# Limit Data - Columns
' Columns      Limit to Employee Count, Revenues, Type, Stage'
sf_data_lim.1 <- select(sf_data %>% filter(Line.of.Business == 'Private/NFP'), 
                        Annual.Revenue, Employees, Submission.Type, Stage, Line.of.Business)
head(sf_data_lim.1)

# Limit Data - Private NFP

# Create Two Datasets - Bound + Declined
sf_data_bound <-    select(sf_data_lim.1 %>% filter(Stage == 'Bound', Annual.Revenue != 0, Employees != 0), 
                           Annual.Revenue, Employees)

sf_data_declined <- select(sf_data_lim.1 %>% filter(Stage == 'Declined', Annual.Revenue != 0, Employees != 0), 
                           Annual.Revenue, Employees) 


boxplot(sf_data_bound$Annual.Revenue)
plot(density(sf_data_declined$Annual.Revenue))


