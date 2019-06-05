'REFERENCES
 Tutorial:    https://www.datacamp.com/community/tutorials/k-means-clustering-r
 Data:        https://www.kaggle.com/fivethirtyeight/uber-pickups-in-new-york-city/downloads/uber-pickups-in-new-york-city.zip/2
'

rm(list=ls())


# Load Libraries
library(dplyr)

# Set Working Directory
target_wd = 'C:\\Users\\Chris.Cirelli\\Desktop\\Programming_Repositories\\Data_sets_all\\uber-pickups-in-new-york-city'
setwd(target_wd)
list.files(target_wd)

# Load the .csv files
apr14 <- read.csv('uber-raw-data-apr14.csv')
may14 <- read.csv('uber-raw-data-may14.csv')
jun14 <- read.csv('uber-raw-data-jun14.csv')
jul14 <- read.csv('uber-raw-data-jul14.csv')
aug14 <- read.csv('uber-raw-data-aug14.csv')
sep14 <- read.csv('uber-raw-data-sep14.csv')

# Bind Data Together
data14 <- bind_rows(apr14, may14, jun14, jul14, aug14, sep14)

# Inspect
head(data14)
summary(data14)

