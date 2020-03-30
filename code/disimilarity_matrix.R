# Dissimilarity Matrix Turorial ----------------------------------------
'	Description:  Pairwise dissimilarities (distances) between observations
	in a dataset. The original values may be of mixed types. 
	Ref:	https://astrostatistics.psu.edu/su07/R/html/cluster/html/daisy.html
 	Ref:  	https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/Dissimilarity_Matrix_Calculation
	Ref:	https://duttashi.github.io/blog/how-to-create-a-dissimilarity-matrix-for-mixed-type-dataset/
	R: 	daisy function.
  		daisy(x, metric=c(euclidean, manhattan, gower))
	
'
  

# Clear Namespace
rm(list=ls())

# Load Libraries
library(readr)
library(cluster)
library(Hmisc)

# Load Data
t1 = read.table("/home/cc2/Desktop/repositories/clustering_algorithms/data/cmc.data")

# Insepct Data
head(t1)
class(t1)




