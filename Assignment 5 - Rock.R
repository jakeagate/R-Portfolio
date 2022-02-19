# Assignment 5 (Rock)
# Data Mining
# Jake Agate

# Question: How many clusters can be found in this rock dataset with measurements 
#     of area, perimeter, shape, and permeability?

# Install Packages:
install.packages("dplyr")
install.packages("magrittr")
install.packages("caret")
install.packages("class")
install.packages("gmodels")
install.packages("lattice")
install.packages("zoo")

library(dplyr)
library(magrittr)
library(caret)
library(class)
library(gmodels)
library(lattice)
library(zoo)

# Read in dataset from R:
data(rock)
str(rock) # area vector needs to be converted to numeric

# Convert area column to numeric:
rock$area <- as.numeric(rock$area)
str(rock)

# Scale the data:
summary(rock) # unscaled data
rock %<>% scale()
summary(rock) # scaled data

# Perform kmeans with four clusters:
set.seed(123)
rock_kmeans <- kmeans(rock, centers = 4, iter.max = 10, nstart = 4)
rock_kmeans

# Add cluster column to dataset:
rock_cluster <- cbind(rock, cluster = rock_kmeans$cluster)
rock_cluster %>% head()

# Number of observations in each cluster:
rock_kmeans$cluster %>% table()

# Cluster means:
rock_kmeans$centers

# Plot the clusters:
fviz_cluster(rock_kmeans, data = rock,
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot")

###################

# Mean vector analysis: ( same as $centers )

area_mean <- aggregate(data = rock_cluster, area ~ cluster, mean)
peri_mean <- aggregate(data = rock_cluster, peri ~ cluster, mean)
shape_mean <- aggregate(data = rock_cluster, shape ~ cluster, mean)
perm_mean <- aggregate(data = rock_cluster, perm ~ cluster, mean)

cluster_means <- as.data.frame(c(area_mean, peri_mean, shape_mean, perm_mean))
cluster_means$cluster.1 <- NULL
cluster_means$cluster.2 <- NULL
cluster_means$cluster.3 <- NULL
cluster_means

cluster_means$perimeter <- peri_mean$peri
cluster_means$permeability <- perm_mean$perm
cluster_means$peri <- NULL
cluster_means$perm <- NULL
cluster_means

####################

# kmeans Template:

kmeans(x, centers, iter.max = 10, nstart = 1)

# x: numeric matrix, numeric data frame or a numeric vector
# centers: Possible values are the number of clusters (k) or a set of initial (distinct) cluster centers.
# If a number, a random set of (distinct) rows in x is chosen as the initial centers.
# iter.max: The maximum number of iterations allowed. Default value is 10.
# nstart: The number of random starting partitions when centers is a number. Trying nstart > 1 
# is often recommended.

