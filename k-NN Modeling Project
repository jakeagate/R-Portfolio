# k-NN Modeling Project

library(magrittr)
library(dplyr)

cars <- read.csv("Car_sales.csv", stringsAsFactors = F)
View(cars)

is.na(cars) %>% sum()

# Question: Can we predict whether a vehicle is a 'passenger' or 'car' (2 seats or more) based off its wheelbase, width, and length?

str(cars)
glimpse(cars)


# Received an error later on due to NA's so replaced them with average of surrounding data:

library(zoo)

cars_no_na <- replace(cars, T, lapply(cars, na.aggregate))
cars <- cars_no_na
is.na(cars) %>% sum() # Zero NA's

cars$Vehicle_type %>% unique()
table(cars$Vehicle_type)


# Create random value for number of rows in df and save into variable:
set.seed(123)
shuffled_cars <- runif(nrow(cars))

# Overwrite df with the order of random numbers from uniform distribution
cars <- cars[order(shuffled_cars),]

# Summary of observed variables (Wheelbase, Width, Length):
summary(cars[c("Wheelbase", "Width", "Length")])

# Change Vehicle_type variable to a factor:
cars$Vehicle_type <- factor(cars$Vehicle_type)
str(cars)

# Create normalize function (NA's were present so formula was altered):
normalize <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

# Apply and check normalization function on df and save to variable:
cars_n <- as.data.frame(lapply(cars[c(9, 10, 11)], normalize))
summary(cars_n[c("Wheelbase", "Width", "Length")])


# Create training and test data:
cars_train <- cars_n[1:137, ]
cars_test <- cars_n[138:157, ]

cars_train_labels <- cars[1:137, 5]
cars_test_labels <- cars[138:157, 5]

# Install 'class' package:
install.packages("class")
library(class)

# Using Knn function:
sqrt(157) # = 12.53 

cars_test_pred <- knn(train = cars_train, test = cars_test, cl = cars_train_labels, k = 13)


# Install packages:
install.packages("gmodels")
library(gmodels)
library(caret)
library(ggplot2)
library(lattice)


CrossTable(x = cars_test_labels, y = cars_test_pred, prop.chisq = F) 

table(cars_test_labels, cars_test_pred)



# Use caret to automate process of evaluating correct k value to achieve highest accuracy:
i = 1
k.optm = 1

for (i in 1:28){
  knn.mod <- knn(train = cars_train, test = cars_test, cl = cars_train_labels, k = i)
  k.optm[i] <- 100 * sum(cars_test_labels == knn.mod) / NROW(cars_test_labels)
  k = i
  cat(k, '=', k.optm[i], '\n')
} # 5 is the optimum k value to achieve an accuracy value of 90%
  
# Test new k value:
cars_test_pred <- knn(train = cars_train, test = cars_test, cl = cars_train_labels, k = 5)
CrossTable(x = cars_test_labels, y = cars_test_pred, prop.chisq = F) 

# Create confusion matrix:
confusionMatrix(cars_test_labels, cars_test_pred) # Confirmed 90% accuracy
  
  
  
  
  
