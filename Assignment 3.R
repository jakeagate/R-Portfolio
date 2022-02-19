# Assignment 3
# Data Mining
# Jake Agate

install.packages("magrittr")
install.packages("dplyr")
install.packages("rattle")

library(magrittr)
library(dplyr)
library(rattle)

# Read in data and normalize variable names:
drugs <- read.csv("drug200.csv")
names(drugs) %<>% normVarNames()

drugs["blood_pressure"] <- drugs$bp
drugs <- select(drugs, -bp)

str(drugs)

# Create random sample of 175 out of 200:
set.seed(123)
train_sample <- sample(200, 175)

# Split the dataframes:
drug_train <- drugs[train_sample, ]
drug_test <- drugs[-train_sample, ]

# Checking to see if training and testing data sets are proportional:
prop.table(table(drug_train$drug))
prop.table(table(drug_test$drug))

# Install rpart:
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

# Create decision tree using rpart to determine the probability of each drug that the variables leads to:
fit <- rpart(drug ~ age + sex + blood_pressure + cholesterol + na_to_k, data = drugs, method = "class")
rpart.plot(fit)

predict_unseen <- predict(fit, drug_test, type = "class")

# Output:
table_mat <- table(drug_test$drug, predict_unseen)
table_mat

# Determine accuracy of decision tree:
accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
print(paste("Accuracy for test:",accuracy_test*100,"%"))

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, drug_test, type = 'class')
  table_mat <- table(drug_test$drug, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- rpart.control(minsplit = 4,
                         minbucket = 3,
                         maxdepth = 3,
                         cp = 0)

tune_fit <- rpart(drug ~ age + sex + blood_pressure + cholesterol + na_to_k, data = drugs, method = "class", control = control)

accuracy_tune(tune_fit)


