# Christine Chang
# MIS 545 Section 02
# GPNaiveBayes.R
# This R application will build naive bayes model, predictions, and calculate
# the accuracy of the model based on ModelChurn.csv

# Install the tidyverse and e1071 packages
# install.packages("tidyverse")
# install.packages("e1071")

# Load the tidyverse and e1071 libraries
library(tidyverse)
library(e1071)

# Set the working directory to my Group Project folder
setwd("~/Desktop/UofA/Fall 2021/MIS 545/Group Project")

# Read ModelChurn.csv into a tibble called customerChurn
customerChurn <- read_csv(file = "ModelChurn.csv",
                         col_names = TRUE)

# Display customerChurn in the console
print(customerChurn)

# Display the structure of customerChurn in the console
str(customerChurn)

# Display the summary of customerChurn in the console
summary(customerChurn)

# Using 154 as the random seed
set.seed(154)

# Create a vector of 75% randomly sample rows from the original dataset
sampleSet <- sample(nrow(customerChurn),
                    round(nrow(customerChurn) * 0.75),
                    replace = FALSE)

# Randomly split the dataset into customerChurnTraining (75% of records) and 
# customerChurnTesting (25% of records)
customerChurnTraining <- customerChurn[sampleSet, ]
customerChurnTesting <- customerChurn[-sampleSet, ]

# Generate the Naive Bayes model to predict customerChurn based on the other 
# variables in the dataset
customerChurnModel <- naiveBayes(formula = Churn ~ .,
                                data = customerChurnTraining,
                                laplace = 1)

# Build probabilities for each record in the testing dataset and store them in 
# customerChurnProbability
customerChurnProbability <- predict(customerChurnModel,
                                    customerChurnTesting,
                                   type = "raw")

# Display customerChurnProbability on the console
print(customerChurnProbability)

# Predict classes for each record in the testing dataset and store them in 
# customerChurnPrediction
customerChurnPrediction <- predict(customerChurnModel,
                                   customerChurnTesting,
                                  type = "class")

# Display customerChurnPrediction on the console
print(customerChurnPrediction)

# Evaluate the model by forming a confusion matrix
customerChurnConfusionMatrix <- table(customerChurnTesting$Churn,
                                     customerChurnPrediction)

# Display the confusion matrix on the console
print(customerChurnConfusionMatrix)

# Calculate the model predictive accuracy and store it into a variable called 
# predictiveAccuracy
predictiveAccuracy <- sum(diag(customerChurnConfusionMatrix)) / 
  nrow(customerChurnTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy)



