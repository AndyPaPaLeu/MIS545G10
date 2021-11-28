
# Install the tidyverse and rpart.plot packages 
# install.packages("tidyverse")
# install.packages("rpart.plot")

# Load the tidyverse, rpart, and rpart.plot libraries
library(tidyverse)
library(rpart.plot)
library(smotefamily)

# Read ModelChurn.csv into a tibble called telecom 
telecom <- read_csv(file = "./data/ModelChurn.csv")

# Display the summary of telecom in the console
summary(telecom)

# Remove customerID
telecom$customerID <- NULL

# Remove NA
telecomNoNA <- telecom[!is.na(telecom$TotalCharges),]

# Randomly split the dataset into telecomTraining (75% of records) and 
# telecomTesting (25% of records) using 370 as the random seed
set.seed(777)
telecomSample <- sample(x= nrow(telecomNoNA),
                        size = round(nrow(telecomNoNA) * 0.75),
                        replace = FALSE
)
telecomTraining <- telecomNoNA[telecomSample,]
telecomTesting <- telecomNoNA[-telecomSample,]

# Generate the decision tree model to predict Churn based on the other 
# variables in the dataset. Use 0.01 as the complexity parameter.
telecomChurnDecisiontreeModel <- rpart(formula = Churn ~.,
                                       method = "class",
                                       cp = 0.01, # 0.01  0.7927314
                                       data = telecomTraining)

# Display the decision tree visualization in R
print(telecomChurnDecisiontreeModel)

# Predict classes for each record in the testing dataset and store them in 
# telecomChurnPrediction
telecomChurnPrediction <- predict(telecomChurnDecisiontreeModel,
                                  telecomTesting,
                                  type = "class")

# Display telecomChurnPrediction on the console
print(telecomChurnPrediction)

# Evaluate the model by forming a confusion matrix
telecomChurnConfusionMatrix <- table(telecomTesting$Churn ,
                                     telecomChurnPrediction)

# Display the confusion matrix on the console
print(telecomChurnConfusionMatrix)

# Calculate the model predictive accuracy and store it into a variable called 
# predictiveAccuracy
predictiveAccuracy <- sum(diag(telecomChurnConfusionMatrix)) / 
  nrow(telecomTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy)


##########################
# No pre-preparation case
##########################
telecom2 <- read_csv(file = './data/WA_Fn-UseC_-Telco-Customer-Churn.csv',
                     col_types = 'cflffiffffffffffffnnf',
                     col_names = TRUE)
summary(telecom2)

# Remove customerID
telecom2$customerID <- NULL

# Remove NA
telecom2NoNA <- telecom2[!is.na(telecom2$TotalCharges),]

# Check again
summary(telecom2NoNA)

# For the Visualizational purpose change SeniorCitizen to YES/NO
telecomVisual <- telecom2NoNA %>% 
  mutate(SeniorCitizen = ifelse(SeniorCitizen ==  0, 'NO', 'YES'))

set.seed(777)
telecomSample <- sample(x= nrow(telecomNoNA),
                        size = round(nrow(telecomNoNA) * 0.75),
                        replace = FALSE
)
telecomVisualTraining <- telecomVisual[telecomSample,]
telecomVisualTesting <- telecomVisual[-telecomSample,]

telecomChurnDecisiontreeModel2 <- rpart(formula = Churn ~.,
                                        method = "class",
                                        cp = .005,  #0.005  0.7906712
                                        data = telecomVisualTraining)
telecomChurnPrediction2 <- predict(telecomChurnDecisiontreeModel2,
                                   telecomVisualTesting,
                                   type = "class")
telecomChurnConfusionMatrix2 <- table(telecomVisualTesting$Churn ,
                                      telecomChurnPrediction2)
print(telecomChurnConfusionMatrix2)
predictiveAccuracy2 <- sum(diag(telecomChurnConfusionMatrix2)) / 
  nrow(telecomVisualTesting)
print(predictiveAccuracy2)
rpart.plot(telecomChurnDecisiontreeModel2)


print(telecomChurnDecisiontreeModel2)
## might need
printcp(telecomChurnDecisiontreeModel2)

