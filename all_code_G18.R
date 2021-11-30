library(tidyverse)
library(dummies)
library(corrplot)
library(smotefamily)
###########################
# Import data
##########################

# setwd()
telecom <- read_csv(file = './data/WA_Fn-UseC_-Telco-Customer-Churn.csv',
                    col_types = 'cflffiffffffffffffnnf',
                    col_names = TRUE)


###########################
# Data Inspection 
##########################

# To see the data condition
summary(telecom)

# TotalCharges have na data, go deeper to see the row
checkNa <- telecom[is.na(telecom$TotalCharges),]
print(checkNa)

# All the na data have MonthlyCharges, but tenure is 0
# Dose 0 tenure cause na TotalCharges?
print(telecom[telecom$tenure == 0,c("customerID","tenure","TotalCharges")])
# Yes~!! They are the new costumer! Considering its tinny ratio , remote them!
telecomNoNA <- telecom[!is.na(telecom$TotalCharges),]

# Check again
summary(telecomNoNA)
# There are no na value any more!

# Remove ID
telecomNoNA$customerID <- NULL
# Check again
summary(telecomNoNA)


###########################
# Start the Visualization
##########################

# For the Visualizational purpose change SeniorCitizen to YES/NO
telecomVisual <- telecomNoNA %>% 
  mutate(SeniorCitizen = ifelse(SeniorCitizen ==  0, 'NO', 'YES'))

#========
# Numeric data analysis
#========
summary(telecomVisual)



telecomVisNumeric <- telecomVisual %>% 
  select(tenure,MonthlyCharges,TotalCharges)

# Display all boxplot
displayAllBoxplots <- function(tibbleDataset) {
  tibbleDataset %>% 
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + 
    geom_boxplot(mapping = aes(x=value, fill=key),
                 color = "black",
                 outlier.shape = 1) +
    facet_wrap( ~ key, scales = "free") +
    theme_minimal() +
    coord_flip()
}
displayAllBoxplots(telecomVisual)
# -- No outlier in the data

# Display all histograms
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>% 
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + 
    geom_histogram(mapping = aes(x=value, fill=key),
                   color = "black") +
    facet_wrap( ~ key, scales = "free") +
    theme_minimal()
}
displayAllHistograms(telecomVisual)
# -- Go to find the interesting points. 
# -- Might need add the range in monthlyCharges as a category when improving


# Display a correlation matrix
round(cor(telecomVisNumeric),2)

corrplot(cor(telecomVisNumeric),
         method = "number",
         type = "lower")
# --  Note: if your algorithm have to do extra preprocess, you have to care 
#           about the threshold of 0.7, then remove the variables.(See HW6) 

# Display scatterplots
telecomVisNumeric %>%
  ggplot() +
  geom_point(mapping = aes(y = tenure , x = MonthlyCharges), color = "blue",
             size = 2) +
  labs(title = "Relation Between Tenure and MonthlyCharges",
       x = "tenure", y = "MonthlyCharges")

#========
# Categorical data analysis
#========
summary(telecomVisual)
telecomVisual %>%
  ggplot() +
  geom_bar(mapping = aes(x =Churn, fill = InternetService), color = "black") +
  labs(title = "Customer Churn based on InternetService",
       x = "Churn", y = "number") 

telecomVisual %>%
  ggplot() +
  geom_bar(mapping = aes(x =Churn, fill = Contract), color = "black") +
  labs(title = "Customer Churn based on Contract duration",
       x = "Churn", y = "number") 

telecomVisual %>%
  ggplot() +
  geom_bar(mapping = aes(x =Churn, fill = OnlineSecurity), color = "black") +
  labs(title = "Customer Churn based on Online Security",
       x = "Churn", y = "number") 


#########################
# pre-process for module
#########################

#========
# Numeric data 
#========
# normalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
telecomNorm <- telecomNoNA %>% 
  mutate(tenure = normalize(tenure),
         MonthlyCharges = normalize(MonthlyCharges),
         TotalCharges = normalize(TotalCharges))
summary(telecomNorm)

#========
# Categorical data 
#========
# gender SeniorCitizen Partner Dependents PhoneService Churn to 0 and 1
telecomYN <- telecomNorm %>% mutate(gender = ifelse(gender == 'Female', 0, 1),
                                    SeniorCitizen = ifelse(SeniorCitizen == 'FALSE', 0, 1),
                                    Partner = ifelse(Partner == 'No', 0, 1),
                                    Dependents = ifelse(Dependents == 'No', 0, 1),
                                    PhoneService = ifelse(PhoneService == 'No', 0, 1),
                                    Churn = ifelse(Churn == 'No', 0, 1)
)
summary(telecomYN)

# Dummy
telecomDF <- data.frame(telecomYN)
telecomLabel <- telecomDF %>% select(Churn)
telecomDF <- telecomDF %>% select(-Churn)
telecomDummy <- dummy.data.frame(data = telecomDF, sep = "_")

# Remove space from the colnames
newName <- sapply(colnames(telecomDummy),
                  function(x) gsub(" ","",x, fixed = TRUE))
newName <- sapply(newName,
                  function(x) gsub("(","",x, fixed = TRUE))
newName <- sapply(newName,
                  function(x) gsub(")","",x, fixed = TRUE))
colnames(telecomDummy) <- newName


# Remove duplicately meaning dummy columns
contract_Month2month <- newName[str_detect(newName,'Contract_Month-to-month')]
PaymentMethod_Bank <- newName[str_detect(newName,'PaymentMethod_Bank')]
telecomDummy <- telecomDummy[, !(names(telecomDummy)) %in% c(newName[str_detect(newName,'_No')],
                                                             contract_Month2month
                                                             ,PaymentMethod_Bank)]

# Add the Churn back
telecomDummy <- cbind(telecomDummy,telecomLabel)
summary(telecomDummy)


# Randomly split the dataset into mobilePhoneTraining (75% of records) and 
# mobilePhoneTesting (25% of records) using 777 as the random seed
set.seed(777)
sampleSet <- sample(nrow(telecomDummy),
                    round(nrow(telecomDummy)*0.75),
                    replace= FALSE)

churnTraining <- telecomDummy[sampleSet,]
churnTesting <- telecomDummy[-sampleSet,]


# Deal with class imbalance using the SMOTE technique 
getImbalanceRatio <- function(column) {
  keys <- unique(column)
  firstNumber <- sum(churnTraining$Churn==keys[1])
  secndNumber <- sum(churnTraining$Churn==keys[2])
  if (firstNumber > secndNumber) {
    return(c(toString(keys[1]),firstNumber/secndNumber)) 
  } else {
    return(c(toString(keys[2]),secndNumber/firstNumber)) 
  }
}
getImbalanceRatio(churnTraining$Churn)

churnTrainingSmoted <- 
  tibble(SMOTE( X = data.frame(churnTraining),
                target = churnTraining$Churn,
                dup_size = 2.8)$data)

summary(churnTrainingSmoted$Churn)

# Change Churn back into factor and remove class created by SMOTE
churnTrainingSmoted$Churn <- as.factor(churnTrainingSmoted$Churn)
churnTesting$Churn <- as.factor(churnTesting$Churn)
churnTrainingSmoted$class <- NULL

# Note:
# No Na data [telecomVisual]
# Non SMOTE data (N)  [churnTraining / churnTesting]
# SMOTE data (N + B) [churnTrainingSmoted / churnTesting]


###############
# LogisticRegression.R
##############
# LogisticRegression.R
# installed tidyverse, dummies packages
# install.packages("tidyverse")
# install.packages("dummies")
# install.packages("olsrr")
# install.packages("smotefamily")
# import a dataset of telecom data to predict whether a customer will churn or
# not churn using Logistic regression

library(tidyverse)
library(dummies)
library(corrplot)
library(olsrr)
library(smotefamily)

# set working directory 
setwd("C:/Users/ngnai/OneDrive/Desktop/ECON 511A")
customerChurn <- read_csv("ModelChurn.csv",
                          col_types = "cllllnlllllllllllllllllnnl",
                          col_names = TRUE)


# display customerChurn in the console
print(customerChurn)

# display structure in the console
str(customerChurn)                  

# display summary in the console
summary(customerChurn)

# remove customerID from the data
customerChurn <- customerChurn %>% select(-customerID)

# remove NA's from the data and fill in mean value
customerChurn$TotalCharges[is.na(customerChurn$TotalCharges)] <-
  mean(customerChurn$TotalCharges,na.rm=TRUE)

# summary of the customerChurn
summary(customerChurn)


#Recreating displayAllHistograms() function
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value, fill=key),
                              color = "black") +
    facet_wrap(~ key, scales = "free") +
    theme_minimal ()
}
# Call the displayAllHistograms() function, passing in zooSpending as an
# argument
displayAllHistograms(customerChurn)

# display the correlation plot using number method and limit output to
# the bottom left
cor(customerChurn)


# display the correlation plot using number method and limit output to
# the bottom left
corrplot(cor(customerChurn),
         method = "number",
         type ="lower")

# setting random seed
set.seed(777)
# is.numeric(customerChurn$Churn)
# customerChurn$Churn <- lapply(customerChurn$Churn, as.logical)

# putting the records from (75%) into customerChurn1
sampleSet <- sample(nrow(customerChurn),
                    round(nrow(customerChurn)*0.75),
                    replace= FALSE)

# putting the records from (75%) into customerChurn1
customerChurnTraining <- customerChurn[sampleSet,]

# putting the record from (25%) into customerChurn1
customerChurnTesting <- customerChurn[-sampleSet,]

summary(customerChurnTraining$Churn)

classImbalance <- 3872/1410

customerChurnTrainingSmoted <- 
  tibble(SMOTE(X = data.frame(customerChurnTraining),
               target = customerChurnTraining$Churn,
               dup_size = 3)$data)
summary(customerChurnTrainingSmoted)

# Convert CancelledService and RecentRenewal back into logical types
customerChurnTrainingSmoted <- customerChurnTrainingSmoted %>%
  mutate(Churn = as.logical(Churn),
         Gender=as.logical(Gender),
         SeniorCitizen= as.logical(SeniorCitizen),
         Partner= as.logical(Partner),
         Dependents= as.logical(Dependents),
         PhoneService= as.logical(PhoneService),
         MultipleLines=as.logical(MultipleLines),
         InternetServiceFiberOptic=as.logical(InternetServiceFiberOptic),
         InternetServiceNo=as.logical(InternetServiceNo),
         InternetServiceDSL=as.logical(InternetServiceDSL),
         OnlineSecurity=as.logical(OnlineSecurity),
         OnlineBackup=as.logical(OnlineBackup),
         DeviceProtection=as.logical(DeviceProtection),
         TechSupport= as.logical(TechSupport),
         StreamingTV=as.logical(StreamingTV),
         StreamingMovies=as.logical(StreamingMovies),
         ContractOneYear=as.logical(ContractOneYear),
         ContractTwoYear=as.logical(ContractTwoYear),
         PaperlessBilling=as.logical(PaperlessBilling),
         PaymentMethodElectronicCheck=as.logical(PaymentMethodElectronicCheck),
         PaymentMethodMailedCheck=as.logical(PaymentMethodMailedCheck),
         PaymentMethodCreditCard= as.logical(PaymentMethodCreditCard))



customerChurnTrainingSmoted <- customerChurnTrainingSmoted %>%
  select(-class)

#is.numeric(customerChurn$Churn)
summary(customerChurnTrainingSmoted)

# Generating the logistic regression model 
churnModel <- glm(data = customerChurnTrainingSmoted,
                  family = binomial,
                  formula = Churn ~.)

# display the logistic regression
summary(churnModel)


#  Calculating the odds ratios 
exp(cbind(OR=coef(churnModel), confint(churnModel)))

# Used model to predict outcomes in the testing dataset and
# and treat anything below or equal to 0.5 as a 0, anything above 0.5 as a 1
predictionModel <- predict(churnModel,
                           customerChurnTesting,
                           type = "response")

# Used model to predict outcomes in the testing dataset and
# and treat anything below or equal to 0.5 as a 0, anything above 0.5 as a 1.
predictionModel <- ifelse(predictionModel >= 0.5,1,0)

print(predictionModel)  

# Generate a confusion matrix of predictions
churnConfusionMatrix <- table(customerChurnTesting$Churn,
                              predictionModel)
print(churnConfusionMatrix)

# Calculate the model prediction accuracy
sum(diag(churnConfusionMatrix))/nrow(customerChurnTesting)


###############
# KNN
##############
library(class)
churnTrainingSmotedLabel <- churnTrainingSmoted %>% select(Churn) 
churnTrainingSmoted <- churnTrainingSmoted %>% select(-Churn) 
print(churnTrainingSmoted)
churnTrainingLabel <- churnTraining %>% select(Churn) 
churnTraining <- churnTraining %>% select(-Churn) 

churnTestingLabel <- churnTesting %>%  select(Churn) 
churnTesting <- churnTesting %>% select(-Churn) 
#------------
# Non-SMOTE data (N)
#------------
# Generate k nearest neighbour
customerPrediction <- knn(train = churnTraining,
                          test = churnTesting,
                          cl = churnTrainingLabel$Churn,
                          k = 137)
print(customerPrediction)
# Display summary of the predictions from the testing dataset
print(summary(customerPrediction))

# Evaluate the model by forming a confusion matrix
churnconfusionMatrix <- table(churnTestingLabel$Churn,
                              customerPrediction)

print(churnconfusionMatrix)

churnpredictiveAccuracy <- sum(diag(churnconfusionMatrix))/
  nrow(churnTesting)

print(churnpredictiveAccuracy)

# Create a matrix of k-values with their predictive accuracy
# Store the matrix into an object called kValueMatrix
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol =2)

colnames(kValueMatrix) <- c("k value", "Predictive accuracy")

# Loop through odd values of k from 1 up to the number of records in the
# training 
for(kValue in 1:1000) {
  if(kValue %% 2 != 0) {
    customerPrediction <- knn(train = churnTraining,
                              test = churnTesting,
                              cl = churnTrainingLabel$Churn,
                              k = kValue)
    churnconfusionMatrix <- table(churnTestingLabel$Churn,
                                  customerPrediction)
    predictiveAccuracy <- sum(diag(churnconfusionMatrix))/
      nrow(churnTesting)
    kValueMatrix <- rbind(kValueMatrix, c(kValue,predictiveAccuracy))
  }
}

# Display the kValueMatrix on the console to determine the best k-value
print(kValueMatrix)

# Display the best k value
maxAUC <- kValueMatrix[kValueMatrix[,"Predictive accuracy"] == 
                         max(kValueMatrix[,"Predictive accuracy"])]
maxAUCMatrix <- kValueMatrix[which(kValueMatrix == maxAUC[1]),]
print(maxAUCMatrix)

#------------
# SMOTE data (N + B)
#------------
# Create a matrix of k-values with their predictive accuracy
# Store the matrix into an object called kValueMatrix
kValueMatrixSMOTE <- matrix(data = NA,
                            nrow = 0,
                            ncol =2)

colnames(kValueMatrixSMOTE) <- c("k value", "Predictive accuracy")

# Loop through odd values of k from 1 up to the number of records in the
# training 
for(kValue in 1:1000) {
  if(kValue %% 2 != 0) {
    customerPrediction <- knn(train = churnTrainingSmoted,
                              test = churnTesting,
                              cl = churnTrainingSmotedLabel$Churn,
                              k = kValue)
    churnconfusionMatrix <- table(churnTestingLabel$Churn,
                                  customerPrediction)
    predictiveAccuracy <- sum(diag(churnconfusionMatrix))/
      nrow(churnTesting)
    kValueMatrixSMOTE <- rbind(kValueMatrixSMOTE, c(kValue,predictiveAccuracy))
  }
}

# Display the kValueMatrixSMOTE on the console to determine the best k-value
print(kValueMatrixSMOTE)

# Display the best k value
SMOTEmaxAUC <- 
  kValueMatrixSMOTE[kValueMatrixSMOTE[,"Predictive accuracy"] == 
                      max(kValueMatrixSMOTE[,"Predictive accuracy"])]
SMOTEmaxAUCMatrix <- kValueMatrixSMOTE[which(kValueMatrixSMOTE == maxAUC[1]),]
print(SMOTEmaxAUCMatrix)

###############
#  Naïve Bayes
##############
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

###############
# Decision tree
##############
library(rpart.plot)
#------------
# No na Data [telecomVisual]
#------------
telecomVisualTraining <- telecomVisual[sampleSet,]
telecomVisualTesting <- telecomVisual[-sampleSet,]

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

#------------
# Non SMOTE data (N)  [churnTraining / churnTesting]
#------------
telecomChurnDecisiontreeModel2 <- rpart(formula = Churn ~.,
                                        method = "class",
                                        cp = .005,  #0.005  0.7906712
                                        data = churnTraining)
telecomChurnPrediction2 <- predict(telecomChurnDecisiontreeModel2,
                                   churnTesting,
                                   type = "class")
telecomChurnConfusionMatrix2 <- table(churnTesting$Churn ,
                                      telecomChurnPrediction2)
print(telecomChurnConfusionMatrix2)
predictiveAccuracy2 <- sum(diag(telecomChurnConfusionMatrix2)) / 
  nrow(churnTesting)
print(predictiveAccuracy2)
rpart.plot(telecomChurnDecisiontreeModel2)
print(telecomChurnDecisiontreeModel2)

#------------
# SMOTE data (N + B) [churnTrainingSmoted / churnTesting]
#------------
telecomChurnDecisiontreeModel2 <- rpart(formula = Churn ~.,
                                        method = "class",
                                        cp = .0015,  #0.001  0.7491468
                                        data = churnTrainingSmoted)
telecomChurnPrediction2 <- predict(telecomChurnDecisiontreeModel2,
                                   churnTesting,
                                   type = "class")
telecomChurnConfusionMatrix2 <- table(churnTesting$Churn ,
                                      telecomChurnPrediction2)
print(telecomChurnConfusionMatrix2)
predictiveAccuracy2 <- sum(diag(telecomChurnConfusionMatrix2)) / 
  nrow(churnTesting)
print(predictiveAccuracy2)
rpart.plot(telecomChurnDecisiontreeModel2)
print(telecomChurnDecisiontreeModel2)

###############
# Neural network
##############
# install.packages("tidyverse")
# install.packages("neuralnet")

library(tidyverse)
library(neuralnet)
library(factoextra)
library(cluster)
library(gridExtra)

# set the working directory
setwd("~/MIS/Projects/DataMining/project/MIS545G18")

# read the csv file
churn <- read_csv(file = "./data/ModelChurn.csv",
                  col_types = "ciiiiiiiiiiiiiiiiiddi",
                  col_names = TRUE)

# drop the 11 rows which contain NULL values
churn <- drop_na(churn)

# print the churn tibble
print(churn)

# print the structure of churn tibble
print(str(churn))

# print the summary of churn tibble
print(summary(churn))



# scaling the tenure to a value between 0 and 1
churn <- churn %>%
  mutate(tenureScaled = (tenure - min(tenure))/
           (max(tenure) - min(tenure)))

# scaling the monthly charges to a value between 0 and 1
churn <- churn %>%
  mutate(MonthlyChargesScaled = (MonthlyCharges - min(MonthlyCharges))/
           (max(MonthlyCharges) - min(MonthlyCharges)))

# scaling the total charges to a value between 0 and 1
churn <- churn %>%
  mutate(TotalChargesScaled = (TotalCharges - min(TotalCharges))/
           (max(TotalCharges) - min(TotalCharges)))


set.seed(591)

# creating the training dataset
sampleSet <- sample(nrow(churn),
                    round(nrow(churn)*0.75),
                    replace = FALSE)


# splitting into 75% training dataset
churnTraining <- churn[sampleSet, ]

# loading the remaining 25% of the dataset for testing
churnTesting <- churn[-sampleSet, ]

# generating the neural network with 1 hidden layer
churnNeuralNet <- neuralnet(
  formula = Churn ~ tenureScaled + MultipleLines + InternetServiceNo + 
    SeniorCitizen + Dependents + 
    PaymentMethodElectronicCheck + 
    TotalChargesScaled + MultipleLines + TechSupport + ContractOneYear + ContractTwoYear,
  data = churnTraining,
  hidden = 1,
  act.fct = "logistic",
  linear.output = FALSE)

# displaying the neural network results
print(churnNeuralNet$result.matrix)

# using churnProbability to generate probablities on the testing dataset
churnProbability <- compute(churnNeuralNet, 
                            churnTesting)

# visualizing the neural network
plot(churnNeuralNet)

# displaying the results from the testing dataset on the console
print(churnProbability$net.result)

# converting probability predictions into 0 or 1 predictions
churnPrediction <- 
  ifelse(churnProbability$net.result > 0.5, 1, 0)

# displaying the predictions on the console
print(churnPrediction)

# evaluating the model by forming a confusion matrix
churnConfusionMatrix <- table(churnTesting$Churn,
                              churnPrediction)

# displaying confusion matrix on the console
print(churnConfusionMatrix)

# calculating model predictive accuracy
predictiveAccuracy <- sum(diag(churnConfusionMatrix)) /
  nrow(churnTesting)

# displaying the predictive accuracy
print(predictiveAccuracy)
