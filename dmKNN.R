library(tidyverse)
library(dummies)
library(corrplot)
library(smotefamily)
###########################
# pre-process
##########################
# setwd()
telecom <- read_csv(file = './data/WA_Fn-UseC_-Telco-Customer-Churn.csv',
                    col_types = 'cflffiffffffffffffnnf',
                    col_names = TRUE)

checkNa <- telecom[is.na(telecom$TotalCharges),]
telecomNoNA <- telecom[!is.na(telecom$TotalCharges),]
telecomNoNA$customerID <- NULL
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
telecomNorm <- telecomNoNA %>% 
  mutate(tenure = normalize(tenure),
         MonthlyCharges = normalize(MonthlyCharges),
         TotalCharges = normalize(TotalCharges))
telecomYN <- telecomNorm %>% mutate(gender = ifelse(gender == 'Female', 0, 1),
                                    SeniorCitizen = ifelse(SeniorCitizen == 'FALSE', 0, 1),
                                    Partner = ifelse(Partner == 'No', 0, 1),
                                    Dependents = ifelse(Dependents == 'No', 0, 1),
                                    PhoneService = ifelse(PhoneService == 'No', 0, 1),
                                    Churn = ifelse(Churn == 'No', 0, 1)
)
telecomDF <- data.frame(telecomYN)
telecomLabel <- telecomDF %>% select(Churn)
telecomDF <- telecomDF %>% select(-Churn)
telecomDummy <- dummy.data.frame(data = telecomDF, sep = "_")
newName <- sapply(colnames(telecomDummy),
                  function(x) gsub(" ","",x, fixed = TRUE))
newName <- sapply(newName,
                  function(x) gsub("(","",x, fixed = TRUE))
newName <- sapply(newName,
                  function(x) gsub(")","",x, fixed = TRUE))
colnames(telecomDummy) <- newName
contract_Month2month <- newName[str_detect(newName,'Contract_Month-to-month')]
PaymentMethod_Bank <- newName[str_detect(newName,'PaymentMethod_Bank')]
telecomDummy <- telecomDummy[, !(names(telecomDummy)) %in% c(newName[str_detect(newName,'_No')],
                                                             contract_Month2month
                                                             ,PaymentMethod_Bank)]
telecomDummy <- cbind(telecomDummy,telecomLabel)
summary(telecomDummy)
set.seed(777)
sampleSet <- sample(nrow(telecomDummy),
                    round(nrow(telecomDummy)*0.75),
                    replace= FALSE)

churnTraining <- telecomDummy[sampleSet,]
churnTesting <- telecomDummy[-sampleSet,]
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
churnTrainingSmoted$Churn <- as.factor(churnTrainingSmoted$Churn)
churnTesting$Churn <- as.factor(churnTesting$Churn)
churnTrainingSmoted$class <- NULL

# Note:
# No Na data [telecomVisual]
# Non SMOTE data (N)  [churnTraining / churnTesting]
# SMOTE data (N + B) [churnTrainingSmoted / churnTesting]

###############
# KNN
##############
library(class)

churnTrainingSmotedLabel <- churnTrainingSmoted %>% select(Churn) 
churnTrainingSmoted <- churnTrainingSmoted %>% select(-Churn) 

churnTrainingLabel <- churnTraining %>% select(Churn) 
churnTraining <- churnTraining %>% select(-Churn) 

churnTestingLabel <- churnTesting %>%  select(Churn) 
churnTesting <- churnTesting %>% select(-Churn) 
#------------
# Non-SMOTE data
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
# SMOTE data
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
