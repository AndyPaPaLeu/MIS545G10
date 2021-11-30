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
# Decision tree
##############
library(rpart.plot)
#------------
# original data [telecomVisual]
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
                                        cp = .005,  #0.0015  0.7923777
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