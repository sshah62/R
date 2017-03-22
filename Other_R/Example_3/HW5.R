# Sanjiv Shah
# DSA 5103 Intelligent Data Analytics
# Fall 2016
# Homework # 5

# ------------------ Libraries
library(readr)
library(caret)
library(dplyr)
library(mlbench)
library(ROCR)
library(Hmisc)
library(sm)
library(rminer)
library(InformationValue)
library(car)
library(MASS)
library(VIM)
library(InformationValue)
library(glmnet)
library(rpart)
library(partykit)      #print trees using
library(rattle)
library(randomForest)   #Random Forests
library(adabag)         #boosting
library(e1071)
library(kernlab)


# Q1 Create a Function for Classification Evaluation

C_P_E <- function (Observed, Predicted)
{
  require(ROCR)
  require(InformationValue)
  require(sm)
  
  CPE = list()
  
  # Confusion Matrix
  CPE$"Confusion Table" = table(true = Observed, pred = Predicted)
  CPE$"Confusion Matrix" = confusionMatrix(Predicted, Observed)
  
  
  #K-s Chart
  ks_plot(Observed, Predicted)
  CPE$"K-S Statistics" = ks_stat(Observed, Predicted)
  
  # ROC
  CPE$Predictions = ROCR::prediction(Predicted, Observed)
  CPE$ROC = ROCR::performance(CPE$Predictions, "tpr", "fpr")
  plotROC(Observed, Predicted)
  plot(CPE$ROC, colorize = FALSE, print.cutoffs.at = c(0.25, 0.5, 0.75))
  abline(0, 1)
  
  # AUC
  CPE$"AUC Performance" = performance(CPE$Predictions, "auc")
  CPE$AUROC = AUROC(Observed, Predicted)
  
  # True positives and True negatives
  sm.density.compare(Predicted, Observed, xlab = "cut-off values")
  
  # Concordant Pairs
  t = Concordance(Observed, Predicted)
  CPE$Concordant = t$Concordance
  CPE$Discordant = t$Discordance
  CPE$Ties = t$Tied
  CPE$pairs = t$Pairs
  
  # Lift Chart
  Lift_Chart = performance(CPE$Predictions, "lift", "rpp")
  plot(Lift_Chart, colorize = TRUE, print.cutoffs.at = c(0.25, 0.5,0.75))
  
  # D-Statistic 
  inputData = cbind(Observed, Predicted)
  Obs1 = inputData[inputData[,1] == 1]
  Obs0 = inputData[inputData[,1] == 0]
  CPE$D.Statistic = mean(Obs1) - mean(Obs0)
  
  return(CPE)
}

# --------------------Problem 3: Bank Marketing Data ---------------------

BMData <- read.table("C:/Users/Sanjiv Shah/OneDrive - Anoopam Holdings, LP/01OU/06Fall2016/ISE5103/R-Projcets/bank-additional-full.csv", header=TRUE, sep=";")
head(BMData)
str(BMData)
summary(BMData)


#### Missing Data

BM_Missing <- vector();

for (i in 1:length(BMData))
{
  BM_Missing[i] = length(BMData[is.na(BMData[,i]),i])
}
BM_Missing 

### No missing values ## look for "unknown"

for (i in 1:length(BMData))
{
  BM_Missing[i] = BM_Missing[i] + length(BMData[BMData[,i]=="unknown",i])
}
BM_Missing
BM_Missing / length(BMData[,1]) * 100

## "default" 20% unknowns. Ignore this column.  Convert rest of "Unknown" to NA
BM_Clean <- subset(BMData, select = -c(default))


## Convert remaining "unknown" to NA
BM_Clean[BM_Clean=="unknown"] <- NA
mean(is.na(BM_Clean))
aggregate(BM_Clean, by=list(BM_Clean$y), function(x) mean(is.na(x)))

## Convert decision column to numeric
BM_Clean$y <- as.numeric(BM_Clean$y == "yes")

## Remove all remaining NA (Since non-numeric variables can't be imputed)
row.has.na <- apply(BM_Clean, 1, function(x){any(is.na(x))})
sum(row.has.na)

## Only 2,943 of 41,188 (approx. 7%) rows have NA. Okay to drop them

BM_Clean <- BM_Clean[!row.has.na,]



## No imputations needed anymore. 

## sPLIT DATA IN TRAINING, CROSS-VALIDATION AND TESTING
## 70% for training and 30% for testing
## Random Sampling

p = length(BM_Clean[,1])
samples = sample(1:p, p)


BM_Train = BM_Clean[samples[1:(0.8*p)],]
BM_Test = BM_Clean[samples[((0.8*p)+1):p],]


#---------------------Problem 3b----------------------
## logistic regression method

BM_FitLR <- glm(data=BM_Train, y~., family="binomial")
summary(BM_FitLR)
plot(BM_FitLR)


#influence
influence.measures(BM_FitLR)
influencePlot(BM_FitLR)

#variance inflation
vif(BM_FitLR)
AIC(BM_FitLR)


mean(vif(BM_FitLR))

BM_LR_Predict = predict(BM_FitLR, BM_Test)
BM_LR_C_P_E = C_P_E (BM_Test$y, BM_LR_Predict)

#Generate comparison Criteria AUC, TPR, ALIFT
Predictions = ROCR::prediction(BM_LR_Predict, BM_Test$y)
AUC_Performance = performance(Predictions, "auc")
AUC_Performance

BM_LR_Predict[BM_LR_Predict >= 0] = 1
BM_LR_Predict[BM_LR_Predict < 0] = 0
hist(BM_LR_Predict - BM_Test$y)

plot(BM_FitLR, which = 6)


alias(BM_FitLR)
stepAIC(BM_FitLR)

#Pearson's Residual
BM_LR_Pearson <-residuals(BM_FitLR,type="pearson")

#Deviance Residuals
BM_LR_deviance <-residuals(BM_FitLR,type="deviance")

# Response Residuals
BM_LR_Res <-residuals(BM_FitLR,type="response")

#Student's Residuaal
BM_LR_Student<-rstudent(BM_FitLR)

#Fitted Values
BM_LR_FV<-fitted(BM_FitLR)

plot(BM_LR_Student) 
barplot(BM_LR_Student)

plot(hatvalues(BM_FitLR), rstudent(BM_FitLR))

#----------------Problem 3(c)------------------------------
#-----------Elastic Net -----------------------------------
#Use 15 fold CV
fitCtrl = trainControl(method="cv", number = 15)
BM_FitEN = train(y~., data = BM_Train, method="glmnet", trControl=fitCtrl)
summary(BM_FitEN)
plot(BM_FitEN)

BM_EN_Predict = predict(BM_FitEN, BM_Test)

#Function Call
BM_EN_C_P_E = C_P_E (BM_Test$y, BM_EN_Predict)

Predictions = ROCR::prediction(BM_EN_Predict, BM_Test$y)
AUC_Performance = performance(Predictions, "auc")
AUC_Performance

BM_EN_Predict[BM_EN_Predict >= 0] = 1
BM_EN_Predict[BM_EN_Predict < 0] = 0

hist(BM_EN_Predict - BM_Test$y)


#-----------Deciesion Tree ---------------------------------
BM_fitDT <- rpart(y~., data = BM_Train, parms=list(split="information"), 
                control=rpart.control(cp=0.001), xval=20)
summary(BM_fitDT)


fancyRpartPlot(BM_fitDT)
printcp(BM_fitDT) #the cost-parameter
plotcp(BM_fitDT)

#Prune the Tree
cp <- BM_fitDT$cptable[which.min(BM_fitDT$cptable[,"xerror"]),"CP"]
BM_fitDT2 <- prune(BM_fitDT, cp=cp)
plot(BM_fitDT2)

BM_DT_Predict = predict(BM_fitDT, BM_Test)
#Function Call
BM_DT_C_P_E = C_P_E (BM_Test$y, BM_DT_Predict)

Predictions = ROCR::prediction(BM_DT_Predict, BM_Test$y)
AUC_Performance = performance(Predictions, "auc")
AUC_Performance

BM_DT_Predict[BM_DT_Predict >= 0] = 1
BM_DT_Predict[BM_DT_Predict < 0] = 0

hist(BM_DT_Predict - BM_Test$y)



#-----------Random Forest ----------------------------------

#### WARNING : THIS TAKES A LONG TIME TO RUN
BM_FitRF <- randomForest(y ~ ., data=BM_Train, importance=T, ntrees=200, mtry=3)
plot(BM_FitRF)

#Plots
barplot(BM_FitRF$importance[, 1], main = "Importance (Dec.Accuracy)")
barplot(BM_FitRF$importance[, 2], main = "Importance (Gini Index)")
varImpPlot(BM_FitRF)


BM_RF_Predict = predict(BM_FitRF, BM_Test)

#Function Call
BM_RF_C_P_E = C_P_E (BM_Test$y, BM_RF_Predict)

Predictions = ROCR::prediction(BM_EN_Predict, BM_Test$y)
AUC_Performance = performance(Predictions, "auc")
AUC_Performance

BM_RF_Predict[BM_RF_Predict >= 0] = 1
BM_RF_Predict[BM_RF_Predict < 0] = 0

hist(BM_RF_Predict - BM_Test$y)

# ----------Boosted Tree -------------------------------------
BM_Train_F = BM_Train
BM_Train_F$y = as.factor(BM_Train_F$y)
BM_FitBT <- boosting(y ~ ., data = BM_Train_F, boos=F, mfinal=10)
plot(BM_FitBT$importance)
plot(BM_FitBT$class)

BM_BT_Predict = predict(BM_FitBT, BM_Test)

#Function Call
BM_BT_C_P_E = C_P_E (BM_Test$y, BM_BT_Predict)
Predictions = ROCR::prediction(BM_BT_Predict, BM_Test$y)
AUC_Performance = performance(Predictions, "auc")
AUC_Performance

BM_Test$y <- as.numeric(as.character(BM_Test$y))
BM_BT_Predict <- as.numeric(as.character(BM_BT_Predict))

hist(BM_BT_Predict - BM_Test$y)


#------- Prob 4: svm --------------------------------
# ----- WARNING: THE NEXT LINE OF CODE TAKES A LONG TIME TO RUN --------------
BM_FitkSVM <- ksvm(y ~ ., data = BM_Train, type = "C-svc")
BM_FitSVM <- svm(y ~ ., data = BM_Train, cost = 50, gamma = 1)

plot(BM_FitSVM)
plot(BM_FitkSVM, data = BM_Train)

BM_SVM_Predict  <- predict(BM_FitSVM, BM_Test)

#Function Call
BM_SVM_C_P_E = C_P_E (BM_Test$y, BM_SVM_Predict)
Predictions = ROCR::prediction(BM_SVM_Predict, BM_Test$y)
AUC_Performance = performance(Predictions, "auc")
AUC_Performance

BM_SVM_Predict[BM_SVM_Predict >= 0] = 1
BM_SVM_Predict[BM_SVM_Predict < 0] = 0
hist(BM_SVM_Predict - BM_Test$y)




