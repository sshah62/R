library(readr)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)
library(stringr)
library(caTools)
library(readxl)
library(DescTools)
library(ggplot2)
library(formatR)
library(caret)
library(pROC)
library(partykit)
library(kernlab)

loan_closed <-
  read.csv("C:/Users/Sanjiv Shah/Desktop/IDA Project/loan_closed.csv")
summary(loan_closed)
str(loan_closed)


table(loan_closed$loan_status)
length(grep("Default", loan_closed$loan_status))
length(grep("Fully_Paid", loan_closed$loan_status))

# List of predictors
predictors <-
  names(loan_closed)[names(loan_closed) != "id" &
                       names(loan_closed) != "member_id" &
                       names(loan_closed) != "grade" &
                       names(loan_closed) != "loan_status" &
                       names(loan_closed) != "issue_d"]
predictors

# Split data in 2 sets - Training and Evaluation

set.seed(316)
split1 <- createDataPartition(loan_closed$loan_status, p = 0.8)[[1]]

loan_train <- loan_closed[split1, ]
loan_eval <- loan_closed[-split1, ]
dim(loan_train)
dim(loan_eval)


eval_results <- data.frame(loan_status = loan_eval$loan_status)


## Model Matrix for models that are running out of memory or taking too long
# loan_trainMM <-
#   data.frame(model.matrix(loan_train$loan_status ~ ., data = loan_train[, predictors]))
# loan_evalMM <-
#   data.frame(model.matrix(loan_eval$loan_status ~ ., data = loan_eval[, predictors]))
# 
# loan_trainMM$loan_status <- loan_train$loan_status
# loan_evalMM$loan_status <- loan_eval$loan_status
# 
# isNZV <- nearZeroVar(loan_trainMM)
# noNZVSet <- names(loan_trainMM)[-isNZV]
# 
# noNZVSet

## Create Wrapper functions to  measure performance
fiveStats <-
  function(...)
    c(twoClassSummary(...), defaultSummary(...))
fourStats <- function (data,
                       lev = levels(data$obs),
                       model = NULL)
{
  accKapp <- postResample(data[, "pred"], data[, "obs"])
  out <- c(accKapp,
           sensitivity(data[, "pred"], data[, "obs"], lev[1]),
           specificity(data[, "pred"], data[, "obs"], lev[2]))
  names(out)[3:4] <- c("Sens", "Spec")
  out
}

ctrl <- trainControl(method = "cv",
                     classProbs = TRUE,
                     summaryFunction = fiveStats)

ctrlNoProb <- ctrl
ctrlNoProb$summaryFunction <- fourStats
ctrlNoProb$classProbs <- FALSE

## Baseline models
### 1. Logistic regression
set.seed(1410)
lrfit <-
  train(
    loan_train$loan_status ~ .,
    data = loan_train[, predictors],
    method = "glm",
    trControl = ctrl,
    metric = "ROC"
  )
lrfit

### LR:LogReg
eval_results$LogReg <- predict(lrfit, loan_eval, type = "prob")[, 1]

lrROC <-
  pROC::roc(eval_results$loan_status,
            eval_results$LogReg,
            levels = rev(levels(eval_results$loan_status)))
lrROC

lrEvalCM <-
  confusionMatrix(predict(lrfit, loan_eval), eval_results$loan_status)
lrEvalCM

## 2. Neural Network
# set.seed(1410)
# nnetGrid <- expand.grid(size = 1:10, decay = c(0, .1, 1, 2))
# maxSize <- max(nnetGrid$size)
#
# nnetfit <-
#   train(x = loan_train[,predictors],
#         y = loan_train$loan_status,
#         method = "nnet",
#         metric = "ROC",
#         preProc = c("center", "scale"),
#         tuneGrid = nnetGrid,
#         trace = FALSE,
#         maxit = 2000,
#         MaxNWts = 1*(maxSize * (length(predictors) + 1) + maxSize + 1),
#         trControl = ctrl
#       )
# 2. random forest
# set.seed(1410)
# rfFit <- train(loan_train$loan_status ~ ., data = loan_train[, predictors],
#                method = "rf",
#                trControl = ctrl,
#                ntree = 500,
#                tuneLength = 5,
#                metric = "ROC")
# rfFit
# eval_results$RF <- predict(rfFit, loan_eval, type = "prob")[, 1]
# rfROC <-
#   pROC::roc(eval_results$loan_status,
#             eval_results$RPART,
#             levels = rev(levels(eval_results$loan_status)))
# rfROC
#
# rfEvalCM <- confusionMatrix(eval_results$RF, eval_results$loan_status)
# rfEvalCM

# 3. RPART
set.seed(1410)
rpartFit <- train(
  x = loan_train[, predictors],
  y = loan_train$loan_status,
  method = "rpart",
  tuneLength = 30,
  metric = "ROC",
  trControl = ctrl
)
rpartFit
dev.off()
plot(as.party(rpartFit$finalModel))

eval_results$RPART <-
  predict(rpartFit, loan_eval, type = "prob")[, 1]

rpartROC <-
  pROC::roc(eval_results$loan_status,
            eval_results$RPART,
            levels = rev(levels(eval_results$loan_status)))
rpartROC

rpartEvalCM <-
  confusionMatrix(predict(rpartFit, loan_eval), eval_results$loan_status)
rpartEvalCM

# 4. SVM

set.seed(1157)
sigma <-
  sigest(loan_trainMM$loan_status ~ .,
         data = loan_trainMM[, noNZVSet],
         frac = .75)
names(sigma) <- NULL

svmGrid1 <- data.frame(sigma = sigma[2],
                       C = 2 ^ c(2:6))

set.seed(1410)

svmFit <- train(
  loan_trainMM$loan_status ~ .,
  data = loan_trainMM[, noNZVSet],
  method = "svmRadial",
  tuneGrid = svmGrid1,
  preProc = c("center", "scale"),
  metric = "Kappa",
  trControl = ctrl
)
svmFit

eval_results$SVM <-
  predict(svmFit, loan_eval[, predictors], type = "prob")[, 1]

svmROC <- pROC::roc(eval_results$loan_status, eval_results$SVM,
                    levels = rev(levels(eval_results$loan_status)))
svmROC

# addressing Imbalance in predictor class
table(loan_closed$loan_status) # Class (loan_status) imbalance in Closed Loan Dataset

table(loan_train$loan_status) # Class (loan_status) imbalance in the Training Data sub-set


# Fully_paid : Default ratio ~ 3:1
# Since we have trouble with many of the models, we will use down sampling techique first.
ds_loan_train <-
  downSample(x = loan_train[, predictors],
             y = loan_train$loan_status,
             ## keep the class variable name the same
             yname = "loan_status")

dim(ds_loan_train)
table(ds_loan_train$loan_status)

dim(ds_loan_train)
table(ds_loan_train$loan_status)

## USE downsampled data for previous models (lr and rpart)
set.seed(1410)
ds_lrfit <-
  train(
    ds_loan_train$loan_status ~ .,
    data = ds_loan_train[, predictors],
    method = "glm",
    trControl = ctrl,
    metric = "ROC"
  )
ds_lrfit

### LR:LogReg
ds_eval_results <- data.frame(loan_status = loan_eval$loan_status)
ds_eval_results$LogReg <-
  predict(ds_lrfit, loan_eval, type = "prob")[, 1]

ds_lrROC <-
  pROC::roc(ds_eval_results$loan_status,
            ds_eval_results$LogReg,
            levels = rev(levels(ds_eval_results$loan_status)))
ds_lrROC

ds_lrEvalCM <-
  confusionMatrix(predict(ds_lrfit, loan_eval), ds_eval_results$loan_status)
ds_lrEvalCM

## rpart with Downsized data
set.seed(1410)
ds_rpartFit <- train(
  x = ds_loan_train[, predictors],
  y = ds_loan_train$loan_status,
  method = "rpart",
  tuneLength = 30,
  metric = "ROC",
  trControl = ctrl
)
ds_rpartFit
dev.off()
plot(as.party(ds_rpartFit$finalModel))

ds_eval_results$RPART <-
  predict(ds_rpartFit, loan_eval, type = "prob")[, 1]

ds_rpartROC <-
  pROC::roc(ds_eval_results$loan_status,
            ds_eval_results$RPART,
            levels = rev(levels(ds_eval_results$loan_status)))
ds_rpartROC

ds_rpartEvalCM <-
  confusionMatrix(predict(ds_rpartFit, loan_eval), ds_eval_results$loan_status)
ds_rpartEvalCM

# Cost Sensitive Training
## Cost sensitive CART Model using rpart package (use original training data)
costMatrix <- matrix(c(0, 20, 1, 0), ncol = 2) # create a cost matrix
rownames(costMatrix) <- levels(loan_train$loan_status)
colnames(costMatrix) <- levels(loan_train$loan_status)
costMatrix

### 20 fold higher cost of a false negative ('default' is the positive class) than a false positive

set.seed(1410)
loan_train$loan_status <- as.character(loan_train$loan_status)
cartWMod <- train(x = loan_train[, predictors],
                  y = loan_train$loan_status,
                  method = "rpart",
                  trControl = ctrlNoProb,
                  tuneLength = 30,
                  metric = "Kappa",
                  parms = list(loss = costMatrix))

cartWMod


dev.off()
plot(as.party(cartWMod$finalModel))

eval_results$cartWMod <-
  predict(cartWMod, loan_eval, type = "prob")[, 1]

cartWModROC <-
  pROC::roc(eval_results$loan_status,
            eval_results$cartWMod,
            levels = rev(levels(eval_results$loan_status)))
cartWModROC

cartWModEvalCM <-
  confusionMatrix(predict(cartWMod, loan_eval), eval_results$loan_status)
cartWModEvalCM

# Develop predictions for 'Current Loans' using downsampled LR model

loan_current <-
  read.csv("C:/Users/Sanjiv Shah/Desktop/IDA Project/loan_current.csv")
dim(loan_current)

loan_current$purpose[loan_current$purpose == "educational"] <- "other"

prediction2 <-
  predict(ds_rpartFit, loan_current[, predictors], type = "prob")[, 1]


loan_current$loan_status <- (prediction2)


table(loan_current$loan_status)
