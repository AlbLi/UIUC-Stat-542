##PSL Project 1
##Fall 2022
##Paul Holaway (paulch2), Albert Li (xiangl9), & Matt Schroeder (mas5)

################################################################################
#Loading Necessary Libraries
library(fastDummies)
library(gbm)
library(glmnet)
library(tidyverse)
################################################################################
#Pre-processing Training Data
#Importing the training data
train <- read.csv("train.csv")
#Log-transforming the sale price
train$Sale_Price = log(train$Sale_Price)
#Renaming Sale_Price to log_Sale_Price and rearranging the data such that log_Sale_Price
#is the first column.
train <- train %>% rename(log_Sale_Price = "Sale_Price") %>% select(log_Sale_Price, everything())
#Filling in NA values with 0
train[is.na(train)] <- 0
#Winsorization for training data
winsor.vars <- colnames(train[,3:ncol(train)])
quan.value = 0.95
for(var in winsor.vars){
  if(class(train[, var]) != "character"){
    temp = train[, var]
    myquan = quantile(temp, probs = quan.value, na.rm = TRUE)
    temp[temp > myquan] <- myquan
    train[, var] <- temp
  }
}
#Dummy Coding of Categorical Variables and Factors
new_train = dummy_columns(train, split = TRUE, remove_selected_columns = TRUE)
#Fitting The Two Models
#Creating Lasso Model
set.seed(0243)
cv.out = cv.glmnet(as.matrix(new_train[,3:ncol(new_train)]),new_train[,1], alpha = 0.5)
min.lam = cv.out$lambda.min #Minimum Lambda
se.lam = cv.out$lambda.1se #1se Lambda
model1a = glmnet(new_train[,3:ncol(new_train)],new_train[,1], alpha = 0.5, lambda = min.lam,
                 verbose = FALSE) #Lasso using min. lambda
model1b = glmnet(new_train[,3:ncol(new_train)],new_train[,1], alpha = 0.5, lambda = se.lam,
                 verbose = FALSE) #Lasso using 1se lambda
#Training Error lambda min.
test1 = predict(model1a, s = min.lam, as.matrix(new_train[,3:ncol(new_train)]))
#Training Error lambda1se
test2 = predict(model1b, s = se.lam, as.matrix(new_train[,3:ncol(new_train)]))
#RMSE of two models
rmse1 = sqrt(mean((test1 - new_train[,1])^2))
rmse2 = sqrt(mean((test2 - new_train[,1])^2))
#Selecting the optimal model
if(rmse1 > rmse2){
  best.lam = se.lam
  model1 = model1b
} else if(rmse1 < rmse2){
  best.lam = min.lam
  model1 = model1a
} else {
  best.lam = min.lam
  model1 = model1a
}
#Creating Boosting Tree Model
set.seed(0243)
model2 = gbm(new_train[,1] ~., data = new_train[,3:ncol(new_train)], 
             distribution = "gaussian", n.trees = 2500, interaction.depth = 4, 
             verbose = FALSE, shrinkage = 0.05)
################################################################################
#Pre-processing Testing Data
#Importing the testing data
test <- read.csv("test.csv")
#Filling in NA values with 0
test[is.na(test)] <- 0
#Winsorization for testing data
winsor.vars <- colnames(test[,3:ncol(test)])
quan.value = 0.95
for(var in winsor.vars){
  if(class(test[, var]) != "character"){
    temp = test[, var]
    myquan = quantile(temp, probs = quan.value, na.rm = TRUE)
    temp[temp > myquan] <- myquan
    test[, var] <- temp
  }
}
#Dummy Coding of Categorical Variables and Factors
new_test = dummy_columns(test, split = TRUE, remove_selected_columns = TRUE)
#Fixing new_test to work in models
fixed_test = data.frame(matrix(nrow = nrow(new_test), ncol = ncol(new_train)))
colnames(fixed_test) <- colnames(new_train)
fixed_test = fixed_test[,-(1:2)]
#for loop 
for(i in 1:ncol(fixed_test)){
  for(j in 1:ncol(new_test)){
    if(colnames(fixed_test)[i] == colnames(new_test)[j]){
      fixed_test[,i] = new_test[,j]
    }
  }
}
#Filling NAs
fixed_test[is.na(fixed_test)] <- 0
#Predicting Using The Two Models
#Lasso
pred1 = predict(model1, s = best.lam, as.matrix(fixed_test))
#Ridge
pred2 = predict(model2, fixed_test, n.trees = 2500)
################################################################################
#Submission File Formatting
#Rearranging Columns For Formatting Purposes
pred1 = data.frame(pred1)
pred2 = data.frame(pred2)
pred1 = pred1 %>% rename(Sale_Price = "s1") %>% mutate(PID = new_test$PID) %>% 
                  select(PID, Sale_Price) %>% mutate(Sale_Price = exp(Sale_Price))
pred2 = pred2 %>% rename(Sale_Price = "pred2") %>% mutate(PID = new_test$PID) %>%
                  select(PID, Sale_Price) %>% mutate(Sale_Price = exp(Sale_Price))
#Writing The Files Into .txt Files
write.table(pred1, file = "mysubmission1.txt", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(pred2, file = "mysubmission2.txt", sep = ",", col.names = TRUE, row.names = FALSE)
################################################################################