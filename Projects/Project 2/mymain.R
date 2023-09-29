# reading in libraries
library(lubridate)
library(tidyverse)
library(glmnet)
# probably need to install
library(prophet)

mypredict = function(){
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_current <- test %>%
    filter(Date >= start_date & Date < end_date) %>%
    select(-IsHoliday)
  
  start_last_year = min(test_current$Date) - 375
  end_last_year = max(test_current$Date) - 350
  tmp_train <- train %>%
    filter(Date > start_last_year & Date < end_last_year) %>%
    mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
    rename(Weekly_Pred = Weekly_Sales) %>%
    select(-Date, -IsHoliday)
  
  test_current <- test_current %>%
    mutate(Wk = week(Date))
  
  # all below code from what we have tried III
  test_depts <- unique(test_current$Dept)
  test_pred <- NULL
  
  # test_depts
  for(dept in test_depts){
    train_dept_data <- train %>% filter(Dept == dept)
    test_dept_data <- test_current %>% filter(Dept == dept)
    
    # no need to consider stores that do not need prediction
    # or do not have training samples
    train_stores <- unique(train_dept_data$Store)
    test_stores <- unique(test_dept_data$Store)
    test_stores <- intersect(train_stores, test_stores)
    
    # test_stores
    for(store in test_stores){
      tmp_train <- train_dept_data %>% 
        filter(Store == store) %>%
        mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
        mutate(Yr = year(Date))
      tmp_test <- test_dept_data %>% 
        filter(Store == store) %>%
        mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
        mutate(Yr = year(Date)) 
      
      tmp_train$Wk = factor(tmp_train$Wk, levels = 1:52)
      tmp_test$Wk = factor(tmp_test$Wk, levels = 1:52)
      
      train_model_matrix <- model.matrix(~ Yr + Wk + I(Yr^2), tmp_train)
      test_model_matrix <- model.matrix(~ Yr + Wk + I(Yr^2), tmp_test)
      
      # changed the response to the sqrt of the abs value
      mycoef <- lm(sqrt(abs(tmp_train$Weekly_Sales)) ~ train_model_matrix)$coef
      mycoef[is.na(mycoef)] <- 0
      
      # squaring the prediction
      tmp_pred <- (mycoef[1] + test_model_matrix %*% mycoef[-1]) ** 2
      
    
      # implementing the shift described in post 964 on Campuswire
      
      # first half of the tmp_test creation
      
       tmp_test <- tmp_test %>%
         mutate(Weekly_Pred = tmp_pred[,1])
      
      # t=5 if shifter
    
      if (length(tmp_test$Weekly_Pred[tmp_test$Wk == 48]) != 0 &&
        length(tmp_test$Weekly_Pred[tmp_test$Wk == 49]) != 0 &&
        length(tmp_test$Weekly_Pred[tmp_test$Wk == 50]) != 0 &&
        length(tmp_test$Weekly_Pred[tmp_test$Wk == 51]) != 0 &&
        length(tmp_test$Weekly_Pred[tmp_test$Wk == 52]) != 0) {
        if (mean(c(tmp_test$Weekly_Pred[tmp_test$Wk == 49],tmp_test$Weekly_Pred[tmp_test$Wk == 50], tmp_test$Weekly_Pred[tmp_test$Wk == 51]))
            >= 1.1 * mean(c(tmp_test$Weekly_Pred[tmp_test$Wk == 48],tmp_test$Weekly_Pred[tmp_test$Wk == 52]))) {
          # shifting one week up except for 52 to 48
          shift = 1/7
          
          # temp vars of the current values
          t48 = tmp_test$Weekly_Pred[tmp_test$Wk == 48]
          t49 = tmp_test$Weekly_Pred[tmp_test$Wk == 49]
          t50 = tmp_test$Weekly_Pred[tmp_test$Wk == 50]
          t51 = tmp_test$Weekly_Pred[tmp_test$Wk == 51]
          t52 = tmp_test$Weekly_Pred[tmp_test$Wk == 52]
          
          # doing the actual shifts
          tmp_test$Weekly_Pred[tmp_test$Wk == 48] <- t48 * (1-shift)+ t52 * shift
          tmp_test$Weekly_Pred[tmp_test$Wk == 49] <- t49 * (1-shift)+ t48 * shift
          tmp_test$Weekly_Pred[tmp_test$Wk == 50] <- t50 * (1-shift)+ t49 * shift
          tmp_test$Weekly_Pred[tmp_test$Wk == 51] <- t51 * (1-shift)+ t50 * shift
          tmp_test$Weekly_Pred[tmp_test$Wk == 52] <- t52 * (1-shift)+ t51 * shift
          
        }
        
      }
      
       # second half of temp test
       tmp_test <- tmp_test %>%
         select(-Wk, -Yr)
    
      test_pred <- test_pred %>% bind_rows(tmp_test)
    }
  }
  
  return(test_pred)
}
