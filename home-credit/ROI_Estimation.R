#Estimating ROI on model

#Test accuracy
## Majority classifier: 0.5
## RF Model: 0.63974

#Approach 1: calc difference in model accuracy, multiple by median value to get total business amount, multiple average interest rate. 

library(tidyverse)


prev_apps <- read.csv('previous_application.csv')
training_data <- read.csv("application_train.csv")
testing_data <- read.csv("application_test.csv")

med_train <- median(training_data$AMT_CREDIT)
med_test <- median(testing_data$AMT_CREDIT)
#12.4% difference in median credit amounts between training and testing set.

print(med_test)

print((med_train - med_test)/med_train)

#interest rates
med_ir <- median(prev_apps$RATE_INTEREST_PRIMARY)
ir_no_na <- prev_apps %>%
  select(ir = RATE_INTEREST_PRIMARY) %>%
  drop_na()

ir_no_na %>%
  ggplot(aes(x=ir)) + geom_histogram()

ir_no_na %>%
  summary()

#48,744 observations in testing set, multiplied by accuracy ratio, multiplied by median loan amount, multipled by median interest rate

#1290 default
#Our model captures 30.4% of defaulters, only losing money for 898 defaulters vs 1290

maj_classifier_roi <- 48744 * 0.5 * 450000 * 0.18912
print(maj_classifier)
#2,074,154,688

rf_classifier_roi <- 48744 * 0.63974 * 450000 * 0.18912
print(rf_classifier_roi)
#2,653,839,440

#assumes simple interest, discounts priveledged interest rates, normal loan amount distribution




