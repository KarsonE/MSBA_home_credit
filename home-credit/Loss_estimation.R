#Comparison
#Prediction script

#SETUP

#copy and paste model or perform this in workbook

#<----------------------PREP--------------------------------->
#packages
library(tidyverse)
library(caret)
library(naivebayes)
library(readr)
library(dplyr)
library(rpart)
library(rminer)
library(randomForest)
library(pROC)
library(MASS)

#new


training_set <- read_csv('clean_training_data.csv')
testing_set <- read_csv('clean_testing_data.csv')

testing_set <- testing_set %>% mutate(across(where(is.character), as.factor))
training_set <- training_set %>% mutate(across(where(is.character), as.factor))

#we should factor the Target variable for classification approaches, too.
training_set$TARGET <- as.factor(training_set$TARGET)

#There appears to be one anomaly in the DAYS_EMPLOYED Values; a very large positive number. 
training_set %>%
  ggplot(aes(DAYS_EMPLOYED)) + geom_boxplot()

testing_set %>%
  ggplot(aes(DAYS_EMPLOYED)) + geom_boxplot()


summary(testing_set$DAYS_EMPLOYED)
summary(training_set$DAYS_EMPLOYED)

#The anomoly occurs in both training and testing. It must be a mis entry as it's impossible to work 365,243 days in a human lifetime. We will remove it from both sets.
training_set <- training_set %>%
  filter(DAYS_EMPLOYED <= 0)

summary(training_set$DAYS_EMPLOYED)

testing_set <- testing_set %>%
  filter(DAYS_EMPLOYED <= 0)

summary(training_set$DAYS_EMPLOYED)

#DAYS_EMPLOYED and DAYS_CREDIT are both negative values, since they are past date - current date. Let's make them absolute values to be easier to interpret. 
training_set$DAYS_EMPLOYED <- abs(training_set$DAYS_EMPLOYED)
testing_set$DAYS_EMPLOYED <- abs(testing_set$DAYS_EMPLOYED)
training_set$DAYS_CREDIT <- abs(training_set$DAYS_CREDIT)
testing_set$DAYS_CREDIT <- abs(testing_set$DAYS_CREDIT)

#remove sk_curr_ID to avoid incidentally using it as a predictor
training_set <- training_set[-c(1)]
testing_set <- testing_set[-c(1)]


set.seed(234)

#creates a training subset of the training data with 70% of the data
t_train_index <- createDataPartition(training_set$TARGET, p = 0.7, list=FALSE)

t_train <- training_set[t_train_index,]
t_test <- training_set[-t_train_index,]


training_sample <- upSample(t_train, t_train$TARGET)

#<-------------------------PLUG IN MODEL HERE ------------------------>
rf_mod <- randomForest(TARGET ~.,
                       data = training_sample[, -c(27:28)],
                       mtry = 7,
                       ntree = 50,
                       nodesize = 200)

#Read in application test file
app_test <- read_csv("application_test.csv")

app_test_match <- app_test %>%
  dplyr::select(
    NAME_CONTRACT_TYPE,
    OCCUPATION_TYPE,
    CODE_GENDER,
    AMT_INCOME_TOTAL,
    AMT_CREDIT,
    AMT_ANNUITY,
    AMT_GOODS_PRICE,
    NAME_FAMILY_STATUS,
    NAME_HOUSING_TYPE,
    REGION_POPULATION_RELATIVE,
    DAYS_ID_PUBLISH,
    REGION_RATING_CLIENT,
    REGION_RATING_CLIENT_W_CITY,
    YEARS_BUILD_MODE,
    AMT_REQ_CREDIT_BUREAU_YEAR,
    DAYS_LAST_PHONE_CHANGE,
    NONLIVINGAREA_MODE,
    FLAG_WORK_PHONE,
    FLAG_CONT_MOBILE,
    DAYS_BIRTH,
    NAME_INCOME_TYPE,
    FLAG_OWN_CAR,
    FLAG_OWN_REALTY,
    NAME_EDUCATION_TYPE,
    DAYS_EMPLOYED
  )

#More formatting for app_test
app_test_match <- app_test_match %>% mutate(across(where(is.character), as.factor))
app_test_match$DAYS_EMPLOYED <- abs(app_test$DAYS_EMPLOYED)

summary(app_test_match$NAME_INCOME_TYPE)
summary(training_sample$NAME_INCOME_TYPE)

#NAME_INCOME_TYPE in app_test has two additional factor levels. Convert them to NAs
app_test_match$NAME_INCOME_TYPE <- recode_factor(app_test_match$NAME_INCOME_TYPE, 'Pensioner' = NA_character_)
app_test_match$NAME_INCOME_TYPE <- recode_factor(app_test_match$NAME_INCOME_TYPE, 'Unemployed' = NA_character_)
summary(app_test_match)

#Impute missing values
app_test_match <- na.roughfix(app_test_match)

#Need to predict probabilities for each observation in app_test
app_test_probabilities <- predict(rf_mod, app_test_match)

app_test_probabilities <- rf_mod %>%
  predict(app_test_match, type = 'prob')

head(app_test_probabilities)

#create dataframe with probabilities and SK_ID_CURR
kaggle_submission <- data.frame(cbind(app_test$SK_ID_CURR, app_test_probabilities[,2]))
head(kaggle_submission)

#NEED SK_ID_CURR and TARGET headers
colnames(kaggle_submission) <- c("SK_ID_CURR", "TARGET")
head(kaggle_submission)

#round probabilities to 1 decimal
kaggle_submission <- kaggle_submission %>% mutate_at(c('TARGET'), funs(round(., 1)))
head(kaggle_submission)

#Both values must be numeric
kaggle_submission <- kaggle_submission %>%
  mutate_at(c('SK_ID_CURR','TARGET'), as.numeric)
head(kaggle_submission)

kaggle_submission <- kaggle_submission %>%
  mutate_at(c('SK_ID_CURR'), as.integer)
head(kaggle_submission)

#round probabilities to 1 decimal
kaggle_submission <- kaggle_submission %>% mutate_at(c('TARGET'), funs(round(., 1)))
head(kaggle_submission)

#Converts probs to 1 and 0
kaggle_binary <- kaggle_submission %>% mutate(
  pred = ifelse(kaggle_submission$TARGET > 0.5, 1, 0)
  )

kaggle_binary <- kaggle_binary %>% dplyr::select(SK_ID_CURR, pred)

#only default stores predicted defaultt values in RF model
only_defaults <- kaggle_binary %>%
  filter(pred == 1)

new_testing_set <- read_csv('application_test.csv')

new_testing_set <- new_testing_set %>%
  dplyr::select(SK_ID_CURR, AMT_CREDIT)

pred_defaults <- merge(only_defaults, new_testing_set, by="SK_ID_CURR")

pred_defaults <- pred_defaults %>%
  mutate(interest = 0.18912 * AMT_CREDIT)

#assumes ~70% recovery rate and total interest is lost.
pred_defaults <- pred_defaults %>%
  mutate(total_loss = interest)

print(sum(pred_defaults$total_loss))
#Create new DF with comparison between majority classfier and RF model





