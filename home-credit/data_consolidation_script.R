#For both the APPLICATION TRAINING and APPLICATION TESTING set, this script 
#merges in two variables from BUREAU and cleans, selects, and exports relevent 
#variables.

#load packages
library(tidyverse)

#reads in training data set as 'raw_data_train' and testing data as raw_data_test
raw_data_train <- read_csv("application_train.csv")
raw_data_test <- read_csv("application_test.csv")

#reads in bureau
raw_bureau <- read_csv("bureau.csv")

#corrects conflicting name complicating merge
raw_bureau <- raw_bureau %>%
  rename("AMT_ANNUITY_BUREAU" = "AMT_ANNUITY")

#merges training data
merged_training_data <- merge(raw_data_train, raw_bureau, by="SK_ID_CURR")

#merges testing data
merged_testing_data <- merge(raw_data_test, raw_bureau, by="SK_ID_CURR")

#Creates training data subset
clean_training_data <- merged_training_data %>%
  select(
    SK_ID_CURR,
    TARGET,
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
    DAYS_EMPLOYED,
    DAYS_CREDIT
  )

#Creates testing data subset
clean_testing_data <- merged_testing_data %>%
  select(
    SK_ID_CURR,
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
    DAYS_EMPLOYED,
    DAYS_CREDIT
  )

#Training/testing data sets (on SK_ID_CURR) have a 1:many relationship for each
#existing line of credit reported to credit bureau. This chooses the max value
#for each customer, indicating their credit length prior to applying for home
#credit application
clean_training_data <- clean_training_data %>%
  group_by(
    SK_ID_CURR,
    TARGET,
    AMT_INCOME_TOTAL,
    AMT_CREDIT,
    AMT_ANNUITY,
    NAME_INCOME_TYPE,
    FLAG_OWN_CAR,
    FLAG_OWN_REALTY,
    NAME_EDUCATION_TYPE,
    DAYS_EMPLOYED
  ) %>% slice_min(n=1,DAYS_CREDIT)

clean_testing_data <- clean_testing_data %>%
  group_by(
    SK_ID_CURR,
    AMT_INCOME_TOTAL,
    AMT_CREDIT,
    AMT_ANNUITY,
    NAME_INCOME_TYPE,
    FLAG_OWN_CAR,
    FLAG_OWN_REALTY,
    NAME_EDUCATION_TYPE,
    DAYS_EMPLOYED
  ) %>% slice_min(n=1,DAYS_CREDIT)

#removes duplicate values from merge, keeping unique values based on curr customer id
clean_training_data <- clean_training_data %>%
  distinct(SK_ID_CURR, .keep_all=TRUE)

clean_testing_data <- clean_testing_data %>%
  distinct(SK_ID_CURR, .keep_all=TRUE)

#Removes NAs from both training and testing data
clean_training_data <- na.omit(clean_training_data)
clean_testing_data <- na.omit(clean_testing_data)

#Writes training and testing data to two separate csv files for modeling purposes
write.csv(clean_training_data, file="clean_training_data.csv", row.names=FALSE)
write.csv(clean_testing_data, file = "clean_testing_data.csv", row.names=FALSE)














