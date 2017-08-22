## adding parcelID to previous model
## results improve slightly but still pretty crappy
## adding fips and removing any elements tried before that had 0 sig. in prev model.

## Starter library list
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
# library(leaflet)
library(lubridate)

## few more
library(stringi)
library(caret)    # also needed in connection w/ parallel processing

## Load Data
clean_TrainTest_wDates <- fread('./input/clean_TrainTest_output_wDates.csv', na.strings=c("", " ", "NA", "N/A", "null"))
clean_bigDat_wDates    <- fread('./input/clean_output_wDates.csv', na.strings=c("", " ", "NA", "N/A", "null"))

## perep fields for use (not all of these will be used in the model)
clean_TrainTest_wDates$fullbathcnt = as.integer(clean_TrainTest_wDates$fullbathcnt)  # imputed: 38%
clean_TrainTest_wDates$fips=as.factor(clean_TrainTest_wDates$fips)
clean_TrainTest_wDates$latitude = as.numeric(clean_TrainTest_wDates$latitude)
clean_TrainTest_wDates$longitude = as.numeric(clean_TrainTest_wDates$longitude)
clean_TrainTest_wDates$numberofstories = as.factor(clean_TrainTest_wDates$numberofstories)
clean_TrainTest_wDates$propertycountylandusecode = as.factor(clean_TrainTest_wDates$propertycountylandusecode)
clean_TrainTest_wDates$transactiondate = month(clean_TrainTest_wDates$transactiondate)  # Will's Month field
clean_TrainTest_wDates$numberofstories = as.numeric(clean_TrainTest_wDates$numberofstories)  # pred err w/o conversion

# make sure both files we use have the same fields the same way:
clean_bigDat_wDates$fullbathcnt = as.integer(clean_bigDat_wDates$fullbathcnt)
clean_bigDat_wDates$fips=as.factor(clean_bigDat_wDates$fips)
clean_bigDat_wDates$latitude = as.numeric(clean_bigDat_wDates$latitude)
clean_bigDat_wDates$longitude = as.numeric(clean_bigDat_wDates$longitude)
clean_bigDat_wDates$numberofstories = as.factor(clean_bigDat_wDates$numberofstories)
clean_bigDat_wDates$propertycountylandusecode = as.factor(clean_bigDat_wDates$propertycountylandusecode)
clean_bigDat_wDates$transactiondate = ifelse(is.na(clean_bigDat_wDates$transactiondate), 0,
                                             as.numeric(month(clean_bigDat_wDates$transactiondate))) 
clean_bigDat_wDates$transactiondate <- as.numeric(clean_bigDat_wDates$transactiondate)
  ### error that was why 0 replaces NA in transaction date:
  # > x_predict = predict(modelxx, newdata = clean_bigDat_wDates)
  # Error: variable 'transactiondate' was fitted with type "numeric" but type "character" was supplied

clean_bigDat_wDates$numberofstories = as.numeric(clean_bigDat_wDates$numberofstories)


set.seed(13)
## create training and test data (simple 1 fold cross validation)
## For this experiment: splitting a set 0f 90K+ records where logerror is present 
## and is what we want to predict
trainRecSetIndx = sample(1:nrow(clean_TrainTest_wDates), nrow(clean_TrainTest_wDates)*0.7)
trainSet1 = clean_TrainTest_wDates[trainRecSetIndx, ]
testSet1  = clean_TrainTest_wDates[-trainRecSetIndx, ]

## create / train the model using training data
### theory:
# have some money
# and some location
# and some other housing features
# but not too many fields so as to avoid unexpected linear death
# and avoid collinearity from like fields

# transactiondate + 

modelxx = lm(logerror ~ taxamount + calculatedbathnbr + heatingorsystemtypeid +
               roomcnt + fips + 
               poolcnt + propertylandusetypeid + regionidzip + 
               taxdelinquencyNoYrs + parcelid, data = trainSet1)

## removed: propertycountylandusecode 
## this error encountered w/ it in during predict():
# > x_predict = predict(modelxx, newdata = testSet1)
# Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
# factor propertycountylandusecode has new levels 0131, 0301, 030G, 040A, 1200

## make predictions on the test data (test set)
x_predict = predict(modelxx, newdata = testSet1)

## check absolute mean of logerror - this is metric specific to this type of test
## other checks indicated for other models and modeling processes
abs(sum(testSet1$logerror - x_predict))/nrow(testSet1)
## [1] 0.0003310404

## predict on the big data set (for this experiment: 2.9M+ records mostly with logerror missing)
x_predict = predict(modelxx, newdata = clean_bigDat_wDates)

## make copy of big data set before adding a column to it
clean_bigDat_wDates2 = clean_bigDat_wDates

## Berk Theory: in final output, we use original logerror values where avialable
## and fill in the blanks with our predictions:
clean_bigDat_wDates2$logerror_xpredict = ifelse(is.na(clean_bigDat_wDates2$logerror), x_predict, clean_bigDat_wDates2$logerror)

## make sure no NAs in our final predicted logerror column (prediction colunn)
sum(is.na(clean_bigDat_wDates2$logerror_xpredict))
nrow(clean_bigDat_wDates2)  ## check number of rows to ensure it is what we expect
## [1] 2985342  ## this is before on row number check ... 

## there were duplicates in the original dataset, this strips off dupes by estimating logerr_xpredict to the mean
## taking the average of log errors to get rid of duplicates
clean_bigDat_wDates2 <- clean_bigDat_wDates2 %>% group_by(parcelid) %>% summarise(logerror_xpredict = mean(logerror_xpredict))
nrow(clean_bigDat_wDates2) ## test number of rows is as expected
# [1] 2985217

## build output submission df
final_submission = data.frame("parcelid" = clean_bigDat_wDates2$parcelid,
                              "201610" = clean_bigDat_wDates2$logerror_xpredict,
                              "201611" = clean_bigDat_wDates2$logerror_xpredict,
                              "201612" = clean_bigDat_wDates2$logerror_xpredict,
                              "201710" = clean_bigDat_wDates2$logerror_xpredict,
                              "201711" = clean_bigDat_wDates2$logerror_xpredict,
                              "201712" = clean_bigDat_wDates2$logerror_xpredict)
colnames(final_submission) = c("parcelid",
                               "201610",
                               "201611",
                               "201612",
                               "201710",
                               "201711",
                               "201712")

## more testing
sum(is.na(final_submission))
summary(final_submission)

## write final answer for submission on Kaggle
fwrite(final_submission, 'submit_modX2.csv')

varImp(modelxx)

# > varImp(modelxx)
# Overall
# taxamount             6.3064526
# calculatedbathnbr     9.1123713
# heatingorsystemtypeid 4.2199194
# roomcnt               1.6627668
# fips6059              1.6167585
# fips6111              2.3422806
# poolcnt               5.3641208
# propertylandusetypeid 2.6653035
# regionidzip           2.5183297
# taxdelinquencyNoYrs   6.3702155
# parcelid              0.4869794

summary(modelxx)
## without parcelid, adj r-square: 0.002868 
## new summary:
# Call:
#   lm(formula = logerror ~ taxamount + calculatedbathnbr + heatingorsystemtypeid + 
#        roomcnt + fips + poolcnt + propertylandusetypeid + regionidzip + 
#        taxdelinquencyNoYrs + parcelid, data = trainSet1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.6256 -0.0378 -0.0063  0.0274  4.7305 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           -5.245e-02  4.024e-02  -1.303  0.19248    
# taxamount             -6.644e-07  1.054e-07  -6.306 2.87e-10 ***
#   calculatedbathnbr      7.057e-03  7.745e-04   9.112  < 2e-16 ***
#   heatingorsystemtypeid -9.851e-04  2.335e-04  -4.220 2.45e-05 ***
#   roomcnt                5.817e-04  3.498e-04   1.663  0.09636 .  
# fips6059               4.931e-03  3.050e-03   1.617  0.10594    
# fips6111               1.028e-02  4.390e-03   2.342  0.01917 *  
#   poolcnt               -9.162e-03  1.708e-03  -5.364 8.16e-08 ***
#   propertylandusetypeid  3.649e-04  1.369e-04   2.665  0.00769 ** 
#   regionidzip           -4.223e-07  1.677e-07  -2.518  0.01179 *  
#   taxdelinquencyNoYrs    7.135e-03  1.120e-03   6.370 1.90e-10 ***
#   parcelid               1.770e-10  3.635e-10   0.487  0.62627    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1613 on 63180 degrees of freedom
# Multiple R-squared:  0.003139,	Adjusted R-squared:  0.002965 
# F-statistic: 18.08 on 11 and 63180 DF,  p-value: < 2.2e-16
