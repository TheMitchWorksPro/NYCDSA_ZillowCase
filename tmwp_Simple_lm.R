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
               numberofstories + yearbuilt + roomcnt +
               poolcnt + propertylandusetypeid + regionidzip + lotsizesqft_imputed +
               taxdelinquencyNoYrs + fireplacecnt, data = trainSet1)

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
## [1] 0.0003484256    ## that's really low ... are we overfitting? - value before fixing numberofstories
## [1] 0.0003386735    ## still really low ... are we overfitting?  value before removing transactiondate due to issues
## [1] 0.0003305883

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
# > summary(final_submission)
# parcelid             201610              201611              201612              201710              201711         
# Min.   : 10711725   Min.   :-4.605000   Min.   :-4.605000   Min.   :-4.605000   Min.   :-4.605000   Min.   :-4.605000  
# 1st Qu.: 11643707   1st Qu.: 0.005463   1st Qu.: 0.005463   1st Qu.: 0.005463   1st Qu.: 0.005463   1st Qu.: 0.005463  
# Median : 12545094   Median : 0.011312   Median : 0.011312   Median : 0.011312   Median : 0.011312   Median : 0.011312  
# Mean   : 13325858   Mean   : 0.010840   Mean   : 0.010840   Mean   : 0.010840   Mean   : 0.010840   Mean   : 0.010840  
# 3rd Qu.: 14097122   3rd Qu.: 0.015620   3rd Qu.: 0.015620   3rd Qu.: 0.015620   3rd Qu.: 0.015620   3rd Qu.: 0.015620  
# Max.   :169601949   Max.   : 4.737000   Max.   : 4.737000   Max.   : 4.737000   Max.   : 4.737000   Max.   : 4.737000  
# 201712         
# Min.   :-4.605000  
# 1st Qu.: 0.005463  
# Median : 0.011312  
# Mean   : 0.010840  
# 3rd Qu.: 0.015620  
# Max.   : 4.737000  

## write final answer for submission on Kaggle
fwrite(final_submission, 'submit_modX.csv')

varImp(modelxx)

#                         Overall
# taxamount             6.3598558
# calculatedbathnbr     8.8549756
# heatingorsystemtypeid 3.9574524
# numberofstories       0.8579876
# yearbuilt             0.9953157
# roomcnt               2.0428750
# poolcnt               5.5254610
# propertylandusetypeid 3.7917282
# regionidzip           2.3900835
# lotsizesqft_imputed   0.5459728
# taxdelinquencyNoYrs   6.2759791
# fireplacecnt          1.3278047

summary(modelxx)

# Call:
#   lm(formula = logerror ~ taxamount + calculatedbathnbr + heatingorsystemtypeid + 
#        numberofstories + yearbuilt + roomcnt + poolcnt + propertylandusetypeid + 
#        regionidzip + lotsizesqft_imputed + taxdelinquencyNoYrs + 
#        fireplacecnt, data = trainSet1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.6229 -0.0377 -0.0061  0.0274  4.7311 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           -3.627e-02  6.004e-02  -0.604  0.54576    
# taxamount             -6.718e-07  1.056e-07  -6.360 2.03e-10 ***
#   calculatedbathnbr      7.490e-03  8.458e-04   8.855  < 2e-16 ***
#   heatingorsystemtypeid -6.259e-04  1.581e-04  -3.957 7.58e-05 ***
#   numberofstories        2.188e-03  2.550e-03   0.858  0.39090    
# yearbuilt             -3.225e-05  3.240e-05  -0.995  0.31959    
# roomcnt                7.683e-04  3.761e-04   2.043  0.04107 *  
#   poolcnt               -9.452e-03  1.711e-03  -5.525 3.30e-08 ***
#   propertylandusetypeid  5.292e-04  1.396e-04   3.792  0.00015 ***
#   regionidzip           -4.006e-07  1.676e-07  -2.390  0.01685 *  
#   lotsizesqft_imputed    2.953e-09  5.409e-09   0.546  0.58509    
# taxdelinquencyNoYrs    7.026e-03  1.119e-03   6.276 3.50e-10 ***
#   fireplacecnt           2.696e-03  2.030e-03   1.328  0.18425    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1613 on 63179 degrees of freedom
# Multiple R-squared:  0.003058,	Adjusted R-squared:  0.002868 
# F-statistic: 16.15 on 12 and 63179 DF,  p-value: < 2.2e-16

