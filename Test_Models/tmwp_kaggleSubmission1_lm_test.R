

set.seed(0)
## create training and test data (simple 1 fold cross validation)
## For this experiment: splitting a set 0f 90K+ records where logerror is present 
## and is what we want to predict
trainRecSetIndx = sample(1:nrow(clean_TrainTest_wDates), nrow(clean_TrainTest_wDates)*0.8)
trainSet1 = clean_TrainTest_wDates[trainRecSetIndx, ]
testSet1  = clean_TrainTest_wDates[-trainRecSetIndx, ]

## create / train the model using training data
modelx = lm(logerror ~ taxamount +
              calculatedbathnbr +
              yearbuilt, data = trainSet1)

## make predictions on the test data (test set)
x_predict = predict(modelx, newdata = testSet1)

## check absolute mean of logerror - this is metric specific to this type of test
## other checks indicated for other models and modeling processes
abs(sum(testSet1$logerror - x_predict))/nrow(testSet1)

## predict on the big data set (for this experiment: 2.9M+ records mostly with logerror missing)
x_predict = predict(modelx, newdata = clean_bigDat_wDates)

## make copy of big data set before adding a column to it
clean_bigDat_wDates2 = clean_bigDat_wDates

## Berk Theory: in final output, we use original logerror values where avialable
## and fill in the blanks with our predictions:
clean_bigDat_wDates2$logerror_xpredict = ifelse(is.na(clean_bigDat_wDates2$logerror), x_predict, clean_bigDat_wDates2$logerror)

## make sure no NAs in our final predicted logerror column (prediction colunn)
sum(is.na(clean_bigDat_wDates2$logerror_xpredict))
nrow(clean_bigDat_wDates2)  ## check number of rows to ensure it is what we expect

## there were duplicates in the original dataset, this strips off dupes by estimating logerr_xpredict to the mean
## taking the average of log errors to get rid of duplicates
clean_bigDat_wDates2 <- clean_bigDat_wDates2 %>% group_by(parcelid) %>% summarise(logerror_xpredict = mean(logerror_xpredict))
nrow(clean_bigDat_wDates2) ## test number of rows is as expected

## build output submission df
mitch_submission = data.frame("parcelid" = clean_bigDat_wDates2$parcelid,
                              "201610" = clean_bigDat_wDates2$logerror_xpredict,
                              "201611" = clean_bigDat_wDates2$logerror_xpredict,
                              "201612" = clean_bigDat_wDates2$logerror_xpredict,
                              "201710" = clean_bigDat_wDates2$logerror_xpredict,
                              "201711" = clean_bigDat_wDates2$logerror_xpredict,
                              "201712" = clean_bigDat_wDates2$logerror_xpredict)
colnames(mitch_submission) = c("parcelid",
                               "201610",
                               "201611",
                               "201612",
                               "201710",
                               "201711",
                               "201712")

## more testing
sum(is.na(mitch_submission))
summary(mitch_submission)

## write final answer for submission on Kaggle
fwrite(mitch_submission, 'submit_5.csv')

varImp(modelx)
summary(modelx)

# > varImp(modelx)
# Overall
# taxamount         6.690131
# calculatedbathnbr 8.203144
# yearbuilt         1.056955
# > summary(modelx)
# 
# Call:
#   lm(formula = logerror ~ taxamount + calculatedbathnbr + yearbuilt, 
#      data = trainSet1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.6200 -0.0374 -0.0063  0.0273  4.7353 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -5.513e-02  5.303e-02  -1.040    0.298    
# taxamount         -6.866e-07  1.026e-07  -6.690 2.25e-11 ***
#   calculatedbathnbr  6.211e-03  7.571e-04   8.203 2.38e-16 ***
#   yearbuilt          2.877e-05  2.722e-05   1.057    0.291    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1612 on 72216 degrees of freedom
# Multiple R-squared:  0.001283,	Adjusted R-squared:  0.001241 
# F-statistic: 30.92 on 3 and 72216 DF,  p-value: < 2.2e-16




