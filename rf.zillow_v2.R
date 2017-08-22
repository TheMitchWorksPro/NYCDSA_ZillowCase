library(tree)
library(dplyr)
library(magrittr)
library(data.table)
library(dplyr)

full = fread('./input/clean_output.csv') #this should be whatever our imputation spits out
train_csv = fread('./input/train_2016_v2.csv') #this is the file path to zillow's train.csv

full$fullbathcnt = as.integer(full$fullbathcnt)
full$logerror =NULL
full$fips=as.factor(full$fips)
full$latitude = as.numeric(full$latitude)
full$longitude = as.numeric(full$longitude)
full$numberofstories = as.factor(full$numberofstories)
full$propertycountylandusecode = as.factor(full$propertycountylandusecode)

full.train1 = left_join(train_csv, full,  by="parcelid")

## test:
# full.train1 = full.train1[1:100, ]  # comment this out when live

full.train1$transactiondate = month(full.train1$transactiondate)

library(caret)

# Setting Parallel processing
library(doMC)
library(parallel)
number_of_cores <- detectCores()
registerDoMC()  # shu setting: cores = number_of_cores/2, help says default is half your detected cores if blank

maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

#TUNING PARAMETERS AND TRAINING MODEL################
tunegrid <- expand.grid(mtry = 1:10) #mtry can be lowered. 6 is the ideal mtry for 36ish columns. (sqrt(36 = 6))

rf_model1 <-train(logerror ~ . -propertycountylandusecode-numberofstories-parcelid, 
                  data=full.train1,
                  method="rf",
                  trControl=trainControl(method="cv", summaryFunction = maeSummary,
                                         number=10, verboseIter = TRUE ), #adjust cv down if taking too long
                  tuneGrid = tunegrid,
                  metric = c("MAE"),
                  maximize = FALSE,
                  prox=TRUE,allowParallel=TRUE,
                  verbose = T)
rf_model1$bestTune
plot(rf_model1)

########################## MAKING PREDICTIONS#########

### test mode
# full_codetest = full[1:100, ]
# full_codetest$transactiondate=as.numeric(10)
# oct = predict(rf_model1, newdata=full_codetest, type="raw")
# full_codetest$transactiondate=as.numeric(11)
# nov = predict(rf_model1, newdata=full_codetest, type="raw")
# full_codetest$transactiondate=as.numeric(12)
# dec = predict(rf_model1, newdata=full_codetest, type="raw")

### live mode
full$transactiondate=as.numeric(10)
oct = predict(rf_model1, newdata=full, type="raw")
full$transactiondate=as.numeric(11)
nov = predict(rf_model1, newdata=full, type="raw")
full$transactiondate=as.numeric(12)
dec = predict(rf_model1, newdata=full, type="raw")

### test mode
# submit <- data.frame(full_codetest[,c("parcelid")])

### live mode
submit <- data.frame(full[,c("parcelid")])

submit$"201610" <- round(oct,4)
submit$"201611" <- round(nov,4)
submit$"201612" <- round(dec,4)
submit$"201710" <- 0
submit$"201711" <- 0
submit$"201712" <- 0
nrow(submit)
write.csv(submit, file = "submit_rf_model1.csv", row.names = FALSE, na="") # export the file for submission
print('Done!')
