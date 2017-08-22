library(tree)
library(dplyr)
library(magrittr)
library(data.table)
library(dplyr)
full = fread('clean_output.csv') #this should be whatever our imputation spits out
full$logerror =NULL
train_csv = fread('input/train_2016_v2.csv') #this is the file path to zillow's train.csv
full$fullbathcnt = as.integer(full$fullbathcnt)



full$fips=as.factor(full$fips)
full$latitude = as.numeric(full$latitude)
full$longitude = as.numeric(full$longitude)
full$numberofstories = as.factor(full$numberofstories)
full$propertycountylandusecode = as.factor(full$propertycountylandusecode)

full.train1 = left_join(train_csv, full,  by="parcelid")
full.train1$transactiondate = month(full.train1$transactiondate)




maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

cv.ctrl <- trainControl(method = "cv",number = 4, 
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        summaryFunction = maeSummary,
                        verboseIter = TRUE,
                        allowParallel=T)

ntrees = 100
xgbGrid <- expand.grid( #Owen Zhang parameter tuning slides
  eta = (2:10)/ntrees, #learning rate
  max_depth = c(4,6,8,10),
  nrounds = ntrees,
  gamma = 0,               #default=0
  colsample_bytree = c(.4, .6,.8, 1),    #default=1
  min_child_weight = c(1,3,5),    #default=1
  subsample = c(.5, .75, 1)
)

xgbGridsmall <- expand.grid(
  eta = c(0.01),
  max_depth = c(3),
  nrounds = c(100),
  gamma = 0,               #default=0
  colsample_bytree = 1,    #default=1
  min_child_weight = c(1),    #default=1
  subsample = c(.6)
)


set.seed(45)
xgb_tune <-train(logerror ~ . -propertycountylandusecode-numberofstories,
                 data=small.full.train1,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgbGrid,
                 verbose=T,
                 verboseIter = T,
                 #preProcess = c("center", "scale"),
                 metric="MAE",
                 maximize = FALSE
                 #objective = 'reg:linear',
)

xgb_imp <- varImp(xgb_tune, scale = FALSE)
plot(xgb_imp, top = 10)


full$transactiondate=as.numeric(10)
oct = predict(gbmFit4, newdata=full, type="raw")
full$transactiondate=as.numeric(11)
nov = predict(gbmFit4, newdata=full, type="raw")
full$transactiondate=as.numeric(12)
dec = predict(gbmFit4, newdata=full, type="raw")



submit <- data.frame(full[,c("parcelid")])
submit$"201610" <- round(oct,4)
submit$"201611" <- round(nov,4)
submit$"201612" <- round(dec,4)
submit$"201710" <- 0
submit$"201711" <- 0
submit$"201712" <- 0
nrow(submit)
write.csv(submit, file = "submit_xgb_1.csv", row.names = FALSE, na="") # export the file for submission
print('Done!')

