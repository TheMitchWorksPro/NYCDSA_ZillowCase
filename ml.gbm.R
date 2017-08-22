library(tree)
library(dplyr)
library(magrittr)
library(data.table)
library(dplyr)
library(caret)
full = fread('clean_output.csv')
full$logerror =NULL
train_csv = fread('input/train_2016_v2.csv')
full$fullbathcnt = as.integer(full$fullbathcnt)



full$fips=as.factor(full$fips)
full$latitude = as.numeric(full$latitude)
full$longitude = as.numeric(full$longitude)
full$numberofstories = as.factor(full$numberofstories)
full$propertycountylandusecode = as.factor(full$propertycountylandusecode)

full.train1 = left_join(train_csv, full,  by="parcelid")
full.train1$transactiondate = month(full.train1$transactiondate)

trainIndex <- createDataPartition(full.train1$logerror, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)
## training set
subTrain <- full.train1[ trainIndex,-1]
## testing set
subTest  <- full.train1[-trainIndex,-1]


##################################
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

gridSearch <- trainControl(method = "cv",
                           number = 3,
                           summaryFunction = maeSummary,
                           verboseIter = TRUE)

gbmGrid <-  expand.grid(interaction.depth = c(11,15,19), 
                      n.trees = (11:20)*50, 
                        shrinkage = c(.001),
                        n.minobsinnode = c(5,10))
set.seed(0)
gbmFit4 <- train(logerror ~ . - propertycountylandusecode,
                 data = full.train1, 
                 method = "gbm", 
                 preProcess = c("center", "scale"),
                 metric = "MAE",
                 maximize = FALSE,
                 tuneGrid = gbmGrid,
                 trControl = gridSearch,
                 verbose = FALSE)

fwrite(gbmFit4$results, "gbmFit4.csv")
gbmImp4 <- varImp(gbmFit4, scale = FALSE)
plot(gbmImp4, top = 10)


full$transactiondate=NULL
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
write.csv(submit, file = "submit_7.csv", row.names = FALSE, na="") # export the file for submission
print('Done!')






######### next part is my own code

caretGrid <- expand.grid(interaction.depth=c(1, 3, 5,7), n.trees = (0:30)*100,
                         shrinkage=c(0.01, 0.001),
                         n.minobsinnode=10)

metric <- "RMSE"
trainControl <- trainControl(method="cv", number=4)

set.seed(99)
gbm.caret <- train(logerror ~ . -propertycountylandusecode-numberofstories, data=full.train1, distribution="gaussian", method="gbm",
                   trControl=trainControl, verbose=FALSE, 
                   tuneGrid=caretGrid, metric=metric, bag.fraction=0.5)  







trainControl.2 <- trainControl(method="cv", number=4, summaryFunction = maeSummary,
                             search = "random")
gbm.caret.2 <- train(logerror ~ . , data=full.train1, distribution="gaussian", method="gbm",
                   trControl=trainControl, verbose=FALSE, 
                   tuneGrid=caretGrid, metric="MAE", bag.fraction=0.5) 

gbm.caret$bestTune
gbmImp2 <- varImp(gbm.caret, scale = FALSE) #IMPORTANCE
plot(gbmImp2, top = 10)

dumb.validation = full.train1[0:100, ]
caret.predict <- predict(gbm.caret$bestTune, newdata=dumb.validation, type="raw")

sum(abs(caret.predict - dumb.validation$logerror)) / nrow(dumb.validation)







#possibly throw out
lr1 <- lm(logerror ~ calculatedfinishedsquarefeet, data=full.train);
summary(lr1)

##################### skip to next hashes



sample <- data.table::fread('../input/sample_submission.csv', header=T, na.strings="NA")


# Need to find a better way to handle the NA values.
x_train <- left_join(train, full, by=c("parcelid","transactiondate"))

x_train$transactiondate=as.factor(x_train$transactiondate)
View(head(x_train))

gbmModel <- gbm(logerror ~ .
                -(propertycountylandusecode + censustractandblock + propertyzoningdesc +
                    buildingclasstypeid + decktypeid + poolcnt + pooltypeid10 + pooltypeid2 +
                    pooltypeid7 + storytypeid + assessmentyear),
                distribution="gaussian", interaction.depth=3, n.cores=detectCores()/2, n.trees = 1000, 
                shrinkage = 0.001, data = x_train)


#train
full.train1 = left_join(full.train, train_2016[-2], by="parcelid") #full.train has all columns - 90150

table(month(full.train1$transactiondate))
full.train1$transactiondate = month(full.train1$transactiondate)

#THIS ONE WORKS :)
gbmModel2 <- gbm(logerror ~ .,
                 distribution="gaussian", interaction.depth=2, n.cores=detectCores()/2, n.trees = 1000, 
                 shrinkage = 0.001, data = full.train1,  bag.fraction=0.75, cv.folds=3)
gbm.perf(gbmModel2, method = "cv")
