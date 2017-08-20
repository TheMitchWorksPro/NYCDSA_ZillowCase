library(data.table)
library(dplyr)
full = fread('clean_output.csv')
full$fullbathcnt = as.integer(full$fullbathcnt)



full$fips=as.factor(full$fips)
full$latitude = as.numeric(full$latitude)
full$longitude = as.numeric(full$longitude)
full$numberofstories = as.factor(full$numberofstories)
full$propertycountylandusecode = as.factor(full$propertycountylandusecode)


#stupid multi regression)
lr1 <- lm(logerror ~ calculatedfinishedsquarefeet + yearbuilt, data=full);
summary(lr1)


full.train = full %>% filter(., !is.na(logerror))


#attempt at random forest
library(randomForest)
set.seed(0)
train = sample(1:nrow(full.train), 7*nrow(full.train)/10)
rf.zillow= randomForest(logerror ~ bedroomcnt+decktypeid +fireplacecnt +garagetotalsqft +
                          latitude, 
                        data = full.train, subset = train, 
                        importance = TRUE, 
                        na.action=na.exclude,
                        ntree = 2)


rf.zillow= randomForest(logerror ~ . - propertycountylandusecode, 
                        data = full.train, subset = train, 
                        importance = TRUE, 
                        na.action=na.exclude,
                        ntree = 2)


#THIS WILL TAKE A LONG TIME IF THE FOR LOOP IS SET FROM 1:36 
oob.err = numeric(36)
for (mtry in 1:1) {
  fit = randomForest(logerror ~ . - propertycountylandusecode, data = full.train, mtry = mtry, ntree=50)
  oob.err[mtry] = fit$mse[50]
  cat(oob.err, '\n')
  cat("We're performing iteration", mtry, "\n")
}



#This makes predicitions
print('Making predictions ...')
predictions <- data.frame(predict(lr1, full))
print('Appending predictions to full ...')
full$p_lr1 <- predictions$predict.lr1..full.
full$p_lr1[is.na(full$p_lr1)] <- mean(full$logerror, na.rm = TRUE)  # Replace missing with average
print('Average prediction value is ...')
mean(full$p_lr1)

# Create submission file
print('Creating submission file')
submit <- data.frame(full[,c("parcelid", "p_lr1")])
submit$"201610" <- round(submit$p_lr1,4)
submit$"201611" <- round(submit$p_lr1,4)
submit$"201612" <- round(submit$p_lr1,4)
submit$"201710" <- round(submit$p_lr1,4)
submit$"201711" <- round(submit$p_lr1,4)
submit$"201712" <- round(submit$p_lr1,4)
submit$p_lr1<- NULL # remove the original prediction from the submit file
nrow(submit)
write.csv(submit, file = "submit_5.csv", row.names = FALSE, na="") # export the file for submission
print('Done!')
