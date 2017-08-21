## source file:  ml.zillow

library(data.table)
library(dplyr)
full = fread('clean_output.csv')


## data field conversions ahead of these models:
### convert type to avoid modeling issues
full$fullbathcnt = as.integer(full$fullbathcnt)
full$latitude = as.numeric(full$latitude)
full$longitude = as.numeric(full$longitude)

### keep as factors for this test
full$fips=as.factor(full$fips)
full$numberofstories = as.factor(full$numberofstories)
full$propertycountylandusecode = as.factor(full$propertycountylandusecode)

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
for (mtry in 1:1) {  #1:1 to test ... 1:oob.err
  fit = randomForest(logerror ~ . - propertycountylandusecode, data = full.train, mtry = mtry, ntree=50)
  oob.err[mtry] = fit$mse[50]
  cat(oob.err, '\n')
  cat("We're performing iteration", mtry, "\n")
}

print("36 iterations Complete! ... Now what?")


##### intitial run using obb.err which was set to 36 is shown here ...
##### based on error ... resetting to 1 and trying again immediately after this
## ==============================
# > ## source file:  ml.zillow
#   > 
#   > library(data.table)
# 
# > library(dplyr)
# 
# > full = fread('clean_output.csv')
# Read 2985217 rows and 36 (of 36) columns from 0.399 GB file in 00:00:04
# 
# > ## data field conversions ahead of these models:
#   > ### convert type to avoid modeling issues
#   > full$fullbathcnt = as.integer(full$fullbathcnt)
# 
# > full$latitude = as.numeric(full$latitude)
# 
# > full$longitude = as.numeric(full$longitude)
# 
# > ### keep as factors for this test
#   > full$fips=as.factor(full$fips)
# 
# > full$numberofstories = as.factor(full$numberofstories)
# 
# > full$propertycountylandusecode = as.factor(full$propertycountylandusecode)
# 
# > full.train = full %>% filter(., !is.na(logerror))
# 
# > #attempt at random forest
#   > library(randomForest)
# 
# > set.seed(0)
# 
# > train = sample(1:nrow(full.train), 7*nrow(full.train)/10)
# 
# > rf.zillow= randomForest(logerror ~ bedroomcnt+decktypeid +fireplacecnt +garagetotalsqft +
#                             +                           latitude, 
#                           +                   .... [TRUNCATED] 
#                           
#                           > rf.zillow= randomForest(logerror ~ . - propertycountylandusecode, 
#                                                     +                         data = full.train, subset = train, 
#                                                     +                   .... [TRUNCATED] 
#                                                     
#                                                     > #THIS WILL TAKE A LONG TIME IF THE FOR LOOP IS SET FROM 1:36 
#                                                       > oob.err = numeric(36)
#                                                     
#                                                     > for (mtry in 1:oob.err) {  #1:1 to test
#                                                       +   fit = randomForest(logerror ~ . - propertycountylandusecode, data = full.train, mtry = mtry, ntree=50)
#                                                       + .... [TRUNCATED] 
#                                                       0.02582922 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
#                                                       We're performing iteration 1 
#                                                       0.02582922 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
#                                                       We're performing iteration 0 
#                                                       
#                                                       > print("36 iterations Complete! ... Now what?")
#                                                       [1] "36 iterations Complete! ... Now what?"
#                                                       Warning messages:
#                                                         1: In 1:oob.err :
#                                                         numerical expression has 36 elements: only the first used
#                                                       2: In randomForest.default(m, y, ...) :
#                                                         invalid mtry: reset to within valid range


#### run two with mtry set to 1 ##############
# 
# > ## source file:  ml.zillow
#   > 
#   > library(data.table)
# 
# > library(dplyr)
# 
# > full = fread('clean_output.csv')
# Read 2985217 rows and 36 (of 36) columns from 0.399 GB file in 00:00:04
# 
# > ## data field conversions ahead of these models:
#   > ### convert type to avoid modeling issues
#   > full$fullbathcnt = as.integer(full$fullbathcnt)
# 
# > full$latitude = as.numeric(full$latitude)
# 
# > full$longitude = as.numeric(full$longitude)
# 
# > ### keep as factors for this test
#   > full$fips=as.factor(full$fips)
# 
# > full$numberofstories = as.factor(full$numberofstories)
# 
# > full$propertycountylandusecode = as.factor(full$propertycountylandusecode)
# 
# > full.train = full %>% filter(., !is.na(logerror))
# 
# > #attempt at random forest
#   > library(randomForest)
# 
# > set.seed(0)
# 
# > train = sample(1:nrow(full.train), 7*nrow(full.train)/10)
# 
# > rf.zillow= randomForest(logerror ~ bedroomcnt+decktypeid +fireplacecnt +garagetotalsqft +
#                             +                           latitude, 
#                           +                   .... [TRUNCATED] 
#                           
#                           > rf.zillow= randomForest(logerror ~ . - propertycountylandusecode, 
#                                                     +                         data = full.train, subset = train, 
#                                                     +                   .... [TRUNCATED] 
#                                                     
#                                                     > #THIS WILL TAKE A LONG TIME IF THE FOR LOOP IS SET FROM 1:36 
#                                                       > oob.err = numeric(36)
#                                                     
#                                                     > for (mtry in 1:1) {  #1:1 to test ... 1:oob.err
#                                                       +   fit = randomForest(logerror ~ . - propertycountylandusecode, data = full.train, mtry = mtry, ntr .... [TRUNCATED] 
#                                                                              0.02582922 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
#                                                                              We're performing iteration 1 
#                                                                              
#                                                                              > print("36 iterations Complete! ... Now what?")
#                                                                              [1] "36 iterations Complete! ... Now what?"
#                                                                              
#                                                                              > ##### intitial run using obb.err which was set to 36 is shown here ...
#                                                                              > ##### based on error ... resetting to 1 and trying again immediately after  .... [TRUNCATED]
