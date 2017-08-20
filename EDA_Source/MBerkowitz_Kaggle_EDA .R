# MBerkowitz EDA

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(lubridate)
library(stringi)
library(Hmisc)
library(car)

# loading in packages. 

#### BELOW ARE NOTES ON HOW TO CLEAN THINGS-- from 8/16

# FIX THE multiple parcel ids by group_by in train before the join


# full$fips=as.factor(full$fips)
# full$latitude = as.numeric(full$latitude)
# full$longitude = as.numeric(full$longitude)
# full$numberofstories = as.factor(full$numberofstories)
# full$propertycountylandusecode = as.factor(full$propertycountylandusecode)
# 
# 
# but propertycountylandusecod sucks and should be avoided
# make fullbathcnt numeric
# heatingorsystemtupe ud- make factor with levels- 2, or NOT 2-- central or not central

## NOTES- USING censustractandblock
## NOTES- fireplaceflag NOT FOUND????

## mode stories is okay
## mode fips - you changed on slack 
## mode long and lat- random sample

#####################################################################
######################### BEGIN EDA #################################
#####################################################################
#####################################################################


train = full[!is.na(full$logerror), ]
test = full[is.na(full$logerror), ]
# splitting out merged data frame into test and train
p = sample(1:nrow(train), 7*nrow(train)/10)
# making a sample
sub_train = train[p, ]
# sub train is now the training portion of our train set
sub_test = train[-p, ]
# sub test is now the test portion of our train set

#### WORK FLOW ###
# Step 1: create algorithms on sub_train. Find the ones that perform best on this
# Step 2: run those algorithms on sub_test and see how it performs
# Step 3: run this algorithm on train to get a value for the log error for 6 time points
# Step 4: Submit to Kaggle and try to win :) 

#### COLUMN NAMES FOR MY REFERENCE 
# [1] "parcelid"                     "airconditioningtypeid"        "basementsqft"                
# [4] "bedroomcnt"                   "buildingqualitytypeid"        "calculatedbathnbr"           
# [7] "decktypeid"                   "calculatedfinishedsquarefeet" "fips"                        
# [10] "fireplacecnt"                 "fullbathcnt"                  "garagecarcnt"                
# [13] "garagetotalsqft"              "hashottuborspa"               "heatingorsystemtypeid"       
# [16] "latitude"                     "longitude"                    "poolcnt"                     
# [19] "propertycountylandusecode"    "propertylandusetypeid"        "regionidcity"                
# [22] "regionidcounty"               "regionidzip"                  "roomcnt"                     
# [25] "unitcnt"                      "yardbuildingsqft17"           "yardbuildingsqft26"          
# [28] "yearbuilt"                    "numberofstories"              "fireplaceflag"               
# [31] "taxamount"                    "taxdelinquencyflag"           "censustractandblock"         
# [34] "logerror"                     "transactiondate"              "lotsizesqft_imputed"    

model1 = lm(logerror ~ taxamount +
              calculatedbathnbr + 
              yearbuilt, data = sub_test)

# simple model
summary(model1)
# looking at the summary

plot(model1)
# a lot of violations in assumptions

model2 = lm(logerror ~ 
              calculatedfinishedsquarefeet +
              calculatedbathnbr + 
              airconditioningtypeid +
              buildingqualitytypeid +
              garagetotalsqft + 
              yearbuilt + 
              fireplacecnt + 
              unitcnt + 
              regionidcity +
              regionidzip + 
              regionidcounty +
              airconditioningtypeid +
              bedroomcnt + 
              lotsizesqft_imputed +
              poolcnt +
              decktypeid +
              numberofstories +
              basementsqft +
              yardbuildingsqft17 + 
              yardbuildingsqft26 +
              garagecarcnt + 
              taxdelinquencyflag +
              heatingorsystemtypeid +
              fullbathcnt +
              taxamount,
              data = sub_test)

# this is where all of my tinkering was done. A lot of models were created 
# here but I only included the final one 
summary(model2)
# looking at the summary 

plot(model2)
# some issues here with the assumptions (QQ plot is s shaped)


# RANDOM STUFF 

logerror = as.data.frame(table(train$logerror))
colnames(logerror) = c("logerror", "freq")
logerror2 = logerror %>% arrange(desc(freq))

logerror_positive = train$logerror[train$logerror > 0]
logerror_negative = train$logerror[train$logerror < 0]
sd(logerror_positive)
sd(logerror_negative)
summary(logerror_positive)
summary(logerror_negative)
ggplot(data = data.frame(logerror_positive), aes(x = logerror_positive)) + geom_histogram(bins = 50)
ggplot(data = data.frame(logerror_negative), aes(x = logerror_negative)) + geom_histogram(bins = 50)
ggplot(data = data.frame(logerror_positive), aes(x = logerror_positive)) + geom_density()
# looking at the positive vs negative logerror

x = train %>% select(calculatedfinishedsquarefeet, 
                           calculatedbathnbr, 
                           airconditioningtypeid, 
                           buildingqualitytypeid, 
                           garagetotalsqft, 
                           yearbuilt, 
                           fireplacecnt, 
                           unitcnt, 
                           regionidcity, 
                           regionidzip, 
                           regionidcounty, 
                           airconditioningtypeid, 
                           bedroomcnt, 
                           lotsizesqft_imputed, 
                           poolcnt, 
                           decktypeid, 
                           numberofstories, 
                           basementsqft, 
                           yardbuildingsqft17,  
                           yardbuildingsqft26, 
                           garagecarcnt, 
                           taxdelinquencyflag, 
                           heatingorsystemtypeid, 
                           fullbathcnt, 
                           taxamount, 
                           transactiondate,
                           logerror)
x = x %>% select(-heatingorsystemtypeid, -fullbathcnt)
x$numberofstories = as.numeric(x$numberofstories)
x_whole = cor(x)
x_cor = corrplot(x_whole, method="shade",shade.col=NA, tl.col="black", tl.srt=45)
x_pos = x[x$logerror > 0, ]
x_neg = x[x$logerror < 0, ]
x2_pos <- cor(x_pos,  use = "pairwise.complete.obs")
x2_neg<- cor(x_neg,  use = "pairwise.complete.obs")
x_cor_pos = corrplot(x2_pos, method="shade",shade.col=NA, tl.col="black", tl.srt=45)
x_cor_neg = corrplot(x2_neg, method="shade",shade.col=NA, tl.col="black", tl.srt=45)
# correlations

y = x[abs(x$logerror) > 3, ]
corrplot(cor(y), method="shade",shade.col=NA, tl.col="black", tl.srt=45)

z = sub_train
z$calculatedfinishedsquarefeet = log(z$calculatedfinishedsquarefeet)



########################### EXPLORING NEW THINGS ###############

full2 = properties %>% left_join(., train, by = 'parcelid')

m_train = full2[!is.na(full2$logerror), ]

missing_cols = apply(m_train, 1, function(x) sum(is.na(x)))
# finding the number of missing columns per parcel id 

m_train$missing_cols = missing_cols
# adding a new column for missing rows


ggplot(data = m_train, aes(x = missing_cols, y = logerror)) + geom_point()
# looks strange

missing_combinations = m_train %>% group_by(missing_cols) %>% 
  summarise(number = n(), mean_abs = mean(abs(logerror)), sd = sd(logerror))
# getting the number of missing combinations

missing_combinations$percent_missing = missing_combinations$number/sum(missing_combinations$number)*100
# adding in percentages 


ggplot(data = missing_combinations, aes(x = missing_cols, y = number)) + geom_col()
# plotting missing combinations

ggplot(data = missing_combinations, aes(x = missing_cols, y = mean_abs)) + geom_col()
# plotting # missing columns against mean absolute logerror

ggplot(data = missing_combinations, aes(x = missing_cols, y = sd)) + geom_col()
# # missing columns against sd--- see an upward trend here


binarize_train = data.frame(sapply(m_train %>% select(-logerror, -parcelid), function(x) ifelse(is.na(x), 0, 1)))
binarize_train$logerror = m_train$logerror
binarize_train$missing_rows = missing_cols
binarize_train$parcelid = m_train$parcelid
# binarizing the data frame


