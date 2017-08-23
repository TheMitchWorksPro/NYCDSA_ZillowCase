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
library(mvoutlier)

# loading in packages. 

#### BELOW ARE NOTES ON HOW TO CLEAN THINGS-- from 8/16

# FIX THE multiple parcel ids by group_by in train before the join


full$fips=as.factor(full$fips)
full$latitude = as.numeric(full$latitude)
full$longitude = as.numeric(full$longitude)
full$numberofstories = as.factor(full$numberofstories)
full$propertycountylandusecode = as.factor(full$propertycountylandusecode)

full$lotsizesqft_imputed = as.numeric(full$lotsizesqft_imputed)
full$yearbuilt = as.numeric(full$yearbuilt)
full$fullbathcnt = as.numeric(full$fullbathcnt)
full$roomcnt = as.numeric(full$roomcnt)
full$regionidcounty = as.numeric(full$regionidcounty)
full$propertylandusetypeid = as.numeric(full$propertylandusetypeid)
full$heatingorsystemtypeid = as.numeric(full$heatingorsystemtypeid)
full$unitcnt = as.numeric(full$unitcnt)
full$regionidzip = as.numeric(full$regionidzip)
full$regionidcity = as.numeric(full$regionidcity)
full$calculatedfinishedsquarefeet = as.numeric(full$calculatedfinishedsquarefeet)
full$fips = as.numeric(full$fips)
full$numberofstories = as.numeric(full$numberofstories)
full$propertycountylandusecode = as.numeric(full$propertycountylandusecode)
full = full %>% select(-transactiondate)

# 
# 
# but propertycountylandusecode sucks and should be avoided
# make fullbathcnt numeric
# heatingorsystemtupe ud- make factor with levels- 2, or NOT 2-- central or not central



#####################################################################
######################### BEGIN EDA #################################
#####################################################################
#####################################################################

set.seed(1)
train = full[!is.na(full$logerror), ]
test = full[is.na(full$logerror), ]
# train$missing_cols = missing_cols$missing_cols
# splitting out merged data frame into test and train
train$fullbathcnt = as.numeric(train$fullbathcnt)
train$lotsizesqft_imputed = as.numeric(train$lotsizesqft_imputed)
train$yearbuilt = as.numeric(train$yearbuilt)
p = sample(1:nrow(train), 8*nrow(train)/10)
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
              yearbuilt, data = sub_train)

# simple model
summary(model1)
# looking at the summary

plot(model1)
# a lot of violations in assumptions

model1_prediction = predict.lm(model1, newdata = sub_test)


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
              data = sub_train)

# this is where all of my tinkering was done. A lot of models were created 
# here but I only included the final one.
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


# Let's examine some statistics about the logerror

summary(train$logerror)

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -4.60500 -0.02530  0.00600  0.01146  0.03920  4.73700 

# Notice the median is substantially different than the mean. Will explore this in a moment

lowerquant = quantile(train$logerror)[2]
# lower quartile
upperquant = quantile(train$logerror)[4]
# upper quartile
IQR = upperquant - lowerquant
# Inner Quartile Range

mild_upper = (IQR * 1.5) + upperquant
# threshold for outlier above
mild_lower = lowerquant - (IQR * 1.5)
# threshold for outlier below

outlier_eda = train %>% filter(logerror > mild_upper | logerror < mild_lower)
outlier_eda = outlier_eda %>% select(-transactiondate)
outlier_eda$fullbathcnt = as.numeric(outlier_eda$fullbathcnt)
outlier_eda$numberofstories = as.numeric(outlier_eda$numberofstories)
outlier_eda = outlier_eda %>% select(-heatingorsystemtypeid)
outlier_eda = outlier_eda %>% select(-propertycountylandusecode)
outlier_eda$fips = as.numeric(outlier_eda$fips)
outlier_eda = outlier_eda %>% select(-censustractandblock)

outlier_cor = corrplot(cor(outlier_eda))
# correlation matrix

outlier_cor[, 31]

# only the logerror column 
# Really not much here at all- but this does not mean there is not any relationship, 
# only that there is not a linear relationship!! 

ggplot(data = train, aes(x = as.numeric(calculatedfinishedsquarefeet), y = logerror)) + geom_point()
summary(train$calculatedfinishedsquarefeet)
# quick look at calculated finished square feet 


lowerquant_abs = quantile(abs(train$logerror))[2]
# lower quartile
upperquant_abs = quantile(abs(train$logerror))[4]
# upper quartile
IQR_abs = upperquant_abs - lowerquant_abs
# Inner Quartile Range

mild_upper_abs = (IQR_abs * 1.5) + upperquant_abs
# threshold for outlier above
mild_lower_abs = lowerquant_abs - (IQR_abs * 1.5)
# threshold for outlier below
outlier_abs_eda = train %>% filter(abs(logerror) > mild_upper_abs | abs(logerror) < mild_lower_abs)

outlier_abs_eda = outlier_abs_eda %>% select(-transactiondate)
outlier_abs_eda$fullbathcnt = as.numeric(outlier_abs_eda$fullbathcnt)
outlier_abs_eda$numberofstories = as.numeric(outlier_abs_eda$numberofstories)
outlier_abs_eda = outlier_abs_eda %>% select(-heatingorsystemtypeid)
outlier_abs_eda = outlier_abs_eda %>% select(-propertycountylandusecode)
outlier_abs_eda$fips = as.numeric(outlier_abs_eda$fips)
outlier_abs_eda = outlier_abs_eda %>% select(-censustractandblock)

outlier_abs_cor = corrplot(cor(outlier_abs_eda))

outlier_abs_cor[, 31]


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
                           missing_cols,
                           logerror)
x = x %>% select(-heatingorsystemtypeid, -fullbathcnt, -transactiondate)
x$numberofstories = as.numeric(x$numberofstories)
x_pos = x[x$logerror > 0, ]
x_neg = x[x$logerror < 0, ]
x$logerror = abs(x$logerror)
x_whole = cor(x)
x_cor = corrplot(x_whole, method="shade",shade.col=NA, tl.col="black", tl.srt=45)
x2_pos <- cor(x_pos,  use = "pairwise.complete.obs")
x2_neg<- cor(x_neg,  use = "pairwise.complete.obs")
x_cor_pos = corrplot(x2_pos, method="shade",shade.col=NA, tl.col="black", tl.srt=45)
x_cor_neg = corrplot(x2_neg, method="shade",shade.col=NA, tl.col="black", tl.srt=45)
# correlations

y = x[abs(x$logerror) > 3, ]
corrplot(cor(y), method="shade",shade.col=NA, tl.col="black", tl.srt=45)

## Let's explore the one that kept coming up- year built

year = train %>% group_by(yearbuilt) %>% summarise(abs_log = mean(abs(logerror)))

year_plot = ggplot(data = year, aes(x = yearbuilt, y = abs_log)) + geom_line() + 
  xlab("Year Built") + ylab("Absolute Value of Log Error")

year_model = loess(abs_log ~ yearbuilt, data = year)
summary(year_model)


# let's look at the same thing except now without absolute value

year2 = train %>% group_by(yearbuilt) %>% summarise(mean_log = mean(logerror))

year_plot = ggplot(data = year2, aes(x = yearbuilt, y = mean_log)) + geom_point() +
  geom_smooth(method = "loess")

loess(logerror ~ yearbuilt + taxamount, data = sub_train)
# RSE of 0.1615 

loess(logerror ~ yearbuilt + calculatedbathnbr, data = sub_train)
# RSE of 0.1618 

loess(logerror ~ calculatedbathnbr + taxamount, data = sub_train)
# RSE of 0.1615 

### checking the loess regression on positive and negative logerrors separately

year_pos = sub_train %>% filter(logerror > 0)
year_neg = sub_train %>% filter(logerror <= 0)

# making separate data frame for positive and negative log errors

loess(logerror ~ yearbuilt, data = year_pos)
# RSE of 0.1577 -- not much better 
loess(logerror ~ yearbuilt, data = year_neg)
# RSE of 0.1296 -- decently better

# retrying positive including 0 now
year_pos = sub_train %>% filter(logerror >= 0)

loess(logerror ~ yearbuilt, data = year_pos)
#barely an improvement. Stick with original


# adding back in tax amount
year_pos = sub_train %>% filter(logerror > 0)
# resetting pos to not include 0

loess(logerror ~ yearbuilt + taxamount, data = year_pos)
# RSE of 0.1579 -- WORSE
loess(logerror ~ yearbuilt + taxamount, data = year_neg)
# RSE of 0.1296 -- ALSO WORSE

### want to look at scatterplots for data 

plot_vars = train %>% select(calculatedfinishedsquarefeet, 
                             calculatedbathnbr, 
                             logerror)

plot_vars = train %>% select(yearbuilt, 
                             taxamount, 
                             logerror)

plot(plot_vars)

###### MAKING TEST SCATTERPLOTS

# taxamount

scatter_frame = train %>% group_by(taxamount) %>% summarise(mean_log = mean(logerror))
scatter_frame_abs = train %>% group_by(taxamount) %>% summarise(abs_log = mean(abs(logerror)))                                                            

ggplot(data = scatter_frame, aes(x = taxamount, y = mean_log)) + geom_point()
# much less accurate as tax amount is really small 
ggplot(data = scatter_frame_abs, aes(x = taxamount, y = abs_log)) + geom_point()
# much less accurate as tax amount is really small 


# yearbuilt

scatter_frame = train %>% group_by(yearbuilt) %>% summarise(mean_log = mean(logerror))
scatter_frame_abs = train %>% group_by(yearbuilt) %>% summarise(abs_log = mean(abs(logerror)))                                                            

ggplot(data = scatter_frame, aes(x = yearbuilt, y = mean_log)) + geom_point()
# much less accurate as year built is a lot older 
ggplot(data = scatter_frame_abs, aes(x = yearbuilt, y = abs_log)) + geom_point()
# much less accurate as year built is a lot older 

# calculatedbathnbr

scatter_frame = train %>% group_by(calculatedbathnbr) %>% summarise(mean_log = mean(logerror))
scatter_frame_abs = train %>% group_by(calculatedbathnbr) %>% summarise(abs_log = mean(abs(logerror)))                                                            

ggplot(data = scatter_frame, aes(x = calculatedbathnbr, y = mean_log)) + geom_point()
# much less accurate as year built is a lot older 
ggplot(data = scatter_frame_abs, aes(x = calculatedbathnbr, y = abs_log)) + geom_point()
# much less accurate as year built is a lot older 

#### IDEA ### calculate mean for a few grouped categories and average those

sub_train3 = sub_train
sub_test2 = sub_test

tax_loess = loess(logerror ~ taxamount, data = sub_train3)
tax_predict = predict(tax_loess, newdata = sub_test)

yearbuilt_loess = loess(logerror ~ yearbuilt, data = sub_train3)
yearbuilt_predict = predict(yearbuilt_loess, newdata = sub_test)

bathnbr_loess = loess(logerror ~ calculatedbathnbr, data = sub_train3)
bathnbr_predict = predict(bathnbr_loess, newdata = sub_test)

Average_loess_prediction = (tax_predict + yearbuilt_predict + bathnbr_predict)/3
# getting 2 NAs for some reason? Ruins the analysis

########################### EXPLORING NEW THINGS ###############
train2 = fread('train_2016_v2.csv')
full2 = properties %>% left_join(., train2, by = 'parcelid')

m_train = full2[!is.na(full2$logerror), ]

missing_cols = data.frame(apply(m_train, 1, function(x) sum(is.na(x))))
# finding the number of missing columns per parcel id 

colnames(missing_cols) = "missing_cols"

m_train$missing_cols = missing_cols$missing_cols
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

ggplot(data = missing_combinations, aes(x = missing_cols, y = mean_abs)) + geom_col() +
  xlab("Missing Columns Per Parcel ID") + ylab("Mean- Absolute Value of Log Error")
# plotting # missing columns against mean absolute logerror

missing_combinations_onepercent = missing_combinations %>% filter(number >= 1000)
# looking only at combinations with 1000 or more observations

ggplot(data = missing_combinations_onepercent, aes(x = missing_cols, y = mean_abs)) + geom_line()

ggplot(data = missing_combinations, aes(x = missing_cols, y = sd)) + geom_col()
# # missing columns against sd--- see an upward trend here



taxcut = cut(train$taxamount, breaks = c())

ggplot(data =)


binarize_train = data.frame(sapply(m_train %>% select(-logerror, -parcelid), function(x) ifelse(is.na(x), 0, 1)))
binarize_train$logerror = m_train$logerror
binarize_train$missing_rows = missing_cols
binarize_train$parcelid = m_train$parcelid
# binarizing the data frame

tax = train %>% filter(taxamount < 1000 | taxamount > 10000)
ggplot(data = tax, aes(x = taxamount, y = logerror)) + geom_point()
# looking at low and high taxes and effect on logerror

extreme_error = train %>% filter(abs(logerror) > 1)
extreme_error_cor = extreme_error %>% select(calculatedfinishedsquarefeet, 
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
                     taxamount, 
                     logerror)
extreme_error_cor$numberofstories = as.numeric(extreme_error_cor$numberofstories)
extreme_error_cor = extreme_error_cor %>% select(-basementsqft, -yardbuildingsqft17, -yardbuildingsqft26, -taxdelinquencyflag)
corrplot(cor(extreme_error_cor))





# FRIDAY NIGHT- IDEA- TAX AMOUNT IS WHAT IS USED BY ZILLOW TO CALCULATE THE PRICES 
# PREDICT TAX AMOUNT--- THEN LOOK AT THE OBSERVATIONS THAT HAVE A WAY OFF TAX AMOUNT

tax_model = lm(taxamount ~ calculatedbathnbr+ 
               calculatedfinishedsquarefeet+ 
               bedroomcnt+
               yearbuilt+
               garagetotalsqft+
               lotsizesqft_imputed,
              data = sub_train)
summary(tax_model)


bc = boxCox(tax_model) #Automatically plots a 95% confidence interval for the lambda
#value that maximizes the likelihhood of transforming to
#normality.

lambda = bc$x[which(bc$y == max(bc$y))] #Extracting the best lambda value.

tax.bc = (sub_train$taxamount^lambda - 1)/lambda #Applying the Box-Cox transformation.

tax_model.bc = lm(tax.bc ~ calculatedbathnbr+ 
                 calculatedfinishedsquarefeet+ 
                 bedroomcnt+
                 yearbuilt+
                 garagetotalsqft+
                 lotsizesqft_imputed,
               data = sub_train)
summary(tax_model.bc)


plot(tax_model.bc) #Assessing the assumptions of the new model.


properties2 = properties %>% filter(!is.na(landtaxvaluedollarcnt))
properties2 = properties2 %>% filter(!is.na(taxamount))
properties2 = properties2 %>% filter(!is.na(taxvaluedollarcnt))
properties2 = properties2 %>% filter(!is.na(structuretaxvaluedollarcnt))
properties2 = properties2 %>% select(landtaxvaluedollarcnt, taxamount, taxvaluedollarcnt, 
                                     structuretaxvaluedollarcnt)

tax_cor = cor(properties2)
corrplot(tax_cor, method="shade",shade.col=NA, tl.col="black", tl.srt=45)

tax_model_properties = lm(taxamount ~ landtaxvaluedollarcnt+ 
                            taxvaluedollarcnt,
                          data = properties)
summary(tax_model_properties)

#TESTING UNI PLOT

sub_train2 = sub_train %>% select(taxamount, 
                                  calculatedbathnbr, 
                                  calculatedfinishedsquarefeet,
                                  longitude,
                                  latitude,
                                  bedroomcnt,
                                  yearbuilt)


check_outliers = uni.plot(sub_train2, symb = T)
# creating uni plot 

outlier_train = sub_train[which(check_outliers$outliers), ]
mean(abs(outlier_train$logerror))

nrow(outlier_train %>% filter(abs(logerror) >= 1))/nrow(outlier_train)

nrow(sub_train %>% filter(abs(logerror) >= 1))

not_IQR = sub_train %>% filter(abs(logerror) > 0.06823 | abs(logerror) < -0.02530)
sum(not_IQR$parcelid %in% outlier_train$parcelid)

sub_train$fips=as.factor(sub_train$fips)
sub_train$latitude = as.numeric(sub_train$latitude)
sub_train$longitude = as.numeric(sub_train$longitude)
sub_train$numberofstories = as.numeric(sub_train$numberofstories)
sub_train$propertycountylandusecode = as.factor(sub_train$propertycountylandusecode)
sub_train$fullbathcnt = as.numeric(sub_train$fullbathcnt)
sub_train$heatingorsystemtypeid = as.numeric(sub_train$heatingorsystemtypeid)
sub_train = sub_train %>% select(-transactiondate, -propertycountylandusecode, 
                                 -heatingorsystemtypeid, -fips)

sub_train_mal = sub_train %>% select(-logerror, -parcelid)
sub_train_mal = sub_train %>% select(basementsqft, 
                                     bedroomcnt, 
                                     buildingqualitytypeid, 
                                     calculatedbathnbr, 
                                     calculatedfinishedsquarefeet, 
                                     garagetotalsqft, 
                                     poolcnt, 
                                     yardbuildingsqft26, 
                                     yearbuilt, 
                                     numberofstories, 
                                     taxamount, 
                                     lotsizesqft_imputed)
mal_dist = mahalanobis(sub_train_mal, colMeans(sub_train_mal), cov(sub_train_mal))

k_test = sub_train %>% select(calculatedbathnbr, taxamount, yearbuilt, logerror)
meanstest = kmeans(k_test, 5, nstart = 20)
logcluster = as.factor(meanstest$cluster)
ggplot(k_test, aes(calculatedbathnbr, logerror, color = logcluster)) + geom_point()
ggplot(k_test, aes(taxamount, logerror, color = logcluster)) + geom_point()
ggplot(k_test, aes(yearbuilt, logerror, color = logcluster)) + geom_point()





# running model now without the points from outlier_train

not_outlier_train = sub_train[-(which(check_outliers$outliers)), ]

modelx = lm(logerror ~ taxamount +
                       calculatedbathnbr + 
                       yearbuilt, data = not_outlier_train)



summary(modelx)
summary(model1)

x_predict = predict(modelx, newdata = sub_test)





sum(abs(sub_test$logerror - x_predict))/nrow(sub_test)



# let's try submitting our x_predict model!!

x_predict = predict(modelx, newdata = full)

full2 = full

full2$logerror_xpredict = ifelse(is.na(full2$logerror), x_predict, full2$logerror)

full2 = full2 %>% group_by(parcelid) %>% summarise(logerror_xpredict = mean(logerror_xpredict))



submission_x_predict = data.frame("parcelid" = full2$parcelid, 
                                  "201610" = full2$logerror_xpredict, 
                                  "201611" = full2$logerror_xpredict, 
                                  "201612" = full2$logerror_xpredict, 
                                  "201710" = full2$logerror_xpredict, 
                                  "201711" = full2$logerror_xpredict, 
                                  "201712" = full2$logerror_xpredict)
colnames(submission_x_predict) = c("parcelid", 
                                   "201610", 
                                   "201611", 
                                   "201612", 
                                   "201710",
                                   "201711", 
                                   "201712")

fwrite(submission_x_predict, "submission_x_predict.csv")


# Now trying this to get it in the same exact order as the properties

submission_x_predict3 = left_join(properties, full2, by = "parcelid")
submission_x_predict3 = submission_x_predict3 %>% select(parcelid, logerror_xpredict)
submission_x_predict3$V2 = submission_x_predict3$logerror_xpredict
submission_x_predict3$V3 = submission_x_predict3$logerror_xpredict
submission_x_predict3$V4 = submission_x_predict3$logerror_xpredict
submission_x_predict3$V5 = submission_x_predict3$logerror_xpredict
submission_x_predict3$V6 = submission_x_predict3$logerror_xpredict
colnames(submission_x_predict3) = c("parcelid", 
                                   "201610", 
                                   "201611", 
                                   "201612", 
                                   "201710",
                                   "201711", 
                                   "201712")

fwrite(submission_x_predict3, "submission_x_predict3.csv")


#### MODEL 1 AND MODEL 2 CHECKING MAE 
model1 = lm(logerror ~ taxamount +
              calculatedbathnbr + 
              yearbuilt, data = sub_train)


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
              as.numeric(fullbathcnt) +
              taxamount,
            data = sub_train)

m1_predict = predict(model1, newdata = sub_test)
m2_predict = predict(model2, newdata = sub_test)
sum(abs(sub_test$logerror - m1_predict))/nrow(sub_test)
sum(abs(sub_test$logerror - m2_predict))/nrow(sub_test)



# YEO JOHNSON TIME BABY!!!! 
PP = "YeoJohnson"
PP2 = c("scale", "YeoJohnson")
set.seed(5)
p = sample(1:nrow(train), 8*nrow(train)/10)
# making a sample
sub_train = train[p, ]
# sub train is now the training portion of our train set
sub_test = train[-p, ]



model_yeo = train(logerror ~ yearbuilt + calculatedfinishedsquarefeet,
                  method ='lm', data = sub_train,
                  preProcess = "YeoJohnson", trControl = myControl)
# Linear model with Yeo-Johnson
yeo1 = predict(model_yeo, newdata = sub_test)
# Predicting on test data
sum(abs(yeo_attempt1-sub_test$logerror))/nrow(sub_test)
# 0.06674093 MAE


model_yeo2 = train(logerror ~ yearbuilt + calculatedbathnbr + calculatedfinishedsquarefeet,
                   method = "lm", data = sub_train, preProcess = PP, trControl = myControl)

yeo_attempt2 = predict(model_yeo2, newdata = sub_test)
sum(abs(yeo_attempt2-sub_test$logerror))/nrow(sub_test)

model_yeo3 = train(logerror ~ taxamount + fullbathcnt,
                   method = "lm", data = sub_train, preProcess = PP, trControl = myControl)
yeo_attempt3 = predict(model_yeo3, newdata = sub_test)
sum(abs(yeo_attempt3-sub_test$logerror))/nrow(sub_test)

yeo_predict = predict(model_yeo3, newdata = full)

full3 = full

full3$logerror_yeo = ifelse(is.na(full3$logerror), yeo_predict, full3$logerror)

full3 = full3 %>% group_by(parcelid) %>% summarise(logerror_yeo = mean(logerror_yeo))

sample_submission = fread("submission_x_predict.csv", header = T)
sample_submission[, 2:7] = 0
submission_y = sample_submission
submission_y[, 2:7] = full3$logerror_yeo

fwrite(submission_y, "submission_y_predict.csv")



model_yeo4 = train(logerror ~ calculatedbathnbr + yearbuilt + calculatedfinishedsquarefeet +
                    missing_cols, method = "lm", data = sub_train, preProcess = PP, trControl = myControl) 
yeo_attempt4 = predict(model_yeo4, newdata = sub_test)
sum(abs(yeo_attempt4-sub_test$logerror))/nrow(sub_test)

yeo_predict2 = predict(model_yeo4, newdata = full)

full4 = full
full4$logerror_yeo = ifelse(is.na(full4$logerror), yeo_predict2, full4$logerror)
full4 = full4 %>% group_by(parcelid) %>% summarise(logerror_yeo = mean(logerror_yeo))
submission_y2 = sample_submission
submission_y2[, 2:7] = full4$logerror_yeo

fwrite(submission_y2, "submission_y_predict2.csv")


model2_predict = predict(model2, newdata = sub_test)
sum(abs(model2_predict-sub_test$logerror))/nrow(sub_test)




# load libraries
library(mlbench)
library(caret)
# load the dataset
# calculate the pre-process parameters from the dataset
yeo_johnson = preProcess(sub_train %>% select(logerror), method=c("YeoJohnson"))

 # boxcox = preProcess(cars %>% select(dist),
             #       method = "BoxCox")

# load libraries
library(mlbench)
library(caret)
folds = 5
repeats = 5

myControl = trainControl(method = "repeatedcv",
                          # search = “random”,
                          number = folds,
                          repeats = repeats,
                          # summaryFunction = maeSummary
                          returnResamp = "final",
                          returnData = FALSE,
                          savePredictions = "final",
                          verboseIter = TRUE,
                          allowParallel = T)


# Ridge Regression
library(glmnet)
set.seed(1)
grid = 10^seq(5, -2, length = 100)
sub_train = sub_train %>% select(-propertycountylandusecode)
sub_test = sub_test %>% select(-propertycountylandusecode)
sub_train = sub_train %>% select(-transactiondate)
sub_test = sub_test %>% select(-transactiondate)
sub_train = sub_train %>% select(-missing_cols)
sub_test = sub_test %>% select(-missing_cols)
sub_train$propertylandusetypeid = as.numeric(sub_train$propertylandusetypeid)
sub_test$propertylandusetypeid = as.numeric(sub_test$propertylandusetypeid)
sub_train$heatingorsystemtypeid = as.numeric(sub_train$heatingorsystemtypeid)
sub_test$heatingorsystemtypeid = as.numeric(sub_test$heatingorsystemtypeid)
sub_train$unitcnt = as.numeric(sub_train$unitcnt)
sub_test$unitcnt = as.numeric(sub_test$unitcnt)
sub_train$regionidzip = as.numeric(sub_train$regionidzip)
sub_test$regionidzip = as.numeric(sub_test$regionidzip)
sub_train$regionidcity = as.numeric(sub_train$regionidcity)
sub_test$regionidcity = as.numeric(sub_test$regionidcity)
sub_train$calculatedfinishedsquarefeet = as.numeric(sub_train$calculatedfinishedsquarefeet)
sub_test$calculatedfinishedsquarefeet = as.numeric(sub_test$calculatedfinishedsquarefeet)
sub_train$regionidcounty = as.numeric(sub_train$regionidcounty)
sub_test$regionidcounty = as.numeric(sub_test$regionidcounty)
sub_train$fips = as.numeric(sub_train$fips)
sub_test$fips = as.numeric(sub_test$fips)
sub_train$numberofstories = as.numeric(sub_train$numberofstories)
sub_test$numberofstories = as.numeric(sub_test$numberofstories)
sub_train = sub_train %>% select(-censustractandblock)
sub_test = sub_test %>% select(-censustractandblock)

grid2 = 10^seq(0.5, -3, length = 100)
x_rln = model.matrix(logerror ~ ., sub_train)[, -1] #Dropping the intercept column.
y_rln = sub_train$logerror
x_test = model.matrix(logerror ~ ., sub_test)[, -1] #Dropping the intercept column.
y_test = sub_test$logerror
ridge_train = glmnet(x_rln, y_rln, alpha = 0, lambda = grid)
cv.ridge_train = cv.glmnet(x_rln, y_rln,
                         lambda = grid, alpha = 0, nfolds = 10)
bestlambda.ridge = cv.ridge_train$lambda.min

ridge_best_train = predict.cv.glmnet(cv.ridge_train, s ="lambda.min", newx = x_test)

sum(abs(ridge_best_train-sub_test$logerror))/nrow(sub_test)
  
full_ridge = full %>% select(-propertycountylandusecode, -transactiondate)
full_ridge$missing_cols = missing_cols
full_ridge$lotsizesqft_imputed = as.numeric(full_ridge$lotsizesqft_imputed)
full_ridge$yearbuilt = as.numeric(full_ridge$yearbuilt)
full_ridge$fullbathcnt = as.numeric(full_ridge$fullbathcnt)
full_ridge$roomcnt = as.numeric(full_ridge$roomcnt)
full_ridge$regionidcounty = as.numeric(full_ridge$regionidcounty)
full_ridge$propertylandusetypeid = as.numeric(full_ridge$propertylandusetypeid)
full_ridge$heatingorsystemtypeid = as.numeric(full_ridge$heatingorsystemtypeid)
full_ridge$unitcnt = as.numeric(full_ridge$unitcnt)
full_ridge$regionidzip = as.numeric(full_ridge$regionidzip)
full_ridge$regionidcity = as.numeric(full_ridge$regionidcity)
full_ridge$calculatedfinishedsquarefeet = as.numeric(full_ridge$calculatedfinishedsquarefeet)
full_ridge$fips = as.numeric(full_ridge$fips)
full_ridge$numberofstories = as.numeric(full_ridge$numberofstories)
full_ridge = full_ridge %>% select(-censustractandblock)
x_ridge = model.matrix(logerror ~ ., full_ridge)[, -1]

ridge_best_train_final = predict.cv.glmnet(cv.ridge_train, s ="lambda.min", newx = x_ridge)
ridge_best_train_final = as.data.frame(ridge_best_train_final)

full_ridge2 = full
full_ridge2$logerror_ridge = ridge_best_train_final$`1`
full_ridge2$logerror_ridge_final = ifelse(is.na(full_ridge2$logerror), full_ridge2$logerror_ridge, full_ridge2$logerror)
full_ridge2 = full_ridge2 %>% group_by(parcelid) %>% summarise(logerror_ridge = mean(logerror_ridge))

submission_ridge = sample_submission
submission_ridge[, 2:7] = full_ridge2$logerror_ridge

fwrite(submission_ridge, "submission_r_predict.csv")


# LASSO REGRESSION
x_lasso = x_ridge
cv.lasso_train = cv.glmnet(x_rln, y_rln,
                           lambda = grid, alpha = 1, nfolds = 10)
lasso_best_train = predict.cv.glmnet(cv.lasso_train, s ="lambda.min", newx = x_lasso)
lasso_best_train = as.data.frame(lasso_best_train)

full_lasso = full
full_lasso$logerror_lasso = lasso_best_train$`1`
full_lasso$logerror_lasso_final = ifelse(is.na(full_lasso$logerror), full_lasso$logerror_lasso, full_lasso$logerror)
full_lasso = full_lasso %>% group_by(parcelid) %>% summarise(logerror_lasso_final = mean(logerror_lasso_final))

submission_lasso = sample_submission
submission_lasso[, 2:7] = full_lasso$logerror_lasso_final

fwrite(submission_lasso, "submission_l_predict.csv")

# ELASTIC NET CROSS-VALIDATION

### Alternative method with caret
library(caret)
tune.grid = expand.grid(lambda = grid, alpha = (1:10) * 0.1)
net.caret = train(x_rln, y_rln,
                    method = 'glmnet',
                    trControl = netControl, tuneGrid = tune.grid)

netControl = trainControl(method = "cv",
                         # search = “random”,
                         number = 10,
                         returnResamp = "final",
                         summaryFunction = maeSummary,
                         returnData = FALSE,
                         savePredictions = "final",
                         verboseIter = TRUE,
                         allowParallel = T)


# gives me an optimal alpha of 0.1

# PART 2-- ACTUAL REGRESSION

x_net = x_ridge
cv.net_train = cv.glmnet(x_rln, y_rln,
                           lambda = grid, alpha = 0.1, nfolds = 10)
net_best_train = predict.cv.glmnet(cv.net_train, s ="lambda.min", newx = x_net)
net_best_train = as.data.frame(net_best_train)

full_net = full
full_net$logerror_net = net_best_train$`1`
full_net$logerror_net_final = ifelse(is.na(full_net$logerror), full_net$logerror_net, full_net$logerror)
full_net = full_net %>% group_by(parcelid) %>% summarise(logerror_net_final = mean(logerror_net_final))

submission_net = sample_submission
submission_net[, 2:7] = full_net$logerror_net_final

fwrite(submission_net, "submission_n_predict.csv")



## RANDOM FOREST
library(tree)
library(ISLR)
library(randomForest)

maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

p = sample(1:nrow(train), 8*nrow(train)/10)
#TUNING PARAMETERS AND TRAINING MODEL################
set.seed(1)
tunegrid = expand.grid(mtry = 1:6) #mtry can be lowered. 6 is the ideal mtry for 36ish columns. (sqrt(36 = 6))
p2 = sample(1:nrow(sub_train), 1*nrow(sub_train)/10)
sub_train_forrest = sub_train[p2, ]
rf_model1 = train(logerror ~ . -numberofstories - parcelid, 
                  data = sub_train_forrest,
                  method="rf",
                  trControl=trainControl(method="cv", summaryFunction = maeSummary,
                                         number=10, verboseIter = TRUE ), #adjust cv down if taking too long
                  tuneGrid = tunegrid,
                  metric = c("MAE"),
                  maximize = FALSE,
                  prox=TRUE,allowParallel=TRUE,
                  verbose = T)
rf_model2 = train(logerror ~ ., 
                  data = sub_train_forrest,
                  method="rf",
                  trControl=trainControl(method="cv", summaryFunction = maeSummary,
                                         number=5, verboseIter = TRUE ), #adjust cv down if taking too long
                  tuneGrid = tunegrid,
                  metric = c("MAE"),
                  maximize = FALSE,
                  prox=TRUE,allowParallel=TRUE,
                  verbose = T)
rf_model1$bestTune
plot(rf_model1)
full_forest = full_ridge
full_forest = full_forest %>% select(-transactiondate)
full_forest = full_forest %>% select(-propertycountylandusecode)
full_forest$logerror[is.na(full_forest$logerror)] = 0
full_forest = full_forest %>% group_by(parcelid) %>% summarise(logerror = mean(logerror))
forest_predict = predict(rf_model1, newdata = full)
sum(abs(forest_predict-sub_test$logerror))/nrow(sub_test)

forest_predict2 = predict(rf_model2, newdata = sub_test)
sum(abs(forest_predict2-sub_test$logerror))/nrow(sub_test)

forest_predict3 = predict(rf_model2, newdata = full_forest)
forest_predict4 = data.frame(forest_predict3)


full_forest$logerror_forest = forest_predict4$forest_predict3
full_forest$logerror = full$logerror

full_forest$logerror_forest_final = ifelse(is.na(full_forest$logerror), full_forest$logerror_forest, full_forest$logerror)
full_forest = full_forest %>% group_by(parcelid) %>% summarise(logerror_forest_final = mean(logerror_forest_final))

submission_forest = sample_submission
submission_forest[, 2:7] = full_forest$logerror_forest_final

fwrite(submission_forest, "submission_rf_predict.csv")



full_yeo = full
yeo_predict = predict(model_yeo, newdata = full)
full_yeo$logerror_yeo = yeo_predict
full_yeo = full_yeo %>% group_by(parcelid) %>% summarise(logerror_yeo = mean(logerror_yeo))



submission_yeo = sample_submission
submission_yeo[, 2:7] = full_yeo$logerror_yeo

fwrite(submission_yeo, "submission_yj_predict.csv")

model_stupid = train(logerror ~ yearbuilt + calculatedfinishedsquarefeet,
                  method ='lm', data = sub_train, trControl = myControl)
stupid_attempt1 = predict(model_stupid, newdata = sub_test)
sum(abs(stupid_attempt1-sub_test$logerror))/nrow(sub_test)

full_stupid = full 
stupid_predict = predict(model_stupid, newdata = full)
full_stupid$logerror_stupid = stupid_predict
full_stupid$logerror_stupid_final = ifelse(is.na(full_stupid$logerror), full_stupid$logerror_stupid, full_stupid$logerror)
full_stupid = full_stupid %>% group_by(parcelid) %>% summarise(logerror_stupid_final = mean(logerror_stupid_final))

submission_stupid = sample_submission
submission_stupid[, 2:7] = full_stupid$logerror_stupid_final

fwrite(submission_stupid, "submission_st_predict.csv")

#YEO RIDGE

tune.grid = expand.grid(lambda = grid2, alpha=c(0))
yeo.ridge = train(logerror ~., 
                  data = sub_train,
                  method = 'glmnet',
                  preProcess = PP,
                  metric = c("MAE"),
                  maximize = FALSE,
                  trControl = yeoControl, tuneGrid = tune.grid)

yeoControl = trainControl(method = "cv",
                          # search = “random”,
                          number = 10,
                          returnResamp = "final",
                          summaryFunction = maeSummary,
                          returnData = FALSE,
                          savePredictions = "final",
                          verboseIter = TRUE,
                          allowParallel = T)
full_yeo = full_yeo %>% select(-transactiondate)
yeo_ridge_predict = predict(yeo.ridge, newdata = full_yeo)
yeo_ridge_predict = data.frame(yeo_ridge_predict)

full_ridge$logerror_yeoridge = yeo_ridge_predict$yeo_ridge_predict
full_ridge$logerror_yeoridge = ifelse(is.na(full$logerror), full_ridge$logerror_yeoridge, full_ridge$logerror)
full_ridge = full_ridge %>% group_by(parcelid) %>% summarise(logerror_yeoridge = mean(logerror_yeoridge))

submission_yeoridge = sample_submission
submission_yeoridge[, 2:7] = full_ridge$logerror_yeoridge

fwrite(submission_yeoridge, "submission_yeor_predict.csv")



loess_dumb = loess(logerror ~ yearbuilt + calculatedfinishedsquarefeet, data = sub_train)
loess_dumb_predict = predict(loess_dumb, newdata = sub_test)
sum(abs(loess_dumb_predict-sub_test$logerror))/nrow(sub_test)

loess_really_dumb_predict = predict(loess_dumb, newdata = full)
loess_really_dumb_predict[is.na(loess_really_dumb_predict)] = 0.0139
full_loess = full
full_loess$logerror_loess = loess_really_dumb_predict
full_loess = full_loess %>% group_by(parcelid) %>% summarise(logerror_loess = mean(logerror_loess))
submission_loess = sample_submission
submission_loess[, 2:7] = full_loess$logerror_loess

fwrite(submission_loess, "submission_lo.csv")
