library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(VIM)
library(mice)
library(tabplot)
library(corrplot)


#################################################################################
## GO TO THE SECTION CALLED "CODE TO BE RUN STARTS HERE" FOR MY CLEANED CODE ####
#################################################################################

train = fread('train_2016_v2.csv')
# read in the data for training

properties = fread('properties_2016.csv')
# reading in the properties data

full = properties %>% left_join(., train, by = 'parcelid')
# joining the two


str(Zillow)
# Getting a sense for the data

Zillow = full
# renaming data frame

Zillow_missing = sapply(Zillow, function(j) sum(is.na(j)))
# calculating # of missing values per column

Zillow_missing_percent = Zillow_missing/nrow(Zillow)*100
# calculating % of missing values per column 

Zillow_values = nrow(Zillow) - Zillow_missing
# calculating # of non NAs per column

Zillow_unique = sapply(Zillow, function(j) length(unique(j)))
# calculating # of unique values per column


# This is the starting point for my Data Cleaning- I am working with columns 2-13

Zillow_missing_percent[2:13]
# A fair number of my columns have high NAs.

############################################################
############################################################
############# STARTING DATA CLEANING HERE ##################
############################################################
############################################################


############# Column 2 -- airconditioningtypeid ############
# Type of cooling system present in the home (if any) #

# looking at the data dictionary, there are 13 different types of air conditioning ids. 

Zillow_unique[2]
# However, there are actually only 8 different unique values in the data set. Let's 
# visualize them. 

unique(Zillow$airconditioningtypeid)

# NA  1 3 5 9 11 12 13   --- these are the distinct values in the air-conditioning column
# Specifically- #5 in the dictionary is listed as "None", 13 is "Yes"
# Also interesting to note that there is a category for "other" which is # 6. There are 
# NO ENTRIES with this category. 

Zillow_missing_percent[2]
# 72.82% missing. This is high. It is unlikely that this high % of houses 
# do not actually have an airconditioner. 

table(Zillow$airconditioningtypeid)
# Table will give me the relative counts for each NON NA value. 


#        1      3      5      9     11     12     13 
#   742392      7   8795     19   1818     59  58462 

# What makes no sense is 58,462 "Yes". How can there be fewer "yes" 
# than "central air conditioner?"

## MY PERSONAL RECOMMENDATION(s)- I belive the "No" number is feasible. There are 2 options- 
# we can either simply go with "yes" and "no", while making the NAs yes-- 
# or we can turn the NAs into other, and leave all the different groups in. It might be worth 
# exploring whether some of these types have an impoact, especially because there are a few 
# categories with REALLY FEW numbers-- those could be interesting data points. 

############# Column 3 -- architecturalstyletypeid ############
#  Architectural style of the home (i.e. ranch, colonial, split-level, etc…)

# the data dictionary lists 27 different types of IDs.  

unique(Zillow$architecturalstyletypeid)

# However, there are only 9 different IDs actually in the data. 
# NA  7 21  8  2  3  5 10 27

# Another category for "other", with no actual values. 

Zillow_missing_percent[3]
# 99.80% missing. This is likely just going to get removed

table(Zillow$architecturalstyletypeid)
#    2    3    5    7    8   10   21   27 
#  201   58   19 5252  380    1  150    1 

# If you have a home, then your home has an architectural style. That we are missing almost 
# all of the data for this column means it will be very hard to actually use it. It would 
# be almost impossible to impute this many values and we would basically be speculating. 
# I think it is the best to completely remove this column. 
# NOTE------- there is SOME data related to this contained in the propertylandusetypeid
# column, still not enough to really impute. This data is also very likely captured by 
# other variables like lot lotsize and room counts 

############# Column 4 -- basementsqft ############
#  Finished living area below or partially below ground level # 

unique(Zillow$basementsqft)
# There are a lot of unique values here, 752 including NA. They are all integers refering to 
# the square feet for the basement. 


Zillow_missing_percent[4]
# 99.95%.......

Zillow_missing[4]
# 2983714 missing.... 

table(Zillow$basementsqft)
# does not help much. 


# Only 1628...  Seems like a small number for total basements, but without really knowing any
# better it's hard to judge whether that is accurate or not. I think the simplest thing to do here 
# is to assume that the NAs are actually residences that do not have a basement. It's 
# possible that this is overestimating the number that do not have basements, but it 
# does not do us any good if we don't have the actual value anyways. 

## EVEN WITH this assumption- it might be wise to just drop this column because it has 
# a ridiculous number of NAs. 

############# Column 5 -- bathroomcnt ############
 #  Number of bathrooms in home including fractional bathrooms #

Zillow_missing_percent[5]
# 0.38% missing. 

Zillow_missing[5]
# 11462 missing (this number comes up a lot)

table(Zillow$bathroomcnt)
# seems normal 

# Want to see what type of missigness we have here. 
count_bathroom = Zillow[is.na(Zillow$bathroomcnt), ]
sapply(count_bathroom, function(j) sum(is.na(j)))
# COME BACK TO THIS ONE 


############# Column 6 -- bedroomcnt ############
 #  Number of bedrooms in home #

Zillow_missing_percent[6]
# 0.38% missing

Zillow_missing[6]
# 11450 total missing 



############# Column 7 -- buildingclasstypeid ############
 # The building framing type (steel frame, wood frame, concrete/brick) # 

Zillow_missing_percent[7]
# missing 99.58%
Zillow_missing[7]
# 2972713

table(Zillow$buildingclasstypeid)
# Unique values of NA 1 2 3 4 5 

# 1    2    3    4    5 
# 65   81 3161 9265   57 

# We should drop this column 

############# Column 8 -- buildingqualitytypeid ############
 #  Overall assessment of condition of the building from best (lowest) to worst (highest) #

Zillow_missing_percent[8]
# missing 35.06%

Zillow_missing[8]
# missing 1046774

table(Zillow$buildingqualitytypeid)

 #     1       2       3       4       5       6       7       8       9      10      11      12 
 # 69472       2       6  692191      21      94 1133279      51      25   39716      19    3692 

# We could try to impute this on the basis of the "year built" column, which seems to have 
# very few missing values. 

Zillow_mode_quality = Zillow %>%
  group_by(yearbuilt, buildingqualitytypeid) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# grouping by yearbuilt and buildingqualitytypeid, summarising with respect to 
# n to get count and frequency. 



############# Column 9 -- calculatedbathnbr ############
 #  Number of bathrooms in home including fractional bathroom #

Zillow_missing_percent[9]
# 4.32% missing

Zillow_missing[9]
# 128918 missing

# This column is extremely similar to bathroomcnt-- from the kaggle site I found this 
# helpful description- bathroomcnt is provided by the assessor 
# and calculatedbathnbr is the number Zillow calculates and uses.

# These two columns also have extremely similar values- something needs to done here 
# and I think we should probably just drop one of them. 

table(Zillow$calculatedbathnbr)

# 1     1.5       2     2.5       3     3.5       4     4.5       5     5.5       6     6.5 
# 499343   45566 1218701  206570  629377   30671  132082   18961   38244    5731   16253    1204 

# 7     7.5       8     8.5       9     9.5      10    10.5      11    11.5      12    12.5 
# 6149     377    4537     102    1329      50     500      12     207       3     265       2 

# 13      14    14.5      15      16      17      18      19    19.5      20 
# 52      45       1      24      30       7      15       3       1      10 

############# Column 10 -- decktypeid ############
 # Type of deck (if any) present on parcel # 

Zillow_missing_percent[10]
# 99.43% missing

Zillow_missing[10]
# 2968246 missing values

table(Zillow$decktypeid)
# One single unique value. NAs are almost certainly representing people that DO NOT have a deck. 
# As the dictionary says this column is for type of deck (if any)
# I am going to convert this column to a True/False-- 0/1 column

############# Column 11 -- threequarterbathnbr ############
 #  Number of 3/4 bathrooms in house (shower + sink + toilet) # 

Zillow_missing_percent[11]
# missing 93.20% 

Zillow_missing[11]
# 2782619 missing values

table(Zillow$threequarterbathnbr)
#      1      2      3      4      5      6      7 
# 308972   2338    261     46     16      9      2 

############################################################
############################################################
############# ULTIMATE ACTION ON COLUMN ####################
############################################################
############################################################

############# Column 2 -- airconditioningtypeid ############

# Impute the missings into the "other" id

############# Column 3 -- architecturalstyletypeid ############

# Drop this column

############# Column 4 -- basementsqft ############

# Impute the NAs to mean "No Basement". No way to impute the footage. 

############# Column 5 -- bathroomcnt ############

# Impute the NAs off of calculatedbathnbr, threequarterbathnbr, and fullbathcnt

############# Column 6 -- bedroomcnt ############

# Impute the NAs using KNN, or if this is too time-intensive, using mode. 

############# Column 7 -- buildingclasstypeid ############

# Drop this column 

############# Column 8 -- buildingqualitytypeid ############

# Impute using KNN based off of the yearbuilt column

############# Column 9 -- calculatedbathnbr ############

# SEE Column 5- being dropped for a merged bathroom column

############# Column 10 -- decktypeid ############

# Impute NAs to mean "No Deck"

############# Column 11 -- threequarterbathnbr ############

# SEE Column 5- being dropped for a merged bathroom column. 

############################################################
############################################################
############# CODE TO BE RUN STARTS HERE ###################
############################################################
############################################################

library(data.table)
library(dplyr)

# make sure the name of the properties dataframe is Zillow_clean (which is done below),
# then you can simply just run the code listed below---
Zillow_clean = full
# making a copy of the data frame to preserve the original

############# Column 2 -- airconditioningtypeid ############

Zillow_clean$airconditioningtypeid[is.na(Zillow_clean$airconditioningtypeid)] = 6
# changing all of the NAs into "6" which is for "other"

Zillow_clean$airconditioningtypeid[Zillow_clean$airconditioningtypeid == 13] = 6
# changing the values that had code 13- "yes" into code 6 "other"

############# Column 3 -- architecturalstyletypeid ############

Zillow_clean = Zillow_clean %>% select(-architecturalstyletypeid)
# dropping the architecturalstyletypeid column

############# Column 4 -- basementsqft ############

Zillow_clean$basementsqft[is.na(Zillow_clean$basementsqft)] = 0
# changing all of the NAs into 0, which will stand for "No Basement"

Zillow_clean$basementsqft[Zillow_clean$basementsqft != 0] = 1
# changing all of the non-NA values to 1, which will stand for "Has Basement"

############# Column 5 -- bathroomcnt ############

Zillow_clean = Zillow_clean %>% select(-bathroomcnt)

############# Column 6 -- bedroomcnt ############

bedroomcnt = data.frame(table(Zillow_clean$bedroomcnt))
# making a table for frequency -- the mode is 3

Zillow_clean$bedroomcnt[is.na(Zillow_clean$bedroomcnt)] = 3
# assigning to the mode 

############# Column 7 -- buildingclasstypeid ############

Zillow_clean = Zillow_clean %>% select(-buildingclasstypeid)
# dropping the buildingclasstypeid column 

############# Column 8 -- buildingqualitytypeid ############

Zillow_mode_quality = Zillow_clean %>%
  group_by(yearbuilt, buildingqualitytypeid) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# grouping by yearbuilt and buildingqualitytypeid, summarising with respect to n then
# adding in n/sum(n)

Zillow_mode_quality_nas = Zillow_mode_quality %>% 
  filter(is.na(buildingqualitytypeid)) %>%
  filter(!is.na(yearbuilt)) %>% 
  filter(freq != 1)

# this code is is getting the NA count for each year but is removing years that are NA
# as well as any buildingqualitytypeid that ONLY has NAs

Zillow_mode_quality2 = Zillow_mode_quality %>% 
  filter(!is.na(buildingqualitytypeid)) %>%
  filter(!is.na(yearbuilt)) %>%
  filter(n == max(n))
Zillow_mode_quality2 = Zillow_mode_quality2[-28, ]

# this code is getting the mode buildingqualitytypeid for each year-- there was one year
# that had a tie, I went with the mode that was most similar to years around it. 

ids_to_repeat = Zillow_mode_quality2$buildingqualitytypeid[which(Zillow_mode_quality_nas$yearbuilt 
              %in% Zillow_mode_quality2$yearbuilt)]
# this code is generating the mode ids that I want to repeat

impute_quality = rep(ids_to_repeat, Zillow_mode_quality_nas$n)
# this is creating a vector of repeated mode ids to impute

Zillow_clean$buildingqualitytypeid[!Zillow_clean$yearbuilt %in% c(1807, 1808, 1821, 1823, 1825, 1831, 1874) &
        !is.na(Zillow_clean$yearbuilt) &
        is.na(Zillow_clean$buildingqualitytypeid)] = impute_quality
# assigning the ids into the Zillow dataframe

# Now I only need to deal with the cases where Year is also NA, and the very few ids
# that only had NA- the years listed above. For both of these cases, I am just going 
# to use the overall mode (from prior to the imputation)

buildingqualitytypeid = table(Zillow_clean$buildingqualitytypeid)
# mode is 7
Zillow_clean$buildingqualitytypeid[is.na(Zillow_clean$buildingqualitytypeid)] = 7

############# Column 9 -- calculatedbathnbr ############

calculatedbathnbr_table = data.frame(table(Zillow_clean$calculatedbathnbr))
# making a table for frequency -- the mode is 2

Zillow_clean$calculatedbathnbr[is.na(Zillow_clean$calculatedbathnbr)] = 2

############# Column 10 -- decktypeid ############

Zillow_clean$decktypeid[is.na(Zillow_clean$decktypeid)] = 0
# changing all of the NAs into 0, which will stand for "No Deck"

Zillow_clean$decktypeid[Zillow_clean$decktypeid == 66] = 1
# changing all of the IDs of 66 into 1, which will stand for "Has Deck"


############# Column 11 -- threequarterbathnbr ############

Zillow_clean = Zillow_clean %>% select(-threequarterbathnbr)

###################### MAKING IT FIT THE ORIGINAL FULL ###############

full = full %>% select(-architecturalstyletypeid, -bathroomcnt, -buildingclasstypeid,
                      -threequarterbathnbr)
# dropping columns

full$airconditioningtypeid = Zillow_clean$airconditioningtypeid
full$basementsqft = Zillow_clean$basementsqft
full$bedroomcnt = Zillow_clean$bedroomcnt
full$buildingqualitytypeid = Zillow_clean$buildingqualitytypeid
full$calculatedbathnbr = Zillow_clean$calculatedbathnbr
full$decktypeid = Zillow_clean$decktypeid

############################################################
############################################################
############# RANDOM UNORGANIZED STUFF #####################
############################################################
############################################################

bathrooms = Zillow[is.na(Zillow$bathroomcnt), ] %>% select(calculatedbathnbr, 
                                               threequarterbathnbr, bathroomcnt, fullbathcnt)
# slicing Zillow with the bathroom columns-- where bathroomcnt, the one with the FEWEST NA 
# is NA.

sum(!is.na(bathrooms))

aggr_Zillow = aggr(Zillow %>% 
              select(bathroomcnt, calculatedbathnbr, fullbathcnt, -buildingclasstypeid),
              varheight = T)
Zillow_bathrooms = Zillow %>% 
  select(bathroomcnt, calculatedbathnbr,
         threequarterbathnbr, fullbathcnt)
Zillow_bathrooms_cor = cor(Zillow_bathrooms, use = "pairwise.complete.obs")
corrplot(Zillow_bathrooms_cor, method="circle")


test = Zillow %>% select(yearbuilt, buildingqualitytypeid, structuretaxvaluedollarcnt, 
                         taxamount, regionidcity, calculatedfinishedsquarefeet, 
                         calculatedbathnbr, longitude, latitude, roomcnt, regionidzip, 
                         propertylandusetypeid, regionidneighborhood, bedroomcnt, logerror)
Zillow_test = cor(test, use = "pairwise.complete.obs")
corrplot(Zillow_test)


Zillow_coltypes = sapply(Zillow, class)
Zillow_numeric_col = Zillow[Zillow_coltypes == "integer"| Zillow_coltypes == "numeric"]
Zillow_numeric_col = Zillow_numeric_col %>% select(-regionidcounty)
Zillow_numeric_col
Zillow_cor_numeric = cor(Zillow_numeric_col[1:10, c(1, 8)], use = "pairwise.complete.obs")

test2 = Zillow %>% filter (!is.na(logerror))
test2_missing = sapply(test2, function(j) sum(is.na(j)))
test2_missing
colnames(test2_missing)
