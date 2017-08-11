
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

# get files
properties <- fread('./input/properties_2016.csv', header=TRUE, na.strings=c("", " ", "NA", "N/A", "null"), blank.lines.skip = TRUE,
                    colClasses=list(character=50))  ## eliminates warning so we know nothing else is wrong
                    ## na.string = common values for NA that may have been used
dim(properties)

######### Planning:  Fields To Be Done ###############
# taxdelinquencyyear:
#    * if NA and flag = True (1), then year should be imputed
#    * year values are a one to two digit number that should be converted to 4 digit year ?
#    * use this as mutated field to weight the delinquency flag?  how many years delinquent?
#    * many records will be NA (not delinquent) ... those that are delinquent - neg weight to formula?

#  fips: Federal Information Processing Standard code: https://en.wikipedia.org/wiki/FIPS_county_code
#        missing: 0.3831212
# 'latitude' - same missingness %
# 'longitude' - same missingness %
#               idea:  calculate mean lat/long by cityregionID
#                      use these means to impute missing values
#                      attempt to get associated fip to nearest mean lat/long ?
##
### do mean of zip and find related lat/long and use for missing values?  what about fip?
# 'lotsizesquarefeet'
#   
# 'numberofstories' => 77.1517782
#    * related: 'storytypeid' => basement, split level, etc.
# 'poolcnt'  => 82.6634379    number pools on lot if any
# 'poolsizesum' => 99.0633847 sq ft all pools
# 'pooltypeid10' => 98.x ... spa or hottub
# 'pooltypeid2' =>  98.x ... pool w/ spa or hottub
# 'pooltypeid7' => 83.7378991 ... pool w/o hottub

########### addtional fields (not Mitch's set) #########
# rawcensustrackandblock, censustrackandblock
#  * look like different numbers
#  * guy at zillow says they are same data
#  * censutrackandblock has more data and is integer versus string

######### fields in this section #####################
# taxdelinquencyflag
# hashottuborspa
# fireplaceflag
# fireplacecnt

## Missingness Cleanup: step 1
##  * These cells, it makes sense that NA is most likely False
##  * Yes or True value conferted to 1, otherwise, 0

properties <- properties %>%  
  mutate(taxdelinquencyflag = ifelse(is.na(taxdelinquencyflag), 0, ifelse(taxdelinquencyflag =="Y", 1, taxdelinquencyflag)),
         hashottuborspa = ifelse(is.na(hashottuborspa), 0, ifelse(hashottuborspa =="true", 1, hashottuborspa)),
         fireplaceflag = ifelse(is.na(fireplaceflag), 0, ifelse(fireplaceflag =="true", 1, fireplaceflag)),
         fireplacecnt = ifelse(is.na(fireplacecnt), 0, fireplacecnt)
  )

## Missingness Cleanup: step 2
##  * if we have tax delinquency year and tax delinquency flag is 0 (NAs set to 0 previously), it should really be set to 1 for True
##  * if fireplacecount > 0, then fireplaceflag should be 1 for true
##  * conversely, if fireplacecnt is 0 and fireplaceflag is true, replace count with -1 so we can 
##                investigate for imputation

properties <- properties %>%  
  mutate(taxdelinquencyflag = ifelse(!is.na(taxdelinquencyyear) & taxdelinquencyflag==0, 1, taxdelinquencyflag),
         fireplaceflag = ifelse(fireplacecnt > 0, 1, fireplaceflag),
         fireplacecnt = ifelse(fireplaceflag==1 & fireplacecnt == 0, -1, fireplacecnt)  
         # -1 is a marker that we need to impute these values if they exist
         # none were actually found when code was run but that may change with 2017 data in October
  )

### from NYC DSA Analysis:  correct Lat/Long to proper numbers
properties <- properties %>%
  mutate(latitude = latitude/1e6, longitude = longitude/1e6)
