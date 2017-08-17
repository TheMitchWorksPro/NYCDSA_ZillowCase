
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
library(Hmisc)  ## used in imputation

curYear = 2016   ## update to 2017 after 2017 data comes out
datDir <- "./input/"
homePropertiesFile <- "properties_2016.csv"
logErrorDataFile <- "train_2016_v2.csv"

# Import libraries
library(caret)

##### experimented with this but system kept crashing:
# Setting Parallel processing
# library(doMC)
# library(parallel)
# number_of_cores <- detectCores(logical=FALSE)
# registerDoMC(cores = number_of_cores/2)  ## further research:
# registerDoMC(cores = 2)
# http://blog.aicry.com/r-parallel-computing-in-5-minutes/

# get files
# ----------------

properties <- fread(paste0(datDir, homePropertiesFile), header=TRUE, na.strings=c("", " ", "NA", "N/A", "null"),
                    blank.lines.skip = TRUE,
                    colClasses=list(character=50))  ## eliminates warning so we know nothing else is wrong
## na.string = common values for NA that may have been used
dim(properties)

train = fread(paste0(datDir, logErrorDataFile))
full = properties %>% left_join(., train, by = 'parcelid')

# reusable functions
stats_mode <- function(x) {
  # pass in variable column and calculate the mode on it
  names(which.max(table(x)))
}

######### fields in this section #####################
#  fips: Federal Information Processing Standard code: https://en.wikipedia.org/wiki/FIPS_county_code
#        missing: 0.3831212 => note: this is less than 1 percent, not 38%
# 'latitude' - same missingness %    ## filter w/ comparisons of fips, lat, long shows they are all missing on same rows
# 'longitude' - same missingness %   ## see startercode1 for these tests (filter w/ one na and one not produced 0 rows)
# hashottuborspa
# fireplaceflag
# fireplacecnt
# taxdelinquencyflag
# taxdelinquencyyear => interim field: taxdelinquencyyear4d => replace w/: taxdelinquencyNoYrs
# numberofstories => 77.1517782 (missing b4 cleanup)
#    * related: 'storytypeid' => basement, split level, etc.
# 'lotsizesquarefeet': 9.2488754 missingness b4 imputation => replace w/: full$lotsizesqft_imputed
# 'poolcnt'  => 82.6634379    number pools on lot if any

## Missingness Cleanup: step 1
##  * These cells, it makes sense that NA is most likely False
##  * Yes or True value conferted to 1, otherwise, 0

full <- full %>%  
  mutate(taxdelinquencyflag = ifelse(is.na(taxdelinquencyflag), 0, ifelse(taxdelinquencyflag =="Y", 1, taxdelinquencyflag)),
         hashottuborspa = ifelse(is.na(hashottuborspa), 0, ifelse(hashottuborspa =="true", 1, hashottuborspa)),
         fireplaceflag = ifelse(is.na(fireplaceflag), 0, ifelse(fireplaceflag =="true", 1, fireplaceflag)),
         fireplacecnt = ifelse(is.na(fireplacecnt), 0, fireplacecnt),
         poolcnt = ifelse(is.na(poolcnt), 0, poolcnt)
  )

## Missingness Cleanup: step 2
##  * if we have tax delinquency year and tax delinquency flag is 0 (NAs set to 0 previously), it should really be set to 1 for True
##  * if fireplacecount > 0, then fireplaceflag should be 1 for true
##  * conversely, if fireplacecnt is 0 and fireplaceflag is true, replace count with -1 so we can 
##                investigate for imputation
##  * most homes do not have a pool - without good data on % homes with/without pool for the 3 target counties
##    it makes sense to just infer that NA is probably usually 0

full <- full %>%  
  mutate(taxdelinquencyflag = ifelse(!is.na(taxdelinquencyyear) & taxdelinquencyflag==0, 1, taxdelinquencyflag),
         fireplaceflag = ifelse(fireplacecnt > 0, 1, fireplaceflag),
         fireplacecnt = ifelse(fireplaceflag==1 & fireplacecnt == 0, -1, fireplacecnt)  
         # -1 is a marker indicating that we need to impute these values if they exist
         # none were actually found when code was run but that may change with 2017 data in October
  )

# taxdelinquencyyear:  created as taxdelinquencyyear4d (for 4 digit year)
#    * if NA and flag = True (1), then year should be imputed, otherwise NA is appropriate
#    * year values were a one to two digit number converted to 4 digit year here
#    * to make this more useful ... NA is imputed as 0, then ...
#    * a "years delinquent" field is created.  2016-0

full <- full %>% mutate(taxdelinquencyyear4d = ifelse(is.na(taxdelinquencyyear), 0,
                                                      ifelse(taxdelinquencyyear < 17, taxdelinquencyyear+2000, 
                                                             taxdelinquencyyear+1900)))

# future planning: uncomment and run this check on 2017 data when available
# if any rows returned, then look at imputing year for combo: flag = 1, year is missing
# full %>% select(taxdelinquencyflag, taxdelinquencyyear, taxdelinquencyyear4d) %>% 
#   filter(is.na(taxdelinquencyyear) & taxdelinquencyflag==1)  ## verified w/ other combination
                                                               ## nothing to return             

## new field: calculate number of years delinquent:
## -----------------
## if 0, house is not in delinquency
## If 1 or more, house is delinquent and this is the x year of delinquincy
## example: 2016 will come out as 1 for first year even though first year is not complete
##          this one field should now reflect full delinquency state
##          update variable: curYear to 2017 after new data comes out

full <- full %>%  
  mutate(taxdelinquencyNoYrs = ifelse(taxdelinquencyyear4d == 0, 0, curYear - taxdelinquencyyear4d + 1))

## Missingness Cleanup: step 3
#  Fields in this section are all different, see comments with each one

full <- full %>%
  mutate(latitude = latitude/1e6, longitude = longitude/1e6)   ## from NYC DSA Analysis:  correct Lat/Long to proper numbers

mode1 = stats_mode(full$numberofstories)

full <- full %>%  
  mutate(numberofstories = ifelse(is.na(numberofstories), mode1, numberofstories),
         fips = ifelse(is.na(fips), mode1, fips),
         latitude = ifelse(is.na(latitude), mode1, latitude),
         longitude = ifelse(is.na(longitude), mode1, longitude)         
  )  ## impute with mode ... all homes have at least 1 floor (in this case mode is 1 also)
     ## note: this mode in our data cannot be generalized outside the 3 counties we were given
     ##       this article suggests it is a local bias:  http://www.redwagonteam.com/single-story-homes-sale/

     ## impute with mode for these fields since we are affecting less than 1% of data anyway:
     #  fips: Federal Information Processing Standard code: https://en.wikipedia.org/wiki/FIPS_county_code
     #        missing: 0.3831212 => note: this is less than 1 percent, not 38%
     # 'latitude' - same missingness %    ## filter w/ comparisons of fips, lat, long shows they are all missing on same rows
     # 'longitude' - same missingness %   ## see startercode1 for these tests (filter w/ one na and one not produced 0 rows)
     #               idea:  calculate mean lat/long by cityregionID
     #                      use these means to impute missing values
     #                      attempt to get associated fip to nearest mean lat/long ?


## in 2016 data, see research file showing that properties with floors greater than 5 appear to be dirty data
#  this rule may not apply to the 2017 data and will either need validation or to be commented out on future data

full <- full %>%  
  mutate(numberofstories = ifelse(as.integer(numberofstories) > 5, mode1, numberofstories)
  )

set.seed(0)
full$lotsizesqft_imputed <- Hmisc::impute(full$lotsizesquarefeet, "random")
  ## lotsize relates to value/price of property, so mean might skew the data
  ## suggests random imputation might be good choice here ...
  ## use new lotsizesqft_imputed field instead of lotsizesquarefeet
  ## not addressed yet: lotsize logically must be smaller than sq ft of the home
  ##     query: lotsize < calculated total sq ft of home => 26000 hits
  ##            how to fix this data?


### fields for removal:
# 'poolsizesum' => 99.0633847 sq ft all pools
# 'pooltypeid10' => 98.x ... spa or hottub
# 'pooltypeid2' =>  98.x ... pool w/ spa or hottub
# 'pooltypeid7' => 83.7378991 ... pool w/o hottub'
#  - checked by query: no useful info to help impute other fields and redundant with other pool field

full$poolsizesum <- NULL
full$pooltypeid10 <- NULL
full$pooltypeid2 <- NULL
full$pooltypeid7 <- NULL
full$lotsizesquarefeet <- NULL    # field replaced
full$taxdelinquencyyear4d <- NULL # interim field
taxdelinquencyyear <- NULL # field replaced

### Write the data to a file
## testing: write what we've done so far to a file
fwrite(full, 'full_2016_step2.csv')


