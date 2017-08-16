########################################
# Reusable Functions: Team Zillow Case
########################################

## note: these functions are copied into the zillow kaggle project code when used
##       to ensure no issues with setup
## in examples:  full = dataframe, numberofstories = a variable in the DF
## examples of how to use each function are commented out after each function

stats_mode <- function(x) {
  # pass in variable column and calculate the mode on it
  # usage example: mode1 = stats_mode(full$numberofstories)
  #                then call model1 as variable into code
  names(which.max(table(x)))
}

missingnessList <- function(x){
  # x = data as data frame
  # returns a list with percentage for each variable
  lapply(x, function(i) sum(is.na(i))/nrow(x)*100) # verify missingness
}
# missing = missingnessList(full) # verify missingness
# access stat for indiv. variable like this:  missing$numberofstories

## more missingness calculations functions
## most of these functions still need to be tested 

na_missingCount <- function(x){
  # x = data frame
  # calculating # of missing values per column
  sapply(x, function(j) sum(is.na(j)))
}
# Zillow_missingCount = na_missing(full)
 
na_missingPercent <- function(x){
  # x = data frame
  # calculating % of missing values per column
  na_missingCount(x)/nrow(x)*100
}
# Zillow_missing_percent = na_missingPercent(full)

na_nonMissing_Values <- function(x){
  # x = data frame
  # calculating # of non NAs per column
  nrow(x) - na_missingCount
}
# Zillow_values = na_nonMissing_Values(full)

uniqueValues_df <- function(x) {
  # returns unique values from each column in the dataframe
  sapply(x, function(j) length(unique(j)))
}
# Zillow_unique = uniqueValues_df(full)

## idea for future:  missingness_summary to combine it all and reduce redundancy
##    # user can then access sub-elements of returned data structure for answers