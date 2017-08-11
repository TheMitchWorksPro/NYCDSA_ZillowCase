library(dplyr)
library(magrittr)
library(data.table)
library(tidyr)
library(ggplot2)
setwd("/Users/Sishe/Desktop/")
train <- fread("train_2016_v2.csv")
properties = fread("properties_2016.csv", header=TRUE, na.strings=c("", " ", "NA", "N/A", "null"), blank.lines.skip = TRUE,
                   colClasses=list(character=50))
full = properties %>% left_join(., train, by = 'parcelid')

data_dict=fread("dictionary.csv")

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



##### MY CODE start###############
### factor columns (### You can add your columns that need to be factorized in the code below)
factor_cols <- c("propertycountylandusecode",
                 "propertylandusetypeid",
                 "propertyzoningdesc", 
                 "rawcensustractandblock",
                 "regionidcity", 
                 "regionidcounty", 
                 "regionidneighborhood",
                 "regionidzip",
                 "storytypeid", 
                 "typeconstructiontypeid",
                 "censustractandblock")

## convert columns to factors
full %<>%
  mutate_each_(funs(factor(.)),factor_cols)
str(full)

#########END######################


percentage_missing = full%>% summarise_all(funs(sum(is.na(.))/n())) 
print(percentage_missing*100)









