library(dplyr)
library(magrittr)
library(data.table)
library(tidyr)
library(ggplot2)
library(corrplot)
library(stringr)
library(mice) 
setwd("/Users/Sishe/Desktop/")
train <- fread("train_2016_v2.csv")
properties = fread("properties_2016.csv", header=TRUE, na.strings=c("", " ", "NA", "N/A", "null"), blank.lines.skip = TRUE,
                   colClasses=list(character=50))
full = properties %>% inner_join(., train, by = 'parcelid')

data_dict=fread("dictionary.csv")



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






#### Coded added on Aug 15, 2017 >>>>>>>>>>>>
#create a col vector: 
my_cols <- c("propertycountylandusecode",
                 "propertylandusetypeid",
                 "propertyzoningdesc", 
                 "rawcensustractandblock",
                 "regionidcity", 
                 "regionidcounty", 
                 "regionidneighborhood",
                 "regionidzip",
                 "storytypeid", 
                 "roomcnt",
                 "typeconstructiontypeid",
                 "censustractandblock",
                 "unitcnt", "yardbuildingsqft17", "yardbuildingsqft26", "yearbuilt")


### for missingness in the below columns: 
missing_values = full %>% select(my_cols)%>%summarize_each(funs(sum(is.na(.))/n()))
missing_values = gather(missing_values, key = 'feature', value = 'missing_pct')
### filter columns missing more than 75% 
bad_features = missing_values%>%filter(missing_pct>=0.75)
good_features = missing_values%>%filter(missing_pct<0.75)
##drop columns that are missing more 75% of data
full= full%>% select(-one_of(bad_features$feature))

#Impute unitcnt using median. 


full$unitcnt <- with(full, impute(unitcnt, median))

# 
# raw<- full %>% mutate(census = as.character(rawcensustractandblock),
#                       tract_number = str_sub(census,5,11), 
#                       tract_block = str_sub(census,12))












