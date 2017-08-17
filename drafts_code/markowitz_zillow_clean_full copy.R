#setwd('~/Desktop/zillow/input/')
library(dplyr)
library(data.table)

properties <- fread(input = "properties_2016.csv", 
                    na.strings = "",
                    colClasses = list(character=c("parcelid", "airconditioningtypeid", "architecturalstyletypeid",
                                                  "buildingclasstypeid", "decktypeid", "fips", "hashottuborspa",
                                                  "heatingorsystemtypeid", "pooltypeid10", "pooltypeid2", "pooltypeid7",
                                                  "propertycountylandusecode", "propertylandusetypeid", 
                                                  "propertyzoningdesc", "rawcensustractandblock", "regionidcity", 
                                                  "regionidcounty", "regionidneighborhood", "regionidzip", "storytypeid", 
                                                  "typeconstructiontypeid", "fireplaceflag", "assessmentyear",
                                                  "taxdelinquencyflag", "censustractandblock")))

train <- fread(input = "train_2016_v2.csv",
               na.strings = "",
               colClasses = list(character=c("parcelid")))

## convert lat/lon
properties <- properties %>%
  mutate(latitude = latitude/1e6, longitude = longitude/1e6)

full = inner_join(properties, train, by = 'parcelid')






full <- full %>% 
  mutate(heatingorsystemtypeid = as.factor(ifelse(is.na(heatingorsystemtypeid), 
                                                  "13", heatingorsystemtypeid)))
full <- full %>% 
  mutate(hashottuborspa = as.factor(ifelse(is.na(hashottuborspa), 0, 1)))

full <- full %>% 
  mutate(fullbathcnt = as.factor(ifelse(is.na(fullbathcnt),0,fullbathcnt)))



#An imputation for taxamount from taxvaluedollarcnt (may include landvaluedollarcnt later)
lm1 = lm(formula = taxamount ~ taxvaluedollarcnt, data = full)


full$taxamount = ifelse(!is.na(full$taxamount), full$taxamount, #if result column is nonempty, return result
                              ifelse(is.na(full$taxvaluedollarcnt), NA, #if result column IS empty, if helper is empty too
                                     lm1$coefficients[1]+lm1$coefficients[2]*full$taxvaluedollarcnt)) #if result column is empty but helper has value

a = full %>% filter(., is.na(taxvaluedollarcnt))
full$taxamount[is.na(full$taxamount)] <- mean(a$taxamount, na.rm = T)


#removing bad columns

col_rm = c("finishedfloor1squarefeet", "finishedsquarefeet12", "finishedsquarefeet13", "finishedsquarefeet15", 
           "finishedsquarefeet50","finishedsquarefeet6", "taxvaluedolarcnt", 'landtaxvaluedollarcnt', 'structuretaxvaluedollarcnt')

full <- full[, setdiff(names(full), col_rm)]
