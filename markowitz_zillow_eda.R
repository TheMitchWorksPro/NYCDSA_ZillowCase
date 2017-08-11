#setwd('~/Desktop/zillow/input/')
library(corrplot)
library(ggplot2)
library(dplyr)
library(data.table)

train = fread('train_2016_v2.csv')
properties = fread('properties_2016.csv')
full = properties %>% left_join(., train, by = 'parcelid')


sub=select(full, starts_with(c('finishedsquarefeet','logerror')))

missing = lapply(full, function(i) sum(is.na(i))/nrow(full)) #to see missingness
missing$finishedfloor1squarefeet
a=full[grepl('finishedsquare|logerror', names(full))]
corrplot(cor(a,use = "pairwise.complete.obs"))

library(mice)
md.pattern(a[-nrow(a)])

full = full[!grepl('finishedsquare', 
                     names(full)[!grepl('calculatedfinishedsquare',names(full))])]

md.pattern(full[grepl('garage', names(full))]) #either none or both



unique(full$hashottuborspa) #either blank or true
sum(full$hashottuborspa=="true") # 2% have hot 
full$hashottuborspa[full$hashottuborspa==""]="false"

unique(full$heatingorsystemtypeid)
full$hasheat = !is.na(full$heatingorsystemtypeid) #added column to say T/F for having heat based on NAs

missing$taxvaluedollarcnt #not too much missing
length(unique(full$taxvaluedollarcnt)) #continuous
md.pattern(full[grepl('dollarcnt', names(full))])
corrplot(cor(full[grepl('dollarcnt', names(full))], use = "pairwise.complete.obs"))


