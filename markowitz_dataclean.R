full = properties %>% left_join(., train, by = 'parcelid')

full <- full %>% 
  mutate(heatingorsystemtypeid = as.factor(ifelse(is.na(heatingorsystemtypeid), 
                                                  "13", heatingorsystemtypeid)))
full <- full %>% 
  mutate(hashottuborspa = as.factor(ifelse(is.na(hashottuborspa), 0, 1)))

full <- full %>% 
  mutate(fullbathcnt = as.factor(ifelse(is.na(fullbathcnt),0,fullbathcnt)))

library(VIM)
a = kNN(full[1:100,], variable = 'taxamount', dist_var = 'taxvaluedollarcnt', k=30)
