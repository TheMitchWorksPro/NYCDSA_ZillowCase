View(full)
full <- full %>% 
  mutate(heatingorsystemtypeid = as.factor(ifelse(is.na(heatingorsystemtypeid), 
                                                  "13", heatingorsystemtypeid)))
full <- full %>% 
  mutate(hashottuborspa = as.factor(ifelse(is.na(hashottuborspa), 0, 1)))

full <- full %>% 
  mutate(fullbathcnt = as.factor(ifelse(is.na(fullbathcnt),0,fullbathcnt)))