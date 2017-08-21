## source file:  ml.zillow

library(data.table)
library(dplyr)
full = fread('clean_output.csv')


## data field conversions ahead of these models:
### convert type to avoid modeling issues
full$fullbathcnt = as.integer(full$fullbathcnt)
full$latitude = as.numeric(full$latitude)
full$longitude = as.numeric(full$longitude)

### keep as factors for this test
full$fips=as.factor(full$fips)
full$numberofstories = as.factor(full$numberofstories)
full$propertycountylandusecode = as.factor(full$propertycountylandusecode)


# stupid multi regression
lr1 <- lm(logerror ~ calculatedfinishedsquarefeet + yearbuilt, data=full);
summary(lr1)

# Call:
#   lm(formula = logerror ~ calculatedfinishedsquarefeet + yearbuilt, 
#      data = full)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.6188 -0.0370 -0.0057  0.0272  4.7177 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  -1.227e-01  4.518e-02  -2.716  0.00660 ** 
#   calculatedfinishedsquarefeet  6.373e-06  5.925e-07  10.756  < 2e-16 ***
#   yearbuilt                     6.243e-05  2.306e-05   2.708  0.00678 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1608 on 90147 degrees of freedom
# (2895067 observations deleted due to missingness)
# Multiple R-squared:  0.001568,	Adjusted R-squared:  0.001545 
# F-statistic: 70.77 on 2 and 90147 DF,  p-value: < 2.2e-16

full.train = full %>% filter(., !is.na(logerror))

#This makes predicitions
print('Making predictions ...')
predictions <- data.frame(predict(lr1, full))
print('Appending predictions to full ...')
full$p_lr1 <- predictions$predict.lr1..full.
full$p_lr1[is.na(full$p_lr1)] <- mean(full$logerror, na.rm = TRUE)  # Replace missing with average
## [1] 0.01154793
print('Average prediction value is ...')
mean(full$p_lr1)

# Create submission file
print('Creating submission file')
submit <- data.frame(full[,c("parcelid", "p_lr1")])
submit$"201610" <- round(submit$p_lr1,4)
submit$"201611" <- round(submit$p_lr1,4)
submit$"201612" <- round(submit$p_lr1,4)
submit$"201710" <- round(submit$p_lr1,4)
submit$"201711" <- round(submit$p_lr1,4)
submit$"201712" <- round(submit$p_lr1,4)
submit$p_lr1<- NULL # remove the original prediction from the submit file
nrow(submit)  # checking right number of rows: [1] 2985217
write.csv(submit, file = "submit_5.csv", row.names = FALSE, na="") # export the file for submission
print('Done!')
