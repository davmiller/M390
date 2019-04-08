library(tidyverse)   # data wrangling
# install.packages('rpart')
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(Metrics)
library(ipred)       # bagging
library(caret)       # bagging


NCAA <- read_csv('https://raw.githubusercontent.com/davmiller/M390/master/data/NCAA_MBB_16_18.csv')

NCAA_train = NCAA %>% 
  filter(Season!=2018)

NCAA_test = NCAA %>% 
  filter(Season==2018)

#------------------------------------------------------------------------------------------------

# Build tree
tree1 <- rpart(
  formula = score_diff ~ fg_pct_diff+TO_diff+rebs_diff+ft_pct_diff+fg3_pct_diff,
  data    = NCAA_train,
  method  = "anova" # Regression tree
  # method  = "class", # Classification tree
)

# Plot the tree
rpart.plot(tree1)
# Plot cross validation
plotcp(tree1)

# Build tree
tree2 <- rpart(
  formula = score_diff ~ fg_pct_diff+TO_diff+rebs_diff+ft_pct_diff+fg3_pct_diff,
  data    = NCAA_train,
  method  = "anova",
  control = list(cp = 0, xval = 10) # cp = 0 is full tree, xval = 10 is 10-fold c.v.
)

# Plot cp on new tree
plotcp(tree2)

# ON YOUR OWN: build different trees with different cp values.  Plot trees using rpart.plot()





# Build new tuned tree
tree3 <- rpart(
  formula = score_diff ~ fg_pct_diff,
  data    = NCAA_train,
  method  = "anova",
  control = list(minsplit = 10, maxdepth = 3, xval = 10)
)

rpart.plot(tree3)



# Use predict to add predictions
NCAA_test <- NCAA_test %>% 
  mutate(pred1 = predict(tree1, newdata=NCAA_test))

mse(NCAA_test$score_diff, NCAA_test$pred1)


#------------------------------------------------------------------------------------------------
# Bagging
set.seed(123)

# train bagged model
bagged_m1 <- bagging(
  formula = score_diff ~ fg_pct_diff+TO_diff+rebs_diff+ft_pct_diff+fg3_pct_diff,
  data    = NCAA_train,
  coob    = TRUE
)
# Look at model
bagged_m1

# Add predictions to test set
NCAA_test <- NCAA_test %>% 
  mutate(pred2 = predict(bagged_m1, newdata=NCAA_test))

# Check MSE
mse(NCAA_test$score_diff, NCAA_test$pred2)

# Better or worse than one tree?



# train bagged model: More bags
bagged_m2 <- bagging(
  formula = score_diff ~ fg_pct_diff+TO_diff+rebs_diff+ft_pct_diff+fg3_pct_diff,
  data    = NCAA_train,
  coob    = TRUE,
  nbagg   = 30
)



# Add to test set
NCAA_test <- NCAA_test %>% 
  mutate(pred3 = predict(bagged_m2, newdata=NCAA_test))

# Check MSE
mse(NCAA_test$score_diff, NCAA_test$pred3)

# Better or worse than previous bag?


# ON YOUR OWN: try with more / less bags











#------------------------------------------------------------------------------------------------
# Compare with lm() 
mod1 = lm(score_diff ~ fg_pct_diff+TO_diff+rebs_diff+ft_pct_diff+fg3_pct_diff,
          data    = NCAA_train)
# Add to test set
NCAA_test <- NCAA_test %>% 
  mutate(pred4 = predict(mod1, newdata=NCAA_test))
