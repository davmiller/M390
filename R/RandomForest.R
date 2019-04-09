library(tidyverse)   # data wrangling
library(Metrics)
library(ipred)       # bagging
#install.packages('randomForest')
library(randomForest) # basic implementation

# Reference website
# http://uc-r.github.io/random_forests


# Ames housing package
Ames=AmesHousing::make_ames()

# Train / test split
set.seed(123)
index <- sample(1:nrow(Ames), 0.7*nrow(Ames))
ames_train = Ames[index,]
ames_test = Ames[-index,]


#------------------------------------------------------------------------------------------------------
# Bagged example from last class
ames_bag <- bagging(
  formula = Sale_Price ~.,
  data    = ames_train,
  coob    = TRUE
)

# Look at model
ames_bag
# OOB RMSE = 36109.51

# Add to test set
ames_test <- ames_test %>% 
  mutate(pred_bag = predict(ames_bag, newdata = ames_test))

# Calculate RMSE of test set
rmse(ames_test$Sale_Price, ames_test$pred_bag)
# 35705.94 (close to OOB RMSE)



#------------------------------------------------------------------------------------------------------
# default RF model
ames_rf <- randomForest(
  formula = Sale_Price ~ .,
  data    = ames_train
)
# Takes a long time

# Look at model
ames_rf

# Plot model
plot(ames_rf) # Plotting the model will illustrate the error rate as we average across more trees 

# Plot variable importance
varImpPlot(ames_rf)

# Look at all of the MSEs for every number of trees in the forest
ames_rf$mse

# number of trees with lowest MSE
which.min(ames_rf$mse)

# RMSE of this optimal random forest
sqrt(ames_rf$mse[which.min(ames_rf$mse)])
# 25629.57 (Much lower than bagging)

#------------------------------------------------------------------------------------------------------

# Tuning RFs
# names of features
features <- setdiff(names(ames_train), "Sale_Price")

ames_rf2 <- randomForest(
  formula = Sale_Price ~ .,
  data    = ames_train,
  ntree   = 500,
  mtry    = floor(length(features) / 3), # m = 26
  min.node.size = 3, # min node size
  sample.fraction = 0.6, # pct of samples to train on
  seed            = 123 # Reproducibility
)

plot(ames_rf2)
# number of trees with lowest MSE
which.min(ames_rf2$mse)

# RMSE of this optimal random forest
sqrt(ames_rf2$mse[which.min(ames_rf2$mse)])
# 25575.08 (Lower than last RF)

# ON YOUR OWN: alter mtry - sample.fraction




#------------------------------------------------------------------------------------------------------
 # Hypergrid searching

# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(20, 30, by = 10),
  node_size  = seq(3, 9, by = 3),
  #sampe_size = c(.6, .70, .80),
  OOB_RMSE   = 0
) 
# Small grid for in-class


# For loop over the hypergrid
for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- randomForest(
    formula         = Sale_Price ~ ., 
    data            = ames_train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = .7,
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$mse[which.min(model$mse)])
}

# Takes a while



# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(20, 30, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
) 
# Larger grid


#------------------------------------------------------------------------------------------------------
# Predicting
ames_test <- ames_test %>% 
  mutate(pred_RF = predict(ames_rf2, newdata = ames_test))

rmse(ames_test$Sale_Price, ames_test$pred_RF)


