library(tidyverse)   # data wrangling
library(Metrics)
install.packages('gbm')
library(gbm)          # basic implementation

# Reference website
# http://uc-r.github.io/gbm_regression


# for reproducibility
set.seed(123)

# train GBM model
ames_gbm <- gbm(
  formula = Sale_Price ~ .,
  distribution = "gaussian", # Regression
  data = ames_train,
  n.trees = 10000, # Default is 100
  interaction.depth = 1, # Bunch of "stumps", depth of tree
  shrinkage = 0.001, # Learning rate
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# get MSE and compute RMSE
sqrt(min(ames_gbm$cv.error))
# [1] 28383.23

# plot loss function as a result of n trees added to the ensemble
gbm.perf(ames_gbm, method = "cv")


# ON YOUR OWN: Tune the model to have 5000 trees of depth 3 with learning rate 0.1 and compute the RMSE






# ON YOUR OWN: Build a hypergrid to search over hyperparameters to find best gbm.