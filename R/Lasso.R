#install.packages('glmnet')
library(glmnet)
library(tidyverse)



Ames <- AmesHousing::make_ames()
set.seed(123)
index   <- sample(1:nrow(Ames), round(nrow(Ames) * 0.7))
ames_train <- Ames[index, ]
ames_test  <- Ames[-index, ]



# Manipulating response variable
# Sale price is skewed right (mean > median):
ames_train %>% 
  ggplot()+
  geom_histogram(aes(x=Sale_Price), color='red', fill='navy')+
  theme_bw()


# Although not required, Linear regression is improved when the response variables are symmetrically distributed
# the log() function transforms skewed data to look more symmetric:

ames_train %>% 
  ggplot()+
  geom_histogram(aes(x=log(Sale_Price)), color='red', fill='navy')+
  theme_bw()


# Create training and testing feature model matrices and response vectors.
# we use model.matrix(...)[, -1] to discard the intercept
ames_train_x <- model.matrix(Sale_Price ~ ., ames_train)[, -1]
ames_train_y <- log(ames_train$Sale_Price)

ames_test_x <- model.matrix(Sale_Price ~ ., ames_test)[, -1]
ames_test_y <- log(ames_test$Sale_Price)


# What is the dimension of of your feature matrix?
dim(ames_train_x)
## [1] 2051  307

# Apply Ridge regression to ames data
ames_ridge <- glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 0
)

plot(ames_ridge, xvar = "lambda")


# lambdas applied to penalty parameter
ames_ridge$lambda %>% head()
## [1] 279.1035 254.3087 231.7166 211.1316 192.3752 175.2851

# coefficients for the largest and smallest lambda parameters
coef(ames_ridge)[c("Gr_Liv_Area", "TotRms_AbvGrd"), 100]
##   Gr_Liv_Area TotRms_AbvGrd 
##  0.0001004011  0.0096383231
coef(ames_ridge)[c("Gr_Liv_Area", "TotRms_AbvGrd"), 1] 
##   Gr_Liv_Area TotRms_AbvGrd 
##  5.551202e-40  1.236184e-37



# Apply CV Ridge regression to ames data
ames_ridge_cv <- cv.glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 0
)

# plot results
plot(ames_ridge_cv)

# minimum MSE
min(ames_ridge_cv$cvm)       
## [1] 0.02147691
# lambda for this min MSE
ames_ridge_cv$lambda.min     
## [1] 0.1236602



# Lasso
## Apply lasso regression to ames data
ames_lasso <- glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 1
)

plot(ames_lasso, xvar = "lambda")



# Apply CV Lasso regression to ames data
ames_lasso_cv <- cv.glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 1
)
# plot results
plot(ames_lasso_cv)

# minimum MSE
min(ames_lasso_cv$cvm)       
## [1] 0.02275227
ames_lasso_cv$lambda.min     # lambda for this min MSE
## [1] 0.003521887



# minimum Ridge MSE
min(ames_ridge_cv$cvm)

# minimum Lasso MSE
min(ames_lasso_cv$cvm)


# Predicting
# some best model
cv_lasso   <- cv.glmnet(ames_train_x, ames_train_y, alpha = 1.0)
min(cv_lasso$cvm)

# predict
pred <- predict(cv_lasso, s = cv_lasso$lambda.min, ames_test_x)
mean((ames_test_y - pred)^2)




# Try your own models regression only using the GR_Living and TotRms variables:
X <- matrix(c(ames_train$Gr_Liv_Area, ames_train$TotRms_AbvGrd), ncol=2)
Y <- matrix(ames_train$Sale_Price)
X = scale(X)


