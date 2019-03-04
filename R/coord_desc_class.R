library(tidyverse)


# Example
# Make Ames housing data
Ames <- AmesHousing::make_ames()
# Train / test split
set.seed(123)
index   <- sample(1:nrow(Ames), round(nrow(Ames) * 0.7))
ames_train <- Ames[index, ]
ames_test  <- Ames[-index, ]

# Put the vars in matrix form for easier computation later.
Y <- matrix(ames_train$Sale_Price) 
X <- matrix(data=c(ames_train$Gr_Liv_Area, ames_train$TotRms_AbvGrd), ncol=2)

# Use the lm() function to find coefficients of linear regression for future comparison
lm(Y~X)


# Create a coordinate descent function that reproduces coefficients above.

# Notes:
# if m <- ncol(X)
# matrix(X[,-j],ncol=(m-1)) # Take out column j of X
# matrix(b[-j,], ncol=1) # Take out jth element of b vector
# matrix(X[,j],ncol=1) # Only column j of X




