library(tidyverse)

# Find linear model 
model1 <- lm(mpg~hp, data=mtcars)
coefficients(model1)

# Build your own gradient descent algorithm and name it grad_descent
grad_descent <- function(){
  
}

# Use your grad_descent algorithm and compare to results above.
grad_descent(x = mtcars$hp, y = mtcars$mpg, alpha = 0.00001, epsilon = 0.001)