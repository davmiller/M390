library(tidyverse)

# Plot ex_1 with ggplot
ggplot(data=mtcars,aes(x=hp, y=mpg))+
  geom_point()+
  labs(title='MPG vs. Horsepower from mtcars')

# Plot ex_1 using ggplot with regression line
ggplot(data=mtcars,aes(x=hp, y=mpg))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  labs(title='MPG vs. Horsepower from mtcars')


set.seed(123)
# Train test split
index   <- sample(1:nrow(mtcars), round(nrow(mtcars) * 0.7))
train_1 <- mtcars[index, ]
test_1  <- mtcars[-index, ]

# Plot train_1 using ggplot with regression line
ggplot(data=train_1,aes(x=hp, y=mpg))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

# Compute the regression line using the built-in lm() function in R:
model1 <- lm(mpg ~ hp, data = train_1)

# Find the coefficients of the linear model using coefficients()
coefficients(model1)

# Add a row to train_1 with the model's predictions
train_1 <- train_1 %>% 
  mutate(y_pred = predict(model1, newdata = train_1))

# Compute the MSE on the training set
mean((train_1$mpg-train_1$y_pred)^2)

# We can write a new function that does the mse for us down the road:
mse <- function(actual, prediction){
  mean((actual-prediction)^2)
}

# On your own: Add a new row to test_1 with model's predictions and calculate the test mse


# Do the same work with another dataset.  You can use ex_1 if you'd like, but I prefer a built-in dataset.
