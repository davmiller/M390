library(tidyverse)
# Import Ames Housing data from AmesHousing package
Ames <- AmesHousing::make_ames()


# Look at data
head(Ames)
names(Ames)
View(Ames)

# plot price vs. square feet
Ames %>% 
  ggplot(aes(x=Gr_Liv_Area, y=Sale_Price))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

# build a linear model with square foot and bedrooms
model1 <- lm(Sale_Price~Gr_Liv_Area, data=Ames)
model2 <- lm(Sale_Price~Bedroom_AbvGr, data=Ames)

summary(model1)
summary(model2)

# Build multivariate regression model
model3 <- lm(Sale_Price~Gr_Liv_Area+Bedroom_AbvGr+ Full_Bath, data=Ames)
summary(model3)

# plot price vs. square feet with no intercept
Ames %>% 
  ggplot(aes(x=Gr_Liv_Area, y=Sale_Price))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE, formula=y~x+0)+
  geom_smooth(method=lm, se=FALSE, formula=y~x, color='red')

# Build multivariate regression model with no intercept
model4 <- lm(Sale_Price~Gr_Liv_Area+Bedroom_AbvGr+ Full_Bath+0, data=Ames)
summary(model4)


# Compare MSE
Ames <- Ames %>% mutate(pred_3=predict(model3, newdata = Ames))
mean((Ames$Sale_Price-Ames$pred_3)^2)

# Build your own gradient descent algorithm for multivariate linear regression



# Ames Data in matrix / vector form
Y <- matrix(Ames$Sale_Price) 
X <- matrix(data=c(Ames$Gr_Liv_Area, Ames$Full_Bath, Ames$Bedroom_AbvGr), ncol=3)

# Initialize B vector
B <- matrix(c(0,0,0))

# Calculuate MSE  Note: %*% is used for matrix / vector multiplication
n <- nrow(Y)
1/n*(t(Y - X%*%B)%*%(Y - X%*%B))

# Calculuate gradient
gradC <- (1/n)*(t(X)%*%(Y - X%*%B))

# Calcluate norm of gradC after building a function to calculuate all norms.
norm <- function(x){
  sum(abs(x))
}
norm(gradC)


