library(tidyverse)
library(Metrics)
library(broom)
# install.packages('mgcv')
library(mgcv)

# Bring in dataset from Github
# A dataset containing 133 observations from a simulated motorcycle accident, used to test crash helmets.
mcycle <- read_csv('https://raw.githubusercontent.com/davmiller/M390/master/data/mcycle.csv')
# times in milliseconds from time of impact
# accel in g, acceleration of the head

#-----------------------------------------------------------------------------------------------
# Plot accel vs. times with ggplot
mcycle %>% 
  ggplot(aes(x=times, y=accel))+
  geom_point(color='navy')+
  theme_bw()

# Plot the model using geom_smooth(method=lm)
mcycle %>% 
  ggplot(aes(x=times, y=accel))+
  geom_point(color='navy')+
  geom_smooth(method=lm, color='red')+
  theme_bw()

# Build linear regression model to predict accel given times with lm()
model_lm <- lm(accel ~ times, data=mcycle)

# Add to mcycle
mcycle <- mcycle %>% 
  mutate(pred_lm = predict(model_lm, newdata=mcycle))

# Plot residuals
plot(model_lm, which=1)

# MSE
mse(mcycle$accel, mcycle$pred_lm)

#-----------------------------------------------------------------------------------------------
# Polynomial modeling
mcycle %>% 
  ggplot(aes(x=times, y=accel))+
  geom_point(color='navy')+
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE),colour="red")+
  theme_bw()

# Change the degree of polynomial on your own to see what happens


#-----------------------------------------------------------------------------------------------
# Piecewise linear modeling


# Functions for piecewise linear modeling

# Function to create jth tent function, b_j(x)
tent <- function(x,xj,j){
  dj <- xj*0
  dj[j] <- 1
  return(approx(xj,dj,x)$y)
}

# Function to create matrix X
tent.X <- function(x,xj){
  nk <- length(xj) # number of knots
  n <- length(x) # number of data points
  X <- matrix(NA,n,nk)
  for (j in 1:nk){
    X[,j] <- tent(x,xj,j)
  }
  return(X)
}

# Piecewise linear model function
pw_lin_model <- function(Y,X,k){
  xj <- seq(min(X), max(X), length=k) # Location of k knots
  
  Xp = tent.X(X,xj) # Build tent Matrix Xp
  
  model <- lm(Y~Xp-1) # Linear model Y ~ Xp*b
  
  return(list(model=model, xj=xj))
}




#-----------------------------------------------------------------------------------------------
model1 <- pw_lin_model(Y=mcycle$accel, X=mcycle$times, k=10)


# Function to predict pw linear model
pw_lin.predict <- function(model,newdata){
  k <- length(coef(model$model)) # Number of knots is same as number of coefficients in model
  xj <- model$xj
  Xp <- tent.X(newdata,xj)
  return(Xp %*% coef(model$model))
}


# Plot lines on over the whole interval:
s <- data.frame(times=seq(min(mcycle$times), max(mcycle$times), length=200)) #prediction data
s <- s %>% 
  mutate(pred = pw_lin.predict(model1,times)) # Add predictions

# Plot data
s %>% 
  ggplot()+
  geom_point(data=mcycle,aes(x=times,y=accel), color='navy')+
  geom_line(aes(x=times, y=pred), color='red', size=1)+
  theme_bw()



# Add predictions to mcycle data
mcycle <- mcycle %>% 
  mutate(pred = pw_lin.predict(model1,times))


# Now we can check the MSE and residuals
# add residuals
mcycle <- mcycle %>% 
  mutate(res = accel - pred)

# Plot residuals
mcycle %>% 
  ggplot(aes(x=pred, y=res))+
  geom_point(shape=1)+
  xlab('fitted vales')+
  ylab('residuals')+
  labs(title='Residuals vs. fitted')+
  theme_bw()

# MSE
mse(mcycle$accel,mcycle$pred)


# Build more models with different k values, which is best?
