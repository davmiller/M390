library(tidyverse)
library(Metrics)
library(broom)

# Bring in dataset from Github
# A dataset containing 133 observations from a simulated motorcycle accident, used to test crash helmets.
mcycle <- read_csv('https://raw.githubusercontent.com/davmiller/M390/master/data/mcycle.csv')
# times in milliseconds from time of impact
# accel in g, acceleration of the head

#-----------------------------------------------------------------------------------------------
# We use the mgcv to fit GAMs

# install.packages('mgcv')
library(mgcv)

#-----------------------------------------------------------------------------------------------
# Fit a GAM using gam(), similar to the lm() or glm() syntax
# However, we use the s() function to indicate a smooth fit

model_gam <- gam(accel ~ s(times), data= mcycle)

# Add predictions using predict()
mcycle <- mcycle %>% 
  mutate(pred = predict(model_gam, newdata=mcycle))

# Check the MSE:
mse(mcycle$accel, mcycle$pred)

# Plot the GAM and the data:
data.frame(times=seq(min(mcycle$times), max(mcycle$times), length=200))%>% #prediction data 
  mutate(pred_gam = predict(model_gam, newdata = .))%>% # Add predictions
  # Plot
  ggplot()+ 
  geom_point(data=mcycle,aes(x=times,y=accel), color='navy', alpha=.5)+
  geom_line(aes(x=times, y=pred_gam), color='limegreen', size=1)+
  theme_bw()+
  labs(title='GAM')


# Plot the gam on its own:
plot(model_gam)

# Check the coefficients of the GAM
coef(model_gam)

# Check the GAM using gam.check()
gam.check(model_gam)



#-----------------------------------------------------------------------------------------------
# Smoothing parameter
model_gam_2 <- gam(accel ~ s(times, sp=0.1), data= mcycle)

# Plot the new GAM:
data.frame(times=seq(min(mcycle$times), max(mcycle$times), length=200))%>% #prediction data 
  mutate(pred_gam = predict(model_gam_2, newdata = .))%>% # Add predictions
  # Plot
  ggplot()+ 
  geom_point(data=mcycle,aes(x=times,y=accel), color='navy', alpha=.5)+
  geom_line(aes(x=times, y=pred_gam), color='limegreen', size=1)+
  theme_bw()+
  labs(title='GAM')


# Plot the gam on its own:
plot(model_gam_2)


# ON YOUR OWN: Try with different values of sp

# What happens when sp is small?  large?


#-----------------------------------------------------------------------------------------------
# Number of basis functions (or knots)

model_gam_3 <- gam(accel ~ s(times, k=5), data= mcycle)

# Plot the new GAM:
data.frame(times=seq(min(mcycle$times), max(mcycle$times), length=200))%>% #prediction data 
  mutate(pred_gam = predict(model_gam_3, newdata = .))%>% # Add predictions
  # Plot
  ggplot()+ 
  geom_point(data=mcycle,aes(x=times,y=accel), color='navy', alpha=.5)+
  geom_line(aes(x=times, y=pred_gam), color='limegreen', size=1)+
  theme_bw()+
  labs(title='GAM')

# Plot the gam on its own:
plot(model_gam_3)


# ON YOUR OWN: Try with different k values

# Which one is produced without specifying?

# What happens when k is small?  large?



#-----------------------------------------------------------------------------------------------
# Logistic GAMs

# Load new data
csale <- read_csv('https://raw.githubusercontent.com/davmiller/M390/master/data/csale.csv') %>% 
  select(-X1)
# Data from insurance company.  Binary "Purchase" shows if a customer purchased an insurance policy given their information

# ON YOUR OWN:  Plot the data with x = mortgage_age, y = purchase




# Build logistic regression model using GAM
log_gam <- gam(purchase ~ s(mortgage_age), data= csale, family='binomial')


# Plot the new GAM with data:
data.frame(mortgage_age=seq(min(csale$mortgage_age), max(csale$mortgage_age), length=200))%>% #prediction data 
  mutate(pred_gam = predict(log_gam, newdata = ., type='response'))%>% # Add predictions
  # Plot
  ggplot()+ 
  geom_point(data=csale,aes(x=mortgage_age,y=purchase), color='navy', alpha=.5)+
  geom_line(aes(x=mortgage_age, y=pred_gam), color='red', size=1)+
  theme_bw()+
  labs(title='logistic GAM')


# Plot new model, trans = plogis converts to probabilities
plot(log_gam, trans=plogis)


# ON YOUR OWN: Add predictions to csale data and find misclassification rate as before


# ON YOUR OWN: Try on your own using other predictors.  Which predicts purchase most accurately?



