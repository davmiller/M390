library(tidyverse)
library(Metrics)
library(broom)
library(mgcv)


#-----------------------------------------------------------------------------------------------
# Multivariate GAMs

# Harry Potter Dataset
harrypotter <- read_csv('https://raw.githubusercontent.com/davmiller/M390/master/data/harrypotter.csv')
# Total revenue of Harry Potter movies  


# Plot 
harrypotter %>% 
  ggplot(aes(x=weeknum, y=revenue))+
  geom_point()+
  theme_bw()+
  labs(title='Harry Potter film revenue vs. weeks in theaters')

# Plot 
harrypotter %>% 
  ggplot(aes(x=theaters, y=revenue))+
  geom_point()+
  theme_bw()+
  labs(title='Harry Potter film revenue vs. number of theaters')


#-----------------------------------------------------------------------------------------------
hp_gam <- gam(revenue ~ s(weeknum), data=harrypotter)

# Plot the new GAM:
data.frame(weeknum=seq(min(harrypotter$weeknum), max(harrypotter$weeknum), length=200))%>% #prediction data 
  mutate(pred_gam = predict(hp_gam, newdata = .))%>% # Add predictions
  # Plot
  ggplot()+ 
  geom_point(data=harrypotter,aes(x=weeknum,y=revenue), color='navy', alpha=.5)+
  geom_line(aes(x=weeknum, y=pred_gam), color='red', size=1)+
  theme_bw()+
  labs(title='GAM')

plot(hp_gam)

# Add predictions
harrypotter <- harrypotter %>% 
  mutate(pred1=predict(hp_gam, newdata=harrypotter))

#-----------------------------------------------------------------------------------------------
# Theaters
hp_gam_2 <- gam(revenue ~ s(theaters), data=harrypotter)

# Plot the new GAM:
data.frame(theaters=seq(min(harrypotter$theaters), max(harrypotter$theaters), length=200))%>% #prediction data 
  mutate(pred_gam = predict(hp_gam_2, newdata = .))%>% # Add predictions
  # Plot
  ggplot()+ 
  geom_point(data=harrypotter,aes(x=theaters,y=revenue), color='navy', alpha=.5)+
  geom_line(aes(x=theaters, y=pred_gam), color='red', size=1)+
  theme_bw()+
  labs(title='GAM')

plot(hp_gam_2)

# Add predictions
harrypotter <- harrypotter %>% 
  mutate(pred2=predict(hp_gam_2, newdata=harrypotter))

#-----------------------------------------------------------------------------------------------
hp_gam_3 <- gam(revenue ~ s(theaters)+s(weeknum), data=harrypotter)

# Add predictions
harrypotter <- harrypotter %>% 
  mutate(pred3=predict(hp_gam_3, newdata=harrypotter))

plot(hp_gam_3, pages=1)

# ON YOUR OWN: Check the MSE values from the three predictions

# # ON YOUR OWN: Alter hp_gam_3 by changing the k and sp values in each smooth term.


#-----------------------------------------------------------------------------------------------
# Multivariate Logistic GAMs

# Load new data
csale <- read_csv('https://raw.githubusercontent.com/davmiller/M390/master/data/csale.csv')
# Data from insurance company.  Binary "Purchase" shows if a customer purchased an insurance policy given their information


# ON YOUR OWN: Build a multivariate logistic GAM to predict purchase given indicators.

# Plot the new GAM


#-----------------------------------------------------------------------------------------------
# Interactions in GAMs
base_data <- read_csv('https://raw.githubusercontent.com/davmiller/M390/master/data/base_data.csv') %>% 
  mutate(strike=as.factor(strike))
# Pitch data on pitches that were not swung at
# pitch_x is horizontal location of the pitch, pitch_y is vertical location

# Plot the data 
base_data %>% 
  ggplot()+
  geom_point(aes(x=pitch_x, y=pitch_y))+
  theme_minimal()

# Add a color factor for strike
base_data %>% 
  ggplot()+
  geom_point(aes(x=pitch_x, y=pitch_y, color=strike))+
  #scale_color_manual(values=c('purple2', 'limegreen'))+ # Change the colors
  theme_minimal()

# Add a shape factor for pitch type
base_data %>% 
  ggplot()+
  geom_point(aes(x=pitch_x, y=pitch_y, color=strike, shape = pitch_type))+
  # scale_color_manual(values=c('purple2', 'limegreen'))+ # Change the colors
  # scale_shape_manual(values=c(1,2,3))+ # Change the shapes
  theme_minimal()

# Separate plots by pitch_type
base_data %>% 
  ggplot()+
  geom_point(aes(x=pitch_x, y=pitch_y, color=strike))+
  # scale_color_manual(values=c('purple2', 'limegreen'))+ # Change the colors
  theme_minimal()+
  facet_grid(pitch_type~.)



# Model building
# Predict if the pitch is called a strike given its location
sz_mod = gam(strike ~ s(pitch_x,pitch_y), family='binomial', data=base_data, method='REML')

# Plot model
plot(sz_mod)  
# Heat map
plot(sz_mod, scheme=2)
# Heat map with probabilities
plot(sz_mod, scheme=2, trans=plogis)

# Check model
gam.check(sz_mod)

# ON YOUR OWN: Try new model with increased k


base_data <- base_data %>% 
  mutate(p_strike=round(predict(sz_mod2, type='response'),2))

# Plot heat map on your own
xx <- seq(-80, 80, length.out=100)
zz <- seq(-100, 100, length.out=100)
data.frame(pitch_x = c(outer(xx, zz * 0 + 1)),
                 pitch_y = c(outer(xx * 0 + 1, zz))) %>% 
  mutate(p_strike=round(predict(sz_mod, newdata=., type='response'),2))%>% 
  ggplot()+
  geom_tile(aes(x=pitch_x,y=pitch_y, fill=p_strike))+
  #scale_fill_gradient2(low = "dodgerblue3", high = "red2")+ # Change colors
  theme_minimal()




#--------------------------------------------------------------------------------------------------------------
base_player <- read_csv('https://raw.githubusercontent.com/davmiller/M390/master/data/base_player.csv') 
# Pitch data for two different hitters
# pitch_x is horizontal location of the pitch, pitch_y is vertical location

# Plot data and build model to predict if player A will swing at the pitch given its location.
# Repeat for player B 


