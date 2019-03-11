library(tidyverse)
#install.packages('broom')
library(broom)
#install.packages("ISLR")
library(ISLR)


# Read new data
df <- read_csv('https://raw.githubusercontent.com/davmiller/M390/master/data/log_data.csv')

# Add new column of 1's if y>5, 0 otherwise
df <- df %>% 
  mutate(z=ifelse(y>5,1,0))

# Plot data with regression line
df %>% 
  ggplot(aes(x=x,y=y))+
  geom_point(color='navy')+
  theme_bw()+
  geom_smooth(method=lm, color='red', se=FALSE)

# Plot new data on your own






# Compare lm() to glm()
coef(lm(y~x, data=df))

coef(glm( y ~ x, data = df, family = "gaussian"))



# Logistic regression
log1 <- glm( z ~ x, data = df, family = "binomial")

# Coefficients

# Summary


# Tidy summary
tidy(log1)


# Make predictions
df <- df %>% 
  mutate(p1 = predict(log1, newdata=df, type='response'))

# Plot new predictions
df %>% 
  ggplot()+
  geom_point(aes(x=x,y=z), color='navy')+
  geom_line(aes(x=x,y=p1), color='red')+
  theme_bw()


# Plot with geom_smooth
df %>% 
  ggplot()+
  geom_point(aes(x=x,y=z), color='navy')+
  geom_line(aes(x=x,y=p1), color='red', size=2)+
  theme_bw()+
  geom_smooth(aes(x=x,y=z),method = "glm", method.args = list(family = "binomial"), color='green', se=FALSE)

# Add new column of predictions given p > 0.5 threshold
df <- df %>% 
  mutate(pred1=ifelse(p1>0.5,1,0))


# Confusion matrix
table(df$z, df$pred1)

# Find classification rate
df %>% 
  summarize(mean(z == pred1))








# Try work on your own using default data
default <- ISLR::Default

# Update column 'default' to have 1 / 0 instead of 'yes' / 'no' using if_else()
# Plot default vs. balance using ggplot with a model using geom_smooth
# Split into training / testing sets
# Build logistic regression model predicting default given balance on training set
# Find the classification rate using p > 0.5 threshold on the test set
# Write a logloss function
# Measure predictive accuracy using logloss function on the test set
