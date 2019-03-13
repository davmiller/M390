library(tidyverse)
#install.packages('broom')
library(broom)
#install.packages("ISLR")
library(ISLR)
#install.packages('Metrics')
library(Metrics)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Another look at residuals

# Example 1 data from class 1
ex_1 <- read.csv('https://raw.githubusercontent.com/davmiller/M390/master/data/example_data_1.csv')


# Plot the data and linear model
ex_1 %>% 
  ggplot(aes(x=x,y=y))+
  geom_point(color='navy')+
  geom_smooth(method='lm',se=FALSE, color='red')+
  theme_bw()

# Build linear model
model1 <- lm(y~x, data=ex_1)

# Look at summary
summary(model1) # Coeffs are significant (Pr(>|t|) small), p-value small, adjusted R-squared ok


# Look at residual plot
plot(model1, which=1) # Residuals are not evenly "scattered" with mean 0, they have a clear pattern.  Thus linear model is not appropriate.

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Back to logistic regression

# Read new data
df <- read_csv('https://raw.githubusercontent.com/davmiller/M390/master/data/log_data.csv')

# Add new column of 1's if y>5, 0 otherwise
# ifelse([test], [value if true], [value if false])
df <- df %>% 
  mutate(z=ifelse(y>5,1,0))

# Naive model
df <- df %>% 
  rowwise() %>% 
  mutate(pred1=rbinom(1,1,.5)) %>% # Naive model that randomly assigns 0 / 1 each with probability p = 0.5
  ungroup()

# Confusion matrix
table(df$z, df$pred1) # Many FPs and FNs.

# Find misclassification rate
1-round(mean(df$z == df$pred1),2) # Poor misclassification rate

# Find the logLoss using function built into Metrics package
Metrics::logLoss(df$z,0.5) # Poor logLoss, lower is better.





# Logistic regression model
log1 <- glm(z ~ x, data = df, family = "binomial")

# Add predicted probabilities and classifications
df <- df %>% 
  mutate(p1 = predict(log1, newdata=df, type='response'),
         pred2 = ifelse(p1>0.5,1,0)) # Classification with p > 0.5 threshold

# Confusion matrix
table(df$z, df$pred2) # Many FPs and FNs.

# Find misclassification rate
1-round(mean(df$z == df$pred2),2) # Poor misclassification rate

# Find the logLoss using function built into Metrics package
Metrics::logLoss(df$z,df$p1)




# Try work on your own using default data
default <- ISLR::Default

# GOAL: predict the probability of a student defaulting on their credit card debt given the remaining balance.

# Update column 'default' to have 1 / 0 instead of 'Yes' / 'No' using ifelse()
# Plot default vs. balance using ggplot with a model using geom_smooth
# Split into training / testing sets
# Build logistic regression model predicting default given balance on training set
# Look at the summary of the model.  Explain anything that is of interest.
# Build the confusion matrix on the training set, then test set
# Find the classification rate using p > 0.5 threshold on the test set
# QUESTION: Is the classification rate better than random guessing of 50%?
# Write a logloss function
# Measure predictive accuracy using logloss function on the test set




# Build a multivariate model that includes income as a predictor


