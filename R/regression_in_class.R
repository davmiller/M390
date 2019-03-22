library(tidyverse)
library(Metrics)
library(broom)

# In this assignment, we are working with results from Men's College Basketball games from 2016-2018.
# Bring in dataset from Github
NCAA <- read_csv('https://raw.githubusercontent.com/davmiller/M390/master/data/NCAA_MBB_16_18.csv')

# Examine the data using head(), View(), etc.

# ------------------------------------------------------------------------------------
# SECTION 1: Linear regression


# GOAL: predict the score difference given the difference in FG %


# Plot a bar graph of the score_diff distribution using geom_bar(aes(x=score_diff)).
# QUESTION: Does the distribution of the score_diff require transformation? Why or why not?


# Plot a dot plot of score_diff vs. fg_pct_diff.  Include a title, colored points and a theme.


# Split into training and testing sets using 2016 and 2017 as training sets, and 2018 as test sets.


# Build a linear model that predicts score difference given the difference in FG % on the training set.


# Analyze the model using summary(), coef() and/or tidy().  Also, plot the residuals of the model.
# QUESTION: Is a linear model appropriate in this case given the plot of the residuals?


# Find the MSE of the model on both the training and testing sets.
# ------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------

# SECTION 2: Logistic regression

# Add a new column called home_win to the dataframes that indicates if the home team won the game using ifelse().  


# Plot home_win vs. fg_pct_diff using ggplot with a title, colored dots and theme.


# QUESTION:  Is a linear regression model appropriate to predict if the home team will win?  Why or why not?


# Build an appropriate model that will predict the probability that the home team will win given the FG% difference.


# Analyze the model using summary(), coef() and/or tidy().
# QUESTION: Does an increase in FG% difference increase the probability that the home team will win? 
# Explain your reasoning while referencing your model.



# Re-plot home_win vs. fg_pct_diff using ggplot with a title, colored dots and theme to include the appropriate model.


# Find the accuracy of the model by creating a confusion matrix on the training set and test set.
# QUESTION: How many false postives does your model produce?  False Negatives?


# Calculate the misclassification rate on the training set and test set.


# Calculate the logloss on both sets.


