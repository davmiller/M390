#install.packages(tidyverse)
library(tidyverse)

# Introduction to Machine Learning 1/23/19

# Look at data pre-loaded in R
data()

# Look at the first 6 rows of mtcars
head(mtcars)

# Look at the mtcars data
View(mtcars)

mtcars %>%
  View()

#-------------------------------------------------------------------------------

# Import data from GitHub
ex_1 <- read.csv('https://raw.githubusercontent.com/davmiller/M390/master/data/example_data_1.csv')

# View the new data
head(ex_1)

ex_1 %>%
  View()

# Plot the new data using ggplot
ggplot(data=ex_1, aes(x=x,y=y))+
  geom_point()

#-------------------------------------------------------------------------------

# Split the data into training and testing sets
# 70% random train / test split

# set.seed makes it so the 'random' splits are all the same
set.seed(123)

# Randomly select 70% of the ex_1 rows
index   <- sample(1:nrow(ex_1), round(nrow(ex_1) * 0.7))

# Select those rows into a new dataset called train_1
train_1 <- ex_1[index, ]

# Select the other 30% of the rows called test_1
test_1  <- ex_1[-index, ]

# Plot the train and test sets. Train in black, test in red.
ggplot()+
  geom_point(data=train_1, aes(x=x,y=y), color='black')+
  geom_point(data=test_1, aes(x=x,y=y), color='red')

#-------------------------------------------------------------------------------




# Try it on your own!
# 1. Choose a dataset from the pre-loaded R sets
# 2. Look at the data.  Choose a response variable and one indicator variable.
# 3. Plot your two variables on the xy-plane using ggplot.
# 4. Split the data into a training / testing set and plot them using different colors.