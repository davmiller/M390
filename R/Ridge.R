library(tidyverse)
#install.packages("corrplot")
library(corrplot)

# Make Ames housing data
Ames <- AmesHousing::make_ames()

# Train / test split
set.seed(123)
index   <- sample(1:nrow(Ames), round(nrow(Ames) * 0.7))
ames_train <- Ames[index, ]
ames_test  <- Ames[-index, ]

# linear model with Gr_Liv_Area , TotRms_AbvGrd
model1 <- lm(Sale_Price ~ Gr_Liv_Area + TotRms_AbvGrd, data = ames_train)
coefficients(model1)


# Correlation plots
# Can only run correlation on numeric values
Ames_num <- Ames[, sapply(Ames, is.numeric)]
# Find correlation and plot them
corr <- cor(Ames_num)
corrplot(corr, method="circle")


# Standardizing inputs
# Add new scaled columns to training 
ames_train = ames_train%>%
  mutate(Gr_Liv_Area_scaled = (Gr_Liv_Area-mean(Gr_Liv_Area))/sd(Gr_Liv_Area),
         TotRms_AbvGrd_scaled = (TotRms_AbvGrd-mean(TotRms_AbvGrd))/sd(TotRms_AbvGrd))

# New model using scaled vars
model1_scaled <- lm(Sale_Price ~ Gr_Liv_Area_scaled + TotRms_AbvGrd_scaled, data = ames_train_scaled)
coefficients(model1_scaled)

# Use the training mean / sd on the test set and predict values
ames_test = ames_test%>%
  mutate(Gr_Liv_Area_scaled = (Gr_Liv_Area-mean(ames_train$Gr_Liv_Area))/sd(ames_train$Gr_Liv_Area),
         TotRms_AbvGrd_scaled = (TotRms_AbvGrd-mean(ames_train$TotRms_AbvGrd))/sd(ames_train$TotRms_AbvGrd))

ames_test <- ames_test_scaled %>% 
  mutate(pred = predict(model1, newdata = ames_test_scaled),
         pred_scaled = predict(model1_scaled, newdata = ames_test_scaled))

ames_test%>%
  select(pred,pred_scaled)

