library(tidyverse)
# rock and trees example

# Linear model on rock shape ~ area-----------------------------------------------------
set.seed(123)
index   <- sample(1:nrow(rock), round(nrow(rock) * 0.7))
train_1 <- rock[index, ]
test_1  <- rock[-index, ]

model1 <- lm(shape ~ area, data=train_1)

train_1 <- train_1%>%
  mutate(pred = predict(model1))

test_1 <- test_1%>%
  mutate(pred = predict(model1, newdata = test_1))


mse_train_1 <- round(mean((train_1$shape-train_1$pred)^2),4)
mse_test_1 <- round(mean((test_1$shape-test_1$pred)^2),4)
# ----------------------------------------------------------------------------------------------------------

# Linear model on trees Volume~Girth -----------------------------------------------------
set.seed(123)
index   <- sample(1:nrow(trees), round(nrow(trees) * 0.7))
train_2 <- trees[index, ]
test_2  <- trees[-index, ]
model2 <- lm(Volume~Girth, data=train_2)

train_2 <- train_2%>%
  mutate(pred = predict(model2))

test_2 <- test_2%>%
  mutate(pred = predict(model2, newdata = test_2))

mse_train_2 <- round(mean((train_2$Volume-train_2$pred)^2),4)
mse_test_2 <- round(mean((test_2$Volume-test_2$pred)^2),4)
# ----------------------------------------------------------------------------------------------------------

# Compare the MSES
data.frame(train_mse=c(mse_train_1), test_mse=c(mse_test_1))
data.frame(train_mse=c(mse_train_2), test_mse=c(mse_test_2))
# ----------------------------------------------------------------------------------------------------------

# Look at datasets
rock %>% 
  ggplot(aes(x=area,y=shape))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

trees %>% 
  ggplot(aes(x=Girth,y=Volume))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)
# ----------------------------------------------------------------------------------------------------------

# Look at summary of each model
summary(model1)
summary(model2)
