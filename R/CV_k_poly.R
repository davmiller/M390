CV_k_polynomial<- function(data,Y,X,max_degree,k){
  
  df <- data.frame(x=data[,X], y=data[,Y])
  shuffled <- df[sample(1:nrow(df)),]
  pct <- 1/k
  fld_len <- floor(nrow(shuffled)*pct)
  
  for (m in 1:5){
    index <- c(((m-1)*fld_len+1):((m)*fld_len))
    test <- shuffled[index,]
    train <- shuffled[-index,]
    
    model1 <- lm(y ~ x, data=train)
    #model10 <- lm(mpg ~ hp, data = train)
    
    train_obs <- train$y
    train_pred <- predict(model1, newdata = train)
    train_mse <- round(mean((train_obs-train_pred)^2),4)
    
    test_obs <- test$y
    test_pred <- predict(model1, newdata = test)
    test_mse <- round(mean((test_obs-test_pred)^2),4)
    
    mse_train <- c(train_mse)
    mse_test <- c(test_mse)
    index <- c(1)
    
    for (i in 2:max_degree){
      poly_fit <- lm(y ~ poly(x,i), data=train)
      train_pred <- predict(poly_fit, newdata = train)
      train_mse <- round(mean((train_obs-train_pred)^2),4)
      
      test_pred <- predict(poly_fit, newdata = test)
      test_mse <- round(mean((test_obs-test_pred)^2),4)
      
      mse_train <- c(mse_train,train_mse)
      mse_test <- c(mse_test,test_mse)
      index <- c(index,i)
    }
    
    if (m==1){
      train_sum <- mse_train
      test_sum <- mse_test
    }
    else{
      train_sum <- train_sum+mse_train
      test_sum <- test_sum+mse_test
    }
  }
  
  train_av <- train_sum/k
  test_av <- test_sum/k
  
  newdata <- data.frame(degree = index, train_mse_av=train_av, test_mse_av=test_av)
  
  return(newdata)
}

# Example
CV_k_polynomial(mtcars,'mpg','hp',max_degree=10,k=5)


