# Function to perform k-fold cross validation for one variable polynomial linear regression
CV_k_polynomial<- function(data,Y,X,max_degree,k){
  
  # Create new dataframe with two columns: x = indicator input, y = response input
  df <- data.frame(x=data[,X], y=data[,Y])
  
  # Shuffle the dataframe
  shuffled <- df[sample(1:nrow(df)),]
  
  # Get the fold length
  pct <- 1/k
  fld_len <- floor(nrow(shuffled)*pct)
  
  # Perform train / test split and model building over the k folds
  for (m in 1:k){
    # Train / test split over the m-th fold
    index <- c(((m-1)*fld_len+1):((m)*fld_len))
    test <- shuffled[index,]
    train <- shuffled[-index,]
    
    # Linear model
    model1 <- lm(y ~ x, data=train)
    
    # MSE on training set
    train_obs <- train$y
    train_pred <- predict(model1, newdata = train)
    train_mse <- round(mean((train_obs-train_pred)^2),4)
    
    # MSE on test set
    test_obs <- test$y
    test_pred <- predict(model1, newdata = test)
    test_mse <- round(mean((test_obs-test_pred)^2),4)
    
    # Create list of MSEs
    mse_train <- c(train_mse)
    mse_test <- c(test_mse)
    index <- c(1)
    
    # Repeat on polynomials of varying degree
    for (i in 2:max_degree){
      # Polynomial model
      poly_fit <- lm(y ~ poly(x,i), data=train)
      
      # Train MSE
      train_pred <- predict(poly_fit, newdata = train)
      train_mse <- round(mean((train_obs-train_pred)^2),4)
      
      # Test MSE
      test_pred <- predict(poly_fit, newdata = test)
      test_mse <- round(mean((test_obs-test_pred)^2),4)
      
      # Append the MSE lists to include new models
      mse_train <- c(mse_train,train_mse)
      mse_test <- c(mse_test,test_mse)
      index <- c(index,i)
    }
    
    # Add MSEs to take average eventually
    if (m==1){
      train_sum <- mse_train
      test_sum <- mse_test
    }
    else{
      train_sum <- train_sum+mse_train
      test_sum <- test_sum+mse_test
    }
  }
  
  # Average MSEs
  train_av <- train_sum/k
  test_av <- test_sum/k
  
  # Create dataframe of average MSEs
  MSE_av <- data.frame(degree = index, train_mse_av=train_av, test_mse_av=test_av)
  
  # Return the table of average MSEs
  return(MSE_av)
}

# Example
CV_k_polynomial(mtcars,'mpg','hp',max_degree=10,k=5)


