# Ridge grad descent
grad_descent_ridge <- function(Y,X,alpha,epsilon,lambda, drop_int=FALSE){
  # Standardize X inputs
  X <- scale(X,center= TRUE, scale=TRUE)
  n <- nrow(Y)
  m <- ncol(X)
  if (drop_int==FALSE){
    # Add column of 1's for 
    X <- matrix(data=c(matrix(1,n),X),ncol=(m+1))  
    # Initialize b vector
    b <- matrix(integer(m+1))
  }
  else{
    b <- matrix(integer(m))}
  
  # Calculuate gradient
  gradC <- (-2/n)*(t(X)%*%(Y - X%*%b))+lambda*2*b
  
  norm <- function(x){sum(abs(x))}
  
  # update b vector
  while(norm(gradC) > epsilon ){
    
    b = b -alpha*gradC
    if (drop_int==FALSE){
      # New B vector to use in ridge regression.  Replacing b_0 = 0 so to not penalize intercept in ridge
      B = matrix(c(0,b[2:(m+1),]))}
    else{B=b}
    
    gradC <- (-2/n)*(t(X)%*%(Y - X%*%b))+lambda*2*B
    # Stop while loop if it diverges
    if(norm(b)==Inf){break}
  }
  
  # Print error if diverges
  if(norm(b)==Inf){
    return('Error: Gradient Descent diverges.  Check learning rate.')}
  
  # Otherwise print coefficients
  return(b)
}

# Example
# Make Ames housing data
Ames <- AmesHousing::make_ames()
# Train / test split
set.seed(123)
index   <- sample(1:nrow(Ames), round(nrow(Ames) * 0.7))
ames_train <- Ames[index, ]
ames_test  <- Ames[-index, ]
# Ames grad descent 
Y <- matrix(ames_train$Sale_Price) 
X <- matrix(data=c(ames_train$Gr_Liv_Area_scaled, ames_train$TotRms_AbvGrd_scaled), ncol=2)

# 
coeffs <- grad_descent_ridge(Y,X,0.001,.001,lambda=0)
