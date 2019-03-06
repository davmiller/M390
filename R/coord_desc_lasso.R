
# Function
coord_desc_lasso <- function(X,Y, epsilon, lambda, drop_int=FALSE){
  n <- nrow(Y)
  m <- ncol(X)
  
  if (drop_int==FALSE){
    # Add column of 1's for 
    X <- matrix(data=c(matrix(1,n),X),ncol=(m+1))  
    # Initialize b vector
    b <- matrix(integer(m+1))
    m=m+1
  }
  else{
    b <- matrix(integer(m))}
  
  # One iteration
  b_old <- b
  for (j in 1:(m)){
    Xj <- matrix(X[,-j],ncol=(m-1)) # Take out column j of X
    bj <- matrix(b[-j,], ncol=1) # Take out jth element of b vector
    XJ <- matrix(X[,j],ncol=1) # Only column j of X
    
    r <- Y-Xj%*%bj # Residual without column j
    pj <- t(XJ)%*%r # Rho j
    zj <- t(XJ)%*%XJ # Denominator ||X||_2
    
    # Update b using soft-thresholding
    b[j,1] <- case_when(
      pj < -n*lambda/2 ~ (pj+n*lambda/2)/zj,
      pj > n*lambda/2 ~ (pj-n*lambda/2)/zj,
      TRUE ~ 0
    )
  }
  # Norm function for while loop
  norm <- function(x){sum(abs(x))}
  
  # More steps until convergence
  while(norm(b_old-b)>epsilon){
    b_old <- b
    for (j in 1:(m)){
      Xj <- matrix(X[,-j],ncol=(m-1)) # Take out column j of X
      bj <- matrix(b[-j,], ncol=1) # Take out jth element of b vector
      XJ <- matrix(X[,j],ncol=1) # Only column j of X
      
      r <- Y-Xj%*%bj # Residual without column j
      pj <- t(XJ)%*%r # Rho j
      zj <- t(XJ)%*%XJ # Denominator ||X||_2
      
      # Update b using soft-thresholding
      b[j,1] <- case_when(
        pj < -n*lambda/2 ~ (pj+n*lambda/2)/zj,
        pj > n*lambda/2 ~ (pj-n*lambda/2)/zj,
        TRUE ~ 0
      )
    }
  }
  return(b)
}
