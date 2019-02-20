library(tidyverse)


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
X <- matrix(data=c(ames_train$Gr_Liv_Area, ames_train$TotRms_AbvGrd), ncol=2)




lm(Y~X+0)



# Coordinate descent
n <- nrow(Y)
m <- ncol(X)
# Initialize b vector
b <- matrix(integer(m))

for(step in 1:200){
  for (j in 1:(m)){
    r <- Y-matrix(X[,-j],ncol=(m-1))%*%matrix(b[-j,], ncol=1)
    xx <- t(matrix(X[,j],ncol=1))%*%matrix(X[,j],ncol=1)
    b[j,1] <- (t(matrix(X[,j],ncol=1))%*%r)/xx
  }
  if(step==1){
    b1=b[1]
    b2=b[2]
    steps=1
  }
  else{
    b1=c(b1,b[1])
    b2=c(b2,b[2])
    steps=c(steps,step)
  }
}

# Plot coefficients over time
data.frame(steps=steps, b1=b1,b2=b2)%>%
  ggplot()+
  geom_line(aes(x=steps, y=b1), color='blue')+
  geom_line(aes(x=steps, y=b2), color='red')+
  ylab('coefficients')



# Function
coord_desc <- function(X,Y, epsilon){
  n <- nrow(Y)
  m <- ncol(X)
  # Initialize b vector
  b <- matrix(integer(m))
  
  # One iteration
  b_old <- b
  for (j in 1:(m)){
    r <- Y-matrix(X[,-j],ncol=(m-1))%*%matrix(b[-j,], ncol=1)
    xx <- t(matrix(X[,j],ncol=1))%*%matrix(X[,j],ncol=1)
    b[j,1] <- (t(matrix(X[,j],ncol=1))%*%r)/xx
  }
  # Norm function for while loop
  norm <- function(x){sum(abs(x))}

  # More steps until convergence
  while(norm(b_old-b)>epsilon){
    b_old <- b
    for (j in 1:(m)){
      r <- Y-matrix(X[,-j],ncol=(m-1))%*%matrix(b[-j,], ncol=1)
      xx <- t(matrix(X[,j],ncol=1))%*%matrix(X[,j],ncol=1)
      b[j,1] <- (t(matrix(X[,j],ncol=1))%*%r)/xx
    }
  }
  return(b)
}

coord_desc(X,Y,0.001)



