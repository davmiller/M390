library(tidyverse)

#------------------------------------------------------------------------------------------------------------------------------------
# Gradient Descent Algorithm as a function
grad_descent <- function(x, y, alpha, epsilon){
  
  # Define initial starting values. 
  beta0 <- 0
  beta1 <- 0
  
  # Find n to be used in gradient calculation
  n <- length(x)
  
  # Compute gradient at initial point
  grad0 <- -2/n*sum(y-(beta0+beta1*x))
  grad1 <- -2/n*sum(x*(y-(beta0+beta1*x)))
  
  # While loop that searches for beta values until norm is under epsilon.  
  while(abs(grad0)+abs(grad1) > epsilon ){
    
    beta0 = beta0 -alpha*grad0
    beta1 = beta1 -alpha*grad1
    
    grad0 = -2/n*sum(y-(beta0+beta1*x))
    grad1 = -2/n*sum(x*(y-(beta0+beta1*x))) 
    
    # Stop while loop if it diverges
    if(abs(beta1)+abs(beta0)==Inf){break}
  }
  
  # Print error if diverges
  if(abs(beta1+abs(beta0))==Inf){
    return('Error: Gradient Descent diverges.  Check learning rate.')}
  
  # Otherwise print coefficients
  else{print(paste('beta0: ',round(beta0,4), ', ', 'beta1: ', round(beta1,4), sep=''))
    return(c(round(beta0,4),round(beta1,4)))}
}
#------------------------------------------------------------------------------------------------------------------------------------


# Running the grad descent function for mtcars.  learning rate = 0.00001, epsilon = 0.01
grad_descent(x = mtcars$hp, y = mtcars$mpg, alpha = 0.00001, epsilon = 0.001)


#------------------------------------------------------------------------------------------------------------------------------------
# Plotting the line using ggplot
coeffs <- grad_descent(x = mtcars$hp, y = mtcars$mpg, alpha = 0.00001, epsilon = 0.001)
# Create the equation for the line
f1 <- function(x) coeffs[1] + coeffs[2]*x

# Plot
ggplot(data=mtcars,aes(x=hp, y=mpg))+
  geom_point()+
  # Plot the line
  stat_function(fun = f1, color='blue')+
  labs(title='MPG vs. Horsepower from mtcars')
