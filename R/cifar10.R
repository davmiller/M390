
cifar10 <- dataset_cifar10()

x_train <- cifar10$train$x # Training inputs
y_train <- cifar10$train$y # Training labels
x_test <- cifar10$test$x # Test inputs
y_test <- cifar10$test$y # Test labels

dim(x_train)


pic = 4
image_1 <- as.data.frame(x_train[pic, , ,1 ])
colnames(image_1) <- seq_len(ncol(image_1))

# Transform image data to look at it in ggplot
image_1$y <- seq_len(nrow(image_1)) # Add row numbers to image_1
image_1 <- gather(image_1, "x", "r", -y)
image_1$x <- as.numeric(image_1$x)

k <- 1
for (i in 1:32){
  for (j in 1:32){
    image_1$g[k] <- x_train[pic,j,i,2]
    image_1$b[k] <- x_train[pic,j,i,3]
    k <- k+1
  }
}


ggplot(data=image_1, aes(x=x, y=y, fill=rgb(r,g,b, maxColorValue = 255))) +
  scale_y_reverse() +
  geom_tile() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")+
  scale_fill_identity()




# Look at more pictures in a grid
p <- list()
for (pic in 1:12){
  image_1 <- as.data.frame(x_train[pic, , ,1 ])
  colnames(image_1) <- seq_len(ncol(image_1))
  
  # Transform image data to look at it in ggplot
  image_1$y <- seq_len(nrow(image_1)) # Add row numbers to image_1
  image_1 <- gather(image_1, "x", "r", -y)
  image_1$x <- as.numeric(image_1$x)
  
  k <- 1
  for (i in 1:32){
    for (j in 1:32){
      image_1$g[k] <- x_train[pic,j,i,2]
      image_1$b[k] <- x_train[pic,j,i,3]
      k <- k+1
    }
  }
  
  # Plot the image
  p[[pic]] <- 
    ggplot(data=image_1, aes(x=x, y=y, fill=rgb(r,g,b, maxColorValue = 255))) +
    scale_y_reverse() +
    geom_tile() +
    theme_minimal() +
    theme(panel.grid = element_blank())   +
    theme(aspect.ratio = 1) +
    xlab("") +
    ylab("")+
    scale_fill_identity()+
    labs(title = paste('Label:', y_train[pic]))
}
do.call(grid.arrange,p)
