#install.packages("keras")
#install.packages('tensorflow')
library(keras)
install_keras()
library(tensorflow)
install_tensorflow(version = "1.12")
library(tidyverse)
library(gridExtra)



# Bring in mnist dataset
mnist <- dataset_mnist()
# Bring in train and test sets
x_train <- mnist$train$x # Training inputs
y_train <- mnist$train$y # Training labels
x_test <- mnist$test$x # Test inputs
y_test <- mnist$test$y # Test labels

# Look at the dimension of the training set
dim(x_train)
# [1] 60000    28    28
# There are 60000 images, all of which are 28 x 28 (images, width, height) of grayscale values.

#---------------------------------------------------------------------------------------------------------------
# Bring in first image
image_1 <- as.data.frame(x_train[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))

# Look at first image
image_1 %>% View('Image 1')

# Transform image data to look at it in ggplot
image_1$y <- seq_len(nrow(image_1)) # Add row numbers to image_1
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.numeric(image_1$x)

# Plot the image
ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile(show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")
#---------------------------------------------------------------------------------------------------------------

# ON YOUR OWN: Look at some more images from the training set using ggplot


#---------------------------------------------------------------------------------------------------------------

# Look at more pictures in a grid
p <- list()
for (i in 1:12){
  img <- as.data.frame(x_train[i, , ])
  colnames(img) <- seq_len(ncol(img))
  img$y <- seq_len(nrow(img)) # Add row numbers to image_1
  img <- gather(img, "x", "value", -y) 
  img$x <- as.numeric(img$x)
  
  # Plot the image
  p[[i]] <- ggplot(img, aes(x = x, y = y, fill = value)) +
    geom_tile(show.legend = FALSE) +
    scale_fill_gradient(low = "white", high = "black", na.value = NA) +
    scale_y_reverse() +
    theme_minimal() +
    theme(panel.grid = element_blank())   +
    theme(aspect.ratio = 1) +
    xlab("") +
    ylab("")+
    labs(title = paste('Label:', y_train[i]))
}
do.call(grid.arrange,p)
















#---------------------------------------------------------------------------------------------------------------
# Prepare the data

# reshape to flatten each 28 x 28 picture to one row of data: 1 x 784
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

# rescale so the grayscale values (0 to 255) are converted between 0 and 1
x_train <- x_train / 255
x_test <- x_test / 255

# One-hot encode (transform) vector of labels to binary class matrices
y_train <- to_categorical(y_train, 10)

#---------------------------------------------------------------------------------------------------------------
# Build model

model <- keras_model_sequential() 

model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dense(units = 10, activation = 'softmax') # For the 10 options 0-9


model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


learn <- model %>% fit(
  x_train, y_train, 
  epochs = 30, 
  batch_size = 128, 
  validation_split = 0.2
)

learn

# Trained on 48,000 samples, validated on 12,000 samples (batch_size=128, epochs=30)
# Final epoch (plot to see history):
# val_loss: 0.126
# val_acc: 0.9805
# loss: 0.0002489
# acc: 1 

# Overfit

#---------------------------------------------------------------------------------------------------------------
model <- keras_model_sequential() 
# Add second layer
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax') # For the 10 options 0-9

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


learn <- model %>% fit(
  x_train, y_train, 
  epochs = 30, 
  batch_size = 128, 
  validation_split = 0.2
)


learn


# Trained on 48,000 samples, validated on 12,000 samples (batch_size=128, epochs=30)
# Final epoch (plot to see history):
# val_loss: 0.1588
# val_acc: 0.9807
# loss: 0.00136
# acc: 0.9996 


# Still overfitting


#---------------------------------------------------------------------------------------------------------------
model <- keras_model_sequential() 
# Add dropout layers
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')


model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


learn <- model %>% fit(
  x_train, 
  y_train, 
  epochs = 30, 
  batch_size = 128, 
  validation_split = 0.2
)

learn

# Trained on 48,000 samples, validated on 12,000 samples (batch_size=128, epochs=30)
# Final epoch (plot to see history):
#   val_loss: 0.1133
# val_acc: 0.9806
# loss: 0.05082
# acc: 0.9869 

# Much better

plot(learn)


#---------------------------------------------------------------------------------------------------------------
model <- keras_model_sequential() 
# Add dropout layers
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')


model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


learn <- model %>% fit(
  x_train, 
  y_train, 
  epochs = 30, 
  batch_size = 128, 
  validation_split = 0.2,
  callbacks = list(
    callback_early_stopping(patience = 2)
  )
)

learn

# Trained on 48,000 samples, validated on 12,000 samples (batch_size=128, epochs=9)
# Final epoch (plot to see history):
#   val_loss: 0.09899
# val_acc: 0.9762
# loss: 0.08545
# acc: 0.9756 

# Similar performance using fewer epochs

plot(learn)


#---------------------------------------------------------------------------------------------------------------

# Predict on the test set
x_test_pred <- model %>% predict_classes(x_test)
# Classification rate on the test set
mean(x_test_pred == y_test)
# [1] 0.9775


#---------------------------------------------------------------------------------------------------------------
# Look at some test images and their predictions
x_test_pic <- mnist$test$x # Test inputs

p <- list()
for (i in 1:12){
  img <- as.data.frame(x_test_pic[i, , ])
  colnames(img) <- seq_len(ncol(img))
  img$y <- seq_len(nrow(img)) # Add row numbers to image_1
  img <- gather(img, "x", "value", -y) 
  img$x <- as.numeric(img$x)
  
  # Plot the image
  p[[i]] <- ggplot(img, aes(x = x, y = y, fill = value)) +
    geom_tile(show.legend = FALSE) +
    scale_fill_gradient(low = "white", high = "black", na.value = NA) +
    scale_y_reverse() +
    theme_minimal() +
    theme(panel.grid = element_blank())   +
    theme(aspect.ratio = 1) +
    xlab("") +
    ylab("")+
    labs(title = paste('Label:', y_test[i], 'Pred:', x_test_pred[i]))
}
do.call(grid.arrange,p)

#---------------------------------------------------------------------------------------------------------------
# Find which were wrong
wrong <- which(x_test_pred != y_test)

# Look at some:

p <- list()
for (i in 1:12){
  img <- as.data.frame(x_test_pic[wrong[i], , ])
  colnames(img) <- seq_len(ncol(img))
  img$y <- seq_len(nrow(img)) # Add row numbers to image_1
  img <- gather(img, "x", "value", -y) 
  img$x <- as.numeric(img$x)
  
  # Plot the image
  p[[i]] <- ggplot(img, aes(x = x, y = y, fill = value)) +
    geom_tile(show.legend = FALSE) +
    scale_fill_gradient(low = "white", high = "black", na.value = NA) +
    scale_y_reverse() +
    theme_minimal() +
    theme(panel.grid = element_blank())   +
    theme(aspect.ratio = 1) +
    xlab("") +
    ylab("")+
    labs(title = paste('Label:', y_test[wrong[i]], 'Pred:', x_test_pred[wrong[i]]))
}
do.call(grid.arrange,p)




