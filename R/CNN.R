library(keras)
install_keras()
library(tensorflow)
install_tensorflow(version = "1.12")
library(tidyverse)
library(gridExtra)

fashion_mnist <- dataset_fashion_mnist()

x_train <- fashion_mnist$train$x # Training inputs
y_train <- fashion_mnist$train$y # Training labels
x_test <- fashion_mnist$test$x # Test inputs
y_test <- fashion_mnist$test$y # Test labels

# Look at dimensions
dim(x_train)


#---------------------------------------------------------------------------------------------------------------
# Prepare the data

# rescale so the grayscale values (0 to 255) are converted between 0 and 1
x_train <- x_train / 255
x_test <- x_test / 255

x_train <- array_reshape(x_train, c(dim(x_train),1))
# Look at dimensions
dim(x_train)


# One-hot encode (transform) vector of labels to binary class matrices
y_train <- to_categorical(y_train, 10)
#---------------------------------------------------------------------------------------------------------------

# Build model

model <- keras_model_sequential() 
# Model structure
model %>%
  layer_conv_2d(filter=32,kernel_size=c(3,3), activation = "relu",padding="same", input_shape=c(28,28,1)) %>%  # Convolution layer
  layer_max_pooling_2d(pool_size=c(2,2)) %>%  # Max pooling layer
  layer_dropout(0.4) %>% # Dropout to avoid overfitting
  layer_flatten() %>% # Flatten the input
  layer_dense(units = 256, activation = 'relu') %>% # Hidden layer 1
  layer_dropout(0.3) %>%
  layer_dense(units = 10, activation = 'softmax') # For the 10 options 0-9, output layer

# Back propogation
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


learn <- model %>% fit(
  x_train, y_train, 
  epochs = 15, 
  batch_size = 128, 
  validation_split = 0.2
)

learn

#---------------------------------------------------------------------------------------------------------------

# ON YOUR OWN:  Add convolution layers / alter structure to avoid overfitting with better validation accuracy than the MLP.
