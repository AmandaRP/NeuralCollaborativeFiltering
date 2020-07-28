# This script implements neural collaborative filtering using R Keras. Returns a model.

# Depends on the following variables derived from data: num_users, num_items

# Load libraries ----------------------------------------------------------

#options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"))
#install.packages("keras")
#install.packages("remotes")
#remotes::install_github("rstudio/pins")

library(tidyverse)
library(keras)

# Variable definitions ----------------------------------------------------

# Embedding dimensions
gmf_embedding_dim <- 64  
mlp_embedding_dim <- 64

# Value to use for N(0, sigma) initializers
sigma <- 0.01

# Regularization parameter. Set to value > 0 to include regularization.
lambda <- 0

# Define network inputs -----------------------------------------------------------

#pass in user/item indexes (not one-hot vectors) b/c this is required by embedding layer
user_input <- layer_input(shape=1, name = "user_input") 
item_input <- layer_input(shape=1, name = "item_input")

# GMF ---------------------------------------------------------------------

gmf_user_embedding <- 
  user_input %>%  
  layer_embedding(input_dim = num_users, # "dictionary" size
                  output_dim = gmf_embedding_dim,
                  embeddings_initializer = initializer_random_normal(0, sigma), # Use N(0,sigma) initialization  
                  # Paper did not mention regularization, but it is included in authors' code. Note sure what value they used, but their default is 0.
                  embeddings_regularizer = regularizer_l2(lambda), 
                  input_length = 1,  # the length of the sequence that is being fed in (one integer)
                  name = "gmf_user_embedding") 
#Embedding is a 3D tensor. Need to convert to 2D to feed into dense layer:
gmf_user_latent <- gmf_user_embedding %>% layer_flatten(name = "gmf_user_latent")

gmf_item_embedding <- 
  item_input %>%
  layer_embedding(input_dim = num_items, 
                  output_dim = gmf_embedding_dim,
                  embeddings_initializer = initializer_random_normal(0, sigma),   
                  embeddings_regularizer = regularizer_l2(lambda),
                  input_length=1,
                  name = "gmf_item_embedding") 
gmf_item_latent <- gmf_item_embedding %>% layer_flatten(name = "gmf_item_latent") 

gmf_branch <- layer_multiply(list(gmf_user_latent, gmf_item_latent))

# MLP ---------------------------------------------------------------------

mlp_user_embedding <- 
  user_input %>% 
  layer_embedding(input_dim = num_users, 
                  output_dim = mlp_embedding_dim,
                  embeddings_initializer = initializer_random_normal(0, sigma), 
                  embeddings_regularizer = regularizer_l2(lambda), 
                  input_length=1,
                  name = "mlp_user_embedding") 
mlp_user_latent <- mlp_user_embedding %>% layer_flatten(name = "mlp_user_latent")

mlp_item_embedding <- 
  item_input %>% 
  layer_embedding(input_dim = num_items, 
                  output_dim = mlp_embedding_dim,
                  embeddings_initializer = initializer_random_normal(0, sigma), 
                  embeddings_regularizer = regularizer_l2(lambda), 
                  input_length=1,
                  name = "mlp_item_embedding") 
mlp_item_latent <- mlp_item_embedding %>% layer_flatten(name = "mlp_item_latent") 

mlp_branch <- 
  layer_concatenate(list(mlp_user_latent, mlp_item_latent)) %>%
  layer_dense(units = 2 * gmf_embedding_dim, 
              activation = "relu", 
              kernel_regularizer = regularizer_l2(lambda),
              name = "mlp_layer1") %>%
  layer_dense(units = gmf_embedding_dim, 
              activation = "relu", 
              kernel_regularizer = regularizer_l2(lambda),
              name = "mlp_layer2") %>% 
  layer_dense(units = 0.5*gmf_embedding_dim, 
              activation = "relu", 
              kernel_regularizer = regularizer_l2(lambda),
              name = "mlp_layer3")     


# NeuMF -------------------------------------------------------------------

label <- 
  layer_concatenate(list(gmf_branch, mlp_branch)) %>%
  layer_dense(units = 1, 
              activation = "sigmoid", 
              kernel_initializer = "lecun_uniform",
              name = "prediction")    

# For non-sequential models, need to specify inputs and outputs: 
model <- keras_model(list(user_input, item_input), label)


# Compile model ---------------------------------------------------

model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy", 
  metrics = c("accuracy")
)

summary(model)










