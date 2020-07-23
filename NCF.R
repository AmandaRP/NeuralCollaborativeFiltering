# This R script implements neural collaborative filtering using R Keras.

# Load libraries ----------------------------------------------------------

#options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"))
#install.packages("keras")
#install.packages("remotes")
#remotes::install_github("rstudio/pins")

library(tidyverse)
library(keras)

# Variable definitions ----------------------------------------------------

capacity <- 32  

# Define network inputs -----------------------------------------------------------

#pass in user/item indexes (not one-hot vectors) b/c this is required by embedding layer
user_input <- layer_input(shape=1) 
item_input <- layer_input(shape=1)

# GMF ---------------------------------------------------------------------

gmf_embedding_dim <- 2*capacity 
gmf_user_embedding <- 
  user_input %>%  
  layer_embedding(input_dim = num_users, output_dim = gmf_embedding_dim) #TODO: Is the input dim correct here?
gmf_item_embedding <- 
  item_input %>%
  layer_embedding(input_dim = num_items, output_dim = gmf_embedding_dim) 

gmf_branch <- layer_multiply(list(gmf_user_embedding, gmf_item_embedding))

# MLP ---------------------------------------------------------------------

mlp_embedding_dim <- 2*capacity
mlp_user_embedding <- 
  user_input %>% 
  layer_embedding(input_dim = num_users, output_dim = mlp_embedding_dim)
mlp_item_embedding <- 
  item_input %>% 
  layer_embedding(input_dim = num_items, output_dim = mlp_embedding_dim)

mlp_branch <- 
  layer_concatenate(list(mlp_user_embedding, mlp_item_embedding)) %>%
  layer_dense(units = 4*capacity, activation = "relu") %>%
  layer_dense(units = 2*capacity, activation = "relu") %>%
  layer_dense(units = capacity, activation = "relu") 

# NeuMF -------------------------------------------------------------------

output <- 
  layer_concatenate(list(gmf_branch, mlp_branch), trainable = TRUE) %>%
  layer_dense(units = 1, activation = "sigmoid")    

#Next step needed for non-sequential models
model <- keras_model(list(user_input, item_input), output)


# Compile and Fit model ---------------------------------------------------

model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy", #TODO: Does this work with index inputs (as opposed to binary)?
  metrics = c("accuracy") #TODO: add my own here? HR and NDCG.
)

history <- 
  model %>% 
  fit(
    x_train,
    y_train,
    epochs = 10,
    batch_size = TODO, #what did paper use?
    validation_data = list(x_val, y_val)
  ) 

plot(hisotry)

(results <- model %>% evaluate(x_test, y_test))










