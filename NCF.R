
# Load libraries ----------------------------------------------------------

options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"))
install.packages("keras")

install.packages("remotes")
remotes::install_github("rstudio/pins")

library(pins)
library(tidyverse)
library(keras)


# Read data ---------------------------------------------------------------

url <- "https://github.com/hexiangnan/neural_collaborative_filtering/raw/master/Data/ml-1m.train.rating"
train_rating <- read_tsv(pins::pin(url), 
                         col_names = c("user","item", "rating", "timestamp")) 
#notes: user & item numbers are 0 based.

url <-  "https://github.com/hexiangnan/neural_collaborative_filtering/raw/master/Data/ml-1m.test.rating"
test_data <- pins::pin(url) 
test_rating   <- read_tsv(test_data[str_detect(test_data, "rating")], 
                          col_names = c("user","item", "rating", "timestamp"))
test_negative <- read_tsv(test_data[str_detect(test_data, "negative")], 
                          col_names = FALSE) %>% 
  extract(X1, into = c("user", "pos_item"), "([[:alnum:]]+),([[:alnum:]]+)")

#See pins issue: https://github.com/rstudio/pins/issues/271
#url <- "https://github.com/hexiangnan/neural_collaborative_filtering/raw/master/Data/ml-1m.test.negative"
#test_negative <- read_tsv(pins::pin(url), 
#                          col_names = FALSE) #TODO: check that these column names are accurate. I just guessed.


# Setup ----------------------------------------------------

num_users <- max(train_rating$user) + 1
num_items <- max(train_rating$item) + 1
capacity <- 32  

# GMF ---------------------------------------------------------------------

gmf_embedding_dim <- 2*capacity 
mf_user_embedding <- layer_input(shape=1) %>%  #pass in user indexes (not binary vectors)
  layer_embedding(input_dim = gmf_embedding_dim, output_dim = num_users) 
mf_item_embedding <- layer_input(shape=1) %>%  #pass in item indexes (not binary vectors)
  layer_embedding(input_dim = gmf_embedding_dim, output_dim = num_items) 

gmf_branch <- layer_multiply(list(mf_user_embedding, mf_item_embedding))


# MLP ---------------------------------------------------------------------

mlp_embedding_dim <- 2*capacity
mlp_user_embedding <- layer_input(shape=1) %>% 
  layer_embedding(input_dim = num_users, output_dim = mlp_embedding_dim)
mlp_item_embedding <- layer_input(shape=1) %>% 
  layer_embedding(input_dim = num_items, output_dim = mlp_embedding_dim)

mlp_branch <- layer_concatenate(list(mlp_user_embedding, mlp_item_embedding)) %>%
  layer_dense(units = 4*capacity, activation = "relu") %>%
  layer_dense(units = 2*capacity, activation = "relu") %>%
  layer_dense(units = capacity, activation = "relu") 


# NeuMF -------------------------------------------------------------------

model <- layer_concatenate(list(gmf_branch, mlp_branch), trainable = TRUE) %>%
  layer_dense(units = 1, activation = "sigmoid")    


# Compile and Fit model ---------------------------------------------------

model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy") #TODO: add my own here? HR and NDCG.
)

history <- model %>% fit(
  x_train,
  y_train,
  epochs = 10,
  batch_size = TODO, #what did paper use?
  validation_data = list(x_val, y_val)
)

plot(hisotry)

(results <- model %>% evaluate(x_test, y_test))





