# Build NCF model for MovieLens and Pinterest datasets

library(tidyverse)
library(pins)

# Read data ---------------------------------------------------------------

# notes: 
# - Pinterest (pinterest-20) and Movie Lense (ml-1m) data
# - user & item indexes are 0 based.
# - train.rating contains user/item/rating triplets (plus a timestamp)
# - test.negative contains a list of 100 negatives and one positive for each user
# - test.rating contains user/item/rating triplets (plus a timestamp) for 
#   each user's single rating in ml-1m.test.negative. 

movielense <- FALSE #set to F for pinterest data
if(movielense){
  file_prefix <- "ml-1m"  
}else{
  file_prefix <- "pinterest-20"   
}

base_name <- str_c("https://github.com/hexiangnan/neural_collaborative_filtering/raw/master/Data/", file_prefix)

url <- str_c(base_name, ".train.rating")
train_rating <- read_tsv(pins::pin(url), 
                         col_names = c("user","item", "rating", "timestamp")) 

url <-  str_c(base_name, ".test.negative")
test_data <- pins::pin(url) 
#test_rating   <- read_tsv(test_data[str_detect(test_data, "rating")], 
#                          col_names = c("user","item", "rating", "timestamp"))
test_negative <- read_tsv(test_data[str_detect(test_data, "negative")], 
                          col_names = FALSE) 

#See pins issue: https://github.com/rstudio/pins/issues/271
#url <- "https://github.com/hexiangnan/neural_collaborative_filtering/raw/master/Data/ml-1m.test.negative"
#test_negative <- read_tsv(pins::pin(url), 
#                          col_names = FALSE) 


# Variable definitions ----------------------------------------------------

num_users <- max(train_rating$user) + 1
num_items <- max(train_rating$item) + 1
neg_pos_ratio_train <- 4 # Ratio of negative training samples to positive to select for training. NCF authors used 4.


# Data wrangling ----------------------------------------------------------

# Test set
test <- test_negative %>% 
  tidyr::extract(X1, into = c("user", "pos_item"), "([[:alnum:]]+),([[:alnum:]]+)", convert = TRUE) %>%
  pivot_longer(cols = pos_item:X100, names_to = "label", values_to = "item") %>%
  mutate(label = as.integer(!str_detect(label,"X")))

# Get number of training ratings per user (will need for sampling negatives)
num_ratings_per_user <- train_rating %>% group_by(user) %>% count() %>% ungroup()

# Sample negatives (those movies that were not rated for each user) to use for training 
# TODO: Method below will likely be problematic for larger datasets. Also, this sampling should be done for each epoch..
tictoc::tic()
train_negative <- 
  lazy_dt(data.frame(user = rep(0:(num_users-1), each=num_items), 
             item = 0:(num_items-1))) %>% # start by listing all user/item pairs 
  anti_join(bind_rows(train_rating, test))  %>% # remove user/item pairs that are in test and positive training sets 
  as_tibble() %>% #because dtdplyr doesn't have nest functionality
  group_by(user) %>%  # Remaining operations used for sampling some of the negatives based on chosen negative to positive ratio
  nest() %>%  #TODO: use data.table function here so that we don't have to convert to tibble
  lazy_dt() %>%
  inner_join(num_ratings_per_user) %>%
  mutate(subsamp = map2(data, n*neg_pos_ratio_train, ~slice_sample(.x, n=.y))) %>% 
  select(user, subsamp) %>%
  as_tibble() %>%
  unnest(cols = c(subsamp))
tictoc::toc() #ml dataset: 20 sec (dplyr,purrr), 14 sec (dtplyr) pinterest: 430 sec (dplyr), too long (dplyr, furrr), 147 sec for dtplyr


# Define validation data by picking the most recent rating for each user from training
validation <- train_rating %>% 
  group_by(user) %>% 
  slice_max(timestamp) %>% 
  slice_sample(n = 1) %>% #some user/item pairs have same timestamp, so randomly pick one
  select(user, item) %>%
  add_column(label = 1)  # Only positive class was sampled for validation. See section 4.1 of NCF paper.

# Define training as data not used for validation
train <- anti_join(train_rating, validation) %>% 
  select(user, item) %>%
  bind_rows("pos" = ., "neg" = train_negative, .id = "label") %>%
  mutate(label = as.integer(str_detect(label,"pos")))



# Define model ------------------------------------------------------------

source("NCF.R")
model <- ncf_model(num_users = max(train_rating$user) + 1, 
                   num_items = max(train_rating$item) + 1)


# Train model -------------------------------------------------------------

# First define callbacks to stop model early when validation loss increases and to save best model
callback_list <- list(
  callback_early_stopping(patience = 2),
  callback_model_checkpoint(filepath = "model.h5", 
                            monitor = "val_loss", 
                            save_best_only = TRUE)
)

# Train model
history <- 
  model %>% 
  fit(
    x = list(user_input = as.array(train$user), 
             item_input = as.array(train$item)),
    y = as.array(train$label),
    epochs = 10,
    batch_size = 2048, 
    validation_data = list(list(user_input = as.array(validation$user), 
                                item_input = as.array(validation$item)), 
                           as.array(validation$label)),
    shuffle = TRUE, 
    callbacks = callback_list
  ) 

# Load best model:
model <- load_model_hdf5("model.h5")



# Evaluate results --------------------------------------------------------

history
#plot(history)

# Evaluate returns same metrics that were defined in the compile (accuracy in this case)
(results <- model %>% evaluate(list(test$user, test$item), test$label))

# Get predictions for test set:
test_pred <- model %>% 
  predict(x = list(test$user, test$item)) %>%
  bind_cols(pred = ., test)

# Compute hit rate and ndcg
source("evaluation.R")
compute_hr(test_pred, 10)
compute_ndcg(test_pred, 10)
