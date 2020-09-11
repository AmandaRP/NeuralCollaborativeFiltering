# Build NCF model for MovieLens and Pinterest datasets

# Read data ---------------------------------------------------------------

# notes: 
# - Pinterest (pinterest-20) and Movie Lense (ml-1m) data
# - user & item indexes are 0 based.
# - train.rating contains user/item/rating triplets (plus a timestamp)
# - test.negative contains a list of 100 negatives and one positive for each user
# - test.rating contains user/item/rating triplets (plus a timestamp) for 
#   each user's single rating in ml-1m.test.negative. 

movielense <- TRUE #set to F for pinterest data
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
num_ratings_per_user <- train_rating %>% group_by(user) %>% count()

# Define negatives (those movies that were not rated for each user) to use for training 
# TODO: Redo this so that it doesn't take so much memory. Also, this sampling should be done for each epoch.
train_negative <- 
  data.frame(user = rep(0:(num_users-1), each=num_items), 
             item = 0:(num_items-1)) %>% # start by listing all user/item pairs 
  anti_join(train_rating) %>% # remove user/item pairs that are in positive training set 
  anti_join(test) %>%       # remove user/item pairs that are in test set
  group_by(user) %>%  # Remaining operations used for sampling some of the negatives based on chosen negative to positive ratio
  nest() %>%
  inner_join(num_ratings_per_user) %>%
  mutate(subsamp = map2(data, n*neg_pos_ratio_train, ~slice_sample(.x, n=.y))) %>% 
  select(user, subsamp) %>%
  unnest(cols = c(subsamp))

train_negative <- array(NA, dim = num_users)
for(u in 0:(num_users-1)){
  x <- test %>% filter(user == u) 
  y <- train_rating %>% filter(user == u) 
  cnt <- num_ratings_per_user %>% filter(user == u) %>% ungroup() 
  user_negative_items <- sample(setdiff(0:(num_items-1), c(x$item, y$item)), size = neg_pos_ratio_train * cnt$n) 
  train_negative[[u+1]] <- list(0) #necessary to avoid error on next line
  train_negative[[u+1]] <- tibble(user = rep(u, length(user_negative_items)), item =  user_negative_items)
}
train_negative <- bind_rows(unlist(train_negative))


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
                   num_items = max(train_rating$user) + 1)


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
