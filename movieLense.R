# This R script uses NCF to recommend movies to users based on implicit feedback
# in the movie lense dataset.

library(tidyverse)
library(magrittr)

# Read data ---------------------------------------------------------------

# notes: 
# - user & item indexes are 0 based.
# - ml-1m.train.rating contains user/item/rating triplets (plus a timestamp)
# - ml-1m.test.negative contains a list of 100 negatives and one positive for each user
# - ml-1m.test.rating contains user/item/rating triplets (plus a timestamp) for 
#   each user's single rating in ml-1m.test.negative. 

url <- "https://github.com/hexiangnan/neural_collaborative_filtering/raw/master/Data/ml-1m.train.rating"
train_rating <- read_tsv(pins::pin(url), 
                         col_names = c("user","item", "rating", "timestamp")) 

url <-  "https://github.com/hexiangnan/neural_collaborative_filtering/raw/master/Data/ml-1m.test.rating"
test_data <- pins::pin(url) 
test_rating   <- read_tsv(test_data[str_detect(test_data, "rating")], 
                          col_names = c("user","item", "rating", "timestamp"))
test_negative <- read_tsv(test_data[str_detect(test_data, "negative")], 
                          col_names = FALSE) 

#See pins issue: https://github.com/rstudio/pins/issues/271
#url <- "https://github.com/hexiangnan/neural_collaborative_filtering/raw/master/Data/ml-1m.test.negative"
#test_negative <- read_tsv(pins::pin(url), 
#                          col_names = FALSE) #TODO: check that these column names are accurate. I just guessed.

# Variable definitions ----------------------------------------------------

num_users <- max(train_rating$user) + 1
num_items <- max(train_rating$item) + 1
neg_pos_ratio_train <- 4 # Ratio of negative training samples to positive to select for training


# Define model --------------------------------------------------------------

# Source the code for the NCF model:
source("NCF.R")


# Data wrangling ----------------------------------------------------------

# Test set
test <- test_negative %>% 
  extract(X1, into = c("user", "pos_item"), "([[:alnum:]]+),([[:alnum:]]+)", convert = TRUE) %>%
  pivot_longer(cols = pos_item:X100, names_to = "label", values_to = "item") %>%
  mutate(label = as.integer(!str_detect(label,"X")))
test_x <- test %>% select(user, item)
test_y <- test %>% select(label)

# Get number of training ratings per user (will need for sampling negatives)
num_ratings_per_user <- train_rating %>% group_by(user) %>% count()

# Define negatives (those movies that were not rated for each user) to use for training 
train_negative <- 
  data.frame(user = rep(0:(num_users-1), each=num_items), 
             item = 0:(num_items-1)) %>% # start by listing all user/item pairs 
  anti_join(train_rating) %>% # remove user/item pairs that are in positive training set 
  anti_join(test_x) %>%       # remove user/item pairs that are in test set
  group_by(user) %>%  # Remaining operations used for sampling some of the negatives based on chosen negative to positive ratio
  nest() %>%
  inner_join(num_ratings_per_user) %>%
  mutate(subsamp = map2(data, n*neg_pos_ratio_train, ~slice_sample(.x, n=.y))) %>% 
  select(user, subsamp) %>%
  unnest(cols = c(subsamp))
  
# Define validation data by picking the most recent rating for each user from training
validation_x <- train_rating %>% 
  group_by(user) %>% 
  slice_max(timestamp) %>% 
  slice_sample(1) %>% #some user/item pairs have same timestamp, so randomly pick one
  select(user, item)

# Only positive class was sampled for validation. See section 4.1 of NCF paper.
validation_y <- rep(1, nrow(validation_x))

# Define training as data not used for validation
train_x <- anti_join(train_rating, validation_x) %>% 
  select(user, item) %>%
  bind_rows("pos" = ., "neg" = train_negative, .id = "label")
train_y <- train_x %>% 
  select(label) %>% 
  mutate(label = as.integer(str_detect(label,"pos")))
train_x %<>% select(-label)


# Train model -------------------------------------------------------------

history <- 
  model %>% 
  fit(
    list(user_input = train_x$user, item_input = train_x$item),
    train_y$label,
    epochs = 10,
    batch_size = 128,
    validation_data = list(list(validation_x$user, validation_x$item), validation_y)
  ) 


# Evaluate results --------------------------------------------------------

plot(hisotry)

(results <- model %>% evaluate(list(test_x$user, test_x$item), test_y))


# Make predictions --------------------------------------------------------




