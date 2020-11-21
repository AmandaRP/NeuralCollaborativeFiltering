# Build NCF model for GoodReads dataset

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(magrittr)
#library(dtplyr)
#library(dplyr, warn.conflicts = FALSE)
library(tictoc)  
library(reticulate)

source_python("sample_implicit_negatives.py") #Used to call function sample_implicit_negatives


# Set variables -----------------------------------------------------------

num_epochs <- 10


# Download GoodReads data ---------------------------------------------------------------

# Data available at: https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/home 
#                See https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/shelves?authuser=0
path <- "/data/"
#download.file("https://drive.google.com/u/0/uc?export=download&confirm=uMLl&id=1zmylV7XW2dfQVCLeg1LbllfQtHD2KUon", str_c(path, "goodreads_interactions.csv"))
#download.file("https://drive.google.com/uc?id=1CHTAaNwyzvbi1TR08MJrJ03BxA266Yxr", str_c(path, "book_id_map.csv"))
#download.file("https://drive.google.com/uc?id=15ax-h0Oi_Oyee8gY_aAQN6odoijmiz6Q", str_c(path, "user_id_map.csv"))

# Read data ---------------------------------------------------------------

interactions <- read_csv(str_c(path, "goodreads_interactions.csv"), col_names = TRUE)
book_id_map <- read_csv(str_c(path, "book_id_map.csv"), col_names = TRUE) # Need for linking to book info dataset
#user_id_map <- read_csv(str_c(path, "user_id_map.csv"), col_names = TRUE) # Don't need

# Use the Christian specific genre book information (available in Data folder):
system("tar -xzf Data/goodreads_books_christian.tar.gz")
load("goodreads_books_christian.RData")
system("rm goodreads_books_christian.RData")


# Wrangle & filter data ---------------------------------------------------------

# Clean up book_info:
book_info <- christian_book_info #rename to be more generic
rm(christian_book_info)
tmp <- christian_book_info[1:10,] %>% 
  mutate(shelf_list = map(popular_shelves, ~select(.x, name))) %>%
  mutate(christian_fiction = map(shelf_list, ~ .x %in% c("christian_fiction", "fiction")))
  #TODO: filtering by christian fiction is NOT working.

map_lgl(twords, ~ all(c("strong", "weak") %in% .x))       
  mutate(genre = cross2(popular_shelves, c("", "fiction"), ~has_element(.x, .y))) %>% View()

  mutate(genre = map_lgl(genre, any)) %>%
  filter(genre) 
book_info %<>% select(-language_code, -is_ebook, -title_without_series, -ratings_count, -text_reviews_count, -series, -isbn, -country_code, -asin, -kindle_asin, -format, -isbn13, -publication_day, -publication_month, -edition_information, -christian)
book_info %<>% select(book_id, work_id, title, popular_shelves:image_url) #reorder
book_info %<>%
  mutate_at(c("book_id", "work_id", "num_pages", "publication_year"), as.integer) %>%
  mutate_at(c("average_rating"), as.numeric)
dim(book_info)

interactions %<>% mutate_at(c("user_id", "book_id", "is_read", "rating", "is_reviewed"), as.integer)
book_id_map %<>% mutate_at(c("book_id_csv", "book_id"), as.integer)

# Create a new id for each work_id (need to use smaller numbers for NN to reduce params)
new_book_id_df <- book_info %>% 
  select(work_id) %>% 
  distinct() %>% 
  mutate(new_book_id = row_number())
# First grab book_id_csv from book_id_map (need for joining to interactions)
book_info %<>% 
  inner_join(book_id_map) %>%  # get id's needed for joining to interaction data (here we're using "book_id")
  inner_join(new_book_id_df) %>% # get new id's that we'll use (joining on work_id)
  rename(book_id_orig = book_id, book_id_interaction = book_id_csv) %>% #book_id_interaction will be used to join to interactions data
  arrange(new_book_id) %>%
  select(new_book_id, book_id_interaction, work_id:image_url) #reorder
# Use book_info to filter interactions data based on genre (need to join with book_id_map)
interactions %<>% 
  inner_join(select(book_info, book_id_interaction, new_book_id), by = c("book_id" = "book_id_interaction")) %>% 
  select(-book_id) %>% 
  rename(book_id = new_book_id) #use our new book id's in the interactions dataset

# Count users per book (to later remove pop bias). Add to book_info df ---------

new_book_id_df <- interactions %>% 
  group_by(book_id) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(p = n/sum(n)) %>% 
  arrange(desc(n)) %>%
  inner_join(new_book_id_df, by = c("book_id" = "new_book_id"))
new_book_id_df

          
# A little EDA ---------------------------------------------------------

# Most popular books:
new_book_id_df %>% 
  inner_join(book_info, by = c("book_id" = "new_book_id")) %>% 
  slice_max(order_by = n, n = 10) %>% 
  select(book_id, n) %>% 
  distinct() %>%
  arrange(desc(n))

sprintf("There are %d interactions, %d users, and %d books in the Christian genre book dataset.", 
        nrow(interactions), length(unique(interactions$user_id)), length(unique(interactions$book_id)))
sparsity <- nrow(interactions) / length(unique(interactions$user_id))
sparsity <- sparsity / length(unique(interactions$book_id))
sprintf("Sparsity is %f",  1 - sparsity)
summary(interactions)

# Max/min publication year:
book_info %>% select(publication_year) %>% filter(publication_year <= 2020) %>% slice_max(n=1, order_by = publication_year) 
book_info %>% select(publication_year) %>% filter(1500 < publication_year & publication_year <= 2020) %>% slice_min(n=1, order_by = publication_year) 
book_info %>% filter(publication_year < 200) %>% select(title, publication_year)

# Count by year
book_info %>% 
  filter(1875 <= publication_year & publication_year <= 2020) %>%
  ggplot(aes(publication_year)) + geom_histogram()


# Determine positive and negative interactions ---------------------------------

# NCF assumes binary output so we need to infer likes and dislikes
# Filter interactions by books that the reader liked. Assume that a reader likes the book if:
# 1) They read the book AND either gave it 4 or 5 stars or didn't provide rating (is_read == 1 & (0 == rating | rating >= 4)), OR
# 2) The book is on their shelf and not read yet (is_read == 0)
# NOTE: For every case where is_read = 0, no rating has been provided (however, sometimes a review exists) 
#       filter(interactions_filtered, is_read == 0) %>% select(rating) %>% table()
#       When is_read = 1, ratings range from 0 to 5 (0 means not rated?)
#       filter(interactions_filtered, is_read == 1) %>% select(rating) %>% table()
# TODO: Could mine sentiment of reviews for instances where review is given but no rating provided

interactions_positive <- 
  interactions %>%
  filter(is_read == 0 | (is_read == 1 & (0 == rating | rating >= 4))) %>%
  select(user_id, book_id)

# Remove users who did not have atleast THRESHOLD positives (not enough data to train)
threshold <- 3
users2keep <- interactions_positive %>% 
  group_by(user_id) %>%  
  count() %>% 
  filter(n >= threshold) %>% 
  select(user_id) %>%
  ungroup() %>%
  mutate(new_user_id = row_number()) #add a new id to reduce max id size

interactions_positive %<>% 
  inner_join(users2keep) %>%
  select(-user_id) %>%
  rename(user_id = new_user_id) %>%
  select(user_id, book_id)
interactions %<>% 
  inner_join(users2keep) %>%
  select(-user_id) %>%
  rename(user_id = new_user_id)
  
# Books that the reader did NOT like i.e. rating is 1 or 2:
interactions_negative <- 
  interactions %>%
  filter(rating %in% c(1,2)) %>%
  select(user_id, book_id)


# Add my book club to training data --------------------------------------------

#Create a new "user_id" for my reading group and list books that we've liked and disliked
book_club_user_id <- max(users2keep$new_user_id) + 1
users2keep <- bind_rows(users2keep, c("user_id" = NA, "new_user_id" = book_club_user_id))
book_club_interactions <- 
  tribble(
    ~user_id,    ~book_id, ~rating,
    book_club_user_id, 4493,  5, # Safely Home
    book_club_user_id, 16031, 4, # Sophie's Heart
    book_club_user_id, 6345,  4, # Long Way Gone
    book_club_user_id, 4145,  4, # When Crickets Cry
    book_club_user_id, 14625, 5, # Redeeming Love - Francine Rivers
    book_club_user_id, 5080, 5, # Mark of the Lion Series - Francine Rivers (A Voice in the Wind)
    book_club_user_id, 9142, 5, # Mark of the Lion Series - Francine Rivers (An Echo in the Darkness)
    book_club_user_id, 5353, 5, # Mark of the Lion Series - Francine Rivers (As Sure as the Dawn)
    book_club_user_id, 501, 1, # The Red Tent- Anita Diamant 
    book_club_user_id, 50008, 1, # Lace Maker by Laura Frantz (though Susan Loved It) 
    book_club_user_id, 7464,  1, # At Home in Mitford
    book_club_user_id, 35269, 1  # The Hideaway
  ) 

interactions_positive %<>%
  bind_rows(
    book_club_interactions %>% 
      filter(rating >= 4) %>%
      select(user_id, book_id)
    )


interactions_negative %<>%
  bind_rows(
    book_club_interactions %>% 
      filter(rating <= 2) %>%
      select(user_id, book_id)
  )


# Compose Train/Validation/Test sets --------------------------------------

# Method (following the paper):
# - Training: 4 negatives for every positive (for each user)
# - Validation: 1 positive (for each user) 
# - Test: 1 positive, 100 negative (for each user)

# Positives:
test_positive <- interactions_positive %>% 
  filter(user_id != book_club_user_id) %>% # Don't want to include my club's books in test (need to keep all training data)
  group_by(user_id) %>% 
  slice_sample(n = 1) %>%
  ungroup()
validation <- interactions_positive %>%
  filter(user_id != book_club_user_id) %>% # Don't want to include my club's books in validation (need to keep all training data)
  anti_join(test_positive) %>% 
  group_by(user_id) %>% 
  slice_sample(n = 1) %>%
  ungroup()
train_positive <- anti_join(interactions_positive, 
                            bind_rows(validation, test_positive) )

# Calculate number of negative items that need sampled for test and training sets:
neg_pos_ratio_train <- 4
interaction_cnt <-
  full_join(train_positive %>% group_by(user_id) %>% count(), 
            interactions_negative %>% group_by(user_id) %>% count(), 
            by = "user_id") %>%
  rename(pos_cnt = n.x, neg_cnt = n.y) %>%
  replace_na(list(pos_cnt = 0, neg_cnt = 0)) %>%
  mutate(excess_neg = neg_cnt - neg_pos_ratio_train * pos_cnt) %>% 
  arrange(desc(excess_neg)) %>% 
  mutate(num_explicit_neg_2sample_4test = min(100, max(0,excess_neg))) %>%
  mutate(num_implicit_neg_2sample_4test = 100 - num_explicit_neg_2sample_4test) %>%
  mutate(num_implicit_neg_2sample_4train = (excess_neg < 0) * abs(excess_neg)) %>%
  ungroup()
interaction_cnt

# Put excess explicit negatives (up to 100) in test set: 
# TODO: Should I keep all explicit negatives in training? This might be too much class imbalance.
# Note: If dataset had timestamps, would be better to split on time instead of randomly
test_negative <- interactions_negative %>% 
  group_by(user_id) %>%
  nest() %>%
  inner_join(interaction_cnt) %>%
  mutate(subsamp = map2(data, num_explicit_neg_2sample_4test, ~slice_sample(.x, n=.y))) %>% 
  select(user_id, subsamp) %>%
  unnest(cols = c(subsamp)) %>%
  ungroup()


# Remaining explicit negatives go in training set:
# TODO: Some users may have more than neg_pos_ratio_train times as many negatives as positives (if they have an abundant number of explicit negatives). OK or need to sample?
train_negative <- anti_join(interactions_negative, test_negative) %>% arrange(user_id)


# Sample implicit negatives to fill out train and test sets

# Negative implicits for TEST:
df <- interaction_cnt %>% 
  filter(num_implicit_neg_2sample_4test > 0) %>%
  filter(user_id != book_club_user_id) %>% # Don't need to sample test items for book club (keep all for training).
  ungroup()
tic()
implicit_neg_samples_test <- sample_implicit_negatives(user_ids = df$user_id,
                                                  item_ids = new_book_id_df$book_id,  
                                                  num_items_to_sample = df$num_implicit_neg_2sample_4test,
                                                  df_exclude = bind_rows(train_positive %>% select(user_id, book_id), 
                                                                         validation %>% select(user_id, book_id), 
                                                                         test_positive %>% select(user_id, book_id), 
                                                                         train_negative %>% select(user_id, book_id), 
                                                                         test_negative  %>% select(user_id, book_id)) %>%
                                                    rename(user = user_id, item = book_id),
                                                  p = new_book_id_df$p)
toc()
implicit_neg_samples_test$item <- as.integer(implicit_neg_samples_test$item) #Change column type from list to integer. #TODO: Can this be fixed in python code?
test_negative <- bind_rows(test_negative, implicit_neg_samples_test %>% rename(user_id = user, book_id = item))


#Put it all together (we'll compose training data for each epoch later):
test <- bind_rows(add_column(test_positive  %>% select(user_id, book_id), label = 1), 
                  add_column(test_negative  %>% select(user_id, book_id), label = 0))
validation <- add_column(validation, label = 1)



# Define model ------------------------------------------------------------

source("NCF.R")
model <- ncf_model(num_users = max(users2keep$new_user_id) + 1, 
                   num_items = max(new_book_id_df$book_id) + 1)



# Train model -------------------------------------------------------------

# First define callbacks to stop model early when validation loss increases and to save best model
#callback_list <- list(
#  callback_early_stopping(patience = 2),
#  callback_model_checkpoint(filepath = "model.h5", 
#                            monitor = "val_loss", 
#                            save_best_only = TRUE)
#)

# training loop
train_loss <- val_loss <- train_acc <- val_acc <- rep(NA, num_epochs)
patience <- 2
tic()
for(epoch in 1:num_epochs){
  cat("Epoch", epoch, "\n")
  
  # Negative implicit samples for TRAIN:
  df <- filter(interaction_cnt, num_implicit_neg_2sample_4train > 0) %>% ungroup()
  implicit_neg_samples_train <- sample_implicit_negatives(user_ids = df$user_id,
                                                          item_ids = new_book_id_df$book_id, 
                                                          num_items_to_sample = df$num_implicit_neg_2sample_4train,
                                                          df_exclude = bind_rows(train_positive %>% select(user_id, book_id), 
                                                                                 validation %>% select(user_id, book_id), 
                                                                                 test_positive %>% select(user_id, book_id), 
                                                                                 train_negative  %>% select(user_id, book_id), 
                                                                                 test_negative %>% select(user_id, book_id)) %>%
                                                            rename(user = user_id, item = book_id),
                                                          num_batches = num_epochs, # Obtain different samples for each epoch
                                                          p = new_book_id_df$p)
  implicit_neg_samples_train$item <- as.integer(implicit_neg_samples_train$item) #Change column type from list to integer. #TODO: Can this be fixed in python code?
  # Replicate the explicit negatives for each epoch and add an epoch number:
  #train_negative %<>% 
  #  mutate(num = num_epochs) %>%
  #  uncount(num_epochs) %>%
  #  group_by(user_id, book_id) %>% # Label epoch number
  #  mutate(epoch_id = 1:num_epochs) %>%
  #  ungroup()
  # Add in the implicit negatives (TODO: Make sure there is an epoch_id column)
  #train_negative <- bind_rows(train_negative, implicit_neg_samples_train %>% rename(user_id = user, book_id = item, epoch_id = TODO))
  
  
  train <- bind_rows(add_column(train_positive %>% 
                                  select(user_id, book_id), 
                                label = 1), 
                     add_column(implicit_neg_samples_train %>% 
                                  rename(user_id = user, book_id = item),
                                label = 0),
                     add_column(train_negative, #%>%
                                  #filter(epoch_id == epoch) #%>%
                                  #select(-epoch_id),
                                label = 0
                                )
                     )
  
  history <- model %>% 
    fit(
      x = list(user_input = as.array(train$user_id), 
               item_input = as.array(train$book_id)),
      y = as.array(train$label),
      epochs = 1, batch_size = 256, verbose = 1, shuffle = TRUE,
      validation_data = list(list(user_input = as.array(validation$user_id), 
                                  item_input = as.array(validation$book_id)), 
                             as.array(validation$label))
    ) 
  
  train_loss[epoch] <- history$metrics$loss #train loss
  train_acc[epoch] <- history$metrics$accuracy
  val_loss[epoch] <- history$metrics$val_loss
  val_acc[epoch] <- history$metrics$val_accuracy
  
  # Save the best model (according to validation loss) 
  if(epoch == 1 || val_loss[epoch] <= best){
    best <- val_loss[epoch]
    save_model_hdf5(model, "model.h5")
  }
  
  # Stop early if the validation loss is greater than (or equal to) the previous X epochs where X = patience
  if(epoch > patience && all(val_loss[epoch] >= val_loss[epoch - 1:patience])){
    break
  }
}
toc()

# Load best model:
model <- load_model_hdf5("model.h5")


# Evaluate results --------------------------------------------------------

#history
#plot(history)
# TODO: Plot train_loss val_loss train_acc  val_acc

# Evaluate returns same metrics that were defined in the compile (accuracy in this case)
(results <- model %>% evaluate(list(test$user_id, test$book_id), test$label))

# Get predictions for test set:
test_pred <- model %>% 
  predict(x = list(test$user_id, test$book_id)) %>%
  bind_cols(pred = ., test) %>%
  rename(user = user_id, item = book_id)

# Compute hit rate and ndcg
source("evaluation.R")
compute_hr(test_pred, 10)
compute_ndcg(test_pred, 10)

# Before accounting for popularity bias:
# hr 0.912
# ndcg 0.753
# test loss: 0.08511109 
# test accuracy: 0.97462940 

# 11/15 run: pop bias. sampling impl negatives for each epoch:
# hr 0.89 
# ndcg 0.731

# Make recommendations ----------------------------------------------------

#Remove the books that we've already read:
books_to_score <- anti_join(new_book_id_df, book_club_interactions) 

#Score the remaining books:
recommendations <- 
  model %>% 
  predict(x = list(rep(book_club_user_id, nrow(books_to_score)), 
                   books_to_score$book_id)) %>%
  bind_cols(pred = ., books_to_score)  %>%
  select(-n, -p) %>%
  arrange(desc(pred))

recommendations %>% 
  inner_join(book_info) %>% 
  select(pred, book_id, work_id, title, publication_year, url) %>% 
  View()

# Top recommendations (before accounting for popularity bias and before excluding our books from test and validation):
# 1. The Lion, the Witch, and the Wardrobe
# 2. Redeeming Love by Francine Rivers (https://www.goodreads.com/book/show/24377351-redeeming-love)
# 3. The 5 Love Languages: The Secret to Love that Lasts	(not fiction)
# 4. The Hiding Place
# 5. A Voice in the Wind (Mark of the Lion, #1)
# 6. Chronicles Of Narnia Boxed Set
# 7. Unbroken: A World War II Story of Survival, Resilienc...
# 8. An Echo in the Darkness (Mark of the Lion, #2)
# 9. Lamb: The Gospel According to Biff, Christ's Childhood...
# 10. Mere Christianity
  
# Top recommendations (before accounting for popularity bias but AFTER excluding our books from test and validation): 
# 1. Waterfall (River of Time #1)
# 2. The Hiding Place
# 3. Bonhoeffer: Pastor, Martyr, Prophet, Spy
# 4. How Do You Kill 11 Million People?: Why the Truth Mat
# 5. The Lion, the Witch and the Wardrobe
# 6. Unbroken: A World War II Story of Survival, Resilience, and Redemption	
# 7. Chronicles Of Narnia Boxed Set
# 8. Th3ee https://www.goodreads.com/book/show/8875201-thr3e
# 9. So Not Happening (The Charmed Life, #1)
#10. Holy Bible: King James Version
  
# Top recommendations (after accounting for popularity bias and AFTER excluding our books from test and validation): 
# 1. All popular books are being recommended (chronicals of narnia, mere Christianity, the Lion the Witch and the Wardrobe. I think this is because I'm not sampling implicit negatives for my group now.   
  
# Top recommendations (after accounting for popularity bias and AFTER excluding our books from test and validation, but making sure that group is given implicit negatives): 
# 1. All popular books are being recommended (chronicals of narnia, mere Christianity, the Lion the Witch and the Wardrobe). 
# Not sure why. Maybe my group doesn't have enough data? Maybe need a different populartiy bias sampling dist? Maybe need to sample for each epoch?
  
# 2020-11-16 model:
# Top recommendations (after accounting for pop bias, after excluding our books from test & val, giving our group implicit negs, 
#                      re-sampling impl negatives for each epoch, and decreasing batch size to 256). Took a 2.5 days to run.
# 1. Unbroken  (Probably not. Not fiction) https://www.goodreads.com/book/show/18770394-unbroken
# 2. The Brothers Karamazov (No. Not Christian) https://www.goodreads.com/book/show/19538.The_Brothers_Karamazov
# 3. Blankets (No. Not Christian.) https://www.goodreads.com/book/show/2928869-blankets
# 4. Just Mercy: A Story of Justice and Redemption (Maybe?) https://www.goodreads.com/book/show/21803032-just-mercy
# 5. 	Ordinary Grace (Yes) https://www.goodreads.com/book/show/32660478-ordinary-grace
# 6. The Hiding Place
# 7. The Five Love Languages: How to Express Heartfelt Commitment to Your Mate	(no)
# 8. Under The Banner Of Heaven (No!)
# 9. Waterfall (River of Time #1) (no, based on reviews) https://www.goodreads.com/book/show/7879278-waterfall?from_search=true&from_srp=true&qid=x0KUH4IxfU&rank=1
# 10. Lamb: The Gospel According to Biff, Christ's Childhood (no. Children's book)


  