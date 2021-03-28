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
# Files needed: goodreads_interactions.csv, book_id_map.csv
# Did not use user_id_map.csv
path <- "/data/"
#download.file("https://drive.google.com/uc?id=1CHTAaNwyzvbi1TR08MJrJ03BxA266Yxr", str_c(path, "book_id_map.csv"))


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
# Filter out non-fiction:
book_info %<>% 
  mutate(shelf_list = map(popular_shelves, ~select(.x, name))) %>% 
  mutate(christian_fiction = map(shelf_list, ~ any(unlist(.x) %in% c("christian-fiction", "fiction")))) %>% #can't look for "fiction" string pattern b/c it will be found in "nonfiction"
  filter(christian_fiction ==  TRUE) 
  
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

# Create a new "user_id" for my reading group and list books that we've liked and disliked
# Book ids are the work_id's provided in the original dataset.
book_club_user_id <- max(users2keep$new_user_id) + 1
users2keep <- bind_rows(users2keep, c("user_id" = NA, "new_user_id" = book_club_user_id))
book_club_interactions <- 
  tribble(
    ~user_id,    ~work_id, ~rating,
    book_club_user_id, 281710,  5, # Safely Home
    book_club_user_id, 1080682, 4, # Sophie's Heart
    book_club_user_id, 49776093,  4, # Long Way Gone
    book_club_user_id, 233843,  4, # When Crickets Cry
    book_club_user_id, 12832808, 4, # Pearl in the Sand
    book_club_user_id, 820210, 5, # Redeeming Love - Francine Rivers
    book_club_user_id, 883913, 5, # Mark of the Lion Series - Francine Rivers (A Voice in the Wind)
    book_club_user_id, 522675, 5, # Mark of the Lion Series - Francine Rivers (An Echo in the Darkness)
    book_club_user_id, 92159, 5, # Mark of the Lion Series - Francine Rivers (As Sure as the Dawn)
    book_club_user_id, 1041558, 1, # The Red Tent- Anita Diamant 
    book_club_user_id, 56364628, 1, # The Lacemaker by Laura Frantz (though Susan Loved It) 
    book_club_user_id, 1222486,  1, # At Home in Mitford
    book_club_user_id, 42455432, 1 # The Hideaway
  ) 

#Convert work_id's above to new book id
book_club_interactions %<>% left_join(new_book_id_df) %>% select(-n, -p)

#Add book club pos list to positive interactions set
interactions_positive %<>%
  bind_rows(
    book_club_interactions %>% 
      filter(rating >= 4) %>%
      select(user_id, book_id)
    )

#Add book club neg list to negative interactions set
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
# Don't want to put my group in test set:
interaction_cnt[which(interaction_cnt$user_id == book_club_user_id),"num_implicit_neg_2sample_4test"] <- 0
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
train_loss <- val_loss <- train_acc <- val_acc  <- rep(NA, num_epochs)
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
  
  #val_pred <- model %>% 
  #  predict(x = list(validation$user_id, validation$book_id)) %>%
  #  bind_cols(pred = ., validation) %>%
  #  rename(user = user_id, item = book_id)
  #val_ndgc[epoch] <- compute_ndcg(val_pred, 10) # I would have added this as a custom metric to the fit function, but it is not in the form fn(y_true, y_pred).
  
  # Save the best model (according to validation loss) 
  if(epoch == 1 || val_loss[epoch] <= best){
    best <- val_loss[epoch]
    save_model_hdf5(model, "model.h5")
  }
  # Save the best model (according to validation ndcg) 
  #if(epoch == 1 || val_ndgc[epoch] >= best){
  #  best <- val_ndgc[epoch]
  #  save_model_hdf5(model, "model.h5")
  #}
  
  # Stop early if the validation loss is greater than (or equal to) the previous X epochs where X = patience
  if(epoch > patience && all(val_loss[epoch] >= val_loss[epoch - 1:patience])){
  # Stop early if the validation ndgc is less than (or equal to) the previous X epochs where X = patience
  #if(epoch > patience && all(val_ndgc[epoch] <= val_ndgc[epoch - 1:patience])){
    break
  }
}
toc()

# Load best model:
model <- load_model_hdf5("model.h5")
#model <- load_model_hdf5("model_20201202.h5")

# Evaluate results --------------------------------------------------------

#history
#plot(history)
# Plot train_loss val_loss train_acc  val_acc
metrics <- as_tibble(list(train_acc=train_acc, train_loss=train_loss,val_acc=val_acc,val_loss=val_loss)) %>%
  drop_na() %>%
  mutate(epoch = row_number()) %>% 
  pivot_longer(cols = c("train_acc", "train_loss", "val_acc", "val_loss"),
               names_to = "metric") %>%
  separate(col = metric, into = c("split", "metric"), "_")
metrics %>% 
  ggplot(aes(epoch, value, color=split)) + 
  geom_line() +
  facet_grid(. ~metric)


# Evaluate returns same metrics that were defined in the compile for the test set (accuracy in this case)
(results <- model %>% evaluate(list(test$user_id, test$book_id), test$label))

# Get predictions for test set:
test_pred <- model %>% 
  predict(x = list(test$user_id, test$book_id)) %>%
  bind_cols(pred = ., test) %>%
  rename(user = user_id, item = book_id)

# Compute hit rate and ndcg
source("evaluation.R")
(hr_test <- compute_hr(test_pred, 10))
(ndcg_test <- compute_ndcg(test_pred, 10))


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

reccdf <- recommendations %>% 
  mutate(rank = row_number()) %>%
  inner_join(book_info) %>% 
  inner_join(new_book_id_df, by = c("book_id", "work_id")) %>% # Add popularity measure to recommendation data frame
  select(rank, pred, p, book_id, work_id, title, authors, publication_year, url) %>% 
  mutate(authors = map(authors, ~filter(., !(role %in% c("Narrator", "Translator", "Read by"))))) %>% #Remove narrator, translator
  mutate(francinerivers = map_lgl(authors, ~filter(., author_id == 6492) %>% nrow() > 0))
View(reccdf)


save(hr_test, ndcg_test, test_pred, new_book_id_df, reccdf, file = "results_20210327.Rda")
load("results_20210327.Rda")
