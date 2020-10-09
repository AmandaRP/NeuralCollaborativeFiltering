# Build NCF model for GoodReads dataset

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(magrittr)
#library(dtplyr)
#library(dplyr, warn.conflicts = FALSE)
library(tictoc)
library(reticulate)

source_python("sample_implicit_negatives.py") #Used to call function sample_implicit_negatives


# Download GoodReads data ---------------------------------------------------------------

# Data available at: https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/home 
#     See https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/shelves?authuser=0
path <- "/data/"
#download.file("https://drive.google.com/u/0/uc?export=download&confirm=uMLl&id=1zmylV7XW2dfQVCLeg1LbllfQtHD2KUon", str_c(path, "goodreads_interactions.csv"))
#download.file("https://drive.google.com/uc?id=1CHTAaNwyzvbi1TR08MJrJ03BxA266Yxr", str_c(path, "book_id_map.csv"))
#download.file("https://drive.google.com/uc?id=15ax-h0Oi_Oyee8gY_aAQN6odoijmiz6Q", str_c(path, "user_id_map.csv"))

# Read data ---------------------------------------------------------------

interactions <- read_csv(str_c(path, "goodreads_interactions.csv"), col_names = TRUE)
book_id_map <- read_csv(str_c(path, "book_id_map.csv"), col_names = TRUE) #Need for linking to book info dataset
#user_id_map <- read_csv(str_c(path, "user_id_map.csv"), col_names = TRUE)

# Use the Christian specific genre book information (available in Data folder):
system("tar -xzf Data/goodreads_books_christian.tar.gz")
load("goodreads_books_christian.RData")
system("rm goodreads_books_christian.RData")


# Wrangle & filter data ---------------------------------------------------------

# Clean up book_info:
book_info <- christian_book_info #rename to be more generic
rm(christian_book_info)
book_info %<>% select(-language_code, -is_ebook, -title_without_series, -ratings_count, -text_reviews_count, -series, -isbn, -country_code, -asin, -kindle_asin, -format, -isbn13, -publication_day, -publication_month, -edition_information, -work_id)
book_info %<>% select(book_id, title, popular_shelves:image_url) #reorder
book_info %<>%
  mutate_at(c("book_id", "num_pages", "publication_year"), as.integer) %>%
  mutate_at(c("average_rating"), as.numeric)
dim(book_info)

interactions %<>% mutate_at(c("user_id", "book_id", "is_read", "rating", "is_reviewed"), as.integer)
book_id_map %<>% mutate_at(c("book_id_csv", "book_id"), as.integer)

# Use book_info to filter interactions data based on genre (need to join with book_id_map)
book_info %<>% 
  inner_join(book_id_map) %>%  
  rename(book_id_orig = book_id, book_id_interaction = book_id_csv) %>% #book_id_interaction will be used to join to interactions data
  arrange(book_id_interaction) %>%
  mutate(book_id = row_number()) %>%  #assign new book id's so that we aren't dealing with such large numbers
  select(book_id, book_id_interaction, book_id_orig:image_url) #reorder
interactions %<>% 
  inner_join(select(book_info, book_id_interaction, book_id), by = c("book_id" = "book_id_interaction")) %>% 
  select(-book_id) %>% 
  rename(book_id = book_id.y) #use our new book id's in the interactions dataset

# Count users per book (to later remove pop bias). Add to book_info df ---------

book_info <- interactions %>% 
  group_by(book_id) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(p = n/sum(n)) %>% 
  arrange(desc(n)) %>%
  inner_join(book_info)
book_info

          
# A little EDA ---------------------------------------------------------

# Most popular books:
book_info %>% slice_max(order_by = n, n = 10) %>% select(title, n) %>% arrange(desc(n))

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
  rename(user_id = new_user_id)
interactions %<>% 
  inner_join(users2keep) %>%
  select(-user_id) %>%
  rename(user_id = new_user_id)
  
# Books that the reader did NOT like i.e. rating is 1 or 2:
interactions_negative <- 
  interactions %>%
  filter(rating %in% c(1,2)) %>%
  select(user_id, book_id)


# Compose Train/Validation/Test sets --------------------------------------

# Method (following the paper):
# - Training: 4 negatives for every positive (for each user)
# - Validation: 1 positive (for each user) 
# - Test: 1 positive, 100 negative (for each user)

# Positives:
test_positive <- interactions_positive %>% group_by(user_id) %>% slice_sample(n = 1) 
validation <- anti_join(interactions_positive, test_positive) %>% group_by(user_id) %>% slice_sample(n = 1) 
train_positive <- anti_join(interactions_positive, bind_rows(validation, test_positive) )

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
  mutate(num_implicit_neg_2sample_4train = (excess_neg < 0) * abs(excess_neg)) 
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
  unnest(cols = c(subsamp)) 

# Remaining explicit negatives go in training set:
# TODO: Some users may have more than neg_pos_ratio_train times as many negatives as positives (if they have an abundant number of explicit negatives). OK or need to sample?
train_negative <- anti_join(interactions_negative, test_negative) %>% arrange(user_id)


# Sample implicit negatives to fill out train and test sets
# TODO: Try the following using R's lapply or purrr

# Negative implicits for TEST:
df <- filter(interaction_cnt, num_implicit_neg_2sample_4test > 0) 
tic()
implicit_neg_samples_test <- sample_implicit_negatives(user_ids = df$user_id,
                                                  item_ids = book_info$book_id,  
                                                  num_items_to_sample = df$num_implicit_neg_2sample_4test,
                                                  df_exclude = bind_rows(train_positive, 
                                                                         validation, 
                                                                         test_positive, 
                                                                         train_negative, 
                                                                         test_negative) %>%
                                                    rename(user = user_id, item = book_id),
                                                  p = book_info$p)
toc()
implicit_neg_samples_test$item <- as.integer(implicit_neg_samples_test$item) #Change column type from list to integer. #TODO: Can this be fixed in python code?
test_negative <- bind_rows(test_negative, implicit_neg_samples_test %>% rename(user_id = user, book_id = item))

# Negatives for TRAIN:
df <- filter(interaction_cnt, num_implicit_neg_2sample_4train > 0) 
tic()
implicit_neg_samples_train <- sample_implicit_negatives(user_ids = df$user_id,
                                                  item_ids = book_info$book_id, 
                                                  num_items_to_sample = df$num_implicit_neg_2sample_4train,
                                                  df_exclude = bind_rows(train_positive, 
                                                                         validation, 
                                                                         test_positive, 
                                                                         train_negative, 
                                                                         test_negative) %>%
                                                    rename(user = user_id, item = book_id),
                                                  p = book_info$p)
toc()
implicit_neg_samples_train$item <- as.integer(implicit_neg_samples_train$item) #Change column type from list to integer. #TODO: Can this be fixed in python code?
train_negative <- bind_rows(train_negative, implicit_neg_samples_train %>% rename(user_id = user, book_id = item))


# Define model ------------------------------------------------------------

source("NCF.R")
model <- ncf_model(num_users = max(users2keep$new_user_id) + 1, 
                   num_items = max(book_info$book_id) + 1)

#OOM error. Issue: Use smaller book/user ids (re-number for those in smaller dataset)

