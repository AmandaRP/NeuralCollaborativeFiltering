# Build NCF model for GoodReads dataset

# Load libraries ----------------------------------------------------------

library(tidyverse)
#library(lubridate)
library(magrittr)
#library(purrr)

# Download GoodReads data ---------------------------------------------------------------

# Data available at: https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/home 
#     See https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/shelves?authuser=0
path <- "/data/"
download.file("https://drive.google.com/open?id=1zmylV7XW2dfQVCLeg1LbllfQtHD2KUon", str_c(path, "goodreads_interactions.csv"))
download.file("https://drive.google.com/uc?id=1CHTAaNwyzvbi1TR08MJrJ03BxA266Yxr", str_c(path, "book_id_map.csv"))
#download.file("https://drive.google.com/uc?id=15ax-h0Oi_Oyee8gY_aAQN6odoijmiz6Q", str_c(path, "user_id_map.csv"))

# Read data ---------------------------------------------------------------

interactions <- read_csv(str_c(path, "goodreads_interactions.csv"), col_names = TRUE)
book_id_map <- read_csv(str_c(path, "book_id_map.csv"), col_names = TRUE)
#user_id_map <- read_csv(str_c(path, "user_id_map.csv"), col_names = TRUE)

# Use the Christian specific genre book information (available in Data folder):
system("tar -xzf Data/goodreads_books_christian.tar.gz")
load("goodreads_books_christian.RData")
system("rm goodreads_books_christian.RData")


# Wrangle & filter data ---------------------------------------------------------

# Clean up book_info:
book_info <- christian_book_info
rm(christian_book_info)
book_info %<>% select(-language_code, -is_ebook, -title_without_series, -ratings_count, -text_reviews_count, -series, -isbn, -country_code, -asin, -kindle_asin, -format, -isbn13, -publication_day, -publication_month, -edition_information, -work_id)
book_info %<>% select(book_id, title, popular_shelves:image_url) #reorder
book_info %<>%
  mutate_at(c("book_id", "num_pages", "publication_year"), as.integer) %>%
  mutate_at(c("average_rating"), as.numeric)
dim(book_info)

# Use book_info to filter interactions dataset (need to join with book_id_map)
(book_id_map_filtered <- semi_join(book_id_map, book_info))
(interactions_filtered <- semi_join(interactions, book_id_map_filtered, c("book_id" = "book_id_csv")))
          
# A little EDA ---------------------------------------------------------

sprintf("There are %d interactions, %d users, and %d books in the Christian genre book dataset.", 
        nrow(interactions_filtered), length(unique(interactions_filtered$user_id)), length(unique(interactions_filtered$book_id)))
summary(interactions_filtered)

# Max publication year:
book_info %>% select(publication_year) %>% filter(publication_year <= 2020) %>% slice_max(n=1, order_by = publication_year) 
book_info %>% select(publication_year) %>% filter(1500 < publication_year & publication_year <= 2020) %>% slice_min(n=1, order_by = publication_year) 
book_info %>% filter(publication_year < 200) %>% select(title, publication_year)

# Count by year
book_info %>% 
  filter(1875 <= publication_year & publication_year <= 2020) %>%
  ggplot(aes(publication_year)) + geom_histogram()


# Filter interactions by books that the reader liked. Assume that a person likes the book if:
# 1) They read the book AND either gave it 4 or 5 stars or didn't provide rating (is_read == 1 & (0 == rating | rating >= 4)), OR
# 2) The book is on their shelf and not read yet (is_read == 0)
# NOTE: For every case where is_read = 0, no rating has been provided (however, sometimes a review exists) 
#       filter(interactions_filtered, is_read == 0) %>% select(rating) %>% table()
#       When is_read = 1, ratings range from 0 to 5 (0 means not rated?)
#       filter(interactions_filtered, is_read == 1) %>% select(rating) %>% table()
# TODO: Could mine sentiment of reviews for instances where review is given but no rating provided

interactions_positive <- 
  interactions_filtered %>%
  filter(is_read == 0 | (is_read == 1 & (0 == rating | rating >= 4)))

# Remove users who did not have atleast THRESHOLD positives (not enough data to train)
threshold <- 3
users2keep <- interactions_positive %>% group_by(user_id) %>%  count() %>% filter(n >= threshold) %>% select(user_id)

interactions_positive <- inner_join(interactions_positive, users2keep)
interactions_filtered <- inner_join(interactions_filtered, users2keep)
  
# Books that the reader read and did NOT like i.e. rating is 1 or 2:
interactions_negative <- 
  interactions_filtered %>%
  filter(rating %in% c(1,2))


# Compose Train/Validation/Test sets --------------------------------------

# Training goal: 4 negatives for every positive
# DONE Validation goal: 1 positive for every user
# Test goal: 1 positive, 100 negative

validation_positive <- interactions_positive %>% group_by(user_id) %>% slice_sample(n = 1) 
interactions_positive <- anti_join(interactions_positive, validation)
test_positive <- interactions_positive %>% group_by(user_id) %>% slice_sample(n = 1)
interactions_positive <- anti_join(interactions_positive, test)

# Calculate number of negative items that need sampled for test and training sets:
neg_pos_ratio_train <- 4
interaction_cnt <-
  full_join(interactions_positive %>% group_by(user_id) %>% count(), 
            interactions_negative %>% group_by(user_id) %>% count(), 
            by = "user_id") %>%
  rename(pos_cnt = n.x, neg_cnt = n.y) %>%
  replace_na(list(pos_cnt = 0, neg_cnt = 0)) %>%
  mutate(excess_neg = neg_cnt - neg_pos_ratio_train * pos_cnt) %>% 
  arrange(desc(excess_neg)) %>% 
  mutate(num_explicit_neg_2sample_4test = min(100, max(0,excess_neg))) %>%
  mutate(num_implicit_neg_2sample_4train = (excess_neg < 0) * abs(excess_neg)) %>%
  mutate(num_implicit_neg_2sample_4test = 100 - num_explicit_neg_2sample_4test)

# Put excess explicit negatives (up to 100) in test set: (TODO: Is this working how I expect?)
test_negative <- interactions_negative %>% 
  select(user_id, book_id) %>%
  group_by(user_id) %>%
  nest() %>%
  inner_join(interaction_cnt) %>%
  mutate(subsamp = map2(data, num_explicit_neg_2sample_4test, ~slice_sample(.x, n=.y))) %>% 
  select(user_id, subsamp) %>%
  unnest(cols = c(subsamp)) 

# Remaining explicit negatives go in training set:
train_negative <- anti_join(interactions_negative, test_negative)

# sample implicit negatives to fill out train and test sets
data.frame(user_id = rep(users2keep$user_id, each=length(unique(TODO))), 
           book_id = unique(TODO)) %>% # start by listing all user/item pairs 
  anti_join(TODO) %>% # remove user/item pairs that are in positive training set 
  anti_join(TODO) %>%       # remove user/item pairs that are in test set
  group_by(user) %>%  # Remaining operations used for sampling some of the negatives based on chosen negative to positive ratio
  nest() %>%
  inner_join(interaction_cnt) %>%
  mutate(subsamp = map2(data, num_implicit_neg_2sample_4train + num_implicit_neg_2sample_4test, ~slice_sample(.x, n=.y))) %>% 
  select(user_id, subsamp) %>%
  unnest(cols = c(subsamp))


