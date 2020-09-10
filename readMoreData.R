# Scratch pad for playing with other datasets


# Load libraries ----------------------------------------------------------

library(tidyverse)
#library(lubridate)
library(magrittr)
#library(purrr)

# Read GoodReads data ---------------------------------------------------------------

# Data available at: https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/home 
#     See https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/shelves?authuser=0
#download.file("https://drive.google.com/uc?id=15ax-h0Oi_Oyee8gY_aAQN6odoijmiz6Q", "/data/user_id_map.csv")
#download.file("https://drive.google.com/uc?id=1CHTAaNwyzvbi1TR08MJrJ03BxA266Yxr", "/data/book_id_map.csv")

interactions <- read_csv("/data/goodreads_interactions.csv", col_names = TRUE)
book_id_map <- read_csv("/data/book_id_map.csv", col_names = TRUE)
user_id_map <- read_csv("/data/user_id_map.csv", col_names = TRUE)


# Wrangle data ------------------------------------------------------------

# Clean up book_info:
book_info <- christian_book_info

book_info %<>% select(-language_code, -is_ebook, -title_without_series, -ratings_count, -text_reviews_count, -series, -isbn, -country_code, -asin, -kindle_asin, -format, -isbn13, -publication_day, -publication_month, -edition_information, -work_id)
book_info %<>% select(book_id, title, popular_shelves:image_url)
book_info %<>%
  mutate_at(c("book_id", "num_pages", "publication_year"), as.integer) %>%
  mutate_at(c("average_rating"), as.numeric)


# Filter by genre ---------------------------------------------------------

# Use the Christian specific genre book information (available in Data folder):
system("tar -xzf Data/goodreads_books_christian.tar.gz")
load("goodreads_books_christian.RData")
system("rm goodreads_books_christian.RData")

filtered_book_id_map <- inner_join(book_info, book_id_map)
filtered_interactions <- inner_join(interactions, book_id_map, c("book_id" = "book_id_csv"))
