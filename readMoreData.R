# Scratch pad for playing with other datasets


# Load libraries ----------------------------------------------------------

library(jsonlite)
library(lubridate)
library(magrittr)
library(purrr)

# Read GoodReads data ---------------------------------------------------------------

# Data available at: https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/home

# The following csv files available for download from: https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/shelves?authuser=0
interactions <- read_csv("/home/rstudio/goodreads_interactions.csv", col_names = TRUE)
book_id_map <- read_csv("/home/rstudio/book_id_map.csv", col_names = TRUE)
user_id_map <- read_csv("/home/rstudio/user_id_map.csv", col_names = TRUE)

# Use the Christian specific genre book information (available in Data folder):
system("tar -xzf Data/goodreads_books_christian.tar.gz")
load("goodreads_books_christian.RData")
system("rm goodreads_books_christian.RData")

# Wrangle data ------------------------------------------------------------

book_info %<>% select(-language_code, -is_ebook, -title_without_series, -ratings_count, -text_reviews_count, -series, -isbn, -country_code, -asin, -kindle_asin, -format, -isbn13, -publication_day, -publication_month, -edition_information, -work_id)
book_info %<>% select(book_id, title, popular_shelves:image_url)
book_info %<>%
  mutate_at(c("book_id", "num_pages", "publication_year"), as.integer) %>%
  mutate_at(c("average_rating"), as.numeric)


