# Scratch pad for playing with other datasets

library(jsonlite)
library(lubridate)
library(magrittr)
library(purrr)



# Rent the Runway data ----------------------------------------------------

url <-  "http://deepx.ucsd.edu/public/jmcauley/renttherunway/renttherunway_final_data.json.gz"
data <- jsonlite::stream_in(file(pins::pin(url)[1])) %>% as_tibble()

data %<>%
  select(user_id, item_id, rating, review_date, size, review_text) %>%
  mutate_at(c("user_id", "item_id", "rating"), as.integer) %>%
  mutate_at(c("review_date"), mdy) 

summary(data)
data %>% 
  ggplot(aes(rating)) + geom_histogram() #Ratings are only 2,4,6,8,10 (heavily skewed toward higher numbers)

data %>% filter(item_id== 1406963)


# GoodReads Data ----------------------------------------------------------

#TODO: Download/load data programatically
#install.packages("googledrive")
#library(googledrive)
#drive_download("goodreads_interactions.csv")
#url <-  "https://drive.google.com/u/0/uc?export=download&confirm=seuV&id=1zmylV7XW2dfQVCLeg1LbllfQtHD2KUon"
#test <- pins::pin(url)
#data <- read_csv(test)
#gunzip(file)
#library(dplyr)
#con_in <- gzcon(url("http://jeroen.github.io/data/nycflights13.json.gz"))
#con_out <- file(tmp <- tempfile(), open = "wb")
#stream_in(con_in, handler = function(df){
#  df <- dplyr::filter(df, distance > 1000)
#  df <- dplyr::mutate(df, delta = dep_delay - arr_delay)
#  stream_out(df, con_out, pagesize = 1000)
#}, pagesize = 5000)
## stream it back in
#mydata <- stream_in(file(tmp))
#nrow(mydata)
#unlink(tmp)

#tmp <- tempfile()
#download.file("https://drive.google.com/u/1/uc?export=download&confirm=N8uE&id=1LXpK1UfqtP89H1tYy0pBGHjYk8IhigUK", tmp)
#test <- stream_in(gzcon(file(tmp)))

interactions <- read_csv("/home/rstudio/goodreads_interactions.csv", col_names = TRUE)
book_id_map <- read_csv("/home/rstudio/book_id_map.csv", col_names = TRUE)
user_id_map <- read_csv("/home/rstudio/user_id_map.csv", col_names = TRUE)

book_info <- jsonlite::stream_in(file("/home/rstudio/goodreads_books_small.json")) %>% as_tibble() #small test file

# Filter full book info dataset by genre (because it's too large)
genre_string = "christian"
con_in <- file("/home/rstudio/goodreads_books.json")
con_out <- file(tmp <- tempfile(), open = "wb")
stream_in(con_in, handler = function(df){
  df %<>%
    mutate(genre = map(popular_shelves, str_detect, pattern = genre_string)) %>%
    mutate(genre = map_lgl(genre, any)) %>%
    filter(genre)
  stream_out(df, con_out, pagesize = 1000)
}, pagesize = 5000)
close(con_out)
# stream it back in
book_info <- stream_in(file(tmp)) %>% as_tibble()
nrow(book_info)
unlink(tmp)
#Save data to disk:
save(book_info, file = sprintf("goodreads_books_%s.RData", genre_string)) #TODO: tar this up to see if I can put it on GitHub



book_info %<>% select(-language_code, -is_ebook, -title_without_series, -ratings_count, -text_reviews_count, -series, -isbn, -country_code, -asin, -kindle_asin, -format, -isbn13, -publication_day, -publication_month, -edition_information, -work_id)
book_info %<>% select(book_id, title, popular_shelves:image_url)
book_info %<>%
  mutate_at(c("book_id", "num_pages", "publication_year"), as.integer) %>%
  mutate_at(c("average_rating"), as.numeric)


