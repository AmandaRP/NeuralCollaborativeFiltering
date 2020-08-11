# Scratch pad for playing with other datasets

library(jsonlite)
library(lubridate)
library(magrittr)
  

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

#TODO: Filter goodreads_books.json to just Christian & fiction (too big to keep everything). 
#      see stream_in help file to see how to do this on the fly.
book_info <- jsonlite::stream_in(file("/home/rstudio/goodreads_books.json")) %>% as_tibble()