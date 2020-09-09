# Function to filter full book info dataset by genre (because it's too large)
# Instructions: 
#   1. Download goodreads_books.json.gz from the link provided at https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/books?authuser=0 
#   2. Unpack the file using: 
#      gunzip goodreads_books.json.gz
#   3. Specify the filepath for the resulting json file when using the genre_subset function. See example at bottom of this script.

library(jsonlite)
library(tidyverse)
library(magrittr)
library(purrr)


genre_subset <- function(genre, file_path, save_to_disk = FALSE){
  con_in <- file(file_path)
  con_out <- file(tmp <- tempfile(), open = "wb")
  stream_in(con_in, handler = function(df){
    df %<>%
      mutate(genre = map(popular_shelves, str_detect, pattern = genre)) %>%
      mutate(genre = map_lgl(genre, any)) %>%
      filter(genre)
    stream_out(df, con_out, pagesize = 1000)
  }, pagesize = 5000)
  close(con_out)
  
  # stream it back in
  book_info <- stream_in(file(tmp)) %>% as_tibble()
  unlink(tmp)
  
  #Save data to disk:
  if(save_to_disk){
    save(book_info, file = sprintf("goodreads_books_%s.RData", genre))
  }
  return(book_info)
}

book_info <- genre_subset(genre = "christian", file_path = "/home/rstudio/goodreads_books.json")

