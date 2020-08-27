#load in libraries
library(rvest)
library(tidytext)
library(tidyverse)
library(lsa)
library(tm)



get_page_info <- read_html("https://www.travelandleisure.com/trip-ideas/best-places-to-travel-in-2020")
description <- get_page_info %>% 
  html_nodes(".paragraph") %>% 
  html_text()

description <- description[7:56] %>% 
  str_trim()

places <- get_page_info %>% 
  html_nodes("h2") %>% 
  html_text() %>% 
  str_trim(side="both")

places <- places[5:54]

holiday_des <- tibble(destinations=places, 
                      description=description)

holiday_des <-holiday_des %>% 
  mutate(destinations=str_remove(destinations, "^[\\d]+\\.\\s"))


holiday_des_tidy <- holiday_des %>%
  unnest_tokens(word, description)

word_count <- holiday_des_tidy %>% 
  anti_join(stop_words) %>% 
  count(destinations, word, sort = TRUE) %>% 
  ungroup()

# TF-IDF

word_count <- word_count %>% 
  bind_tf_idf(word, destinations, n)

# Document term matrix
destinations_dtm <- word_count %>% 
  cast_dtm(destinations, word, tf_idf)


similarities <- cosine(t(as.matrix(destinations_dtm)))

give_rec <- function(title){
  all <- names(sort(similarities[title,], decreasing = TRUE))
  rec <- as.data.frame(all[2:6]) %>% 
    rename(Recommendations="all[2:6]")
  rec
}