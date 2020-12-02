# Libraries ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# Bike-Category Links ----

url_home <- "https://www.rosebikes.de/fahrr%C3%A4der"
html_home <- read_html(url_home)

bike_category_url_tbl <- html_home %>%
  
  html_nodes(css = ".catalog-navigation__link") %>%
  html_attr('href') %>%
  
  
  # Categorries Sale, Kinder, E-Bike and Bike-Finder ignored
  discard(.p = ~stringr::str_detect(.x,"sale|zoovu|kinder|e-bike")) %>%
  
  enframe(name = "position", value = "subdirectory") %>%
  
  mutate(
    url = glue("https://www.rosebikes.de{subdirectory}")
  ) %>%
  
  distinct(url)

bike_category_url_tbl

bike_category_char_tbl <- html_home %>%
  html_nodes(css = ".catalog-categories-item__title") %>%
  html_text() %>%
  discard(.p = ~stringr::str_detect(.x,"Sale|Bike-Finder|Kinder|E-Bike")) %>%
  enframe(name = "position", value = "category")
bike_category_char_tbl



# Load Bikes ----

l1 = 1
l2 = nrow(bike_category_char_tbl)
all_bikes <- tibble(position = 1, category = "-" , title = "-" , price = "-" )

for (i in seq(l1, l2)) {

bike_category_url <- bike_category_url_tbl$url[i]
bike_category_url 
bike_category_char <- bike_category_char_tbl$category[i]
bike_category_char

# xopen(bike_category_url)


bike_models_title <- read_html(bike_category_url) %>%
  
  html_nodes(css = ".catalog-category-bikes__title-text") %>%
  html_text(trim = TRUE) %>%
  
  enframe(name = "position", value = "title")

# bike_models_title


bike_models_price <- read_html(bike_category_url) %>%
  
  html_nodes(css = ".catalog-category-bikes__price-title") %>%
  html_text(trim = TRUE) %>%
  
  enframe(name = "position", value = "price")

# bike_models_price

L = nrow(bike_models_price)

bike_category <- tibble(position = 1:L, category = c(bike_category_char))
# bike_category

bike_models <- left_join(bike_category, bike_models_title, by = "position") %>%
  left_join(., bike_models_price, by = "position")


print(bike_models)

all_bikes <- all_bikes %>% add_row(bike_models) 

Sys.sleep(15)

}


all_bikes_final <- all_bikes[-c(1), ]
all_bikes_final


