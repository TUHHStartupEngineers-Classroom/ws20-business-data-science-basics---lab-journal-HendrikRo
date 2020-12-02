
library(tidyverse)
library(httr)
library(glue)
library(jsonlite)

# All NBA Teams

# resp <- GET("https://www.balldontlie.io/api/v1/players/<ID>")

nba_api <- function(path) {
  url <- modify_url(url = "https://www.balldontlie.io", path = glue("/api/v1{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- nba_api("/players/1")
resp

teams <- resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()

teams