# Challenge Part TWO

library(tidyverse)
library(lubridate)
library(glue)
library(maps)

covid_data_2_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv") %>% 
  
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  )) 



world <- map_data("world")

covid_data_2_tbl %>% glimpse()

# Data Manipulation

covid_deaths_tbl <- covid_data_2_tbl %>%
  
  select(countriesAndTerritories, deaths, popData2019) %>%
  
  group_by(countriesAndTerritories, popData2019) %>%
  summarize(total_deaths = sum(deaths)) %>% 
  ungroup() %>%
  
  transmute(countriesAndTerritories ,mortality = total_deaths/popData2019)


world_deaths <- left_join(x = world, y = covid_deaths_tbl, by = c("region" = "countriesAndTerritories"))


# Data Visualization

world_deaths %>%
  
  ggplot() +
  
  geom_map(aes(x = long, y = lat, map_id = region, fill = mortality), map = world) +
  
  scale_fill_gradient(low = "red", high = "black",
                      breaks = seq(0, 0.0013, by = 0.0003) ,
                      labels = scales::label_number(scale = 100,
                                                    prefix = "",
                                                    suffix = " %")) +
  
  theme_minimal() +
  
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  
  
  labs(
    title = "Confirmed COVID-19 deaths relative to the population size",
    subtitle = " ",
    x = " ",
    y = " ",
    fill = "Mortality Rate"
   ) 





