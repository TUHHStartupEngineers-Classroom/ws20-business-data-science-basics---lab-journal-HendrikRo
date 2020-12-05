
library(tidyverse)
library(lubridate)
library(glue)

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv") 

covid_data_tbl %>% glimpse()

# Data Manipulation

covid_cases_com_tbl <- covid_data_tbl %>%
  
  select(cases, countriesAndTerritories, dateRep) %>%
  filter(countriesAndTerritories %in% c("Germany",
                                        "France",
                                        "Spain",
                                        "United_Kingdom",
                                        "United_States_of_America")) %>%
  
  
  
  mutate(date       = lubridate::dmy(dateRep)) %>% 
  # mutate(date_floor = floor_date(date, unit = "month")) %>%
  
  group_by(countriesAndTerritories, date) %>%
  summarize(total_cases = sum(cases)) %>% 
  mutate(total_cases = cumsum(total_cases)) %>%
  ungroup()
  


# Data Visualization
  
covid_cases_com_tbl %>% 
  
  ggplot(aes(date, total_cases, color = countriesAndTerritories)) +
  
  geom_label(aes(label = str_glue("{max(total_cases)}")) ,
             data = filter(covid_cases_com_tbl, countriesAndTerritories == last(countriesAndTerritories) &
                             date == last(date) ),
             show.legend = FALSE,
             hjust = 1.1, 
             size  = 5,
             fill  = "yellow",
             color = "black",
             fontface = "italic") +
  
  geom_line(size = 1, linetype = 1) +
  

  
  scale_color_viridis_d(option = "C") +
  theme_minimal() +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%B" ) +
  
  scale_y_continuous(breaks = seq(0, 20e6, by = 5e6),
                     labels = scales::label_number(scale = 1e-6,
                                                   prefix = "",
                                                   suffix = " M")) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(legend.position = "bottom") +

  labs(
    title = "COVID-19 confirmed cases",
    subtitle = "Selected countires",
    x = "Year 2020",
    y = "Total Cases",
    color = "Country"
  ) 





