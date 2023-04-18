
# Introduction of challenge one ----

library(tidyverse)
library(readxl)
library(lubridate)

bike_orderlines_wrangled_tbl <- read_excel("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

bike_orderlines_wrangled_tbl
glimpse(bike_orderlines_wrangled_tbl)

location_bike_orderlines_wrangled_tbl <- bike_orderlines_wrangled_tbl %>%
  separate(col = location,
           into = c("city", "state"),
           sep = ", ") 

glimpse(location_bike_orderlines_wrangled_tbl)

# 1. Part of challenge one ----

sales_by_state_tbl <- location_bike_orderlines_wrangled_tbl %>%
  select(state, total_price) %>%
  group_by(state) %>%
  summarise(revenue = sum(total_price)) %>%
  mutate(revenue_text = scales::dollar(revenue, big.mark = ".", 
                                       decimal.mark = ",", 
                                       prefix = "", 
                                       suffix = " €"))
sales_by_state_tbl

sales_by_state_tbl %>%
  ggplot(aes(x = state, y = revenue)) +
  geom_col(fill = "#2DC6D6") +
  geom_label(aes(label = revenue_text)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    subtitle = " ",
    x = "",
    y = "Revenue"
  )

# 2. Part of challenge one ----

sales_by_state_year_tbl <- location_bike_orderlines_wrangled_tbl %>%
  select(state, total_price, order_date) %>%
  mutate(year = year(order_date)) %>%
  group_by(state, year) %>%
  summarise(revenue = sum(total_price)) %>%
  ungroup() %>%
  mutate(revenue_text = scales::dollar(revenue, big.mark = ".", 
                                       decimal.mark = ",", 
                                       prefix = "", 
                                       suffix = " €"))
sales_by_state_year_tbl

sales_by_state_year_tbl %>%
  ggplot(aes(x = year, y = revenue, fill = state)) +
  geom_col() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ state) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = " ",
    fill = "State"
  )
  
  
  