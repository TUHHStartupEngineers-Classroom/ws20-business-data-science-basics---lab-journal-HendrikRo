
library(tidyverse) # loads ggplot2
library(lubridate)
library(ggthemes)

bike_orderlines_tbl <- read_rds("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

# Data Manipulation

sales_by_year_category_1_tbl <- bike_orderlines_tbl %>%
  select(order_date, category_1, total_price) %>%
  
  mutate(order_date = ymd(order_date)) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(category_1, year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%
  
  # Convert character vectors to factors
  # Arrange by year and revenue
  mutate(category_1 = fct_reorder2(category_1, year, revenue))

sales_by_year_category_1_tbl

# Uncover the factor levels (just for demonstration)
# sorted by years and the highest revenues
sales_by_year_category_1_tbl %>%
  mutate(category_1_num = as.numeric(category_1)) %>%
  arrange(category_1_num)

# Colors

sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = viridisLite::viridis(n = 20)[10])

# Position

sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, fill = category_1)) +
  geom_area(color = "black") +
  theme_dark()

# Scales ----
# Set Plot

g_facet_continuous <- sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, color = revenue)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  
  facet_wrap(~ category_1, scales = "free_y") +
  expand_limits(y = 0) +
  
  theme_minimal()

g_facet_continuous

g_facet_discrete <- sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, color = category_1)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  
  facet_wrap(~ category_1, scales = "free_y") +
  expand_limits(y = 0) +
  
  theme_minimal()

g_facet_discrete

g_area_discrete <- sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, fill = category_1)) +
  geom_area(color = "black") +
  
  theme_minimal()

g_area_discrete

# Fill

g_facet_continuous +
  
  # scale_color_continuous(
  #     low   = "black",
  #     high  = "cornflowerblue"
  # )
  # This is basically like adding a theme
  scale_color_viridis_c(option = "E", direction = -1)

g_facet_discrete +
  scale_color_viridis_d(option = "D") +
  theme_dark()
g_facet_discrete +
  scale_color_brewer(palette = "Set3") +
  theme_dark()

g_area_discrete +
  scale_fill_brewer(palette = "Set3")
g_area_discrete +
  scale_fill_viridis_d()

# labels

g_facet_continuous +
  
  scale_x_continuous(breaks = seq(2015, 2019, by = 2)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    suffix = "M")) +
  
  geom_smooth(method = "lm", se = FALSE) +
  
  scale_color_viridis_c() +
  theme_dark() +
  
  labs(
    title = "Bike Sales",
    subtitle = "Sales are trending up",
    caption = "5-year sales trends\ncomes from our ERP Database",
    x = "Year",
    y = "Revenue (M €)",
    color = "Revenue" # Legend text
  )

# Themes 

# Example all together

sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, fill = category_1)) +
  
  geom_area(color = "black") +
  
  # Scales
  scale_fill_brewer(palette = "Blues", direction = -1) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " €")) +
  
  # Labels
  labs(
    title = "Sales Over Year by Category 1",
    subtitle = "Sales Trending Upward",
    x = "",
    y = "Revenue (M €)",
    fill = "2nd Category",
    caption = "Bike sales trends look strong heading into 2020"
  ) +
  
  # Theme
  theme_light() +
  theme(
    title = element_text(face = "bold", color = "#08306B")
    
  )

# Factors ----

starwars %>% 
  filter(!is.na(species)) %>%
  mutate(species = as_factor(species) %>% 
           fct_lump(n = 5)) %>%
  count(species)




