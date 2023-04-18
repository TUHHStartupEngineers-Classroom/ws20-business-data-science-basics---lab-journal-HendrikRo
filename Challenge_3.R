
# Import Librarys ----

library(tidyverse)
library(vroom)
library(data.table)
library(tictoc)

# 1 Import Data ----

# assignee

col_types_a <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_skip(),
  name_last = col_skip(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/assignee.tsv/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_a,
  na         = c("", "NA", "NULL")
)

assignee_tbl %>% glimpse()

# patent_assignee

col_types_pa <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_skip()
)

patent_assignee_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/patent_assignee.tsv/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_pa,
  na         = c("", "NA", "NULL")
)

patent_assignee_tbl %>% glimpse()

# patent

col_types_p <- list(
  id = col_character(),
  type = col_skip(),
  number = col_skip(),
  country = col_skip(),
  date = col_date("%Y-%m-%d"),
  abstract = col_skip(),
  title = col_skip(),
  kind = col_skip(),
  num_claims = col_skip(),
  filename = col_skip(),
  withdrawn = col_skip()
)

patent_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/patent.tsv/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_p,
  na         = c("", "NA", "NULL")
)

patent_tbl %>% glimpse()

# uspc

col_types_u <- list(
  uuid = col_skip(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_skip(),
  sequence = col_skip()
)

uspc_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/uspc.tsv/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_u,
  na         = c("", "NA", "NULL")
)

uspc_tbl %>% glimpse()

# 2 Convert

class(assignee_tbl)
class(patent_assignee_tbl)
class(patent_tbl)
class(uspc_tbl)

tic()
setDT(assignee_tbl)
setDT(patent_assignee_tbl)
setDT(patent_tbl)
setDT(uspc_tbl)
toc()
 
class(assignee_tbl)
class(patent_assignee_tbl)
class(patent_tbl)
class(uspc_tbl)

assignee_tbl %>% glimpse()
patent_assignee_tbl %>% glimpse()
patent_tbl %>% glimpse()
uspc_tbl %>% glimpse()

# Challenge Part One ----

# 3 Wrangling

assignee_tbl <- assignee_tbl %>% rename(assignee_id = id)
assignee_tbl


combined_data <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = TRUE)


combined_data %>% glimpse()

setkey(combined_data, "assignee_id")
key(combined_data)

setorderv(combined_data, c("assignee_id", "patent_id"))

combined_data %>% glimpse()

combined_data_1 <- combined_data %>% filter(type == 2)


combined_data_count <- combined_data_1[!is.na(organization), .N, by = organization]
   

combined_data_count %>% glimpse()

combined_data_count <- combined_data_count %>% arrange(desc(N))
               
combined_data_count %>% slice(1:10)

write_rds(combined_data_count %>% slice(1:10), file = "Challenge_3_results/results_1.rds")

# Challenge Part 2 ----

# 3 Wrangling

patent_tbl <- patent_tbl %>% rename(patent_id = id)
patent_tbl


combined_data_2 <- merge(x = combined_data, y = patent_tbl, 
                       by    = "patent_id", 
                       all.x = TRUE, 
                       all.y = TRUE)


combined_data_2

setkey(combined_data_2, "patent_id")
key(combined_data_2)

keep_cols <- c("organization", "date", "type")

combined_data_2 <- combined_data_2[, ..keep_cols]

combined_data_2 <- combined_data_2 %>% mutate(date = year(date)) %>% filter(date == 2019, type == 2)

combined_data_2 


combined_data_2_count <- combined_data_2[!is.na(organization), .N, by = organization]
 

combined_data_2_count <- combined_data_2_count %>% arrange(desc(N))

combined_data_2_count %>% slice(1:10)

write_rds(combined_data_2_count %>% slice(1:10), file = "Challenge_3_results/results_2.rds")


# Challenge Part Three

# Wrangling


combined_data_3 <- merge(x = combined_data, y = uspc_tbl, 
                         by    = "patent_id", 
                         all.x = TRUE, 
                         all.y = TRUE)


combined_data_3 %>% glimpse()

setkey(combined_data_3, "mainclass_id")
key(combined_data_3)

keep_cols <- c("organization", "mainclass_id")

combined_data_3 <- combined_data_3[, ..keep_cols]


top_all <- combined_data[!is.na(organization), .N, by = organization] %>% arrange(desc(N))

top_ten <- top_all %>% slice(1:10) %>% pull(organization)
top_ten

combined_data_3_ten <- combined_data_3 %>% filter(organization == top_ten)


combined_data_3_ten_count <- combined_data_3_ten[!is.na(mainclass_id), .N, by = mainclass_id]


combined_data_3_ten_count <- combined_data_3_ten_count %>% arrange(desc(N))
combined_data_3_ten_count %>% slice(1:5)


write_rds(combined_data_3_ten_count %>% slice(1:5), file = "Challenge_3_results/results_3.rds")

