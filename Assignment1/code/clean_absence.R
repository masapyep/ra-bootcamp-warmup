library(tidyverse)

student_df <- readRDS("Assignment1/data/original/num_student_data.rds")

truancy_list <- readRDS("Assignment1/data/original/num_truancy_data.rds")

names(truancy_list) <- 2013:2022
truancy_df <- bind_rows(truancy_list, .id = "year")%>%
    mutate(year = as.numeric(year)) %>%
    select(year, prefecture, num_truancy)

student_df_cleaned <- student_df %>%
    select(prefecture, year, num_students)

all_data_df <- left_join(student_df_cleaned, truancy_df, by = c("prefecture", "year"))

final_df <- all_data_df %>%
  mutate(
    year = as.numeric(year),
    num_students = as.numeric(num_students),
    num_truancy = as.numeric(num_truancy)
  ) %>%
  mutate(truancy_rate_percent = (num_truancy / num_students) * 100)

saveRDS(final_df, file = "Assignment1/data/cleaned/num_student_truancy_data.rds")