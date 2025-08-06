library(tidyverse)
library(readxl)

num_student_data <- read_excel("Assignment1/data/raw/生徒数/生徒数.xlsx")

truancy_files <- list.files(path = "Assignment1/data/raw/不登校生徒数", pattern = "不登校生徒数", full.names = TRUE)
num_truancy_data <- map(truancy_files, read_excel)

num_student_data <- num_student_data %>% 
    rename(
        prefecture = "都道府県",
        year = "年度",
        num_students = "生徒数"
        )

num_truancy_data <- map(num_truancy_data, ~ .x %>%
    rename(
        prefecture = "都道府県",
        num_truancy = "不登校生徒数"
    )
)
saveRDS(num_student_data, file = "Assignment1/data/original/num_student_data.rds")
saveRDS(num_truancy_data, file = "Assignment1/data/original/num_truancy_data.rds")