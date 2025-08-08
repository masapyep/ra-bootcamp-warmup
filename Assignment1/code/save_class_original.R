library(tidyverse)
library(readxl)

class_files <- list.files(path = "Assignment1/data/raw/学級数", pattern = "data_", full.names = TRUE)
class_data <- map(class_files, ~ read_excel(.x, skip = 1, col_types = "text"))

translate_variables <- function(df){
    df %>%
        rename_with(
            .fn = ~ case_when(
        # 最初の列は常に都道府県データなので "prefecture" にします。
                .x == names(df)[1] ~ "prefecture",
        
        # 「計」を含む列を "Sum" に変更
                str_detect(.x, "計") ~ "Sum",
        
        # 「以上」を含む列 (例: "61学級以上") を "61-" に変更
                str_detect(.x, "以上") ~ str_replace(.x, "(\\d+).*", "\\1-"),
        
        # 「～」を含む範囲指定の列 (例: "25～30学級") を "25-30" に変更
                str_detect(.x, "～") ~ str_replace(.x, "(\\d+)～(\\d+).*", "\\1-\\2"),
        
        # 「学級」を含む列 (例: "1学級") から数字のみを抽出
                str_detect(.x, "学級") ~ str_extract(.x, "\\d+"),
        
        # 上記のいずれにも一致しない場合は、元の名前を維持
                TRUE ~ as.character(.x)
      )
    )
}

gregorian_year <- function(era_string) {
  
  # Check if the string contains "令和" (Reiwa)
  if (str_detect(era_string, "令和")) {
    
    # Extract the number (e.g., "1", "2", etc.)
    era_year <- as.integer(str_extract(era_string, "\\d+"))
    
    # Convert to Gregorian year (Reiwa Year + 2018)
    if (!is.na(era_year)) {
      return(as.character(era_year + 2018))
    }
    
  # If not Reiwa, check if the string contains "平成" (Heisei)
  } else if (str_detect(era_string, "平成")) {
    
    # Extract the number
    era_year <- as.integer(str_extract(era_string, "\\d+"))
    
    # Convert to Gregorian year (Heisei Year + 1988)
    if (!is.na(era_year)) {
      return(as.character(era_year + 1988))
    }
  }
  
  # If neither era is found, return NA
  return(NA_character_)
}

class_df <- map(class_data, translate_variables)
names(class_df) <- map_chr(class_data, ~ gregorian_year(names(.x)[1]))

print(names(class_df))

saveRDS(class_df, file = "Assignment1/data/original/class_data.rds")