library(tidyverse)
library(readxl)

class_files <- list.files(path = "../data/raw/学級数", pattern = "data_", full.names = TRUE)
class_data <- map(class_files, ~ read_excel(.x, skip = 1))

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

gregorian_year <- function(df) {
    # 元データのヘッダー（最初の列名）から年を抽出します (例: "平成25年度")
    first_col_name <- names(df)[1]
    # 数字を抽出し、1988を足して西暦に変換します (平成元年 = 1989年)
    heisei_year <- as.integer(str_extract(first_col_name, "\\d+"))
    gregorian_year <- heisei_year + 1988
    return(as.character(gregorian_year))
}

class_df <- map(class_data, translate_variables)
names(class_df) <- map_chr(class_data, gregorian_year)

saveRDS(class_df, file = "Assignment1/data/original/class_data.rds")