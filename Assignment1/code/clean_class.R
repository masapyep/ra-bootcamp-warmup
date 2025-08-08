library(tidyverse)

class_data <- readRDS("Assignment1/data/original/class_data.rds")
class_df <- bind_rows(class_data, .id = "school_year")
class_df <- class_df %>%
  select(-any_of("Sum"))

class_df_sorted <- class_df %>%
  arrange(school_year, prefecture) %>% 
  group_by(school_year) %>%
  mutate(pref_id = row_number()) %>%
  ungroup() %>%
  arrange(pref_id, school_year)

class_df_long <- class_df_sorted %>%
  pivot_longer(
    cols = -c(prefecture, school_year, pref_id),
    names_to = "class_size_range",
    values_to = "num_schools"
  )


class_df_calc <- class_df_long %>%
  mutate(
    total_classes = map_dbl(class_size_range, function(range) {
      # "61-" のように下限のみの場合は、"-" を削除して数値に変換します。
      if (str_ends(range, "-")) {
        return(as.numeric(str_remove(range, "-")))
      }
      # "25-30" のような範囲の場合は、"-" で分割し、平均を計算します。
      if (str_detect(range, "-")) {
        parts <- as.numeric(str_split(range, "-", simplify = TRUE))
        return(mean(parts))
      }
      # "1" のような単一の数値の場合は、そのまま数値に変換します。
      return(as.numeric(range))
    }) * as.numeric(num_schools) # 計算結果に直接学校数を掛け合わせます。
  )
  
class_df_yearpref <- class_df_calc %>%
  group_by(school_year, prefecture, pref_id) %>%
  summarise(
    total_class_yearpref = sum(total_classes, na.rm = TRUE),
    .groups = 'drop' # group化を解除
  ) %>%
  # 最後に都道府県番号と年度順に並び替えます。
  arrange(pref_id, school_year)

saveRDS(class_df_yearpref, file = "Assignment1/data/cleaned/num_class_data.rds")