library(ggwordcloud)

chocolate_data_char <- chocolate %>%
  mutate(cocoa_percent = str_replace_all(cocoa_percent, "%", ""),
         cocoa_percent = as.numeric(cocoa_percent), # between 42 and 100 
         cocoa_level = factor(case_when(
           cocoa_percent >= 42 & cocoa_percent < 50 ~ "Level1",
           cocoa_percent >= 50 & cocoa_percent < 60 ~ "Level2",
           cocoa_percent >= 60 & cocoa_percent < 70 ~ "Level3",
           cocoa_percent >= 70 & cocoa_percent < 80 ~ "Level4",
           cocoa_percent >= 80 & cocoa_percent < 90 ~ "Level5",
           cocoa_percent >= 90 & cocoa_percent <= 100 ~ "Level6",
         ), levels = c("Level1", "Level2", "Level3",
                       "Level4", "Level5", "Level6"))) %>%
  select(cocoa_level, most_memorable_characteristics, country_of_bean_origin) %>%
  unnest_tokens(word, most_memorable_characteristics) %>%
  group_by(cocoa_level, word) %>%
  summarise(n = n()) %>%
  ungroup()

set.seed(12)
ggplot(chocolate_data_char, aes(label = word, 
                                size = n)) +
  geom_text_wordcloud_area(area_corr_power = 1, shape = "square",
                           rm_outside = TRUE) +
  scale_size_area(max_size = 20) +
  facet_wrap(~cocoa_level, ncol = 6)



set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()

