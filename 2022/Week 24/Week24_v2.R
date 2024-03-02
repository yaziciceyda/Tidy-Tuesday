library(tidyverse)
library(janitor)
library(lubridate)
library(statebins)
library(PrettyCols)
library(showtext)

# Data Import

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')


Sys.setlocale(locale = "English")

# Font in the Plot

font_add_google('Libre Bodoni', 'lb', db_cache = FALSE)
showtext_auto()

# Data Wrangling

df_data <- drought_fips %>%
  clean_names() %>%
  mutate(date = ymd(date),
         month = month(date, label = TRUE)) %>%
  group_by(month, state) %>%
  summarise(mean_overall = mean(dsci)) %>%
  ungroup() 

# Subtitle of the plot

subtitle_text <- str_wrap("Drought Score (0 to 500) Zero means that none of 
                          the area is abnormally dry or in drought, and 500
                          means that all of the area is in exceptional drought.
                          As expected, the states in the west have higher score 
                          which indicates more drought and the scores of the
                          states in the east shows less drought for all months
                          between 2000 - 2022.", 120)

# The Plot

p <- ggplot(df_data, aes(state = state, fill = mean_overall)) +
  geom_statebins(border_col = "ivory", lbl_size = 4, family = "lb") +
  facet_wrap(~month, ncol = 4) +
  scale_fill_pretty_c("Tangerines", direction = -1) +
  labs(title = "US DROUGHT",
       subtitle = subtitle_text,
       fill = "Average Drought\nScore",
       caption = "Data Source: Drought.gov | TidyTuesday 2022 - Week 24 | Prepared by: @Cyd_yzc") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.background = element_rect(fill = "ivory"),
        plot.title = element_text(family = "lb", size = 35, hjust = 0),
        plot.caption = element_text(family = "lb", size = 18, hjust = 1),
        plot.subtitle = element_text(family = "lb", size = 22, hjust = 0),
        legend.title = element_text(family = "lb", size = 15),
        legend.text = element_text(family = "lb", size = 12),
        strip.background = element_rect(fill = "#F6A376"),
        strip.text = element_text(family = "lb", size = 13),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# Save the Plot

ggsave("Week24_2022_v2.png", p, width = 23, height = 15, dpi = 72)





