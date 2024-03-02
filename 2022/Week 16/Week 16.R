library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(showtext)

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 16)

big_dave <- tuesdata$big_dave
times <- tuesdata$times


Sys.setlocale(locale = "English")

# Font in the Plot

font_add_google("DM Serif Display", "sd")
showtext_auto()

# Data Wrangling

times2 <- times %>%
  select(c(clue, answer, definition, clue_number, puzzle_date)) %>%
  drop_na() %>%
  mutate(year = year(puzzle_date),
       month = month(puzzle_date, label = T),
       week_day = wday(as.Date(puzzle_date), label = T),
       answer = str_to_lower(answer),
       nr_char = nchar(answer)) %>%
  filter(year == 2020) %>%
  arrange(month, week_day) %>%
  select(week_day, month, nr_char) %>%
  group_by(week_day, month) %>%
  summarise(longest_word = max(nr_char)) %>%
  ungroup()

# The Plot

p <- ggpubr::ggballoonplot(times2, x = "week_day", y = "month", 
                      show.label = T, fill = "longest_word",
                      size = "longest_word") +
  gradient_fill(c("#F794A0", "#F77D91", "#F70724")) +
  scale_y_discrete(limits = rev) +
 scale_size_area(max_size = 22) +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        text = element_text(family = "sd"),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 0, 
                                   vjust = 0.5, hjust = 1),
    plot.title.position = "plot",
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(size = 16),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    plot.margin = margin(0.5, 2, 0.5, 2, "cm"),
    panel.grid = element_blank()
    ) +
  guides(size = FALSE) +
  labs(title = "Maximum number of characters in the puzzles in 2020",
       subtitle = "\nSaturdays had the longest answers in New York Times crossword puzzles in 2020; while\nthe rest of the days showed similar pattern.",
       caption = "\n#TidyTuesday Week 16 - 2022 | Source: Cryptic Crossword Clues - Prepared by: @Cyd_yzc",
       fill = "Maximum number\nof characters")

# Save the Plot

ggsave("Week16_2022.png", p, width = 20, height = 10, dpi = 72) 

                  