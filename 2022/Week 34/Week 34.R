# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 34)
chips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-23/chips.csv')

library(tidyverse)
library(showtext)

# The font in the plot

font_add_google("Electrolize", "el")
showtext_auto()

# Data Wrangling

dt <- chips %>%
  drop_na(year)

final_plot <- ggplot(dt) + 
  geom_line(aes(x = year, y = process_size_nm), colour = "red") +
  geom_point(aes(x = year, y = process_size_nm), shape = 5,
             colour = "red", size = 5) +
  labs(title = "CHIP Dataset",
       subtitle = "Process size and Thermal design profile through time",
       x = "Year",
       y = "Process size (nm)",
       caption = "Data Source:  https://chip-dataset.vercel.app/ | TidyTuesday 2022 - Week 34 | Prepared by: @Cyd_yzc") +
  theme(plot.background = element_rect(fill = "ivory"),
        panel.background = element_rect(fill = "ivory"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "el", size = 30),
        plot.subtitle = element_text(family = "el", size = 20),
        plot.title.position = "plot",
        plot.caption = element_text(family = "el", size = 19, hjust = 1),
        axis.title = element_text(family = "el", size = 18),
        axis.text = element_text(family = "el", size = 15))
# Save the Plot

ggsave("Week34_2022.png", final_plot, width = 25, height = 12, dpi = 72)

        