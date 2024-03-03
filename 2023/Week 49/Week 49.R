library(tidyverse)
library(janitor)
library(showtext)
library(viridis)

# Save the Plot

ggsave("Week46.png", final_plot, width = 17, height = 15, dpi = 72)


font_add_google(name = "Poppins",
                family = "poppins")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 49)

life_expectancy <- tuesdata$life_expectancy
life_expectancy_different_ages <- tuesdata$life_expectancy_different_ages
life_expectancy_female_male <- tuesdata$life_expectancy_female_male

# Data Wrangling

life_data <- life_expectancy %>%
  clean_names() %>%
  filter(year == 2021 | year == 2010 |year == 2000 | year == 1990 | year == 1980 |
         year == 1970 | year == 1960 | year == 1950)

# Subtitle in the plot

subtitle_text <- str_wrap("Across the world, people are living longer. In 1900, the
average life expectancy of a newborn was 32 years. By 2021 this had more
than doubled to 71 years. People living in Asia, Africa and South America
increased their life expectancies more than the people living in other
regions.", 50)


# The Plot

p <- life_data %>%
ggplot(aes(fill = life_expectancy, map_id = entity)) +
  geom_map(map = world_map) +
  expand_limits(x = c(-110, 150), y = world_map$lat) +
  coord_map("moll") +
  facet_wrap(~year, ncol = 2) +
  scale_fill_viridis(option = "plasma", direction = -1) +
  labs(fill = "Life Expectancy\n(in years)",
       title = "Life Expectancy",
       subtitle = subtitle_text,
       caption = "Data Source: Our World in Data\nTidyTuesday 2023 - Week 49\nPrepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.background = element_rect(fill = "ivory"),
        legend.title = element_text(family = "poppins", size = 15),
        legend.text = element_text(family = "poppins", size = 14),
        legend.key.width = unit(2, "cm"),
        legend.position = "top",
        legend.box = "horizontal",
        strip.text = element_text(family = "poppins", size = 15),
        strip.background = element_rect(fill = "ivory"),
        plot.title = element_text(family = "poppins", size = 40),
        plot.subtitle = element_text(family = "poppins", size = 22),
        plot.caption = element_text(family = "poppins", size = 16, hjust = 1),
        plot.title.position = "plot",
        plot.margin = margin(0.6, 0.2, 0.6, 0.2, "cm"),
        aspect.ratio = 5/9) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

# Save the Plot

ggsave("Week49.png", p, width = 15, height = 15, dpi = 72)

