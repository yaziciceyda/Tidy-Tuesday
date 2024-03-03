# Week 11 - 2023 - TidyTuesday

library(tidyverse)
library(lubridate)
library(showtext)

# Font in the Plot

font_add_google(name = "Playfair Display",
                family = "pd")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 11)
drugs <- tuesdata$drugs

# Data Wrangling

disease_data <- drugs %>%
  filter(str_detect(therapeutic_area, "Hypertension"),
         category == "human") %>%
  mutate(year = year(marketing_authorisation_date)) %>%
  group_by(year) %>%
  mutate(n = n(),
         fill_index = round(n/2)) %>%
  ungroup() %>%
  arrange(year)

# Subtitle of the Plot

subtitle_text <- str_wrap("A total of 74 hypertension drugs are approved by European
Medicines Agency each year between 1997 and 2021. Even though only a single
drug is approved in several years, the maximum value appeared in 2007
with 13 approvals.", 90)

# The Plot

p <- ggplot(disease_data) +
  stat_count(aes(x = factor(year), xend = factor(year),
                 yend = stat(count) - stat(count)),
             geom = 'segment', size = 10, lineend = 'round',
             color = "blue") +
  # If n of the year > 1
  geom_point(disease_data %>% filter(n > 1),
             mapping = aes(x = factor(year), y = 0), size = 10.5,
             color = "red", fill = "red") +
  geom_segment(disease_data %>% filter(n > 1),
               mapping = aes(x = factor(year), xend = factor(year),
                y = 0, yend = fill_index), colour = "red", linewidth = 9.5) +
  # If n of the year = 1
  geom_point(disease_data %>% filter(n == 1),
             mapping = aes(x = factor(year), y = -0.05), size = 10,
             color = "red", fill = "red") +
  geom_segment(disease_data %>% filter(n == 1),
               mapping = aes(x = factor(year), xend = factor(year),
               y = 0, yend = 0.5), colour = "red", linewidth = 9.5) +
  # The white line
  geom_segment(aes(x = as.numeric(factor(year)) - 0.05, xend =
                     as.numeric(factor(year)) - 0.05,
                   y = 0.08, yend = n - 0.08),
               color = "ivory", linewidth = 2) +
  scale_x_discrete() +
  coord_cartesian() +
  labs(title = "European Drug Development\n",
       subtitle = subtitle_text,
       x = "",
       y = "Number of Approved Drugs",
       caption = "Data Source: European Medicines Agency | TidyTuesday 2023 - Week 11\nPrepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill ="ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        axis.text.x = element_text(angle = 90, family = "pd", size = 23),
        axis.text.y = element_text(family = "pd", size = 23),
        axis.title.y = element_text(family = "pd", size = 22, vjust = 2),
        axis.ticks = element_blank(),
        plot.caption = element_text(family = "pd", size = 23, hjust = 1),
        plot.title = element_text(family = "pd", size = 40),
        plot.subtitle = element_text(family = "pd", size = 28),
        plot.title.position = "plot",
        plot.margin = margin(0.8, 0.7, 0.5, 0.7, "cm"))

# Save the Plot

ggsave("Week11.png", p, width = 21, height = 15, dpi = 72)










