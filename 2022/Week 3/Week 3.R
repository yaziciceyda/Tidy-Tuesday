library(tidyverse)
library(tidytext)
library(ggchicklet)
library(showtext)

# Font in the Plot

font_add_google('Space Grotesk', 'sg', db_cache = FALSE)
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 3)

chocolate <- tuesdata$chocolate

# Data Wrangling

chocolate_data <- chocolate %>%
  mutate(cocoa_percent = str_replace_all(cocoa_percent, "%", ""),
         cocoa_percent = as.numeric(cocoa_percent), # between 42 and 100 
         cocoa_level = factor(case_when(
           cocoa_percent >= 42 & cocoa_percent < 50 ~ "40% - 50%",
           cocoa_percent >= 50 & cocoa_percent < 60 ~ "50% - 60%",
           cocoa_percent >= 60 & cocoa_percent < 70 ~ "60% - 70%",
           cocoa_percent >= 70 & cocoa_percent < 80 ~ "70% - 80%",
           cocoa_percent >= 80 & cocoa_percent < 90 ~ "80% - 90%",
           cocoa_percent >= 90 & cocoa_percent <= 100 ~ "90% - 100%",
         ), levels = c("40% - 50%", "50% - 60%", "60% - 70%",
                       "70% - 80%", "80% - 90%", "90% - 100%"))) %>%
 group_by(cocoa_level) %>%
  summarise(avg_rating = mean(rating)) %>%
  ungroup() %>%
  mutate(xmin = c(1, 3, 5, 7, 9, 11),
         xmax = c(3, 5, 7, 9, 11, 13))

# Subtitle of the Plot

subtitle_text <- str_wrap("\nThe cocoa percent in chocolate ranges between 42% - 100%. Here, 
it is categorized into six and the ratings of them (between 1 and 5) is 
represented as the heights. The bitter and the most sweet ones have the lowest
average ratings; while the ones with the 60% - 80% cocoa percent have the 
highest average ratings.", 100)

# The Plot

p <- ggplot(chocolate_data) +
  ggchicklet:::geom_rrect(aes(xmin = xmin, xmax = xmax, 
                              ymin = 1 - avg_rating, ymax = 2 + avg_rating, 
                              fill = cocoa_level), 
                          radius = unit(0.8, units = "cm")) +
  scale_fill_manual(values = c("#b97053", "#9c5a3f", "#794631",
                               "#573223", "#341e15", "#110a07"),
                    labels = levels(chocolate_data$cocoa_level)) +
  ggchicklet:::geom_rrect(aes(xmin = xmin + 0.2, xmax = xmax - 0.2, 
                              ymin = 1.4 - avg_rating, ymax = 1.6 + avg_rating), 
                          fill = "#dab4a4",
                          radius = unit(0.8, units = "cm")) +
  geom_text(aes(x = (xmin + xmax)/2, y = 1.3, label = cocoa_level),
            family = "sg", size = 7) +
  geom_text(aes(x = (xmin + xmax)/2, y = 2.8, label = round(avg_rating, 2)),
            family = "sg", size = 10) +
  coord_fixed() +
  labs(fill = "Cocoa Percent",
       title = "CHOCOLATE RATINGS",
       subtitle = subtitle_text,
       caption = "Data Source: Flavors of Cacao | TidyTuesday 2022 - Week 3 | Prepared by: @Cyd_yzc") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(family = "sg", size = 30, hjust = 0, 
                                  color = "brown"),
        plot.subtitle = element_text(family = "sg", size = 20, hjust = 0,
                                     color = "brown"),
        plot.caption = element_text(family = "sg", size = 18, hjust = 1,
                                    color = "brown"),
        legend.background = element_rect(fill = "ivory"),
        legend.title = element_text(family = "sg", size = 15, color = "brown"),
        legend.text = element_text(family = "sg", size = 13, color = "brown"),
        plot.margin = unit(c(0.5, 0.3, 0.5, 0.3), "cm"))

# Save the Plot

ggsave("Week3_2022.png", p, width = 25, height = 15, dpi = 72)


