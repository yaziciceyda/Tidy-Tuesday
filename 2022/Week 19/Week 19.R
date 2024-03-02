# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 19)

library(tidyverse)
library(ggchicklet)
library(showtext)
library(scales)

# Font in the Plot

font_add_google('Shantell Sans', 'ss', db_cache = FALSE)
showtext_auto()

# Data Wrangling

nyt_titles <- tuesdata$nyt_titles
nyt_full <- tuesdata$nyt_full


ny_data <- nyt_titles %>%
  arrange(desc(total_weeks)) %>%
  filter(total_weeks > 100) %>%
  mutate(x = seq(1, 21, by = 2),
         y = rescale(total_weeks, to = c(0, 30)))

tag_text <- "The books which stayed on the list longer than 100 weeks"

# The Plot

p <- ggplot(ny_data) +
  geom_rect(aes(ymin = -10, ymax = 185,
                xmin = -Inf, xmax = 27),
            fill = "#d3c399",
            colour = NA,
            size = 0.5) +
  annotation_custom(grid::rasterGrob(paste0("#f9f9f7", as.hexmode(1:240)), 
                                     width = unit(1,"npc"), 
                                     height = unit(1,"npc"), 
                                     interpolate = TRUE), 
                    xmin = -Inf, xmax = Inf, 
                    ymin = -10, ymax = 185) +
  ggchicklet:::geom_rrect(aes(xmin = x, xmax = x + 2, 
                              ymin = 0, ymax = total_weeks), 
                          fill = "gray80",
                          color = "black",
                          radius = unit(0.2, units = "cm")) +
  ggchicklet:::geom_rrect(aes(xmin = 0, xmax = 27, 
                              ymin = -10, ymax = 0), 
                          fill = "#994D0A",
                          color = "black",
                          radius = unit(0.2, units = "cm")) +
  geom_text(aes(x = x + 1, y = total_weeks/2 - 25, label = title), angle = 90, 
            family = "ss", fontface = "bold", size = 4) +
  geom_text(aes(x = x + 1, y = total_weeks/2 + 25, label = author), 
            angle = 90, family = "ss") +
  geom_text(aes(x = 18, y = 160, label = "NYTimes Best Sellers"), size = 20,
            family = "ss") +
  labs(tag = tag_text,
    caption = "Data Source: Post45 Data | TidyTuesday 2022 - Week 19 | Prepared by: @Cyd_yzc") +
  theme(panel.background = element_rect(fill = "#d3c399", color = NA),
        plot.background = element_rect(fill = "#d3c399", color = NA),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.tag.position = c(0.7, 0.75),
        plot.tag = element_text(family = "ss", size = 25),
        plot.caption = element_text(family = "ss", size = 20),
        plot.margin = unit(c(2.0, 0.5, 2.0, 0.5), "cm"))

# Save the Plot

ggsave("Week19_2022.png", p, width = 23, height = 15, dpi = 72)

  