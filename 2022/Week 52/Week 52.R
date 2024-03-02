tlBooks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlBooks.csv')
tlFootnotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlFootnotes.csv')

library(tidyverse)
library(ggforce)
library(showtext)
library(trekfont)
library(trekcolors)
library(ggtext)
library(ggstar)


data_vulcan <- tlBooks %>%
  filter(str_detect(title, "Vulcan|vulcan"),
         format == "book") %>%
  arrange(title, year) %>%
  left_join(tlFootnotes, by = c("footnote")) %>%
  group_by(title) %>%
  slice_min(n = 1, year) %>%
  ungroup() %>%
  arrange(year) %>%
  mutate(y = -1:9)


# Stars Data

stars <- data.frame(x = runif(min = -1, max = 2, n = 100),
                    y = runif(min = -2, max = 10, n = 100))


p <- ggplot(data_vulcan) +
  geom_rect(mapping = aes(ymin = -2, ymax = 14,
                          xmin = -1, xmax = 2),
            fill = "#F0EDB9",
            colour = NA,
            size = 0.5) +
  annotation_custom(grid::rasterGrob(paste0("#0F0E0E", as.hexmode(1:240)), 
                                     width = unit(1,"npc"), 
                                     height = unit(1,"npc"), 
                                     interpolate = TRUE), 
                    xmin = -1, xmax = 2, 
                    ymin = -2.5, ymax = 14) +
  geom_star(data = stars, aes(x = x, y = y), size = 2, fill = "grey50") +
  scale_starshape_manual(values = 1) +
  geom_line(aes(x = 0, y = y), size = 3, color = "grey20") +
  geom_line(aes(x = 0, y = y), size = 2, color = "grey50") +
  geom_point(aes(x = 0, y = y), color = "grey20", size = 8) +
  geom_point(aes(x = 0, y = y), color = "grey50", size = 6) +
  # Titles
  geom_text(aes(x = 0.1, y = y, label = title, color = title), 
            hjust = 0, family = "Khan", size = 10) +
  # Years
  geom_text(aes(x = -0.2, y = y, label = year), 
            hjust = 1, family = "Khan", size = 10) +
  # Label - Year
  geom_text(aes(x = -0.2, y = 10, label = "Year"), 
            hjust = 1, family = "Khan", size = 15) +
  # Label - Title
  geom_text(aes(x = 0.1, y = 10, label = "Title"), 
            hjust = 0, family = "Khan", size = 15) +
  # Title
  geom_text(x = -1, y = 13, label = "STAR TREK", 
            hjust = 0, family = "StarNext", size = 22) +
  # Subtitle
  geom_text(x = -1, y = 11.5, label = "The Books including Vulcan in their title in the chronological order", 
            hjust = 0, family = "Khan", size = 12) +
  scale_color_trek("tholian") +
    labs(caption = "Data Source: {rtrek} | TidyTuesday 2022 - Week 52 | Prepared by: @Cyd_yzc") +
  theme(plot.subtitle = element_markdown(size = 20, family = "Khan"),
        plot.title = element_markdown(family = "StarNext", size = 30,
                                      margin = margin(t = 10)),
        plot.caption = element_text(size = 15, family = "Khan", hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = NA),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

ggsave("Week52_2022.png", p, width = 25, height = 15, dpi = 72)



