library(ggchicklet)
library(showtext)
library(usefunc)
library(tidyverse)

# Fonts in the Plot

font_add_google('Rampart One', 'ro')
showtext_auto()

font_add_google('Martel', 'martel')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 03)

arthistory <- tuesdata$artists

# Data Preperation and Wrangling

art_data <- arthistory %>%
  arrange(year) %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  ungroup()

art_data1 <- arthistory %>%
  filter(year == 1926) %>%
  count(artist_unique_id) # 21 artists
# only Gardner

art_data2 <- arthistory %>%
  filter(year == 2001, book == "Janson") %>%
  count(artist_unique_id) # 170 artists in Gardner's Book
# 169 artists in Janson's Book

art_data3 <- arthistory %>%
  filter(year == 2020) %>%
  count(artist_unique_id) # 182 artists
# Only Gardner


# Subtitle of the Plot

subtitle_text <- str_wrap("The {arthistory} dataset includes information of artists
between 1926 - 2020 based on Gardner’s and Janson's art history textbooks. Here,
the number of artists and the specified book is given for the first and last 
year in addition to the 2001 when the maximum number of artists made an 
exhibition.", 110)

# The Plot

p <- ggplot() +
  geom_rect(mapping = aes(ymin = -Inf, ymax = Inf,
                          xmin = -Inf, xmax = Inf),
            fill = "#C68FC5",
            colour = NA,
            size = 0.5) +
  annotation_custom(grid::rasterGrob(paste0("#0F0E0E", as.hexmode(1:240)), 
                                     width = unit(1,"npc"), 
                                     height = unit(1,"npc"), 
                                     interpolate = TRUE), 
                    xmin = -Inf, xmax = Inf, 
                    ymin = -Inf, ymax = Inf) +

  # Year : 1926
  ggchicklet:::geom_rrect(aes(xmin = 1, xmax = 3, 
                              ymin = 1, ymax = 3), 
                          color = "#ED441B",
                          fill = "#F1AC9F",
                          radius = unit(0.5, units = "cm")) +
  geom_text(aes(x = 2, y = 2.5), label = "1926", family = "ro", size = 12) +
  geom_text(aes(x = 2, y = 2.0), label = "21 Artists", family = "ro", size = 10) +
  geom_text(aes(x = 2, y = 1.5), label = "Gardner's Book", family = "ro", size = 10) +
  # Year: 2001
  ggchicklet:::geom_rrect(aes(xmin = 4, xmax = 6, 
                              ymin = 1, ymax = 3), 
                          color = "#41ED1B",
                          fill = "#BCF19F",
                          radius = unit(0.5, units = "cm")) +
  geom_text(aes(x = 5, y = 2.5), label = "2001", family = "ro", size = 12) +
  geom_text(aes(x = 5, y = 2.0), label = "170 Artists in\nGardner's Book", 
            family = "ro", size = 10) +
  geom_text(aes(x = 5, y = 1.5), label = "169 Artists in\nJanson's Book",
            family = "ro", size = 10) +
  # Year: 2020
  ggchicklet:::geom_rrect(aes(xmin = 7, xmax = 9, 
                              ymin = 1, ymax = 3), 
                          color = "#1BE3ED",
                          fill = "#9FEDF1",
                          radius = unit(0.5, units = "cm")) +
  geom_text(aes(x = 8, y = 2.5), label = "2020", family = "ro", size = 12) +
  geom_text(aes(x = 8, y = 2.0), label = "182 Artists", family = "ro", 
            size = 10) +
  geom_text(aes(x = 8, y = 1.5), label = "Gardner's Book", family = "ro",
            size = 10) +
  coord_cartesian() +
  labs(title = "ART HISTORY",
       subtitle = subtitle_text,
       caption = "Data Source: {arthistory} | #30DayChartChallenge - Day #4 - TidyTuesday 2023 - Week 3 | Prepared by: C. YAZICI") +
  theme(plot.background = element_rect(fill = "#C68FC5", color = NA),
        panel.background = element_rect(fill = "#C68FC5", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.caption = element_text(family = "martel", size = 20, hjust = 0.5),
        plot.title = element_text(family = "ro", size = 40, hjust = 0.5),
        plot.subtitle = element_text(family = "martel", size = 25, hjust = 0),
        plot.margin = unit(c(1.0, 0.5, 1.0, 0.5), "cm"))

# Save the Plot

ggsave("Day4.png", p, width = 30, height = 15, dpi = 72)










