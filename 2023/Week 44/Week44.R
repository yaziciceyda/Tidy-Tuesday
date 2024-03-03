library(dplyr)
library(lubridate)
library(ggplot2)
library(stringi)
library(showtext)

# Fonts in the Plot

font_add_google('Creepster', 'cr')
showtext_auto()

font_add_google('Raleway Dots', 'rd')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 44)
horror_articles <- tuesdata$horror_articles

# Data Wrangling

data_horror <- horror_articles %>%
  arrange(desc(published)) %>%
  mutate(year = year(published)) %>%
  top_n(40) %>%
  mutate(y = row_number(),
         rating = str_to_sentence(rating))

# The Plot

p <- ggplot(data_horror) +
  geom_text(aes(x = 1, y = y, label = title, alpha = -year), color = "white",
            family = "cr") +
  geom_text(aes(x = 1.5, y = y, label = rating, alpha = -year), color = "white",
            family = "cr") +
  scale_x_continuous(limits = c(0.7, 1.6), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.6, 44), expand = c(0, 0)) +
  annotate(geom = "text", x = 1, y = 43, label = "TITLE",
           color = "white", family = "cr", size = 10, fontface = "bold") +
  annotate(geom = "text", x = 1.5, y = 43, label = "RATING",
           color = "white", family = "cr", size = 10, fontface = "bold") +
  coord_cartesian() +
  labs(title = "HORROR LEGENDS",
       caption = "#TidyTuesday - 2023 - Week 44 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
        plot.title = element_text(family = "cr", hjust = 0, size = 35,
                                  color = "white"),
        plot.caption = element_text(family = "cr", hjust = 1, size = 15,
                                  color = "white"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        aspect.ratio = 10/9,
        plot.margin = unit(c(1, 1.5, 1.3, 1.5), "cm"))

# Save the Plot

ggsave("Week44.png", p, width = 17, height = 15, dpi = 72)


        