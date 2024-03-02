library(tidyverse)
library(lubridate)
library(emoGG)
library(ggflags)
library(countrycode)
library(showtext)


tuesdata <- tidytuesdayR::tt_load(2022, week = 32)

wheels <- tuesdata$wheels

# Fonts in the Plot

sysfonts::font_add_google("Prosto One", "po", db_cache = FALSE)
showtext::showtext_auto()

data_wheels <- wheels %>%
  mutate(year_opened = year(opened),
         year_closed = year(closed),
         age = ifelse(is.na(year_closed), 2022 - year_opened, year_closed - year_opened)) %>%
  arrange(desc(age)) %>%
  top_n(5) %>%
  mutate(name = reorder(name, age),
         country_code = tolower(countrycode(country, 
                                    origin = 'country.name', destination = 'iso2c')))

t <- seq(-3.5, 3.5, length.out = 500) * 15

spiral1 <- data.frame(x    = 15*sin(t) + 128 , 
                     y    = cos(t) + 4.5,
                     text = paste("Great Wheel"),
                     time = 1:500
                     
) 

spiral2 <- data.frame(x    = 15*sin(t) + 128 , 
                      y    = cos(t) + 3.5,
                      text = paste("Wiener Riesenrad"),
                      time = 1:500
                      
) 
spiral3 <- data.frame(x    = 15*sin(t) + 125 , 
                      y    = cos(t) + 2.5,
                      text = paste("Grande Roue de Paris"),
                      time = 1:500
                      
)
spiral4 <- data.frame(x    = 15*sin(t) + 102, 
                      y    = cos(t) + 1.5,
                      text = paste("Wonder Wheel"),
                      time = 1:500
                      
)
spiral5 <- data.frame(x    = 15*sin(t) + 55, 
                      y    = cos(t) + 0.5,
                      text = paste("Shining Flower Wheel"),
                      time = 1:500
                      
)

p <- ggplot(data_wheels, aes(reorder(name, -age), age, country)) +
  geom_segment(aes(x = 0, xend = age, y = name, yend = name)) +
  geom_emoji(aes(x = age, y = name), emoji = "1f3a1", size = 0.1) +
  geom_flag(mapping = aes(x = 1, y = name,
                          country = country_code), size = 25) +
#  geom_text(aes(x = 20, y = name, label = name), size = 10) +
  geom_textpath(data = spiral1, aes(x, y, label = text), 
                size = 7, text_only = TRUE, family = "po") +
  geom_textpath(data = spiral2, aes(x, y, label = text), 
                size = 7, text_only = TRUE, family = "po") +
  geom_textpath(data = spiral3, aes(x, y, label = text), 
                size = 7, text_only = TRUE, family = "po") +
  geom_textpath(data = spiral4, aes(x, y, label = text), 
                size = 7, text_only = TRUE, family = "po") +
  geom_textpath(data = spiral5, aes(x, y, label = text), 
                size = 7, text_only = TRUE, family = "po") +
  scale_x_discrete(limits = c(20, 40, 60, 80, 100, 120, 140)) +
  coord_cartesian() +
  labs(y = "",
       x = "AGE",
       title = "FERRIS WHEELS",
       subtitle = "\nThe oldest four ferris wheels are older than 100 years old, but only the Wiener\nRiesenrad and Wonder Wheel are still operating.",
       caption = "Data Source: {ferriswheels} | TidyTuesday 2022 - Week 32 | Prepared by: @Cyd_yzc") +
  theme(
    panel.background = element_rect(fill = "#F4D65E", color = NA),
        plot.background = element_rect(fill = "#F4D65E", color = NA),
        axis.text.y = element_blank(),
        axis.title.x = element_text(family = "po", size = 18),
        axis.text.x = element_text(family = "po", size = 17),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(family = "po", size = 45),
        plot.caption = element_text(family = "po", hjust = 1, size = 20),
        plot.subtitle = element_text(family = "po", size = 25),
    plot.margin = unit(c(1, 5, 1, 5), "cm"))
ggsave("Week32_2022.png", p, width = 25, height = 15, dpi = 100)

  
