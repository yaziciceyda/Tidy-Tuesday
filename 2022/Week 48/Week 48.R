wcmatches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

library(ggplot2)
library(tidyverse)
library(dplyr)
library(emojifont)
devtools::install_github("jimjam-slam/ggflags")
library(ggflags)
library(countrycode)
library(tidytext)
library(showtext)

wc_winner <- worldcups %>%
  count(winner) %>%
  mutate(stage = "winner",
         medal = "1st_place_medal",
         color = "#D6AF36",
         x = 5.5,
         y = 5) %>%
  rename(country = winner)

wc_second <- worldcups %>%
  count(second) %>%
  mutate(stage = "second",
         medal = "2nd_place_medal",
         color = "#FEE101",
         x = 5.5,
         y = 5.7) %>%
  rename(country = second)

wc_third <- worldcups %>%
  count(third) %>%
  mutate(stage = "third",
         medal = "3rd_place_medal",
         color = "#d7d7d7",
         x = 5.5,
         y = 4.3) %>%
  rename(country = third)

wc_all <- wc_winner %>%
  rbind(wc_second, wc_third) %>%
  filter(n > 1) %>%
  mutate(stage = factor(stage, levels = c("winner", "second", "third"))) %>%
#  arrange(stage, -n) %>%
  mutate(country_code = countrycode(country, 
                                    origin = 'country.name', destination = 'iso2c'),
         country_code = ifelse(country == "England", "GB", country_code),
         country_code = ifelse(country == "Czechoslovakia", "CL", country_code),
         country_code = tolower(country_code),
         country = reorder_within(country, n, stage)) 

font_add_google("DM Serif Display", "sd")
showtext_auto()

 ggplot(wc_all) +
  geom_col(mapping = aes(x = n, y = country), width = 0.1) +
  geom_flag(mapping = aes(x = n, y = country,
                          country = country_code), size = 15) +
  geom_text(mapping = aes(x = x, y = y, 
                          label = emoji(medal), color = color),
            family = "EmojiOne", size = 58) +
  facet_grid(rows = vars(stage), scales = "free_y") +
  scale_color_identity() +
  coord_cartesian() +
  labs(y = "",
       x = "The frequency of winnings",
       title = "\nWorld Cup",
       subtitle = "\nThe number of times that the countries has been the first, second or third place finishers between 1930 - 2018.",
       caption = "Data Source: FIFA World Cup | TidyTuesday 2022 - Week 48 | Prepared by: @Cyd_yzc") +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_text(family = "sd", size = 20),
        axis.text.x = element_text(family = "sd", size = 17),
        panel.background = element_rect(fill = "#7eaf34"),
        plot.title = element_text(family = "sd", size = 30),
        plot.subtitle = element_text(family = "sd", size = 20),
        plot.caption = element_text(family = "sd", size = 14, hjust = 1)) +
   scale_size_area(max_size = 20)

ggsave("Week48_2022.png", final_plot, width = 25, height = 14, dpi = 72)

   







