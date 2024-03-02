
tuesdata <- tidytuesdayR::tt_load(2022, week = 39)
artists <- tuesdata$artists

library(tidyverse)
library(ggplot2)
library(maps)
library(usmap)
library(ggsn)
library(emojifont)
library(showtext)
library(ggtext)
library(cowplot)


font_add_google('Permanent Marker', 'pm')
showtext_auto()


artists_data <- artists %>%
  filter(type == "Musicians") %>%
  group_by(state) %>%
  summarise(avg_q = mean(location_quotient)) %>%
ungroup()
  # distinct(type)  # 13 
 # distinct(race) # 5
 # distinct(state) # 52

us_map <- usmap::us_map()
us_coordinates <- us_map %>%
  group_by(full) %>%
  summarise(avg_x = mean(x),
            avg_y = mean(y)) %>%
  ungroup()

artists_data <- artists_data %>%
  inner_join(us_coordinates, by = c("state" = "full")) %>%
  mutate(music = "notes")

artists_data <- artists_data %>%
  mutate(avg_x = ifelse(state == "Florida", avg_x + 100000, avg_x),
         avg_x = ifelse(state == "Michigan", avg_x + 100000, avg_x),
         avg_y = ifelse(state == "Michigan", avg_y - 10000, avg_y),
         avg_y = ifelse(state == "New York", avg_y + 100000, avg_y),
         avg_y = ifelse(state == "Connecticut", avg_y + 15000, avg_y),
         avg_y = ifelse(state == "Massachusetts", avg_y + 20000, avg_y),
         avg_x = ifelse(state == "Massachusetts", avg_x - 100000, avg_x))


subtitle_text = str_wrap("\n\nLocation quotients (LQ) measure an artist occupation's concentration in the labor force, relative to the U.S. labor force share. For example, an LQ of 1.2 indicates that the state's labor force in an occupation is 20 percent greater than the occupation's national labor force share. An LQ of 0.8 indicates that the state's labor force in an occupation is 20 percent below the occupation's national labor force share.", 70)

p <- usmap::plot_usmap(data = artists_data, values = "avg_q", labels = F) +
geom_text(artists_data %>% filter(!is.na(avg_q)), mapping = aes(x = avg_x, y = avg_y, 
                        label = emoji(music), size = 10), color = "black",
          family = "EmojiOne") +
  labs(fill = 'Location quotients (LQ)',
       title = 'Musicians in the USA\n',
       subtitle = subtitle_text,
       caption = "Data Source: arts.gov | TidyTuesday 2022 - Week 39 | Prepared by: @Cyd_yzc") + 
  scale_fill_gradientn(colours = rev(heat.colors(10)), na.value = "grey90",
                       guide = guide_colourbar(barwidth = 25, barheight = 0.4,
                                               #put legend title on top of legend
                                               title.position = "top")) +
  # put legend at the bottom, adjust legend title and text font sizes
  theme(legend.position = "bottom",
        legend.title = element_text(size = 14, family = "pm"), 
        legend.text = element_text(size = 12, family = "pm"),
        plot.title = element_text(size = 35, family = "pm"),
        plot.subtitle = element_text(size = 18, family = "pm"),
        plot.caption = element_text(size = 13, family = "pm", hjust = 1)) +
  guides(size = "none") 
p

ggsave("Week39_2022.png", p, width = 20, height = 10, dpi = 72)

