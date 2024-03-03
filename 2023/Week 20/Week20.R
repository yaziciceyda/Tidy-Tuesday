# Tidy Tuesday - 2023 - Week 20

# Libraries used

library(tidyverse)
library(showtext)
library(ggtext)
library(ggimage)
library(patchwork)



# Font in the Plot

font_add_google('Metal Mania', 'metman')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 20)
tornados <- tuesdata$tornados

# Data Wrangling for the map

tornado_data <- tornados %>%
  filter(inj >= 500) %>%
  mutate(mag = as.factor(mag),
        img = paste0(here::here(), "/tornado.jpg"))

# Data Wrangling for the time series plot

tornado_year <- tornado_data %>%
  group_by(yr) %>%
  summarise(n = n()) %>%
  ungroup()

# Time series plot

plot_year <- ggplot(tornado_year) +
  geom_point(aes(x = yr, y = n), shape = 16, size = 5, color = "#0C055A") +
  geom_line(aes(x = yr, y = n), color = "#0C055A", linewidth = 1) +
  scale_y_discrete(limits = c(1, 2, 3)) +
  labs(y = "Frequency",
       x = "Year") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.ticks = element_blank(),
        axis.title = element_text(family = "metman", size = 20),
        axis.text = element_text(family = "metman", size = 19),
        plot.margin = unit(c(1, 2, 1, 2), "cm"))
 


# World Map

world <- ne_countries(scale = "medium", returnclass = "sf")

# Subtitle of the plot

subtitle_text <- str_wrap("\nThe location of the tornados that injured more 
than 500 people are shown here. All of them 
occured in the south-east of USA between 1953 - 2021 and have a magnitude of 
4 or 5.", 90)

# The Map

plot_map <- ggplot() +
  geom_sf(data = world %>% filter(name == "United States"),
          colour = "#A16E47",
          fill = "white")  +
  geom_image(data = tornado_data,
             mapping = aes(x = slon, y = slat, image = img)) +
  geom_text(data = tornado_data,
            mapping = aes(slon, y = slat + 1, label = inj, color = inj),
            family = "metman", size = 5) +
  scale_color_gradient(low = "#4B3CFB",
                       high = "#0C055A") +
  coord_sf(xlim = c(-65, -130), ylim = c(22, 50), expand = FALSE) +
  labs(title = "TORNADOS",
       subtitle = subtitle_text,
       color = "Number of\ninjuries",
       caption = "Data Source: NOAA's National Weather Service Storm Prediction Center\nTidyTuesday 2023 - Week 20 | Prepared by: @C. YAZICI") +
  theme(panel.background = element_rect(fill = "lightblue", color = NA),
        plot.background = element_rect(fill = "lightblue", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.background = element_rect(fill = "lightblue"),
        legend.title = element_text(family = "metman", size = 20),
        legend.text = element_text(family = "metman", size = 19),
        legend.key.height = unit(1.3, 'cm'),
        plot.caption = element_text(family = "metman", size = 20, hjust = 1),
        plot.title = element_text(family = "metman", size = 35, hjust = 0),
        plot.subtitle = element_text(family = "metman", size = 28, hjust = 0),
        plot.margin = unit(c(1, 2, 1, 2), "cm"))

# The final plot

final_plot <- plot_map +
  inset_element(plot_year, left = 0.11, bottom = 0.5, 
                           right = 0.48, top = 0.9) +
  theme(plot.margin = unit(c(1, 2, 1, 2), "cm"))
final_plot

# Save the Plot

ggsave("Week20.png", final_plot, width = 28, height = 15, dpi = 72)

