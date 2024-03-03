# Tidy Tuesday - 2023 - Week 22

# Libraries used

library(tidyverse)
library(lubridate)
library(ggforce)
library(ggnewscale)
library(showtext)
library(ggtext)

# Font in the Plot

font_add_google("Inter", "inter")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 22)
centenarians <- tuesdata$centenarians

# Data Wrangling

center_data <- centenarians %>%
  group_by(place_of_death_or_residence) %>%
  mutate(avg_year = mean(age)) %>%
  ungroup() %>%
  mutate(birth_year = year(birth_date),
         name = str_replace_all(name, " ", "\n"),
         x_location = case_when(gender == "female" ~ rank,
                                gender == "male" ~ rank * - 1 - 2)) %>%
  filter(rank < 11) %>%
  mutate(name = ifelse(name == "Emiliano\nMercado\ndel\nToro",
                       "Emiliano Mercado\ndel Toro", name),
         name = ifelse(name == "Horacio\nCeli\nMendoza",
                       "Horacio Celi\nMendoza", name),
         name = ifelse(name == "Tomás\nPinales\nFiguereo",
                       "Tomás Pinales\nFiguereo", name),
         name = ifelse(name == "Juan\nVicente\nPérez",
                       "Juan Vicente\nPérez", name),

         name_age = paste0(name, "\n", round(age, 2)))

# Subtitle of the Plot

subtitle_text <- "<br>The names and ages of the 10 oldest
<span style='color:red'>female
<span style='color:black'>and
<span style='color:blue'>male <span style='color:black'>are shown here.
Only the 10th male is <span style='color:#F27C4C'>alive,
<span style='color:black'>but the others<br> are <span style='color:#873514'>dead.
<span style='color:black'>All of them are older than 113 years and have age
greater than the country average. Moreover, females are older<br> than the country
average, but  males are closer to it."

# The Plot

p <- ggplot(center_data) +
  geom_circle(aes(x0 = x_location, y0 = rank * 3.9, r = 2,
                  color = gender, fill = age - avg_year), linewidth = 2) +
  scale_color_manual(values = c("red", "blue"), guide = "none") +
  scale_y_reverse() +
  geom_segment(aes(x = -1, xend = -1, y = 40, yend = 4), color = "#3D1505",
               linewidth = 2) +
  geom_segment(aes(x = -1, xend = x_location - 2*sign(x_location),
                   y = rank * 3.9, yend = rank * 3.9), color = "#3D1505",
               linewidth = 2) +
  scale_fill_gradient(
    low = "#A2F2A8",
    high = "#0F7314") +
  new_scale_color() +
  geom_text(aes(x = x_location, y = rank * 3.9, label = name_age,
                color = still_alive), family = "inter", fontface = "bold",
            size = 5) +
  scale_color_manual(values = c("#F27C4C", "#873514"), guide = "none") +
  # The arrow shows the rank
  geom_segment(aes(x = -15, xend = -15, y = 0, yend = 40),
               arrow = arrow(length = unit(.5, 'cm')), linewidth = 1.5) +
  geom_text(aes(x = -16, y= 20, label = "rank increases (age decreases)"),
            angle = 90, family = "inter", size = 8) +
  coord_cartesian() +
  labs(title = "VERIFIED OLDEST PEOPLE",
       subtitle = subtitle_text,
       fill = "Difference from\nCountry Average\n(in years)",
       caption = "Data Source: Wikipedia | TidyTuesday 2023 - Week 22 | Prepared by: @C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "ivory"),
        legend.title = element_text(family = "inter", size = 19),
        legend.text = element_text(family = "inter", size = 19),
        legend.key.height = unit(2.0, 'cm'),
        legend.key.width = unit(1.0, 'cm'),
        plot.title = element_text(family = "inter", hjust = 0, size = 50),
        plot.subtitle = element_markdown(size = 22, family = "inter", hjust = 0),
        plot.caption = element_text(family = "inter", size = 18, hjust = 1),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Save the Plot

ggsave("Week22.png", p, width = 25, height = 15, dpi = 72)


