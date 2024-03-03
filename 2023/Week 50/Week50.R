library(tidyverse)
library(here)
library(ggimage)
library(emojifont)
library(sysfonts)
library(ggtext)
library(showtext)

# Font in the Plot

font_add_google(name = "Dancing Script",
                family = "ds")
showtext_auto()

# Fontawesome Library

font_add('fa-solid', 'Font Awesome 6 Free-Solid-900.otf')

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 50)
holiday_movies <- tuesdata$holiday_movies
holiday_movie_genres <- tuesdata$holiday_movie_genres

# Data Wrangling

movie_data <- holiday_movies %>%
  filter(num_votes > 10000, # Votes greater than 10000
         christmas == TRUE) %>% # Christmas Films
  arrange(desc(average_rating)) %>%
  slice(1:10) %>% #Top 10
  mutate(img = paste0(here(), "/2023/Week 50/film", row_number(), ".jpg"),
         label_bell = "<span style='font-family:fa-solid'>&#xf0f3;</span>",
         img_santa = paste0(here(), "/2023/Week 50/santa.jpg"),
         x = seq(from = 1, to = 20, by = 2),
         label_x = "bell",
         label_title = str_replace_all(original_title, " ", "\n"))

# Subtitle of the Plot

subtitle_text <- "The top 10 most popular and most rated Christmas films with their runtime. "

# The Plot

p <- ggplot(movie_data) +
  # The red line between films
  geom_line(aes(x = x, y = average_rating), color = "red",
            linewidth = 2) +
  # The curve that connects Santa with the films
  geom_curve(aes(x = 19, xend = 23,
                   y = 7.5, yend = 7.5),
                   color = "red",
            linewidth = 2,  curvature = 0.4) +
  # The poster of the films
  geom_image(aes(x = x, y = average_rating, image = img),
             size = 0.10, by = 'height') +
  # The Santa
  geom_image(aes(x = 25, y = 7.5, image = img_santa),
             size = 0.11, by = 'height') +
  # The bells at the top of the films
  geom_richtext(aes(x = x,
                    y = average_rating + 0.7,
                    label = label_bell,
                    size = runtime_minutes / 10),
                    colour = "red",
                family = 'fontawesome-webfont',
                fill = NA, label.colour = NA) +
  # The label of the runtime
  geom_text(aes(x = x, y = average_rating + 0.5,
                label = paste0(runtime_minutes, " mins")),
            family = "ds", size = 8) +
  # The title of the films
  geom_text(aes(x = x, y = average_rating - 0.8, label = label_title),
            family = "ds", size = 7) +
  scale_size_identity() +
  scale_y_continuous(limits = c(6.5, 9.5)) +
  scale_x_continuous(limits = c(0, 26)) +
  labs(y = "Average Rating",
       x = "",
       title = "HOLIDAY MOVIES\n",
       subtitle = subtitle_text,
       caption = "Data Source: IMDB\nTidyTuesday 2023 - Week 50\nPrepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(family = "ds", size = 20),
        axis.text.y = element_text(family = "ds", size = 18),
        plot.title = element_text(family = "ds", size = 40),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "ds", size = 30),
        plot.caption = element_text(family = "ds", size = 20, hjust = 1),
        plot.margin = margin(0.6, 0.6, 0.6, 0.5, "cm"))

# Save the Plot

ggsave("Week50.png", p, width = 23, height = 15, dpi = 72)








