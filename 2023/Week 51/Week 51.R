library(tidyverse)
library(here)
library(ggimage)
library(showtext)

# Font in the Plot

font_add_google(name = "Dancing Script",
                family = "ds")
showtext_auto()

# Data Import
tuesdata <- tidytuesdayR::tt_load(2023, week = 51)
holiday_episodes <- tuesdata$holiday_episodes
holiday_episode_genres <- tuesdata$holiday_episode_genres


# List of parent titles of food shows

cooking_list <- holiday_episodes %>%
  filter(str_detect(parent_simple_title, 
                    "gordon|chef|bake|cook|baking"),
         christmas == TRUE) %>%
  group_by(parent_original_title) %>%
  count() %>%
  ungroup() %>%
  mutate(x = row_number(),
         img_show = paste0(here::here(), 
                           "/Desktop/Tidy Tuesday/2023/Week 51/", 
                           x, ".jpg"))
# Data Wrangling

holiday_data <- holiday_episodes %>%
  filter(parent_original_title %in% 
           cooking_list$parent_original_title,
        christmas == TRUE) %>%
   mutate(img = paste0(here::here(), 
                      "/Desktop/Tidy Tuesday/2023/Week 51/", 
                      "candy", ".jpg"),
         parent_original_title = reorder(parent_original_title, 
                                         parent_average_rating),
         parent_original_title = as.factor(parent_original_title)) %>%
  right_join(cooking_list, by = "parent_original_title") %>%
  arrange(desc(parent_average_rating)) %>%
  mutate(y_level = case_when(
    parent_original_title == "The Great British Bake Off" ~ 6,
    parent_original_title == "Gordon, Gino & Fred's Road Trip" ~ 5,
    parent_original_title == "The Great British Baking Show: Holidays" ~ 4,
    parent_original_title == "Top Chef" ~ 3,
    parent_original_title == "Holiday Baking Championship" ~ 2,
    parent_original_title == "Baking It" ~ 1,
  ),
    y_level_img = case_when(
    parent_original_title == "The Great British Bake Off" &
                       average_rating == 7.2 &
                       year == 2021 ~ y_level - 0.1,
    parent_original_title == "The Great British Bake Off" &
                       average_rating == 7.2 &
                       year == 2016 ~ y_level + 0.1,
    parent_original_title == "The Great British Bake Off" &
                       average_rating == 7.7 &
                       year == 2020 ~ y_level + 0.1,
    parent_original_title == "The Great British Bake Off" &
                       average_rating == 7.9 &
                       year == 2019 ~ y_level + 0.1,
    parent_original_title == "The Great British Bake Off" &
                       average_rating == 7.8 &
                       year == 2015 ~ y_level + 0.1,
  TRUE ~ as.numeric(y_level)))

  
# Subtitle of the plot

subtitle_text <- str_wrap("The Christmas episodes of the Food 
Shows generally have lower ratings than the average rating of 
the show.\n\n\n", 120)

# The plot

p <- ggplot(holiday_data) +
  # The Name of the Show
    geom_text(aes(x = 7.25,
        y = reorder(parent_original_title, parent_average_rating),
        label = paste0(parent_original_title, " (",
        parent_average_rating, ")")), 
        vjust = -3,
        hjust = 0, color = "#00873E", family = "ds", 
        size = 7, fontface = "bold", check_overlap = TRUE) +
  # The line of each show
    geom_segment(aes(x = 6.7, xend = 8.7, 
                     y = parent_original_title, 
                     yend = parent_original_title), color = "red",
                 linewidth = 1.5) +
  # The cotton candy image representing the rating of the episode
    geom_image(aes(x = average_rating, 
                   y = y_level_img, 
                   image = img), size = 0.08, by = 'height') +
  # The poster of each food show
    geom_image(aes(x = 6.4, 
                   y = parent_original_title, 
                   image = img_show), size = 0.12) +
  # The line indicating the average rating of the show
    geom_segment(aes(x = parent_average_rating,
                     xend = parent_average_rating,
                     y = y_level - 0.4, 
                     yend = y_level + 0.4),
                 color = "red", linetype = "dashed", 
                 linewidth = 1.5) +
    # The arrow
    geom_curve(aes(x = 8.5, xend = 8.6,
                  y = 6.4, yend = 6.1), 
              curvature = -0.2,
              arrow = arrow(length = unit(0.01, "npc")),
              linewidth = 1) +
  # The text explaining the average rating of the show
    geom_text(aes(x = 8.3, y = 6.3, 
                label = "Average IMDb Rating\nfor the Series "),
            family = "ds", size = 7) +
    scale_x_continuous(limits = c(6.3, 8.8), breaks = c(7, 7.5, 8, 8.5)) +
    scale_y_discrete(expand = waiver()) +
    labs(y = "",
         x = "Average IMDb Rating",
         subtitle = subtitle_text,
         title = "HOLIDAY EPISODES\n",
         caption = "Data Source: IMDB | TidyTuesday 2023 - Week 51 | Prepared by: C. YAZICI") +
    theme(panel.background = element_rect(fill = "#FAFAFA", color = NA),
          plot.background = element_rect(fill = "#FAFAFA", color = NA),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_text(family = "ds", size = 25, 
                                      vjust = -1),
          axis.text.x = element_text(family = "ds", size = 23),
          plot.caption = element_text(family = "ds", hjust = 1, 
                                      size = 25, vjust = -2),
          plot.title = element_text(family = "ds", size = 30),
          plot.title.position = "plot",
          plot.subtitle = element_text(family = "ds", size = 28),
          plot.margin = margin(0.6, 0.6, 1.0, 0.6, "cm"))

# Save the Plot

ggsave("Week51.png", p, width = 23, height = 15, dpi = 72)




