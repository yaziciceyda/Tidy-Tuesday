library(tidyverse)
library(scales)
library(ggimage)
library(showtext)

# Font in the Plot

font_add_google(name = "Protest Riot",
                family = "pr", db_cache = FALSE)
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2024, week = 10)
trashwheel <- tuesdata$trashwheel

# Data Wrangling

data_trash <- trashwheel %>%
  group_by(Year) %>%
  summarise(total_pl_bottles = sum(PlasticBottles, na.rm = TRUE),
            total_poly = sum(Polystyrene, na.rm = TRUE),
            total_cigarettes = sum(CigaretteButts, na.rm = TRUE),
            total_gl_bottles = sum(GlassBottles, na.rm = TRUE),
            total_pl_bags = sum(PlasticBags, na.rm = TRUE),
            total_balls = sum(SportsBalls, na.rm = TRUE),
            total_wrappers = sum(Wrappers, na.rm = TRUE),
            total_homes = sum(HomesPowered, na.rm = TRUE)
            ) %>%
  pivot_longer(-Year, names_to = "types", values_to = "total_values") %>%
  filter(Year == 2023) %>%
  arrange(total_values) %>%
  mutate(img_trash = paste0(here::here(),
                                    "/Desktop/Tidy Tuesday/2024/Week 10/",
                                    "trash_wheel", ".jpg"),
         linewidth = rescale(total_values, to = c(0, 1)))

# Subtitle of the Plot

subtitle_text <- str_wrap("Mr. Trash Wheel is a semi-autonomous trash 
interceptor that is placed at the end of a river, stream or other 
outfall. In 2023, the most collected type of trash is cigarette butts
(496,090), while the least collected type is sports balls (1,242). 
Moreover, 8,425 homes are powered with the electricity produced by the 
trash.", 110)

# The Plot 

p <- ggplot(data_trash) +
  # glass bottles
  annotate(
    'curve',
    x = 0.55, xend = 1,
    y = 1, yend = 0.4,
    linewidth = 1.5,
    curvature = -0.2,
    alpha = 0.25,
    arrow = arrow(length = unit(0.5, 'cm'))
  )   +
  annotate("text", x = 0.4, y = 1, label = "Glass\nBottles\n(1,701)",
           family = "pr", hjust = 0, size = 8,
           color = alpha("black", 0.25)) + 
  # Balls
  annotate(
    'curve',
    x = 0.5, xend = 1,
    y = 0.5, yend = 0.3,
    linewidth = 1.5,
    curvature = -0.2,
    alpha = 0.25,
    arrow = arrow(length = unit(0.5, 'cm'))
  ) +
  annotate("text", x = 0.4, y = 0.5, label = "Sports\nBalls\n(1,242)",
           family = "pr", hjust = 0, size = 8,
           color = alpha("black", 0.25)) + 
  # Plastic Bags
  annotate(
    'curve',
    x = 1, xend = 1.2,
    y = 1.2, yend = 0.5,
    linewidth = 1.5,
    curvature = -0.08,
    alpha = 0.5,
    arrow = arrow(length = unit(0.5, 'cm'))
  ) +
  annotate("text", x = 0.9, y = 1.45, label = "Plastic\nBags\n(33,089)",
           family = "pr", hjust = 0, size = 8,
           color = alpha("black", 0.5)) + 
 # polystyrene
  annotate(
    'curve',
    x = 1.3, xend = 1.3,
    y = 1.2, yend = 0.5,
    linewidth = 2,
    curvature = 0,
    alpha = 0.6,
    arrow = arrow(length = unit(0.5, 'cm'))
  ) +
  annotate("text", x = 1.25, y = 1.4, label = "Polystyrene\n(38,195)",
           family = "pr", hjust = 0, size = 8,
           color = alpha("black", 0.6)) +
  # wrappers
  annotate(
    'curve',
    x = 1.5, xend = 1.4,
    y = 1.25, yend = 0.5,
    linewidth = 2.5,
    curvature = 0.08,
    alpha = 0.7,
    arrow = arrow(length = unit(0.5, 'cm'))
  ) +
  annotate("text", x = 1.5, y = 1.4, label = "Wrappers\n(284,570)",
           family = "pr", hjust = 0, size = 8,
           color = alpha("black", 0.7)) +
  # Plastic Bottles
  annotate(
    'curve',
    x = 2, xend = 1.5,
    y = 1, yend = 0.4,
    linewidth = 3,
    curvature = 0.2,
    alpha = 0.8,
    arrow = arrow(length = unit(0.5, 'cm'))
  ) +
  annotate("text", x = 2.02, y = 1, label = "Plastic\nBottles\n(337,980)",
           family = "pr", hjust = 0, size = 8,
           color = alpha("black", 0.8)) +
  # cigarette butts
  annotate(
    'curve',
    x = 2, xend = 1.5,
    y = 0.5, yend = 0.3,
    linewidth = 3.6,
    curvature = 0.2,
    alpha = 0.9,
    arrow = arrow(length = unit(0.5, 'cm'))
  ) +
   annotate("text", x = 2.02, y = 0.5, label = "Cigarette\nButts\n(496,090)",
            family = "pr", hjust = 0, size = 8,
            color = alpha("black", 0.9)) +
  # Homes Powered
  annotate(
    'curve',
    x = 1.25, xend = 1.25,
    y = 0.2, yend = -0.2,
    linewidth = 2,
    curvature = 0,
    alpha = 1,
    arrow = arrow(length = unit(0.5, 'cm'))
  ) +
  annotate("text", x = 1.2, y = -0.39, label = "Homes\nPowered\n(8,425)",
           family = "pr", hjust = 0, size = 8,
           color = alpha("black", 1)) +
    geom_image(aes(x = 1.25,
                 y = 0.3,
                 image = img_trash), size = 0.13, by = 'height') +
  scale_size_identity() +
  coord_cartesian(xlim = c(0.4, 2.1),
                  ylim = c(-0.5, 1.55)) +
  labs(title = "Trash Wheel Collection in 2023\n",
       subtitle = subtitle_text,
       caption = "Data Source: Healthy Harbor Trash Wheel Collection Data\nTidyTuesday 2024 - Week 10 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "#fafafa", color = NA),
        plot.background = element_rect(fill = "#fafafa", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "pr", size = 40, hjust = 0),
        plot.title.position = "plot",
        plot.caption = element_text(family = "pr", size = 20, hjust = 1),
        plot.subtitle = element_text(family = "pr", size = 25, hjust = 0),
        plot.margin = margin(1, 0.8, 0.5, 0.8, "cm"))  

# Save the Plot

ggsave("Week10.png", p, width = 22, height = 12, dpi = 72)



  
  
  
  




          

             