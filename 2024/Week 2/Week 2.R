library(tidyverse)
library(lubridate)
library(ggfx)
library(scales)
library(showtext)
library(cowplot)


# Font in the Plot

font_add_google(name = "Salsa",
                family = "salsa")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2024, week = 2)
canada_births_1991_2022 <- tuesdata$canada_births_1991_2022
nhl_player_births <- tuesdata$nhl_player_births
nhl_rosters <- tuesdata$nhl_rosters
nhl_teams <- tuesdata$nhl_teams

# Data Preprocessing

data_rosters <- nhl_rosters %>%
  mutate(year = year(birth_date),
         month = month(birth_date),
         country = case_when(
           birth_country == "CAN" ~ "CANADA",
           .default = "OTHER",
         )) %>%
  filter(year >= 1991) %>%
  arrange(year, month) %>%
  group_by(month, country) %>%
  summarise(avg_height = mean(height_in_centimeters, na.rm = TRUE),
         avg_weight = mean(weight_in_kilograms, na.rm = TRUE),
         n = n()) %>%
  ungroup()

# Plot of Average Weight of Players

p1 <- ggplot(data_rosters) +
  with_shadow(geom_point(aes(x = month, y = 1, colour = avg_weight),
                         size = 27)) +
  facet_wrap(vars(country), nrow = 2) +
  scale_color_gradient(low = "#D8CEF5", high = "#1A045D") +
  scale_y_continuous(limits = c(0.975, 1.025)) +
  scale_x_continuous(breaks = seq_along(month.name),
    labels = month.abb) +
  labs(x = "",
       y = "",
       colour = "Average\nWeight (kg)",
       title = "\nAverage Weight\n") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "salsa", size = 14),
        panel.spacing = unit(0,'npc'),
        strip.text = element_text(family = "salsa", size = 18),
        strip.background = element_rect(fill = "ivory"),
        legend.background = element_rect(fill = "ivory"),
        legend.title = element_text(family = "salsa", size = 17),
        legend.text = element_text(family = "salsa", size = 16),
        legend.key.height = unit(1.6, 'cm'),
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(family = "salsa", size = 25,
                                  hjust = 0.5, color = "#1A045D"))

# Plot of Average Height of Players

p2 <- ggplot(data_rosters) +
  with_shadow(geom_point(aes(x = month, y = 1, colour = avg_height),
                         size = 27)) +
  facet_wrap(vars(country), nrow = 2) +
  scale_color_gradient(low = "#B3F3C7", high = "#044418") +
  scale_y_continuous(limits = c(0.975, 1.025)) +
  scale_x_continuous(breaks = seq_along(month.name),
                     labels = month.abb) +
  labs(x = "",
       y = "",
       colour = "Average\nHeight (cm)",
       title = "Average Height\n") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "salsa", size = 14),
        panel.spacing = unit(0,'npc'),
        strip.text = element_text(family = "salsa", size = 18),
        strip.background = element_rect(fill = "ivory"),
        legend.background = element_rect(fill = "ivory"),
        legend.title = element_text(family = "salsa", size = 17),
        legend.text = element_text(family = "salsa", size = 16),
        legend.key.height = unit(1.6, 'cm'),
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(family = "salsa", size = 25,
                                  hjust = 0.5, colour = "#044418"))

# The Final Plot

plots <- align_plots(p1, p2, align = 'v', axis = 'r')

# Title

title1 <- ggdraw() +
  draw_label(
    "PLAYERS IN ROSTERS",
    fontface = 'bold',
    fontfamily = "salsa",
    hjust = 0.5,
    x = 0.5,
    size = 35
  )

# Subtitle

subtitle_text <- str_wrap("The average weight and height of the
players born after 1990 are maximum for the ones born in December or
September in Canada. Even though the minimum values for both
measurements occur in the players born in October for Canadaians and
June for the others. The weight and height of the players are similar
for the others born in other months for both citizen groups.\n", 90)

title2 <- ggdraw() +
  draw_label(
    subtitle_text,
    fontface = 'bold',
    fontfamily = "salsa",
    hjust = 0,
    x = 0,
    size = 20
  )

# Caption

caption <- ggdraw() +
  draw_label(
    "Data Source: Statistics Canada | TidyTuesday 2024 - Week 2 | Prepared by: C. YAZICI",
    fontface = 'bold',
    fontfamily = "salsa",
    hjust = 0.5,
    x = 0.5,
    y = 0.5,
    size = 18
  )

# The Plots

bottom_row <- plot_grid(
  plots[[1]], plots[[2]],
  labels = "",
  rel_heights = c(0.8, 0.8),
  ncol = 1
)

final_plot <- plot_grid(title1, title2, bottom_row, caption,
                        ncol = 1,
                        rel_heights = c(0.1, 0.15, 0.8, 0.2),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "ivory", colour = NA),
        plot.margin = margin(0.3, 0.6, 0.5, 0.5, "cm"),
        aspect.ratio = 8/9)


 # Save the Plot

ggsave("Week2.png", final_plot, width = 19, height = 15, dpi = 72)

