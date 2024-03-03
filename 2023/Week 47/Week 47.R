library(tidyverse)
library(ggforce)
library(showtext)
library(scales)
library(usefunc)
library(cowplot)

# Font in the Plot

font_add_google("Space Grotesk", "sg")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 47)
rladies_chapters <- tuesdata$rladies_chapters

# Data Wrangling

rladies_data <- rladies_chapters %>%
  select(year, location) %>%
  group_by(year, location) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(percent = round(n / sum(n) * 100)) %>%
  group_by(year) %>%
  mutate(total_year = sum(n),
         x = case_when(
           year == 2012 ~ 1,
           year == 2013 ~ 2,
           year == 2014 ~ 3,
           year == 2015 ~ 4,
           year == 2016 ~ 5,
           year == 2017 ~ 6,
           year == 2018 ~ 7,
           year == 2019 ~ 8,
           year == 2020 ~ 9,
           year == 2021 ~ 10,
           year == 2022 ~ 11,
           year == 2023 ~ 12
         ))

# SUbtitle of the Next Plot

subtitle_text <- str_wrap("The number of events in R-Ladies increased significantly
       after 2016 and reached its peak at 2020. It has a decreasing pattern
       for the last three years.\n", 80)

# The First Plot

plot_time <- ggplot(rladies_data) +
  geom_line(aes(x = year, y = total_year)) +
  geom_point(aes(x = year, y = total_year, size = total_year),
             color = "#160BC4") +
  geom_text(aes(x = year, y = total_year + 80, label = total_year),
            family = "sg", size = 7) +
  scale_x_continuous(breaks = c(2012:2023)) +
  coord_cartesian() +
  labs(title = "Total Number of Events\n",
       subtitle = subtitle_text,
       y = "Number of Events") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "sg", size = 17),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "sg", vjust = 1.5, size = 15),
        legend.position = "none",
        plot.title = element_text(family = "sg", size = 28, hjust = 0.5),
        plot.subtitle = element_text(family = "sg", size = 20, hjust = 0,
                                     vjust = -1))

# Subtitle of the Next Plot

subtitle_text2 <- str_wrap("\nThere is only inperson events until 2020, while
the online ones occured in the last four years with the highest percentage of 90
in 2021.", 90)

# The Second Plot

plot_online <- ggplot(rladies_data) +
  geom_circle(aes(x0 = x, y0 = 1, r = 0.45), fill = "#160BC4",
              color = "#160BC4") +
  geom_circle(rladies_data %>% filter(location == "online"),
             mapping = aes(x0 = x, y0 = 1, r = percent/200),
             fill = "#E865E6") +
  # Percent
  geom_text(rladies_data %>% filter(location == "online"),
            mapping = aes(x = x, y = 1, label = paste0(percent, "%")),
            family = "sg", size = 7) +
  # Arrow
  geom_curve(aes(x = 9, xend = 9.1,
                 y = 1.5, yend = 1.8), curvature = -0.3, size = 1.5,
             arrow = arrow(length = unit(0.3, "cm"))) +
  # Coronavirus Outbreak
  annotate("text", x = 9.1, y = 2, label = "Coronavirus Outbreak",
           family = "sg", size = 7) +
  # Year
  geom_text(aes(x = x, y = 0.4, label = year), family = "sg", size = 8) +
  coord_fixed(xlim = c(0.5, 12), ylim = c(0, 2.2)) +
  labs(title = "Percentage of Online Events\n",
       subtitle = subtitle_text2) +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(family = "sg", hjust = 0.5, size = 28,
                                  vjust = 1),
        plot.subtitle = element_text(family = "sg", size = 20, hjust = 0,
                                     vjust = -1))


# The Final Plot

plots <- align_plots(plot_online, plot_time, align = 'v', axis = 'r')

# Title

title1 <- ggdraw() +
  draw_label(
    "R-Ladies Chapter Events",
    fontface = 'bold',
    fontfamily = "sg",
    hjust = 0.5,
    x = 0.5,
    size = 35
  )

# Caption

caption <- ggdraw() +
  draw_label(
    "Data Source: R-Ladies Chapter Events data | TidyTuesday 2023 - Week 46 | Prepared by: C. YAZICI",
    fontface = 'bold',
    fontfamily = "sg",
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

final_plot <- plot_grid(title1, bottom_row, caption,
                        ncol = 1,
                        rel_heights = c(0.1, 0.8, 0.2),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "ivory", colour = NA),
        plot.margin = margin(0.3, 0.5, 0.5, 0.5, "cm"),
        aspect.ratio = 8/9)

final_plot

# Save the Plot

ggsave("Week46.png", final_plot, width = 17, height = 15, dpi = 72)

