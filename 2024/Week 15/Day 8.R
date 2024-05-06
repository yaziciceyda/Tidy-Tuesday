library(tidyverse)
library(rnaturalearth)
library(sf)
library(lubridate)
library(ggblur)
library(showtext)
library(cowplot)

# Font in the Plot

font_add_google('Rubik Doodle Shadow', 'rds', db_cache = FALSE)
showtext_auto()

font_add_google('Vast Shadow', 'vs')
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2024, week = 15)

eclipse_annular_2023 <- tuesdata$eclipse_annular_2023
eclipse_total_2024 <- tuesdata$eclipse_total_2024
eclipse_partial_2023 <- tuesdata$eclipse_partial_2023
eclipse_partial_2024 <- tuesdata$eclipse_partial_2024

# Total - 2024

total_eclipse_2024 <- eclipse_total_2024 %>%
  mutate(annularity_duration = eclipse_4 - eclipse_3,
         annularity_duration = as.numeric(annularity_duration)) %>%
  group_by(state) %>%
  summarize(avg_lon = mean(lon),
            avg_lat = mean(lat),
            avg_annularity = mean(annularity_duration)) %>%
  ungroup() %>%
  rename("annularity_24" = avg_annularity,
         "avg_lon_24" = avg_lon,
         "avg_lat_24" = avg_lat)

# Annular - 2023

annular_eclipse_2023 <- eclipse_annular_2023 %>%
  mutate(annularity_duration = eclipse_4 - eclipse_3,
         annularity_duration = as.numeric(annularity_duration)) %>%
  group_by(state) %>%
  summarize(avg_lon = mean(lon),
            avg_lat = mean(lat),
            avg_annularity = mean(annularity_duration)) %>%
  ungroup() %>%
  rename("annularity_23" = avg_annularity,
         "avg_lon_23" = avg_lon,
         "avg_lat_23" = avg_lat)

# The merged data set

total_eclipse_all <- total_eclipse_2024 %>%
  full_join(annular_eclipse_2023, by = "state") 

# The USA Map

usa_sf <- ne_states(iso_a2 = 'us', returnclass = "sf")

# The Map of Annual Eclipses

p1 <- ggplot() +
  geom_sf(data = usa_sf,
          colour = "grey60",
          fill = "grey60",
          size = 0.2) +
  # 2024
  geom_point(data = total_eclipse_all %>%
               filter(!is.na(annularity_24)),
             mapping = aes(x = avg_lon_24, y = avg_lat_24), 
             size = 13, color = "white") +
  geom_point_blur(data = total_eclipse_all %>%
                    filter(!is.na(annularity_24)),
                  mapping = aes(x = avg_lon_24, y = avg_lat_24, 
                                size = annularity_24), 
                  blur_size = 10) +
  # 2023
  geom_point(data = total_eclipse_all %>%
               filter(!is.na(annularity_23)),
             mapping = aes(x = avg_lon_23 + 1, y = avg_lat_23), 
             size = 13, color = "yellow") +
  geom_point_blur(data = total_eclipse_all %>%
                    filter(!is.na(annularity_23)),
                  mapping = aes(x = avg_lon_23 + 1, y = avg_lat_23, 
                                size = annularity_23), 
                  blur_size = 10) +
  coord_sf(xlim = c(-125, -67),
           ylim = c(25, 50)) +
  labs(title = "Annular and\nTotal Eclipses",
       size = "\nDuration (in secs)") +
  theme(panel.background = element_rect(fill = "#CEE7FA", color = NA),
        plot.background = element_rect(fill = "#CEE7FA", color = NA),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.background = element_rect(fill = "#CEE7FA"),
        legend.title = element_text(hjust = 0, 
                                    size = 18, family = "vs"),
        legend.text = element_text(hjust = 0.5, 
                                   size = 15, family = "vs"),
        # legend.key.height = unit(1.5, 'cm'),
        legend.key.size = unit(1.8, 'cm'),
        legend.key = element_rect(fill = "#CEE7FA"),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, 
                                  size = 26, family = "vs"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  guides(size = guide_legend(override.aes = list(size = 6),
                             nrow = 1,
                             title.position = "top",
                             title.hjust = 0.5))



##### Partial Eclipses

# Partial - 2024

partial_eclipse_2024 <- eclipse_partial_2024 %>%
  mutate(annularity_duration = eclipse_4 - eclipse_2,
         annularity_duration = as.numeric(annularity_duration)) %>%
  group_by(state) %>%
  summarize(avg_lon = mean(lon),
            avg_lat = mean(lat),
            avg_annularity = mean(annularity_duration)) %>%
  ungroup() %>%
  rename("annularity_24" = avg_annularity,
         "avg_lon_24" = avg_lon,
         "avg_lat_24" = avg_lat)

  
# Partial - 2023

partial_eclipse_2023 <- eclipse_partial_2023 %>%
  mutate(annularity_duration = eclipse_4 - eclipse_2,
         annularity_duration = as.numeric(annularity_duration)) %>%
  group_by(state) %>%
  summarize(avg_lon = mean(lon),
            avg_lat = mean(lat),
            avg_annularity = mean(annularity_duration)) %>%
  ungroup() %>%
  rename("annularity_23" = avg_annularity,
         "avg_lon_23" = avg_lon,
         "avg_lat_23" = avg_lat)

# The merged data set

partial_eclipse_all <- partial_eclipse_2024 %>%
  full_join(partial_eclipse_2023, by = "state") 


p2 <- ggplot() +
  geom_sf(data = usa_sf,
          colour = "grey60",
          fill = "grey60",
          size = 0.2) +
  # 2024
  geom_point(data = partial_eclipse_all %>%
               filter(!is.na(annularity_24)),
             mapping = aes(x = avg_lon_24 + 1, y = avg_lat_24), 
             size = 13, color = "white") +
  geom_point_blur(data = partial_eclipse_all %>%
                    filter(!is.na(annularity_24)),
                  mapping = aes(x = avg_lon_24 + 1, y = avg_lat_24, 
                                size = annularity_24), 
                  blur_size = 10) +
  # 2023
  geom_point(data = partial_eclipse_all %>%
               filter(!is.na(annularity_23)),
             mapping = aes(x = avg_lon_23, y = avg_lat_23), 
             size = 13, color = "yellow") +
  geom_point_blur(data = partial_eclipse_all %>%
                    filter(!is.na(annularity_23)),
                  mapping = aes(x = avg_lon_23, y = avg_lat_23, 
                                size = annularity_23), 
                  blur_size = 10) +
  coord_sf(xlim = c(-125, -67),
           ylim = c(25, 50)) +
  labs(title = "Partial Eclipses",
       size = "\nDuration (in secs)") +
  theme(panel.background = element_rect(fill = "#CEE7FA", color = NA),
        plot.background = element_rect(fill = "#CEE7FA", color = NA),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.background = element_rect(fill = "#CEE7FA"),
        legend.title = element_text(hjust = 0, 
                                    size = 18, family = "vs"),
        legend.text = element_text(hjust = 0.5, 
                                   size = 15, family = "vs"),
       # legend.key.height = unit(1.5, 'cm'),
        legend.key.size = unit(1.8, 'cm'),
        legend.key = element_rect(fill = "#CEE7FA"),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, 
                                  size = 26, family = "vs"),
       plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  guides(size = guide_legend(nrow = 3,
         title.position = "top",
         title.hjust = 0.5))


# The Combination of Plots

plots <- align_plots(p1, p2, align = 'h', axis = 'r')

title1 <- ggdraw() + 
  draw_label(
    "2023 & 2024 US Solar Eclipses",
    fontface = 'bold',
    fontfamily = "rds",
    hjust = 0.5,
    x = 0.5,
    size = 60,
    color = "black"
  ) 

subtitle_text1 <- str_wrap("On October 14, 2023, an annular solar 
eclipse created a path of 
annularity in which Moon passes between the Sun and Earth while at 
its farthest point from Earth. Then, On April 8, 2024, a total solar 
eclipse created a path of totality in which the Moon completely blocks 
the Sun. The average duration of annularity, totality and partial 
eclipses in ", 75)

  
  
title2 <- ggdraw() + 
  draw_label(
    subtitle_text1,
    fontface = 'bold',
    fontfamily = "vs",
    hjust = 0,
    x = 0.05,
    y = -0.15, 
    size = 23,
    color = "black"
  ) 

title3 <- ggdraw() + 
  draw_label(
    "2023",
    fontface = 'bold',
    fontfamily = "vs",
    hjust = 0,
    x = 0.6,
    y = 0.22, 
    size = 23,
    color = "#EED509"
  ) 

title4 <- ggdraw() + 
  draw_label(
    " and ",
    fontface = 'bold',
    fontfamily = "vs",
    hjust = 0,
    x = 0.66,
    y = 1.24, 
    size = 23,
    color = "black"
  ) 

title5 <- ggdraw() + 
  draw_label(
    "2024",
    fontface = 'bold',
    fontfamily = "vs",
    hjust = 0,
    x = 0.72,
    y = 2.2, 
    size = 23,
    color = "white"
  ) 

title6 <- ggdraw() + 
  draw_label(
    "in each state are ",
    fontface = 'bold',
    fontfamily = "vs",
    hjust = 0,
    x = 0.78,
    y = 3.2, 
    size = 23,
    color = "black"
  ) 

title7 <- ggdraw() + 
  draw_label(
    "shown.",
    fontface = 'bold',
    fontfamily = "vs",
    hjust = 0,
    x = 0.05,
    y = 3.85, 
    size = 23,
    color = "black"
  ) 
caption <- ggdraw() + 
  draw_label(
    "Data Source: NASA | #30DayChartChallenge - Day #8\nTidyTuesday 2024 - Week 15 | Prepared by: C. YAZICI", 
    fontface = 'bold',
    fontfamily = "vs",
    hjust = 1,
    x = 0.9,
    y = 0.5, 
    size = 20,
    color = "black"
  ) 

bottom_row <- plot_grid(
  plots[[1]], plots[[2]],
  labels = "",
  rel_widths = c(0.5, 0.5), 
  nrow = 1
)

final_plot <- plot_grid(title1, title2, title3, title4, title5, title6,
                        title7, bottom_row, caption, 
                        labels = "", ncol = 1,
                        rel_heights = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 
                                        0.1, 0.9, 0.1)) +
  theme(plot.background = element_rect(fill = "#CEE7FA",
                                       colour = NA),
        panel.background = element_rect(fill = "#CEE7FA",
                                       colour = NA),
        plot.margin = unit(c(0.5, 0.9, 0.5, 0.5), "cm"))



# Save the Plot

ggsave("Day8.png", final_plot, width = 25, height = 19, dpi = 72)



