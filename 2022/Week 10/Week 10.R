library(tidyverse)
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(cowplot)
library(showtext)

# Font in the Plot

font_add_google('Labrada', 'labrada')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 10)
erasmus <- tuesdata$erasmus

# Sending Data 

erasmus2 <- erasmus %>%
  select(academic_year, sending_city, participant_age, sending_country_code) %>%
  mutate(sending_city = gsub("[^A-Za-z ]","", sending_city),
         sending_city = ifelse(sending_city == "Stanbul", "Istanbul", 
                               sending_city),
         sending_city = ifelse(sending_city == "Muratpaa", "Antalya", 
                               sending_city),
         sending_city = ifelse(sending_city == "Uak", "Usak", sending_city), 
         sending_city = ifelse(sending_city == "Zmir", "Izmir", sending_city),
         sending_city = str_trim(sending_city)) %>%
  filter(sending_city != "",
         participant_age > 0) %>%
  rename(NUTS_ID = "sending_country_code") %>%
  group_by(NUTS_ID, academic_year) %>%
  summarise(avg_age = mean(participant_age)) %>%
  ungroup() %>%
  left_join(SHP_0, by = "NUTS_ID") %>%
  st_as_sf()
  
# The first plot

p_sending <- ggplot(erasmus2, aes(fill = avg_age)) +
   geom_sf() +
  scale_fill_gradient2(
    low = "#F9E3F6",
    mid = "#F565E5",
    high = "#830B76",
    midpoint = 35)  +
  facet_wrap(~academic_year, ncol = 2) +
   scale_x_continuous(limits = c(-10, 35)) +
   scale_y_continuous(limits = c(35, 65)) +
  labs(title = "Receiving Countries") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        strip.text = element_text(family = "labrada", size = 15, color = "#723608"),
        strip.background = element_rect(fill = "#F7A0C2"),
        plot.title = element_text(family = "labrada", size = 25, hjust = 0.5,
                                  color = "#723608"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

p_sending

# Receiving Data 

erasmus3 <- erasmus %>%
  select(academic_year, receiving_city, participant_age, 
         receiving_country_code) %>%
  mutate(receiving_city = gsub("[^A-Za-z ]","", receiving_city),
         receiving_city = ifelse(receiving_city == "Stanbul", "Istanbul",
                                 receiving_city),
         receiving_city = ifelse(receiving_city == "Muratpaa", "Antalya", 
                                 receiving_city),
         receiving_city = ifelse(receiving_city == "Uak", "Usak", 
                                 receiving_city), 
         receiving_city = ifelse(receiving_city == "Zmir", "Izmir", 
                                 receiving_city),
         receiving_city = str_trim(receiving_city)) %>%
  filter(receiving_city != "",
         participant_age > 0) %>%
  rename(NUTS_ID = "receiving_country_code") %>%
  group_by(NUTS_ID, academic_year) %>%
  summarise(avg_age = mean(participant_age)) %>%
  ungroup() %>%
  left_join(SHP_0, by = "NUTS_ID") %>%
  st_as_sf()

# The second plot

p_receiving <- ggplot(erasmus2, aes(fill = avg_age)) +
  geom_sf() +
  scale_fill_gradient2(
    low = "#F9E3F6",
    mid = "#F565E5",
    high = "#830B76",
    midpoint = 35)  +
  facet_wrap(~academic_year, ncol = 2) +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  labs(title = "Sending Countries",
       fill = "Average Age") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = "labrada", size = 15, 
                                  color = "#723608"),
        strip.background = element_rect(fill = "#F7A0C2"),
        legend.text = element_text(family = "labrada", size = 12),
        legend.title = element_text(family = "labrada", size = 15),
        legend.background = element_rect(fill = "ivory"),
        plot.title = element_text(family = "labrada", size = 25, hjust = 0.5,
                                  color = "#723608"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

p_receiving 

# The Final Plot

plots <- align_plots(p_sending, p_receiving,  align = 'r', axis = 'r')

title1 <- ggdraw() + 
  draw_label(
    "Erasmus Student Mobility",
    fontface = 'bold',
    fontfamily = "labrada",
    hjust = 0.5,
    x = 0.5,
    size = 40,
    color = "#723608"
  )  
title2 <- ggdraw() + 
  draw_label(
    "The participants of Erasmus program become younger as time passes. 
    CRNA GORA had the oldest participants in 2014 - 2015, but the rest 
    are around 25 years old.",
    fontface = 'bold',
    fontfamily = "labrada",
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 18,
    color = "#723608"
  ) 
caption <- ggdraw() + 
  draw_label(
    "Data Source: https://Data.Europa.eu | TidyTuesday 2022 - Week 10 | Prepared by: @Cyd_yzc", 
    fontface = 'bold',
    fontfamily = "labrada",
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 18,
    color = "#723608"
  ) 

bottom_row <- plot_grid(
  plots[[1]], NULL, plots[[2]],
  labels = "",
  rel_widths = c(1.5, -0.8, 1.5), 
  nrow = 1
)

final_plot <- plot_grid(title1, title2, bottom_row, caption, 
                        labels = "", ncol = 1,
                        rel_heights = c(0.1, 0.1, 0.9, 0.1),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "ivory", colour = "ivory"),
        plot.margin = margin(0.5, 2, 0.5, 2, "cm"))

final_plot

# Save the Plot

ggsave("Week10.png", final_plot, width = 25, height = 12, dpi = 72)


 
