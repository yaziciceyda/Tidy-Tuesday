tuesdata <- tidytuesdayR::tt_load(2022, week = 45)

font_add_google(name = "Mouse Memoirs",   
                family = "mm")
showtext_auto()

state_stations <- tuesdata$state_stations
station_info <- tuesdata$station_info

oldies_data <- state_stations %>%
  group_by(state) %>%
  mutate(state_n = cur_group_id(),
         n_state = n()) %>%
  select(format, state, state_n, n_state) %>% 
  filter(str_detect(format, pattern = "Oldies|Classics")) %>%
  mutate(oldies_n = cur_group_id(),
         n_oldies = n()) %>%
  mutate(ratio = n_oldies / n_state * 100) %>%
  ungroup() %>% 
  rowwise() %>%
mutate(State = str_replace_all(state, "_", " ")) %>%
  select(State, ratio) %>%
  distinct() %>%
full_join(geofacet::us_state_grid2, by = c("State" = "name")) %>%
  arrange(desc(row)) %>%
  mutate(row2 = case_when(
    row == 8 ~ 1,
    row == 7 ~ 2,
    row == 6 ~ 3,
    row == 5 ~ 4,
    row == 4 ~ 5,
    row == 3 ~ 6,
    row == 2 ~ 7,
    row == 1 ~ 8))

subtitle_text <- "New Hampshire and North Carolina are the states with highest percentage of Oldies or Classics radio stations. However, 
Alaska, Delaware, North Dakota and District of Columbia have neither of them."

p <- ggplot(data = oldies_data) +
  ggchicklet:::geom_rrect(aes(xmin=-5, xmax=18, ymin=-8, ymax=9.5), 
                          fill="black",
                          radius = unit(0.5, units = "cm")) +
  ggchicklet:::geom_rrect(aes(xmin=-2.2, xmax=16, ymin=-4, ymax=9), 
                          fill="#FFFFF0",
                          radius = unit(0.5, units = "cm")) +
  ggchicklet:::geom_rrect(aes(xmin=2, xmax=10.7, ymin=-3.1, ymax=-0.5), 
                          color ="black",
                          radius = unit(0.5, units = "cm")) +
  ggchicklet:::geom_rrect(aes(xmin=2.2, xmax=10.5, ymin=-2.9, ymax=-0.7), 
                          fill ="white",
                          radius = unit(0.5, units = "cm")) +
  geom_point(aes(x = 3.3, y = -1.8), color = "black", 
             shape = 21, size = 19,  stroke = 2, fill = "black") +
  geom_point(aes(x = 3.3, y = -1.8), color = "black", 
             shape = 8, size = 17,  stroke = 2) +
  geom_point(aes(x = 3.3, y = -1.8), color = "black", 
             shape = 21, size = 15, fill = "white", stroke = 2) +

  geom_point(aes(x = 9.5, y = -1.8), color = "black", 
             shape = 21, size = 19,  stroke = 2, fill = "black") +
  geom_point(aes(x = 9.5, y = -1.8), color = "black", 
             shape = 8, size = 17,  stroke = 2) +
  geom_point(aes(x = 9.5, y = -1.8), color = "black", 
             shape = 21, size = 15, fill = "white", stroke = 2) +
  

  geom_point(aes(x = -4, y = -7),
              shape = 21, size = 8, fill = "gray50") +
  geom_point(aes(x = 17, y = -7),
             shape = 21, size = 8, fill = "gray50") +
  geom_point(aes(x = -4, y = 8.5),
             shape = 21, size = 8, fill = "gray50") +
  geom_point(aes(x = 17, y = 8.5),
             shape = 21, size = 8, fill = "gray50") +
  
  geom_point(aes(x = 6.5, y = -5),
             shape = 21, size = 8, fill = "gray50") +
  geom_point(aes(x = 1, y = -7),
             shape = 21, size = 11, fill = "gray80") +
  geom_point(aes(x = 12, y = -7),
             shape = 21, size = 11, fill = "gray80") +
  
  ggchicklet:::geom_rrect(aes(xmin=2.5, xmax=3.8, ymin=-7, ymax=-5.5), 
                          fill="gray80",
                          radius = unit(0.2, units = "cm")) +
  ggchicklet:::geom_rrect(aes(xmin=8.8, xmax=10, ymin=-7, ymax=-5.5), 
                          fill="gray80",
                          radius = unit(0.2, units = "cm")) +
  geom_segment(aes(x=-3, xend=-0.5, y=-8, yend=-4.3), color="grey20", size=2) +
  geom_segment(aes(x=15, xend=12.5, y=-8, yend=-4.3), color="grey20", size=2) +
  geom_segment(aes(x=-0.5, xend=12.5, y=-4.3, yend=-4.3), color="grey20", size=2) +

  geom_circle(mapping = aes(x0 = 1.2*col,
                            y0 = 1.06*row2,
                            r = 0.45,
                            fill = ratio),
              size = 0.6) +
  geom_arc_bar(mapping = aes(x0 = 1.2*col,
                             y0 = 1.06*row2,
                             r0 = 0,
                             r = 0.45,
                             start = 0,
                             end = (2*pi*0.01)*ratio),
               fill = "black",
               size = 0.4) +
  geom_spoke(mapping = aes(x = 1.2*col, 
                           y = 1.06*row2,
                           angle = (pi/2) - (2*pi*0.01)*ratio),
             radius = 0.45,
             colour = "white",
             size = 0.4) +
  geom_text(mapping = aes(x = 1.2*col,
                          y = 1.05*row2 - 0.2,
                          label = code,
                          colour = (code %in% c("NJ", "MT"))), 
            fontface = "bold",
            family = "kelly",
            size = 3) +
  scale_fill_carto_c(palette = "Burg") +
  scale_colour_manual(values = c("black", "#FFFFF0")) +
#  scale_y_reverse() +
  coord_fixed() +
  theme(legend.position = "none",
        plot.title = element_text(family = "mm", color = "#C13C21",
                                  size = 30, hjust = 0.5),
        plot.subtitle = element_text(family = "mm", color = "#C13C21",
                                  size = 20, hjust = 0.5),
        axis.ticks = element_blank(),
      axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#FFFFF0"),
      plot.background = element_rect(fill = "#FFFFF0"),
      plot.caption = element_text(size = 12, hjust = 1, vjust = 0,
                                  family = "mm"),
    plot.margin = margin(2,8,2,8, unit="cm")) +
  labs(title = "Oldies or Classics",
       subtitle = subtitle_text,
       caption = "Data Source: Wikipedia | TidyTuesday 2022 - Week 45 | Prepared by: @Cyd_yzc") 
p
ggsave("Week45_2022.png", p, width = 20, height = 10, dpi = 72)


