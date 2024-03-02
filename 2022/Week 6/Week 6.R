library(tidyverse)
library(usmap)
library(lubridate)
library(showtext)
library(ggtext)

# library to use fontawesome icons

font_add('fa-solid', 'Font Awesome 6 Free-Solid-900.otf')

font_add_google('Bellefair', 'bellefair')
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2022, week = 6)

airmen <- tuesdata$airmen

states_list <- usmap::us_map(regions = "states") %>%
  distinct(abbr) %>%
  rename(state = abbr)

airmen_freq_data <- airmen %>%
  filter(rank_at_graduation %in% c("1st Lt", "2nd Lt", "Flight Officer"),
         pilot_type == "Single engine") %>%
  count(state) %>%
  right_join(states_list, by = c("state")) %>%
  mutate(n = ifelse(is.na(n), 0, n),
    rank = factor(case_when(
   n > 0 & n <= 10 ~ "1 - 10",
   n > 10 & n <= 20 ~ "11 - 20",
   n > 20 & n <= 30 ~ "21 - 30",
   n > 30 & n <= 40 ~ "31 - 40",
   n > 40 & n <= 50 ~ "41 - 50",
   n > 50 & n <= 60 ~ "51 - 60",
   n > 60 & n <= 70 ~ "60+",
   TRUE ~ "NA",
  ),
  levels = c("1 - 10", "11 - 20", "21 - 30", "31 - 40",
             "41 - 50", "51 - 60", "60+", "NA")),
  new_color = factor(case_when(
    n > 0 & n <= 10 ~ "#2459a6",
    n > 10 & n <= 20 ~ "#bd0f2e",
    n > 20 & n <= 30 ~ "#428e6b",
    n > 30 & n <= 40 ~ "#494030",
    n > 40 & n <= 50 ~ "#EAE1D6",
    n > 50 & n <= 60 ~ "#eeaf01",
    n > 60 & n <= 70 ~ "#131211",
     TRUE ~ "#a9a9b5",
  ),
levels = c("#2459a6", "#bd0f2e", "#428e6b", 
               "#494030", "#EAE1D6", "#eeaf01",
               "#131211", "#a9a9b5")))
 

us_map <- usmap::us_map()
us_coordinates <- us_map %>%
  mutate(abbr = state.abb[match(full, state.name)])

us_coordinates_avg <- us_map %>%
  mutate(abbr = state.abb[match(full, state.name)]) %>%
  group_by(abbr) %>%
  mutate(avg_x = mean(x),
         avg_y = mean(y)) %>%
  ungroup() %>%
  select(abbr, full, avg_x, avg_y) %>%
  distinct()


airmen_data <- airmen_freq_data %>%
  inner_join(us_coordinates, by = c("state" = "abbr"))




airmen_grad_data <- airmen %>%
  filter(rank_at_graduation %in% c("1st Lt", "2nd Lt", "Flight Officer"),
         pilot_type == "Single engine") %>%
  group_by(state) %>%
  count(rank_at_graduation) %>%
  ungroup() %>%
  mutate(new_color2 = factor(case_when(
    rank_at_graduation == "1st Lt" ~ "#CBD0F4",
    rank_at_graduation == "2nd Lt" ~ "#61CA8C",
    rank_at_graduation == "Flight Officer" ~ "#F15B40"
  ),
  levels = c("#CBD0F4", "#61CA8C", "#F15B40")), 
  label = "<span style='font-family:fa-solid'>&#xf072;</span>",
  rank_at_graduation = factor(rank_at_graduation, 
                    levels = c("1st Lt", "2nd Lt", "Flight Officer"))) %>%
  inner_join(us_coordinates_avg, by = c("state" = "abbr")) %>%
  mutate(new_size_cat = factor(case_when(
    n > 0 & n <= 10 ~ 3,
    n > 10 & n <= 20 ~ 6,
    n > 20 & n <= 30 ~ 9,
    n > 30 & n <= 40 ~ 12,
    n > 40 & n <= 50 ~ 15,
    n > 50 ~ 18),
    levels = c("1 - 10", "11 - 20", "21 - 30",
               "31 - 40", "41 - 50", " 50+")),
    new_size = case_when(
      n > 0 & n <= 10 ~ 3,
      n > 10 & n <= 20 ~ 6,
      n > 20 & n <= 30 ~ 9,
      n > 30 & n <= 40 ~ 12,
      n > 40 & n <= 50 ~ 15,
      n > 50 ~ 18),
    avg_y = ifelse(rank_at_graduation == "Flight Officer", avg_y - 90000, avg_y),
    avg_y = ifelse(rank_at_graduation == "2nd Lt", avg_y + 90000, avg_y),
    avg_y = ifelse(state == "MI", avg_y - 90000, avg_y),
    avg_x = ifelse(state == "MI", avg_x + 90000, avg_x),
    avg_y = ifelse(state == "KY", avg_y - 10000, avg_y),
    avg_x = ifelse(state == "KY", avg_x + 70000, avg_x),
    avg_y = ifelse(state == "TN" & rank_at_graduation == "2nd Lt",
                   avg_y - 50000, avg_y),
    avg_x = ifelse(state == "LA", avg_x - 50000, avg_x),
    avg_y = ifelse(state == "SC", avg_y - 10000, avg_y),
    avg_x = ifelse(state == "FL", avg_x + 50000, avg_x),
    avg_x = ifelse(state == "NC", avg_x + 190000, avg_x),
    avg_x = ifelse(state == "MD", avg_x + 30000, avg_x),
    avg_y = ifelse(state == "MD" & rank_at_graduation == "1st Lt", 
                   avg_y + 50000, avg_y),
    avg_y = ifelse(state == "MD" & rank_at_graduation == "Flight Officer", 
                   avg_y + 70000, avg_y),
    avg_x = ifelse(state == "MA", avg_x - 50000, avg_x),
    avg_y = ifelse(state == "MA", avg_y - 30000, avg_y))

p <- ggplot() +
  geom_polygon(data = airmen_data, aes(x = x, y = y, fill = new_color, group = group)) +
  geom_path(data = airmen_data, aes(x = x, y = y, fill = new_color, group = group)) +
  scale_fill_identity(guide = 'legend', 
                      labels = levels(airmen_data$rank),
                      breaks = levels(airmen_data$new_color)) +
#  geom_richtext(data = airmen_grad_data, 
#                mapping = aes(x = avg_x, y = avg_y, size = new_size, 
#                              col = new_color2,
#                              label.colour = NA, fill = NA,
#                              label =  label, 
#                              group = full),
#                family = 'fontawesome-webfont') +
  geom_text(data = airmen_grad_data, 
            mapping = aes(x = avg_x, y = avg_y, size = new_size,
                          group = state,
                          colour = new_color2),
            label = fontawesome('fa-fighter-jet'),
            family = 'fontawesome-webfont') +
  scale_colour_identity(guide = 'legend',
                        labels = levels(airmen_grad_data$rank_at_graduation),
                        breaks = levels(airmen_grad_data$new_color2)) +
  scale_size_identity(guide = 'legend',
                      labels = levels(airmen_grad_data$new_size_cat),
                      breaks = sort(unique(airmen_grad_data$new_size))) +
  coord_cartesian() +
  labs(title = "TUSKEGEE AIRMEN",
       fill = "Single Engine Airmen\nper State",
       color = "Military Rank Type",
       size = "Military Rank\nper State",
       caption = "Data Source: Commemorative Airforce (CAF) by way of the VA-TUG | TidyTuesday 2022 - Week 6 | Prepared by: @Cyd_yzc") +
  theme(panel.background = element_rect(fill = "#a27c65", color = NA),
        plot.background = element_rect(fill = "#a27c65", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.background = element_rect(fill = "#a27c65"),
        legend.text = element_text(family = "bellefair", size = 15),
        legend.title = element_text(family = "bellefair", size = 20),
        plot.title = element_text(family = "bellefair", size = 30, hjust = 0.5),
        plot.caption = element_text(family = "bellefair", size = 20, hjust = 1),
        plot.margin = margin(0.5, 2, 0.5, 2, "cm"))

# Save the Plot

ggsave("Week6.png", p, width = 25, height = 12, dpi = 72)




        