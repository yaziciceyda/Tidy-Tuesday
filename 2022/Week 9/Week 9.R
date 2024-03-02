library(janitor)
library(statebins)
library(lubridate)
library(scales)
library(showtext)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)

# Font in the Plot

font_add_google('Electrolize', 'electrolize')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 9)
stations <- tuesdata$stations

# Data Wrangling for the First Plot

stations_data <- stations %>%
  clean_names() %>%
  select(x, y, fuel_type_code, country, status_code, state,
         date_last_confirmed) %>%
  mutate(year = year(date_last_confirmed)) %>%
  filter(country == "US",
         status_code == "E",
         year == 2021,
         y > 20,
         x > -130) %>%
  mutate(new_energy = ifelse(fuel_type_code %in% c("ELEC", "BD", 
                                                   "CNG", "RD", "LNG"),
                1, 0)) %>%
  group_by(state, new_energy) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n/sum(n) * 100)) %>%
  ungroup() %>%
  filter(new_energy == 1)

# The First Plot


first_plot <- ggplot(stations_data, aes(state = state, fill = freq)) +
  geom_statebins(border_col = "black", lbl_size = 4, family = "electrolize") +
  scale_fill_gradient2(
    low = "#CBD0F4",
    mid = "#7885EE",
    high = "#142BF4",
    midpoint = 50) +
  labs(fill = "Frequency") +
  theme(panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.background = element_rect(fill = "grey60"),
        legend.title = element_text(family = "electrolize"),
        legend.text = element_text(family = "electrolize"))

first_plot

# Data Wrangling for the Second Plot

stations2_data <- stations %>%
  clean_names() %>%
  select(x, y, fuel_type_code, country, status_code, state,
         date_last_confirmed) %>%
  mutate(year = year(date_last_confirmed)) %>%
  filter(country == "US",
         status_code == "E",
         year == 2021,
         fuel_type_code %in% c("ELEC", "BD", 
                              "CNG", "RD", "LNG"),
         y > 20,
         x > -130)

us <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(geounit == "US")

# The Second Plot

second_plot <- ggplot(data = us) +
  geom_sf() +
  geom_point(data = stations2_data, aes(x = x, y = y),  
             size = 1, shape = 23, fill = "#142BF4") +
  labs(title = "Locations of the Stations") +
  theme(panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "electrolize", color = "grey60",
                                  hjus = 0.5, size = 20))

# Data Wrangling for the Third Plot

stations_data3 <- stations %>%
  clean_names() %>%
  select(x, y, fuel_type_code, country, status_code, state,
         open_date, date_last_confirmed) %>%
  mutate(year = year(date_last_confirmed),
         opened = as_date(open_date),
         opened_year = year(open_date)) %>%
  filter(country == "US",
         status_code == "E",
         year == 2021,
         y > 20,
         x > -130) %>%
  group_by(opened_year) %>%
  summarise(n = n()) %>%
  ungroup()

# The Third Plot

third_plot <- ggplot(stations_data3) +
  geom_line(aes(x = opened_year, y = n), color = "#142BF4", linewidth = 2) +
  labs(x = "Year",
       y = "Number of Stations") +
  theme(panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "electrolize", size = 13),
        axis.title = element_text(family = "electrolize", 
                                  color = "grey60", size = 15))


# The Final Plot

plots <- align_plots(second_plot, third_plot, first_plot,
                     align = 'r', axis = 'r')

title1 <- ggdraw() + 
  draw_label(
    "Alternative Fuel Stations",
    fontface = 'bold',
    fontfamily = "electrolize",
    hjust = 0.5,
    x = 0.5,
    size = 40,
    color = "#142BF4"
  )  
title2 <- ggdraw() + 
  draw_label(
    "The alternative fuel stations in the United States started to increase after
    2000s. The total number of Biodiesel, Electric, Compressed Natural Gas, 
    Renewable Diesel and Liquefied Natural Gas stations in the west and east states
    are higher than the ones in the middle. The locations of them is presented
    in the plot in the bottom. ",
    fontface = 'bold',
    fontfamily = "electrolize",
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 18,
    color = "grey60"
  ) 
caption <- ggdraw() + 
  draw_label(
    "Data Source: US DOT | TidyTuesday 2022 - Week 9 | Prepared by: @Cyd_yzc", 
    fontface = 'bold',
    fontfamily = "electrolize",
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 18,
    color = "grey60"
  ) 
top_row <-  plot_grid(
  title2, plots[[3]], 
  labels = "",
  rel_widths = c(1.0, 1.0), 
  nrow = 1
)
  
bottom_row <- plot_grid(
  plots[[1]], plots[[2]],
  labels = "",
  rel_widths = c(1.0, 1.0), 
  nrow = 1
)

final_plot <- plot_grid(title1, top_row, bottom_row, caption, 
                        labels = "", ncol = 1,
                        rel_heights = c(0.1, 0.9, 0.9, 0.1),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "black", colour = NA),
        plot.margin = margin(0.5, 2, 0.5, 2, "cm"))

final_plot

# Save the Plot

ggsave("Week9.png", final_plot, width = 25, height = 12, dpi = 72)






