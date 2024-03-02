tuesdata <- tidytuesdayR::tt_load(2022, week = 49)
elevators <- tuesdata$elevators

library(lubridate)
library(janitor)
library(PrettyCols)
library(ggrepel)
library(showtext)

font_add_google("Poppins", "pp")
showtext_auto()

# Data Wrangling 

dt <- elevators %>%
  clean_names() %>%
  select(dv_device_number, borough, device_type, dv_speed_fpm,
         dv_travel_distance, dv_approval_date, dv_floor_to, dv_capacity_lbs,
         longitude, latitude) %>%
  mutate(year = year(ymd(dv_approval_date)),
         floors = str_extract(dv_floor_to, "\\d+"),
         floors = as.numeric(floors),
         latitude2 = latitude, 
         longitude2 = longitude) %>%
  group_by(borough, device_type) %>%
  slice(which.min(year)) %>%
  drop_na(latitude, longitude) %>%
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%
  sf::st_set_crs(4326) %>%
  sf::st_transform(crs = 4326)


getbb("New York City")

bb <- matrix(c(-74.252, -73.707, 40.502, 40.914),
             ncol = 2,
             nrow = 2,
             byrow = TRUE,
             dimnames = list(c("x", "y"), c("min", "max")))

bg_map <- get_map(bb,
                  source = "stamen",
                  maptype = "toner-hybrid",
                  color = "bw")

# The Plot

final_plot <- ggmap(bg_map) +
  geom_sf(data = dt,
          size = 5,
          inherit.aes = FALSE,
          mapping = aes(colour = device_type)) +
  geom_text_repel(dt, mapping = aes(x = longitude2, 
                                    y = latitude2, 
  label = 2015 - year), 
                  color = "brown", size = 5, fontface = "bold") +
    scale_colour_pretty_d("Rainbow") +
  labs(title = "Elevators in NYC",
  subtitle = "The location of the oldest elevator of each type in every borough of NYC with its age.",
  colour = "Type of the elevator",
  caption = "Data Source:  {elevators} | TidyTuesday 2022 - Week 49 | Prepared by: @Cyd_yzc") +
  theme(plot.background = element_rect("ivory"),
        panel.background = element_rect("ivory"),
        plot.title = element_text(family = "pp", size = 30),
        plot.subtitle = element_text(family = "pp", size = 20),
        plot.caption = element_text(family = "pp", size = 15, hjust = 1),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.background = element_rect(fill = "ivory"),
        legend.text = element_text(family = "pp", size = 13),
        legend.title = element_text(family = "pp", size = 15))

# Save the Plot

ggsave("Week49_2022.png", final_plot, width = 25, height = 12, dpi = 72)


