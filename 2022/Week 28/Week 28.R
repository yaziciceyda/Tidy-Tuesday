
library(janitor)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggnetwork)
library(ggrepel)
library(showtext)
library(scales)
library(cowplot)
library(patchwork)

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 28)
flights <- tuesdata$flights

# Font in the plot

font_add_google('Oswald', 'oswald')
showtext_auto()

# Data Wrangling

data_flight_annual <- flights %>%
  clean_names() %>%
  filter(state_name == "Türkiye") %>%
  select(year, apt_name, flt_arr_1, flt_dep_1) %>%
  group_by(apt_name, year) %>%
  arrange(year, .by_group = TRUE) %>%
  summarise(annual_arrival = sum(flt_arr_1),
         annual_dept = sum(flt_dep_1)) %>%
  ungroup() %>%
  mutate(longitude = case_when(
    apt_name == 	"Ankara - Esenboða" ~ 32.9916,
    apt_name == 	"Antalya" ~ 30.8018,
    apt_name == 	"Istanbul Atatürk" ~ 28.8208,
    apt_name == 	"Istanbul Sabiha Gökçen" ~ 29.3168,
    apt_name == 	"iGA Istanbul Airport" ~ 28.7300,
    apt_name == 	"Izmir - Adnan Menderes" ~ 27.1520,
    
  ),
  lattitude = case_when(
    apt_name == 	"Ankara - Esenboða" ~ 40.1244,
    apt_name == 	"Antalya" ~ 36.9043,
    apt_name == 	"Istanbul Atatürk" ~ 40.9825,
    apt_name == 	"Istanbul Sabiha Gökçen" ~ 40.9053,
    apt_name == 	"iGA Istanbul Airport" ~ 41.2767,
    apt_name == 	"Izmir - Adnan Menderes" ~ 38.2937,
  ),
  annual_arrival_text = paste0(round(annual_arrival/1000), "K", sep = ""),
  annual_dept_text = paste0(round(annual_dept/1000), "K", sep = ""))

# Turkey Map

world_sf <- ne_countries(returnclass = "sf", scale = "large")
tr_sf <- ne_states(country = "turkey", returnclass = "sf")

tr_sf_data <- st_as_sf(data_flight_annual,
                            crs = 4326,
                            coords = c("longitude", "lattitude"),
                            remove = FALSE)
tr_sf_data2 <- tr_sf_data %>%
  distinct(apt_name, longitude, lattitude)


p2 <- ggplot() +
  geom_sf(data = tr_sf,
          colour = NA,
          fill = "ivory",
          lwd = 0) +
  # Arrivals
  geom_edges(data = data_flight_annual, aes(x = longitude + 2, xend = longitude,
                 y = lattitude + 1, yend = lattitude), color = "red", 
                 curvature = 0.1, alpha = 0.4, size = 1,
             arrow = arrow(length = unit(5, "pt"), type = "closed")) +
  geom_text_repel(data = data_flight_annual, aes(x = longitude + 2, y = lattitude + 1,
                                           label = annual_arrival_text, color = apt_name),
                  fontface = "bold", family = "oswald") +
  # Departures
  geom_edges(data = data_flight_annual, aes(x = longitude, xend = longitude - 2,
                 y = lattitude, yend = lattitude + 1), color = "red", 
             curvature = -0.1, alpha = 0.4, size = 1,
             arrow = arrow(length = unit(5, "pt"), type = "closed")) +
  geom_text_repel(data = data_flight_annual, aes(x = longitude - 2, y = lattitude + 1,
                                           label = annual_dept_text, color = apt_name),
                  fontface = "bold", family = "oswald") +
  geom_nodes(size = 10, color = "#1c192c", alpha = 0.2) +
  facet_wrap(~year, ncol = 3) +
  scale_color_manual("", values = c("#3939F0", "#5DF039", "#F039A2", 
                                    "#CC39F0", "#6D416F", "#0A4A4A")) +
  labs(title = "FLIGHTS IN TURKEY") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey80", color = NA),
        plot.background = element_rect(fill = "grey80", color = NA),
        panel.grid = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "#688D4F"),
        strip.text = element_text(family = "oswald", size = 15),
        plot.title = element_text(family = "oswald", size = 25))


p3 <-  ggplot(data_flight_annual) +
  geom_line(aes(x = year, y = annual_arrival + annual_dept, color = apt_name),
            size = 2) +
  scale_color_manual("", values = c("#3939F0", "#5DF039", "#F039A2", 
                                    "#CC39F0", "#6D416F", "#0A4A4A")) +
  geom_text(aes(x = 2017, y = 9800), label = "The first flight of IGA Istanbul Airport", family = "oswald", size = 4) +
   geom_vline(xintercept = 2020, lty = 2) +
   geom_vline(xintercept = 2018, lty = 2) +
   geom_text(aes(x = 2020, y = 400000), label = "The coronavirus outbreak", family = "oswald", size = 5) +
   scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = "Year",
       y = "Total Flights",
       title = "Total Flights in Turkey between 2016 -2022") +
   theme(axis.ticks = element_blank(),
         axis.text = element_text(family = "oswald", size = 14),
         axis.title = element_text(family = "oswald", size = 15),
         panel.background = element_rect(fill = "grey80", color = NA),
         plot.background = element_rect(fill = "grey80", color = NA),
         panel.grid = element_blank(),
         legend.background = element_rect(fill = "grey80", color = NA),
         legend.text = element_text(family = "oswald", size = 12),
         legend.key = element_rect(colour = "transparent", fill = "grey80"),
         plot.title = element_text(family = "oswald", size = 21))

# Final Plot

final_plot <- p2 +
  inset_element(p3, 0.4, 0.06, 0.98, 0.3) +
   labs(caption = "Data Source: Eurocontrol | TidyTuesday 2022 - Week 28 | Prepared by: @Cyd_yzc") +
  theme(
    plot.caption = element_text(family = "oswald", size = 15, hjust = 1),
    plot.background = element_rect(fill = "grey80", color = NA),
    plot.margin = unit(c(1, 0, 1, 0), "cm")
  )

ggsave("Week28_2022.png", final_plot, width = 23, height = 16, dpi = 72)


    