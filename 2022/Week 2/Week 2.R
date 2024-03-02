library(showtext)
library(tidyverse)
library(geojsonio)
library(rgdal)
library(broom)
library(rgeos)

# Font in the Plot

font_add_google("Codystar", "cs")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 2)

colony <- tuesdata$colony
stressor <- tuesdata$stressor

# Data Wrangling

colony_data <- colony %>%
  drop_na(months) %>%
  group_by(months, state) %>%
  mutate(lost_avg = mean(colony_lost_pct, na.rm = TRUE),
         reno_avg = mean(colony_reno_pct, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na(months)

# Preperation for the plot

spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
# plot(spdf)

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)",
                                                    "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), 
                                       id=spdf@data$iso3166_2))

spdf_fortified <- spdf_fortified %>%
  left_join(. , colony_data, by = c("id" = "state")) %>%
  drop_na(months)

# The Plot

p <- ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = lost_avg, x = long, 
                                          y = lat, group = group)) +
  geom_text(data = centers, aes(x = x, y = y, label = id), 
            color = "brown", size = 5, alpha = 0.6) +
  scale_fill_gradient2(low = "#EEE5AB",
                       mid = "#f9c901",
                       high = "#985b10",
                       midpoint = 15,
                       na.value = "grey50") +
  theme_void() +
  coord_map() +
  facet_wrap(~factor(months, levels = c("January-March", "April-June",
                                        "July-September", "October-December")),
             ncol = 2) +
  labs(fill = "% of total\ncolonies lost",
    title = "BEE COLONY LOSSES\n",
       caption = "Data Source: USDA | TidyTuesday 2022 - Week 2 | Prepared by: @Cyd_yzc") +
  theme(panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
        strip.text = element_text(color = "ivory", family = "cs", size = 20),
        plot.title = element_text(color = "ivory", family = "cs", size = 40,
                                  hjust = 0.5),
        plot.caption = element_text(color = "ivory", family = "cs", size = 15,
                                  hjust = 1),
        legend.title = element_text(color = "ivory", family = "cs", size = 15),
        legend.text = element_text(color = "ivory", family = "cs", size = 15),
        plot.margin = unit(c(0.5, 0.3, 0.5, 0.3), "cm"))

# Save the Plot

ggsave("Week2_2022.png", p, width = 25, height = 15, dpi = 72)




