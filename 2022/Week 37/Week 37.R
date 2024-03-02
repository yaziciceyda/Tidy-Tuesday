
library(lubridate)
library(showtext)
library(PrettyCols)

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 37)
bigfoot <- tuesdata$bigfoot


font_add_google("Hanalei Fill", "hf")
showtext_auto()

bigfoot_data <- bigfoot %>%
  mutate(observed = tolower(observed),
    type = case_when(
    str_detect(observed, "bigfoot") ~ "bigfoot", 
    str_detect(observed, "sasquatch") ~ "sasquatch", 
    str_detect(observed, "skunk ape") ~ "skunk ape", 
             TRUE ~"others"),
    year = year(date),
    state_abbr = state.abb[match(state, state.name)]) %>%
  filter(season != "Unknown",
         visibility < 6.5,
         latitude < 55) %>%
  drop_na(c(visibility, year, wind_bearing, season))


ggplot(data = bigfoot_data) +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat,
                                             group = group),
               color = "#f0f7e7",fill = "#690907", size = 1) +
  geom_point(aes(x = longitude, y = latitude, color = wind_speed),
             size = 3) +
  facet_wrap(~factor(season, levels = c("Fall", "Winter",
                                        "Spring", "Summer")), ncol = 2) +
  scale_colour_pretty_c("Tangerines",
                        direction = -1, 
                        limits = c(0, 21),
                        breaks = c(0, 10, 20),
                        labels = c(0, "10", "20")) +
  labs(title = "Bigfoot",
       subtitle = "The locations of footprints When there is low visibility",
       colour = "Wind Speed",
       caption = "Data Source: BFRO, 2022 | TidyTuesday 2022 - Week 37 | Prepared by: @Cyd_yzc") +
  theme(plot.background = element_rect(colour = "#F2EFDE"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "hf", size = 35),
        plot.subtitle = element_text(family = "hf", size = 20),
        plot.caption = element_text(family = "hf", size = 15,
                                    hjust = 1),
        legend.title = element_text(family = "hf", size = 20),
        legend.text = element_text(family = "hf", size = 15),
        strip.text = element_text(family = "hf", size = 15),
        strip.background = element_rect(fill = "#F28500"))




