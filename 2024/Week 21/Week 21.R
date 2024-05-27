library(tidyverse)
library(janitor)
library(ggstream)
library(paletteer) # most of the colors are taken from "palettetown::poliwrath"
library(colorspace)
library(showtext)
library(patchwork)
library(ggchicklet)
library(stars)

# Font in the Plot

font_add_google('Radio Canada Big', 'rcb', db_cache = FALSE)
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2024, week = 21)

emissions <- tuesdata$emissions 

carbon_data <- emissions %>%
  clean_names() %>%
  group_by(commodity, year) %>%
  summarise(total_carbon = sum(total_emissions_mt_co2e)) %>%
  ungroup()


year_data <- tibble(year = c(1900, 1910, 1914, 1920, 1930, 1932, 1940, 
                             1945, 1950, 1960, 1970, 1979, 1980, 1990, 
                             1992, 2000, 2008, 2010, 2020),
                    title = NA) %>%
  mutate(title = ifelse(year == 1914, "World\nWar I", title),
         title = ifelse(year == 1932, "Great\nDepression", title),
         title = ifelse(year == 1945, "End of\nWorld War II", title),
         title = ifelse(year == 1932, "Second Oil\nCrisis", title),
         title = ifelse(year == 1992, "Dissolution of\nSoviet Union", title),
         title = ifelse(year == 2008, "Global\nFinancial\nCrisis", title),
         title = ifelse(year == 2020, "Covid-19\nPandemic", title))

color_hex <- c("#5e74b2", "#B0B8E0FF", "#8088B0FF", "#bbbbdb", 
"#a3a3c4", "#303068FF", "#D8D8D8FF", "#505050FF", "#787878FF")

subtitle_text <- str_wrap("Carbon Majors is a database of historical 
production data from 122 of the world’s largest oil, gas, coal, and 
cement producers. This data is used to quantify the direct operational 
emissions and emissions from the combustion of marketed products that 
can be attributed to these entities. The total amount of carbon emissions 
from the nine different commodities has an increasing trend 
especially after the World War II.", 90)


p1 <- ggplot(carbon_data) +
  geom_segment(data = year_data %>%
                 filter(!is.na(title)), aes(y = -23000, yend = 0, 
                                     x = year, xend = year),
               linetype = "dashed") +
  geom_text(data = year_data %>%
              filter(!is.na(title)), aes(x = year, y = -25000,
                                         label = paste0(title, "\n", "(",
                                          year, ")")),
            family = "rcb", size = 6) +
  geom_stream(aes(x = year, y = total_carbon, fill = commodity)) +
  scale_fill_manual(values = darken(color_hex, 0.2)) +
  scale_x_continuous(breaks = seq(1910, 2022, by = 10),
    labels = seq(1910, 2022, by = 10)) +
  annotate("rect", ymin = -30000, ymax = 0,
           xmin = 1902, xmax = 1905, fill = "grey60") +
  annotate("rect",xmin = 1901, xmax = 1906,
                          ymin = -1000, ymax = 1000,
                          fill = "black") +
  coord_cartesian(xlim = c(1900, 2025)) +
  labs(caption = "Data Source: Carbon Majors\nTidyTuesday 2024 - Week 21\nPrepared by: C. YAZICI",
       fill = "Commodity",
       title = "Carbon Emissions",
       subtitle = subtitle_text) +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(family = "rcb", size = 18),
        legend.title = element_text(family = "rcb", size = 17),
        legend.text = element_text(family = "rcb", size = 15),
        legend.background = element_rect(fill = "ivory"),
        legend.key.height = unit(1.3, "cm"),
        legend.key.width = unit(1, "cm"),
        plot.caption = element_text(family = "rcb", size = 18,
                                    hjust = 1),
        plot.title = element_text(family = "rcb", size = 50,
                                  hjust = 0),
        plot.subtitle = element_text(family = "rcb", size = 28,
                                  hjust = 0),
        plot.margin = margin(1, 1, 1, 1, "cm"))
 
# Preparation for the World Globe

world <- rgeoboundaries::gb_adm0()


world_ortho <- world %>% 
  st_transform(crs = "+proj=ortho +lon_0=45 +lat_0=40")

rect <- st_as_sf(data.frame(lat = 35, long = 36), coords = c("lat", "long"), 
                 crs = 4326)

ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = "+proj=ortho +lon_0=45 +lat_0=40")


# World Globe

p2 <- ggplot(world_ortho) +
  geom_sf(data = ocean, color = NA, fill = "#1da2d8") +
  geom_sf(fill = "#E3C575", color = "#A16E47", linewidth = 0.1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA)
  )



final_plot <- p1 +
  inset_element(p2, left = 0.007, bottom = -0.15, right = 0.12,
                top = 0.25) +
  theme(plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"))

# Save the Plot

ggsave("Week21.png", final_plot, width = 25, height = 18, dpi = 72)



 



