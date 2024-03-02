library(tidyverse)
library(patchwork)
library(ggtext)
library(showtext)

font_add_google("Kanit", "kanit")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load('2022-09-20')
HydroWASTE_v10 <- tuesdata$HydroWASTE_v10

# Istanbul Data

ist_data <- HydroWASTE_v10 %>% 
  filter(COUNTRY == "Turkey",
         LON_WWTP < 30 & LON_WWTP > 28.2,
         LAT_WWTP < 41.5 & LAT_WWTP > 40.9) %>% 
  select(LAT_WWTP, LON_WWTP, WASTE_DIS, POP_SERVED)

summary(ist_data$WASTE_DIS)
summary(country_data$WASTE_DIS)

tag_text <- str_wrap("Turkey has a total of 320 Hydro Wastewater Plants with an average 125,000 cubic metres waste. However, Istanbul, the most crowded city in the country has 25 plants with an average of 55,000 cubic metres waste. Moreover, the minimum amount of waste in Istanbul is 10 times higher than the same in the whole country.", 28)


# Turkey Data

country_data <- HydroWASTE_v10 %>% 
  filter(COUNTRY == "Turkey") %>% 
  select(LAT_WWTP, LON_WWTP, WASTE_DIS, POP_SERVED)

# Istanbul & Turkey Maps

world_sf <- ne_countries(returnclass = "sf", scale = "large")
tr_sf <- ne_states(country = "turkey", returnclass = "sf")
ist_sf <- tr_sf %>%
  filter(name == "Istanbul")

# Spatial Data 

ist_sf_data <- st_as_sf(ist_data,
                    coords = c("LON_WWTP", "LAT_WWTP"),
                    crs = 4326,
                    remove = FALSE)

country_sf_data <- st_as_sf(country_data,
                   coords = c("LON_WWTP", "LAT_WWTP"),
                   crs = 4326,
                   remove = FALSE)
# Istanbul Plot

p <- ggplot() +
  geom_sf(data = ist_sf,
          colour = "black",
          fill = "#F9F0FC",
          size = 0.2) +
  geom_sf(data = ist_sf_data, 
          mapping = aes(colour = WASTE_DIS, 
          size = POP_SERVED)) +
  labs(x = "", 
       y = "",
       title = "Istanbul") +
  scale_colour_pretty_c("Greens",
                        direction = -1, 
                        limits = c(20, 450000),
                        breaks = c(0, 200000, 450000),
                        labels = c(0, "200K", "450K")) +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "kanit"),
        panel.background = element_rect(fill = "grey85"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), unit = "cm"))

# Turkey Plot
p2 <- ggplot() +
  geom_sf(data = tr_sf,
          colour = "black",
          fill = "#F9F0FC",
          size = 0.2) +
  geom_sf(data = country_sf_data, 
          mapping = aes(colour = WASTE_DIS, 
                        size = POP_SERVED)) +
  labs(x = "", 
       y = "",
       size = "Population Served\n by the Plant",
       tag = tag_text,
       title = "Wastewater Plants",
       caption = "Data Source: Macedo et al., 2022 | TidyTuesday 2022 - Week 38 | Prepared by: @Cyd_yzc") +
  scale_colour_pretty_c("Greens",
                        direction = -1, 
                        limits = c(20, 450000),
                        breaks = c(0, 200000, 450000),
                        labels = c("0", "200K", "450K")) +
  scale_size_continuous(breaks=c(1000000, 2000000, 3000000),
                    labels=c("1000K", "2000K", "3000K")) +
  
  theme(plot.margin = unit(c(0.5, 1.5, 0.5, 9.5), unit = "cm"),
        plot.tag.position = c(-0.085, 0.6),
        plot.tag = element_text(family = "kanit", size = 15),
        plot.title = element_text(size = 40, family = "kanit",
                                  margin = margin(t = -8, unit = "cm")),
        plot.caption = element_text(family = "kanit", size = 17, hjust = 1),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(family = "kanit", size = 20),
        legend.text = element_text(family = "kanit", size = 15)) +
  guides(colour = guide_colourbar(title = bquote(
    "Treated wastewater\n discharged"(m^3))))
                      
# Combine the plots
final_plot <- p2 +
  inset_element(p, left = 0.2, bottom = 0.85, right = 0.4, top = 1.2) 
  
final_plot

# Save the Plot
ggsave("Week38_2022.png", final_plot, width = 25, height = 12, dpi = 72)

