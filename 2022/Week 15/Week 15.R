library(tidyverse)
library(geofacet)
library(ggtext)
library(showtext)

# Font in the Plot

font_add_google('PT Sans', 'pt', db_cache = FALSE)
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load('2022-04-12')

indoor_pollution <- tuesdata$indoor_pollution
fuel_access <- tuesdata$fuel_access
fuel_gdp <- tuesdata$fuel_gdp
death_source <- tuesdata$death_source
death_full <- tuesdata$death_full
death_timeseries <- tuesdata$death_timeseries

# Data Wrangling

# fuel_gdp - Access to clean fuels and technologies for cooking

europe_data <- fuel_gdp %>%
  filter(Continent == "Europe") %>%
  drop_na(`Access to clean fuels and technologies for cooking (% of population)`)

fuel_data <- fuel_gdp %>%
  filter(Entity %in% europe_data$Entity,
         Year < 2021 & Year > 1999) %>%
  mutate(Entity = ifelse(Entity == "United Kingdom", "UK", Entity),
         Entity = ifelse(Entity == "Bosnia and Herzegovina", "Bosnia & H.",
                         Entity),
         Entity = ifelse(Entity == "North Macedonia", "N. Macedonia", Entity)
  )

# indoor_pollution 

europe_data_indoor <- indoor_pollution %>%
  filter(Entity %in% europe_data$Entity,
         Year < 2021 & Year > 1999) %>%
  rename(Deaths = `Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Percent)`) %>%
  mutate(Entity = ifelse(Entity == "United Kingdom", "UK", Entity),
         Entity = ifelse(Entity == "Bosnia and Herzegovina", "Bosnia & H.",
                         Entity),
         Entity = ifelse(Entity == "North Macedonia", "N. Macedonia", Entity)
  )

full_data <- fuel_data %>%
  select(-c(Code, Continent)) %>%
  right_join(europe_data_indoor, by = c("Entity", "Year"))

# Subtitle

subtitle_text = "<span style='color:red'>Indoor air pollution 
<span style='color:black'>is caused by burning solid fuel 
sources – such as firewood, crop waste, and dung – for cooking and heating. 
4.1% of global deaths <br>are attributed to indoor air pollution;
while only 60% of the world has <span style='color:#27A41D'>access
to clean fuels for cooking. <span style='color:black'>In Europe, most of the 
countries <br>have access to clean fuels for cooking and smaller amount of
indoor air pollution since 2000."

# The Plot

p <- ggplot(full_data) +
  geom_line(aes(x = Year, 
                y = `Access to clean fuels and technologies for cooking (% of population)`),
            colour = "#27A41D", linewidth = 2) +
  geom_line(aes(x = Year, 
                y = Deaths), colour = "red", linewidth = 2) +
  facet_geo(~Entity, grid = "europe_countries_grid2") +
  labs(title = "Indoor Air Pollution",
       subtitle = subtitle_text,
    y = "Percentage",
    caption = "Data Source: OurWorldInData.org | TidyTuesday 2022 - Week 15 | Prepared by: @Cyd_yzc") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        strip.background = element_rect(
          color = "black", fill = "#F6B3EF", size = 1.5, linetype = "solid"),
        strip.text = element_text(family = "pt", size = 12),
        panel.border = element_rect(fill = NA, colour = "grey40"),
        plot.subtitle = element_markdown(size = 20, family = "pt"),
        plot.caption = element_text(family = "pt", hjust = 1, size = 15),
        plot.title = element_text(family = "pt", hjust = 0, size = 28),
        axis.text = element_text(family = "pt", size = 10),
        axis.title = element_text(family = "pt", size = 18),
        plot.margin = unit(c(0.5, 1.0, 0.5, 1.0), "cm"))


# Save the Plot

ggsave("Week15_2022.png", p, width = 25, height = 15, dpi = 72)


