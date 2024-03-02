library(lubridate)
install.packages('PostcodesioR')
library(PostcodesioR)
library(PrettyCols)
library(showtext)

# Font in the Plot

font_add_google('Ubuntu', 'ubuntu')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 26)
paygap <- tuesdata$paygap

# Data Wrangling

pay_data <- paygap %>%
  mutate(year = year(date_submitted)) %>%
  filter(diff_mean_hourly_percent < 100 & diff_mean_hourly_percent > -100) %>%
  group_by(year) %>%
  arrange(diff_mean_hourly_percent, .by_group = TRUE) %>%
  slice_head(n = 10) %>%
  ungroup() 

# Longitute and Latitude values

long_list <- c()
lat_list <- c()

  for(i in 1:length(pay_data$post_code))
  {
  pc <- pay_data$post_code[i]
  long_list <- c(long_list,  postcode_lookup(pc)$longitude)   
  lat_list <- c(lat_list,  postcode_lookup(pc)$latitude)  
  }

# Data Wrangling 

pay_data <- pay_data %>%
  mutate(longitude = long_list,
         latitude = lat_list) %>%
  filter(longitude > -5.96)

# England & Scotland Map

england = rnaturalearth::ne_countries(country = 'united kingdom',
                                      type='map_units',scale = 'medium',
                                      returnclass = "sf") %>% 
filter(geounit %in% c("England", "Scotland"))
england2 = st_transform(england, 7405)

en_sf_data <- st_as_sf(pay_data,
                       crs = 4326,
                       coords = c("longitude", "latitude"),
                       remove = FALSE)

# Subtitle of the Plot

subtitle_text <- str_wrap("The top ten companies whose average hourly pay is
                          higher for women in each year are given.
                          Among those, 40% of them have more than 75% of 
                          females in the top hourly pay quarter.", 50)

# Caption is added as tag

tag <- "Data Source: gender-pay-gap.service.gov.uk | 
       TidyTuesday 2022 - Week 26 | 
       Prepared by: @Cyd_yzc"

# The Plot

p <- ggplot() +
  geom_sf(data = england,
          colour = NA,
          fill = "ivory",
          lwd = 0) +
  geom_point(pay_data, mapping = aes(longitude, latitude,
                      colour = year, size = female_top_quartile), 
             shape = 19,  alpha = 0.5) +
  scale_colour_pretty_c("Pinks", direction = -1) +
  guides(size = guide_legend(override.aes = list(colour = "#F31893"))) +
  labs(title = "UK GENDER PAY GAP",
       colour = "Year",
       size = "% of Females in the\nTop Hourly Pay Quarter",
       subtitle = subtitle_text,
       tag = tag) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#5DA3F1", color = NA),
        plot.background = element_rect(fill = "#5DA3F1", color = NA),
        legend.background = element_rect(fill = "#5DA3F1", color = NA),
        legend.key = element_rect(colour = "transparent", fill = "#5DA3F1"),
        legend.text = element_text(family = "ubuntu", size = 13),
        legend.title = element_text(family = "ubuntu", size = 15),
        plot.tag = element_text(hjust = 1, size = 13, family = "ubuntu"),
        plot.title = element_text(family = "ubuntu", hjust = 1, size = 30),
        plot.subtitle = element_text(family = "ubuntu", size = 20),
        plot.tag.position = c(1, 0.05),
        plot.margin = unit(c(0.5, 1.0, 0.5, 1.0), "cm"))
  
# Save the Plot

ggsave("Week26_2022.png", p, width = 14, height = 15, dpi = 72)


