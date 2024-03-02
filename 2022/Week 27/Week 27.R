library("PrettyCols")
library(scales)
library(showtext)
library(stringr)

# Font in the plot

font_add_google('Playfair Display', 'pd')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 27)
rent <- tuesdata$rent
sf_permits <- tuesdata$sf_permits
new_construction <- tuesdata$new_construction

# Data Wrangling

sun_data_summary <- rent %>%
  filter(beds < 7) %>%
  mutate(sun = ifelse(str_detect(title, "Sun |sun |Sunny|sunny"), "Sunny",
                      "Not Sunny"),
  beds = as.factor(beds),
  sun = as.factor(sun)) %>%
  drop_na(beds, sun) %>%
  group_by(sun, beds) %>%
  summarise(med = median(price)) %>%
  ungroup()

sun_data <- rent %>%
  filter(beds < 7) %>%
  mutate(sun = ifelse(str_detect(title, "Sun |sun |Sunny|sunny"),
                      "Sunny", "Not Sunny"),
         beds = as.factor(beds),
         sun = as.factor(sun)) %>%
  drop_na(beds, sun) %>%
  left_join(sun_data_summary, by = c("sun", "beds"))

# Subtitle of the Plot

subtitle_text = str_wrap("The median price of the Sunny houses are almost equal
to the others up to 5 bedrooms. However, the median is significantly higher for
the sunny houses with 6 bedrooms. But, there are fewer sunny houses for each
number of bedroms.", 100)

# The Plot

p <- ggplot(sun_data) +
  geom_point(aes(x = beds, y = price, color = beds)) +
  geom_jitter(aes(x = beds, y = price, color = beds)) +
  geom_line(data = sun_data_summary, aes(x = beds, y = med, 
                          group = sun), color = "brown", size = 2) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
    facet_wrap(~factor(sun, levels = c("Sunny", "Not Sunny"))) +
  labs(title = "Rental Costs in San Fransisco",
       x = "Number of Beds",
       y = "Rents",
       subtitle = subtitle_text,
       caption = "Data Source: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018 | TidyTuesday 2022 - Week 27
       | Prepared by: @Cyd_yzc") +
  scale_colour_pretty_d("Tangerines", direction = -1) +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        legend.position = "none",
        panel.spacing = unit(3, "lines"),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "pd", size = 17),
        axis.title.y = element_text(family = "pd", size = 20,
                          margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(family = "pd", size = 20,
                                    margin = margin(t = 20, r = 0, 
                                                    b = 0, l = 0)),
        plot.caption = element_text(size = 15, hjust = 1, family = "pd",
                                    margin = margin(t = 10, r = 10, 
                                                    b = 10, l = 0)),
        plot.title = element_text(family = "pd", size = 30),
        plot.subtitle = element_text(family = "pd", size = 21),
        strip.background = element_rect(fill = "brown"),
        strip.text = element_text(family = "pd", size = 15, color = "ivory"),
        plot.margin = unit(c(0.5, 1.0, 0.5, 1.0), "cm")
        )

# Save the Plot

ggsave("Week27_2022.png", p, width = 25, height = 14, dpi = 62)





