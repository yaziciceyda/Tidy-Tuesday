library(tidyverse)
library(showtext)

# Font in the Plot

font_add_google(name = "Radley",
                family = "radley", db_cache = FALSE)
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2024, week = 11)
fiscal_sponsor_directory <- tuesdata$fiscal_sponsor_directory

# Data Wrangling

fest_data <- fiscal_sponsor_directory %>%
  mutate(type = case_when(
    str_detect(project_types, "Festivals") == TRUE ~ "Festivals",
    str_detect(project_types, "Auditing") == TRUE ~ "Auditing",
    str_detect(project_types, "Education") == TRUE ~ "Education",
    str_detect(project_types, "Environment") == TRUE ~ "Environment",
    str_detect(project_types, "Disaster relief") == TRUE ~ "Disaster",
    str_detect(project_types, "Children, youth and families") == TRUE ~ 
      "Families",
    str_detect(project_types, "Arts and culture") == TRUE ~ 
      "Arts and culture")) %>%
 # arrange(year_fiscal_sponsor)
  group_by(type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(n) %>%
  filter(!is.na(type))

# Preparation for the Plot

packing <- circleProgressiveLayout(fest_data$n, sizetype='area')
fest_data <- cbind(fest_data, packing)
dat.gg <- circleLayoutVertices(packing, npoints = 50)

# Subtitle of the Plot

subtitle_text <- "Most of the organizations sponsored festivals and
education related projects. On the other hand, only a few 
ones sponsored disaster related projects."

# The Plot

p <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, 
                                  fill = as.factor(id)),
               colour = "black", alpha = 0.6) +
  geom_text(data = fest_data, aes(x, y, size = n, label = type),
            family = "radley") +
  scale_size_continuous(range = c(5, 10))  +
  coord_equal() +
  labs(title = "Fiscal Sponsors",
       subtitle = subtitle_text,
       caption = "Data Source: Fiscal Sponsor Directory\nTidyTuesday 2024 - Week 11 | Prepared by: C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "radley", hjust = 0,
                                  size = 50),
        plot.subtitle = element_text(family = "radley", hjust = 0,
                                  size = 25),
        plot.caption = element_text(family = "radley", hjust = 1,
                                  size = 20),
        plot.margin = margin(0.5, 0.8, 0.5, 0.8, "cm")
        )
# Save the Plot

ggsave("Week11.png", p, width = 18, height = 14, dpi = 72)
