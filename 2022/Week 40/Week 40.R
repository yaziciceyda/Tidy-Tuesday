# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 40)
product_hunt <- tuesdata$product_hunt

library(tidyverse)
library(lubridate)
library(showtext)

# Data Wrangling

ct <- product_hunt %>%
  select(category_tags) %>%
  mutate(ct2 = str_replace_all(category_tags, "\\[", " "),
         ct3 = str_replace_all(ct2, "\\]", " "),
         ct4 = str_replace_all(ct3, "\\'", " ")) %>%
  select(category_tags, ct4)


iphone <- product_hunt %>% 
  filter(str_detect(product_hunt$category_tags,
                    "IPHONE")) %>%
  mutate(year = year(release_date),
         fraction = 1/nrow(iphone)) %>%
group_by(year) %>%
  slice(which.min(product_ranking)) %>%
  ungroup() %>%
  mutate(images_cropped = circle_crop(main_image),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n = -1)),
         color = c("#F91CD0", "#F90830", "#5808F9", "#08D8F9",
                   "#08F922", "#EEF908", "#F96708", "#0896F9"),
         xloc = c(11.7, 11, 10.5, 11.5, 
               11.7, 11.7, 11.7, 11.7),
         yloc = c(0.05, 0.15, 0.26, 0.45,
                  0.5625, 0.665, 0.845, 0.95))

font_add_google('Solway', 'sw')
showtext_auto()

# Plot 

final_plot <- ggplot(iphone) +
  coord_polar(theta = "y") +
  xlim(c(2, 11.7)) +
  # line
  geom_segment(mapping = aes(x = 4,
                             y = (ymax + ymin)/2,
                             xend = 10,
                             yend = (ymax + ymin)/2),
                             color = "black",
                             size = 1,
                             linetype = "dotted") +
  geom_rect(mapping = aes(ymax=ymax, ymin=ymin, xmax=6, xmin=4,
                          fill = color)) +
  # year
  geom_text(x = 5, aes(y = (ymax + ymin)/2, label = year, 
                         color = "black"), size = 6, family = "sw") +
  geom_point(data = iphone, 
             mapping = aes(x = 10, y = (ymax + ymin)/2, color = "#B708F9"),
             size = 35) +
  geom_image(data = iphone, 
             mapping = aes(x = 10, y = (ymax + ymin)/2, image = images_cropped),
             size = 0.09) +
  # Name
  geom_text( aes(x = xloc, y = yloc, label = name), 
            size = 6.8, family = "sw") +
  scale_color_identity() +
  labs(title = "Product Hunts",
       subtitle = "The first ranked product of each year in the **IPHONE** category.",
       x = "",
       y = "",
       caption = "Data Source: Data is Plural | TidyTuesday 2022 - Week 40 | Prepared by: @Cyd_yzc") +
  theme(panel.background = element_rect(fill = "#D6C7D3"),
        panel.grid = element_blank(), 
        plot.title = element_text(size = 30, family = "sw"),
        plot.subtitle = element_markdown(size = 20, family = "sw"),
        plot.caption = element_text(size = 15, family = "sw", hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
final_plot
ggsave("Week40_2022.png", final_plot, width = 20, height = 15, dpi = 72)

 