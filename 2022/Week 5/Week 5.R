library(janitor)
library(dplyr)
library(tidyverse)
library(cropcircles)
library(ggtext)
library(showtext)

# library to use fontawesome icons

font_add('fa-solid', 'Font Awesome 6 Free-Solid-900.otf')

#Font in the plot

font_add_google('Delicious Handrawn', 'dh', db_cache = TRUE)
showtext_auto()

# Data Wrangling
tuesdata <- tidytuesdayR::tt_load(2022, week = 5)

breed_traits <- tuesdata$breed_traits %>%
  clean_names() %>%
  mutate(breed = stringi::stri_enc_toascii(breed),
         breed = gsub("\\\032", " ", breed)) 

breed_trait_description <- tuesdata$trait_description%>%
  clean_names()

breed_breed_rank <- tuesdata$breed_rank%>%
  clean_names()

# https://stackoverflow.com/questions/70980101/left-join-produces-nas-when-key-has-spaces

breed_data <- breed_breed_rank %>%
  arrange(x2020_rank) %>%
  slice_min(x2020_rank, n = 4) %>%
  select(breed, x2020_rank, image) %>%
  mutate(breed = stringi::stri_enc_toascii(breed),
         breed = gsub("\\\032", " ", breed),
         image_cropped = circle_crop(image))  %>%
  left_join(breed_traits, by = "breed") %>%
  mutate(breed = fct_reorder(breed, x2020_rank))


# Plot

p <- ggplot(breed_data) +
  geom_image(aes(x = 0.5, y = x2020_rank, image = image_cropped),
             size = 0.1) +
  geom_richtext(aes(x = 1.5, y = x2020_rank, size = affectionate_with_family),
            label = "<span style='font-family:fa-solid'>&#xe537;</span>",
            fill = NA, label.colour = NA,
            family = 'fontawesome-webfont') +
  geom_richtext(aes(x = 2, y = x2020_rank, size = good_with_young_children),
                label = "<span style='font-family:fa-solid'>&#xe59d;</span>",
                fill = NA, label.colour = NA,
                family = 'fontawesome-webfont')  +
  geom_richtext(aes(x = 2.5, y = x2020_rank, size = good_with_other_dogs),
                label = "<span style='font-family:fa-solid'>&#xf6d3;</span>",
                fill = NA, label.colour = NA,
                family = 'fontawesome-webfont') +
  geom_richtext(aes(x = 3, y = x2020_rank, size = energy_level),
                label = "<span style='font-family:fa-solid'>&#xf0e7;</span>",
                fill = NA, label.colour = NA,
                family = 'fontawesome-webfont') +
  geom_richtext(aes(x = 3.5, y = x2020_rank, size = watchdog_protective_nature),
                label = "<span style='font-family:fa-solid'>&#xf1b0;</span>",
                fill = NA, label.colour = NA,
                family = 'fontawesome-webfont')  +
  scale_y_reverse() + 
  geom_text(aes(x = -0.95, y = x2020_rank, label = breed), 
            hjust = 0, family = "dh", size = 5) +
  geom_text(aes(x = 1.2, y = -0.2, label = "Affectionate\nwith\nfamily"),
            hjust = 0, family = "dh", size = 5) +
  geom_text(aes(x = 1.8, y = -0.2, label = "Good with\nyoung\nchildren"), 
            hjust = 0, family = "dh", size = 5) +
  geom_text(aes(x = 2.4, y = -0.2, label = "Good with\nother dogs"), 
            hjust = 0, family = "dh", size = 5) +
  geom_text(aes(x = 3, y = -0.2, label = "Energy\nlevel"), 
            hjust = 0, family = "dh", size = 5) +
  geom_text(aes(x = 3.5, y = -0.2, label = "Watchdog\nprotective\nnature"), 
            hjust = 0, family = "dh", size = 5) +
 coord_fixed(xlim = c(-1, 5), ylim = c(5, -1))  +
  labs(title = "DOG BREEDS",
       caption = "Data Source: American Kennel Club | TidyTuesday 2022 - Week 5 | Prepared by: @Cyd_yzc") +
  theme(panel.background = element_rect(fill = "pink", color = NA),
        plot.background = element_rect(fill = "pink", color = NA),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 30, hjust = 0.5, family = "dh"),
        plot.caption = element_text(size = 15, hjust = 1, family = "dh"),
        plot.margin = margin(0.5, 2, 0.5, 2, "cm"))
  
# Save the Plot

ggsave("Week5.png", p, width = 25, height = 12, dpi = 72)


