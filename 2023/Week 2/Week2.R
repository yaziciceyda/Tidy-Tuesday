# Tidy Tuesday - 2023 - Week 2

# Libraries used

library(tidyverse)
library(janitor)
library(showtext)
library(ggbrace)

# Font in the Plot

font_add_google('Bitter', 'bitter')
showtext_auto()

# Change the local settings to English

Sys.setlocale("LC_ALL","English")

# Data Import

feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
site_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')
species_codes <- read.csv("https://feederwatch.org/wp-content/uploads/2022/08/PFW-species-translation-table.csv")

# Preparation for the name of the species

code_data <- species_codes %>%
  clean_names() %>%
  select(i_species_code, american_english_name)

# Data Wrangling

feederwatch_data <- feederwatch %>%
  filter(valid == 1,
         how_many >= 100) %>%
  arrange(Month, Day) %>%
  right_join(code_data, by = c("species_code" = "i_species_code")) %>%
  drop_na(Year) %>%
  mutate(y = row_number()) %>%
  group_by(Month) %>%
  mutate(avg_y = mean(y)) %>%
  ungroup() %>%
  mutate(month_abb = month(feederwatch_data$Month, label = TRUE, abbr = FALSE))
  
# Coordinates for the month names

feeder_summary <- feederwatch_data %>%
  distinct(month_abb, avg_y)

# SUbtitle of the plot

subtitle_text <- str_wrap("\nFeederWatch is a survey of birds in which
citizen scientists count birds in North America. Here, the
name of the species which have counted more than 100 times in each month are 
                          shown\n\n\n.", 45)

# Plot

p <- ggplot(feederwatch_data) +
    geom_text(aes(x = 2.00, y = y, label = american_english_name,
            alpha = how_many), color = "white", family = "bitter",
            fontface = "bold") +
  geom_text(aes(x = 2.0, y = 37, label = "Bird Species"), color = "white",
            family = "bitter", size = 5) +
  geom_text(data = feeder_summary,
            mapping = aes(x = -7.5, y = avg_y + 0.5, label = month_abb),
            color = "white", family = "bitter", hjust = 0.5) +
  geom_text(aes(x = -7.5, y = 37, label = "Month"), color = "white",
            family = "bitter", size = 5) +
  geom_brace(aes(c(-6, -3), c(1, 15.4)), inherit.data = F, color = "white",
             rotate = 270) +
  geom_brace(aes(c(-6, -3), c(15.6, 27.4)), inherit.data = F, color = "white",
             rotate = 270) +
  geom_brace(aes(c(-6, -3), c(27.6, 32.5)), inherit.data = F, color = "white",
             rotate = 270) +
  # coord_cartesian(xlim = c(0.92, 1.05), ylim = c(0, 36), expand = FALSE,
#                  clip = "off") +
  coord_fixed(clip = "off", expand = FALSE) +
  labs(title = "PROJECT FEEDERWATCH",
       subtitle = subtitle_text,
    caption = "Data Source: FeederWatch | TidyTuesday 2023 - Week 2\nPrepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(family = "bitter", size = 10,
                                    color = "white", hjust = 0),
        plot.title = element_text(family = "bitter", size = 25,
                                    color = "white", hjust = 0.5),
        plot.subtitle = element_text(family = "bitter", size = 15,
                                  color = "white", hjust = 0.5),
        plot.margin = unit(c(1.0, 6.5, 1.0, 5), "cm"))

# Save the Plot

ggsave("Week2.png", p, width = 12, height = 14, dpi = 72)





