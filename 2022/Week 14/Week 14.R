library(showtext)
library(tidyverse)

# Font in the Plot

font_add_google('Mukta', 'mukta', db_cache = FALSE)
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 14)

news_orgs <- tuesdata$news_orgs

# Data Wrangling

data_news <- news_orgs %>%
  select(tax_status_current, primary_language, country) %>%
  drop_na(tax_status_current, primary_language, country) %>%
  mutate(primary_language = ifelse(primary_language == 
        "Spanish, English", "Bilingual (Spanish & English)", primary_language))

# Subtitle in the Plot

subtitle_text <- str_wrap("Digitally focused, local news organizations 
are located in United States, Canada and U.S. Virgin Islands 
and generally publish in English. Spanish publications have profit tax status 
in Canada; while they have under umbrella of a 501c, sole proprietor, profit
and not for profit status in United States.", 100)

# The Plot

p <- ggplot(data_news) +
  geom_point(aes(x = factor(country, level = c('United States', 
                                               'Canada', 
                                               'U.S. Virgin Islands')), 
                            y = tax_status_current,
                 colour = primary_language)) +
  geom_jitter(aes(x = country, y = tax_status_current,
                  colour = primary_language)) +
  scale_colour_manual(values = c("red", "blue", "green")) +
  labs(x = "Country",
       y = "Current Tax Status",
       colour = "Language",
       title ="Digital Publications",
       caption = "Data Source: Data is Plural | TidyTuesday 2022 - Week 14 | Prepared by: @Cyd_yzc",
       subtitle = subtitle_text) +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "mukta", size = 13),
        axis.title = element_text(family = "mukta", size = 17),
        legend.background = element_rect(fill = "ivory"),
        legend.key = element_rect(colour = "transparent", fill = "ivory"),
        legend.text = element_text(family = "mukta", size = 12),
        legend.title = element_text(family = "mukta", size = 16),
        plot.subtitle = element_text(family = "mukta", size = 20),
        plot.title = element_text(family = "mukta", size = 40),
        plot.caption = element_text(family = "mukta", size = 18, hjust = 1),
        plot.margin = unit(c(1.0, 0.5, 1.0, 0.5), "cm")) +
guides(colour = guide_legend(override.aes = list(size = 5)))

# Save the Plot

ggsave("Week14_2022.png", p, width = 25, height = 15, dpi = 72)
