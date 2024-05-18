library(tidyverse)
library(ggtext)
library(showtext)
library(ggnewscale)

# library to use fontawesome icons

font_add('fa-solid', 'Font Awesome 6 Free-Solid-900.otf')
showtext_auto()

# Font in the Plot

font_add_google('Yanone Kaffeesatz', 'yk')
showtext_auto()

coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')

coffee_data <- coffee_survey %>%
  drop_na(favorite, roast_level, caffeine) %>%
  select(favorite, roast_level, caffeine) %>%
  filter(roast_level %in% c("Light", "Medium", "Dark")) %>%
  group_by(favorite) %>%
  mutate(n_favorite = n()) %>%
  ungroup() %>%
  group_by(favorite, roast_level) %>%
  mutate(n_roast_level = n(),
         percentage_roast_level = round(n_roast_level / n_favorite * 100)) %>%
  ungroup() %>%
  group_by(favorite, caffeine) %>%
  mutate(n_caffeine = n(),
         percentage_caffeine = round(n_caffeine / n_favorite * 100)) %>%
  ungroup() %>%
  unique() %>%
  arrange(desc(n_favorite)) %>%
  mutate(roast_level = factor(roast_level,
                              levels = c("Dark", "Medium", "Light"),
                              ordered = TRUE),
         label_coffee = "<span style='font-family:fa-solid'>&#xf0f4;</span>",
         x_roast = case_when(
           roast_level == "Light" ~ 0.95,
           roast_level == "Medium" ~ 1,
           roast_level == "Dark" ~ 1.05,
         ),
         y_roast = case_when(
           roast_level == "Light" ~ 1.3,
           roast_level == "Medium" ~ 1.35,
           roast_level == "Dark" ~ 1.3,
         ),
         x_caffeine = case_when(
           caffeine == "Decaf" ~ 0.95,
           caffeine == "Half caff" ~ 1,
           caffeine == "Full caffeine" ~ 1.05,
         ),
         y_caffeine = case_when(
           caffeine == "Decaf" ~ 0.7,
           caffeine == "Half caff" ~ 0.65,
           caffeine == "Full caffeine" ~ 0.7,
         )
         ) %>%
  mutate(percent_favorite = round(n_favorite / 3704 * 100))




subtitle_text <- str_wrap("According to The Great American Coffee Taste Test 
Survey, Pourover is the most popular type; while blended drink
is the least. The preference of roast and caffeine levels
are also shown. Light or medium roast and full caffeine are the most 
popular choices.", 150)

p <- ggplot(coffee_data) +
  geom_richtext(aes(x = 1, y = 1,
                    label = label_coffee,
                    family = 'fontawesome-webfont'),
                size = 13,
                colour = "ivory",
                label.colour = NA, fill = NA,
                hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 1, y = 1.04, label = paste(percent_favorite, "%")),
            hjust = 0.5, vjust = 0.5, family = "yk",
            size = 5) +
  # Roast
  geom_richtext(aes(x = x_roast, y = y_roast,
                    label = label_coffee,
                    color = percentage_roast_level,
                    family = 'fontawesome-webfont'),
                size = 9,
                label.colour = NA, fill = NA,
                hjust = 0.5, vjust = 0.5) +
  # Roast Level Labels
  geom_text(aes(x = x_roast, y = y_roast + 0.25, 
                label = roast_level), hjust = 0.5, family = "yk",
            size = 5) +
  scale_color_gradient(low = "#af7f5e",
                       high = "#2b1e15") +
  labs(color = "Roast Level") +
  new_scale_color() +
  # Caffeine
  geom_richtext(aes(x = x_caffeine, y = y_caffeine,
                    label = label_coffee,
                    color = percentage_caffeine,
                    family = 'fontawesome-webfont'),
                size = 9,
                label.colour = NA, fill = NA,
                hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = x_caffeine, y = y_caffeine - 0.2, 
                label = caffeine), hjust = 0.5, family = "yk",
            size = 5) +
  scale_color_gradient(low = "#d2a790",
                       high = "#b25d2f") +
  # scale_x_continuous(limits = c(0.75, 1.2)) +
  labs(color = "Caffeine",
       subtitle = subtitle_text,
       title = "The Great American Coffee Taste Test",
       caption = "Data Source: The Great American Coffee Taste Test Survey\nTidyTuesday 2024 - Week 20\nPrepared by: C. YAZICI") +
  facet_wrap(~factor(favorite, levels = c("Pourover", "Latte", 
                                          "Regular drip coffee",
                                          "Cappuccino",
                                          "Espresso", "Cortado",
                                          "Americano", "Iced coffee",
                                          "Mocha", "Other", 
                                          "Cold brew",
                                          "Blended drink (e.g. Frappuccino)")),
             ncol = 3) +
  #  coord_cartesian(xlim = c(0.82, 1.18), expand = c(0, 0)) +
  coord_fixed(ratio = 0.05, xlim = c(0.93, 1.07), ylim = NULL,
              expand = TRUE, clip = "on") +
  theme(panel.background = element_rect(fill = "grey80", color = NA),
        plot.background = element_rect(fill = "grey80", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.background = element_rect(fill = "grey80"),
        legend.title = element_text(family = "yk", size = 18),
        legend.text = element_text(family = "yk", size = 17),
        legend.key.height = unit(1.2, 'cm'),
        legend.key.width = unit(0.8, 'cm'),
        strip.text = element_text(family = "yk", size = 20,
                                  color = "#6F4E37"),
        plot.caption = element_text(family = "yk", hjust = 1,
                                    size = 18, color = "#6F4E37"),
        plot.title = element_text(family = "yk", color = "#6F4E37",
                                  size = 35, hjust = 0),
        plot.subtitle = element_text(family = "yk", color = "#6F4E37",
                                     size = 20, hjust = 0),
        plot.margin = margin(0.8, 0.2, 0.5, 0.2, "cm"))

# Save the Plot

ggsave("Week20.png", p, width = 20, height = 14, dpi = 72)

