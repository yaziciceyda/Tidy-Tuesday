library(tidyverse)
library(ggimage)
library(showtext)

# Font in the Plot

font_add_google('Protest Riot', 'pr', db_cache = FALSE)
showtext_auto()

# library to use fontawesome icons
font_add('fa-regular', 'Font Awesome 6 Free-Regular-400.otf')

# Data Import

groundhogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/groundhogs.csv')
predictions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/predictions.csv')

# Data Wrangling 

pred_data <- predictions %>%
  filter(!is.na(shadow)) %>%
  select(-c(id, details)) %>%
  arrange(year, shadow) %>%
  group_by(year, shadow) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  distinct(year, shadow, .keep_all = TRUE) %>%
  group_by(year) %>%
  mutate(total = sum(n),
         percent_shadow = round(n /total * 100, 0)) %>%
  slice(which.max(percent_shadow)) %>%
  ungroup() %>%
    filter(year >= 1994) %>%
  mutate(percent_shadow = ifelse(shadow == TRUE, -1 * percent_shadow,
                                 percent_shadow),
           label_shadow = ifelse(shadow == TRUE,
           "<span style='font-family:fa-solid'>&#xf2dc;</span>",
           "<span style='font-family:fa-solid'>&#xf185;</span>"),
         label_colour = ifelse(shadow == TRUE, "#12F2F9", 
                               "#FDB813"))

# Subtitle in the Plot

subtitle_text <- "<span style='color:black'>Traditionally, 
various groundhogs across North America are consulted by local public
figures for an annual weather<br>prediction: if the groundhog sees its 
shadow that is a prediction of six more weeks of winter which is
<span style='color:#12F2F9'>Longer Winter<span style='color:black'>.<br>
Otherwise spring will come early and that is called 
as <span style='color:#FDB813'>Early 
Spring<span style='color:black'>.
The  consensus that predicts the spring or winter<br>in the last 30 
years is around 60%.<br><br>"  

# The Plot

p <- ggplot(pred_data) +
  geom_line(aes(x = year, y = percent_shadow), size = 2,
            color = "grey50") +
  geom_richtext(aes(x = year, y = percent_shadow, 
                    label = label_shadow, colour = label_colour),
                label.colour = NA, family = 'fontawesome-webfont',
                fill = NA, size = 26) +
  geom_text(aes(x = year, y = percent_shadow + 1, 
                label = abs(percent_shadow)), 
            size = 10, family = "pr") +
  annotate("text", x = 2026, y = 50, label = "Early\nSpring",
           family = "pr", size = 15) +
  annotate("text", x = 2026, y = -50, label = "Longer\nWinter",
           family = "pr", size = 15) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.2) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(1994, 2027), 
                     breaks = seq(1994, 2024, 5)) +
  scale_y_continuous(limits = c(-80, 89)) +
  labs(x = "",
       y = "Percentage of Consensus",
       title = "Groundhog Predictions",
       subtitle = subtitle_text,
       caption = "Data Source: Groundhog-day.com API\nTidyTuesday 2024 - Week 5 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "pr", size = 22),
        axis.title = element_text(family = "pr", size = 25),
        plot.title = element_text(family = "pr", size = 45),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(size = 24, family = "pr",
                                         hjust = 0, vjust = 2),
        plot.caption = element_text(family = "pr", size = 22, 
                                    hjust = 1),
        plot.margin = margin(1, 1, 1, 1, "cm")) 

# Save the Plot

ggsave("Week5.png", p, width = 25, height = 12, dpi = 72)


