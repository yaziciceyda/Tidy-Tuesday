library(tidyverse)
library(janitor)
library(ggforce)
library(showtext)
library(ggimage)


# Font in the Plot

font_add_google('Graduate', 'graduate')
showtext_auto()

team_data <- team_results %>%
  clean_names() %>%
  mutate(champpercent = str_replace_all(champpercent, "%", ""),
         champpercent = as.numeric(champpercent)) %>%
  filter(champpercent > 0,
         top2 > 0,
         champ > 0) %>%
  arrange(desc(champpercent))



public_data <- public_picks %>%
  clean_names() %>%
  mutate(r64 = str_replace_all(r64, "%", ""),
         r64 = as.numeric(r64),
         r32 = str_replace_all(r32, "%", ""),
         r32 = as.numeric(r32),
         s16 = str_replace_all(s16, "%", ""),
         s16 = as.numeric(s16),
         e8 = str_replace_all(e8, "%", ""),
         e8 = as.numeric(e8),
         f4 = str_replace_all(f4, "%", ""),
         f4 = as.numeric(f4),
         finals = str_replace_all(finals, "%", ""),
         finals = as.numeric(finals)
  ) %>%
  filter(team %in% team_data$team) %>%
  left_join(team_data %>%
              select(team, champ),
            by = c("team")) %>%
  arrange(desc(r64)) %>%
  mutate(x = row_number())

legend_data <- tibble(x = seq(4.5, 10, length.out = 6),
                      label = c("Finals", "Final 4", "Elite 8",
                                "Sweet 16", "Round of\n32",
                                "Round of\n64"),
                      color = rev(c("#341204", "#9e370d", "#cb4610",
                                "#ec5b20", "#f2936b", "#f6ae91")),
                      y = 5)

img <- "C:\\Users\\Ceyda\\Desktop\\Tidy Tuesday\\2024\\Week 13\\pic1.png"

subtitle_text <- str_wrap("March Madness is the NCAA Division I basketball 
tournament that has six rounds for the national championship. Here,
the teams which had been Champion, had awarded 1 or 2 with the highest
likelihood of winning at least 1 Championship are selected. Then, the 
percentage of people who picked these teams to win the game in these six
rounds are shown from outer to inner of circles.\n\n\n\n", 80)

p <- ggplot(public_data) +
  geom_circle(aes(x0 = 2 * x, y0 = 3, r = r64/100), fill = "#341204") +
  geom_circle(aes(x0 = 2 * x, y0 = 3, r = r32/100), fill = "#9e370d") +
  geom_circle(aes(x0 = 2 * x, y0 = 3, r = s16/100), fill = "#cb4610") +
  geom_circle(aes(x0 = 2 * x, y0 = 3, r = e8/100), fill = "#ec5b20") +
  geom_circle(aes(x0 = 2 * x, y0 = 3, r = f4/100), fill = "#f2936b") +
  geom_circle(aes(x0 = 2 * x, y0 = 3, r = finals/100), fill = "#f6ae91") +
  geom_text(aes(x = 2 * x, y = 4.2, label = team), color = "#ee6730",
            family = "graduate", size = 6) +
  geom_rect(legend_data, mapping = aes(xmin = x - 0.25, xmax = x + 0.25,
                                      ymin = y - 0.2, ymax = y + 0.2,
            fill = color)) +
  geom_text(legend_data, mapping = aes(x = x, y = 5.4, label = label),
            family = "graduate", size = 5) +
  scale_fill_identity() +
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  labs(title = "NCAA Men's March Madness",
       subtitle = subtitle_text,
       caption = "Data Source: Kaggle\nTidyTuesday 2024 - Week 13 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "graduate", hjust = 0,
                                  size = 50),
        plot.subtitle = element_text(family = "graduate", hjust = 0,
                                  size = 22),
        plot.caption = element_text(family = "graduate", hjust = 1,
                                  size = 17),
        plot.margin = margin(-3, 1, -5, 1, "cm"))


# Save the Plot

ggsave("Week13.png", p, width = 20, height = 14, dpi = 72)





  