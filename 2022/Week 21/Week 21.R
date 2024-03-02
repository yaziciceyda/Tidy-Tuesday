tuesdata <- tidytuesdayR::tt_load(2022, week = 21)
sevens <- tuesdata$sevens
fifteens <- tuesdata$fifteens

library(showtext)
library(tidyverse)

# Font in the Plot

font_add_google('Rubik', 'rubik')
showtext_auto()

# The teams which played more than50 matches

teams <- fifteens %>%
  count(team_2) %>% 
  filter(n > 50) %>%
  select(team_2) %>%
  rename(team_list = team_2)

# Data Wrangling

fifteens_data <-  fifteens %>%
  filter(team_1 %in% teams$team_list,
         team_2 %in% teams$team_list) %>%
  select(date, team_1, team_2, score_1, score_2, home_away_win) %>%
  filter(home_away_win %in% c("A", "H")) %>%
  select(-date)
 
team1_data <- fifteens_data %>%
  select(team_1, score_1, home_away_win) %>%
  rename(team = team_1,
         score = score_1)

team2_data <- fifteens_data %>%
  select(team_2, score_2, home_away_win) %>%
  rename(team = team_2,
         score = score_2)

final_data <- team1_data %>%
  rbind(team2_data) %>%
  group_by(home_away_win, team) %>%
  summarise(m_score = mean(score)) %>%
  mutate(m_score = ifelse(home_away_win == "A", -1 * m_score, m_score),
         team = as.factor(team),
         team = fct_reorder(team, -m_score),
         color = ifelse(home_away_win == "A", "#108D1F", "#C7AF15")) %>%
  ungroup() 

# SUbtitle in the Plot

subtitle_text <- str_wrap("The average scores of the teams which played more than 50 matches ", 100)

# The Plot

p <- ggplot(final_data) +
  geom_point(aes(x = m_score, y = team, color = color), size = 6) +
  geom_text(aes(x = 0, y = team, label = team),
            color = "grey40", vjust = 0, size = 8, family = "rubik") +
  geom_segment(aes(x = 0, xend = m_score,
                   y = team, yend = team)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(x = -23, y = 5, label = "Average Scores\nin Away",
                color = "#108D1F", family = "rubik", size = 30)) +
  geom_text(aes(x = 23, y = 5, label = "Average Scores\nin Home",
                color = "#C7AF15", family = "rubik", size = 30)) +
  scale_color_identity() +
  scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30)) +
  labs(title = "WOMEN'S RUGBY",
       subtitle = subtitle_text,
       caption = "Data Source: Women's Rugby - ScrumQueens | TidyTuesday 2022 - Week 21 | Prepared by: @Cyd_yzc") +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#D3E8EC", color = NA),
        plot.background = element_rect(fill = "#D3E8EC", color = NA),
        plot.title = element_text(size = 30, hjust = 0, family = "rubik"),
        plot.subtitle = element_text(size = 25, hjust = 0, family = "rubik"),
        plot.caption = element_text(size = 15, hjust = 1, family = "rubik"),
        axis.text.x = element_text(size = 13, hjust = 0.5, family = "rubik"),
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Save the Plot

ggsave("Week21_2022_v2.png", p, width = 23, height = 15, dpi = 72)
  
