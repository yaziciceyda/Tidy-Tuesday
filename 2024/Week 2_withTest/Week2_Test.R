library(tidyverse)
library(scales)
library(showtext)


# Font in the Plot

font_add_google(name = "Salsa",
                family = "salsa")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2024, week = 2)
canada_births_1991_2022 <- tuesdata$canada_births_1991_2022
nhl_player_births <- tuesdata$nhl_player_births

# Data Wrangling for players

data_player <- nhl_player_births %>%
  filter(birth_year > 1990) %>%
  group_by(birth_year, birth_month) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(birth_year) %>%
  mutate(total = sum(n),
         percentage = round(n / total * 100)) %>%
  ungroup() %>%
  group_by(birth_month) %>%
  summarise(avg_monthly_births = round(mean(percentage))) %>%
  ungroup() %>%
  rename(month = birth_month) %>%
  mutate(group = "players") %>%
  arrange(month)

# Data Wrangling for Canadians

data_canada <- canada_births_1991_2022 %>%
  filter(year > 2006) %>%
  group_by(year) %>%
  mutate(total = sum(births),
         percentage = round(births / total * 100)) %>%
  ungroup() %>%
  group_by(month) %>%
  summarise(avg_monthly_births_canada = round(mean(percentage))) %>%
  ungroup() %>%
  mutate(group = "canada") %>%
  arrange(month) %>%
  right_join(data_player, by = "month") %>%
  mutate(difference = avg_monthly_births_canada - avg_monthly_births)
  
# Mann-Whitney Test

wilcox.test(data_canada$avg_monthly_births_canada, 
            data_canada$avg_monthly_births)

# 	Wilcoxon rank sum test with continuity correction

# data:  data_canada$avg_monthly_births_canada and 
# data_canada$avg_monthly_births
# W = 67.5, p-value = 0.8122
# alternative hypothesis: true location shift is not equal to 0

# Subtitle of the plot

subtitle_text <- str_wrap("The average percentage of births for 
Canadian hockey players are higher than the Canadian people in the 
first five months of 1991 - 2005. Since the percentage of births for 
Canadian people is around 8 throughout the year, it is 1 - 2 points 
higher than the perventages of players for the last five months of 
the year. However, the test shows that this difference is not 
statistically significant.", 90)

# The plot

p <- ggplot(data_canada) +
  geom_segment(aes(y = avg_monthly_births, 
                   yend = avg_monthly_births_canada,
                   x = month, xend = month),
               linetype = "dashed", linewidth = 0.8) +
  geom_line(aes(x = month, y = avg_monthly_births), color = "red",
            linewidth = 1.5) +
  geom_point(aes(x = month, y = avg_monthly_births), size = 28,
             color = "red") +
  geom_text(aes(x = month, y = avg_monthly_births, 
                label = avg_monthly_births), size = 12) +
  geom_line(aes(x = month, y = avg_monthly_births_canada), 
            color = "blue", linewidth = 1.5) +
  geom_point(aes(x = month, y = avg_monthly_births_canada), size = 28,
             color = "blue") +
  geom_text(aes(x = month, y = avg_monthly_births_canada, 
                label = avg_monthly_births_canada), size = 12) + 
  annotate("text", x = 13, y = 6.05, label = "Players", 
           fontface = "italic", color = "red", family = "salsa",
           size = 10) +
  annotate("text", x = 13, y = 8, label = "Canada", 
           fontface = "italic", color = "blue", family = "salsa",
           size = 10) +
  scale_x_continuous(breaks = seq_along(month.name),
                     labels = month.abb) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "",
       y = "Average Percentage of Births",
       title = toupper("Canadian Hockey Player Birth Months"),
       subtitle = subtitle_text,
       caption = "Data Source: Statistics Canada | TidyTuesday 2024 - Week 2 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 18),
        axis.title.y = element_text(family = "salsa", size = 20),
        plot.caption = element_text(family = "salsa", size = 20, 
                                    hjust = 1),
        plot.title = element_text(family = "salsa", size = 40),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "salsa", size = 25,
                                     hjust = 0),
        plot.margin = margin(1, 1, 1, 1, "cm"))


# Save the Plot

ggsave("Week2_test.png", p, width = 21, height = 15, dpi = 72)

