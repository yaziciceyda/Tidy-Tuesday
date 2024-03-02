library(tidyverse)
library(janitor)
library(cowplot)
require(ggrepel)
library(showtext)
library(ggtext)
library(geomtextpath)

# Font in the Plot

font_add_google('Dancing Script', 'ds')
showtext_auto()

# Fontawesome Library

font_add('fa-solid', 'Font Awesome 6 Free-Solid-900.otf')

# Data Import

historicshowtexthistorical_spending <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/historical_spending.csv')
gifts_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_age.csv')
gifts_gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_gender.csv')

# Data Wrangling for gifts data

gifts_data <- gifts_age %>%
  clean_names() %>%
  select(-spending_celebrating) %>%
  pivot_longer(-age, names_to = "gift", values_to = "percent") %>%
  mutate(gift = str_replace_all(gift, "_", " "),
         label_gift = "<span style='font-family:fa-solid'>&#xf004;</span>",
         color_gift = case_when(
           gift == "candy" ~ "#C00000",
           gift == "flowers" ~ "#FF3334",
           gift == "evening out" ~ "#FF6F77",
           gift == "jewelry" ~ "#FF8896",
           gift == "clothing" ~ "#CB757C",
           gift == "gift cards" ~ "#D35A63",
           gift == "greeting cards" ~ "#d54d4d",
         ))

subtitle_text <- str_wrap("Candy is the most popular gift for all age groups in
terms of average percentage of spending. 
Moreover, flowers, jewelry, evening out and clothing have a decreasing 
trend as people get older.", 120)

# Plot of gifts data

p_age <- ggplot(data = gifts_data, aes(x = age, y = percent, 
                                       group = gift)) +
  geom_line(aes(color = color_gift), size = 2) +
  # Hearts
  geom_richtext(aes(label = label_gift,
                  colour = color_gift),
                size = 15,
                family = 'fontawesome-webfont',
                fill = NA, label.colour = NA) + 
  # Percentages inside the heart
  geom_text(aes(label = percent), size = 7, family = "ds",  
            fontface = "bold") +
  # The gift name 
  geom_text_repel(data = gifts_data %>%
              filter(age == "65+"),
            mapping = aes(x = 7, y = percent, label = toupper(gift),
                          color = color_gift), size = 6, family = "ds",
            fontface = "bold", hjust = 0) +
  scale_colour_identity() +
  coord_cartesian() +
  labs(x = "Age",
       y = "Percentage",
       title = "Valentine's Day Average Spending\n",
     #  subtitle = subtitle_text,
       caption = "Data Source: Valentine's Days consumer survey data\nTidyTuesday 2024 - Week 7\nPrepared by: C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_text(family = "ds", size = 24),
        axis.text = element_text(family = "ds", size = 18),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0, family = "ds",
                                  size = 40),
        plot.caption = element_text(hjust = 1, family = "ds",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0, family = "ds",
                                  size = 30),
        plot.title.position = "plot",
        plot.margin = margin(0.9, 1.0, 0.9, 1.0, "cm"))


# Data Wrangling for historical data

historical_data <- historical_spending %>%
  clean_names() %>%
  select(-c(percent_celebrating, per_person)) %>%
  pivot_longer(-year, names_to = "gift", values_to = "avg_amount") %>%
  mutate(gift = str_replace_all(gift, "_", " "),
         color_gift = case_when(
           gift == "candy" ~ "#C00000",
           gift == "flowers" ~ "#FF3334",
           gift == "evening out" ~ "#FF6F77",
           gift == "jewelry" ~ "#FF8896",
           gift == "clothing" ~ "#CB757C",
           gift == "gift cards" ~ "#D35A63",
           gift == "greeting cards" ~ "#d54d4d"
         ))

# Plot for historical data

p_historical <- ggplot(historical_data) +
  geom_line(aes(x = year, y = avg_amount, colour = color_gift, 
                group = gift)) +
  geom_textline(aes(x = year, y = avg_amount, label = toupper(gift), 
                    colour = color_gift), linewidth = 2, family = "ds",
                fontface = "bold", size = 8, hjust = 0.3) +
  scale_colour_identity() +
  scale_x_continuous(breaks = seq(2010, 2022), expand = c(0, 0)) +
  labs(x = "", 
       y = "Average Amount Spending",
       title = "Average Amount Spending on Gifts",
       caption = "Data Source: Valentine's Days consumer survey data\nTidyTuesday 2024 - Week 7\nPrepared by: C. YAZICI") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(family = "ds", size = 28),
        axis.text = element_text(family = "ds", size = 18),
        plot.title = element_text(family = "ds", hjust = 0,
                                  size = 40),
        plot.title.position = "plot",
        plot.caption = element_text(family = "ds", hjust = 1,
                                  size = 20),
        plot.margin = margin(0.9, 2, 0.9, 2, "cm"))

# Save the Plots

ggsave("Week7.png", p_age, width = 23, height = 15, dpi = 72)

ggsave("Week7_v2.png", p_historical, width = 23, height = 15, dpi = 72)
