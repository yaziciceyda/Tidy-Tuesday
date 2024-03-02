library(tidyverse)
library(ggchicklet)
library(showtext)

# Font in the Plot

font_add_google('Comic Neue', 'cn')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2024, week = 9)

events <- tuesdata$events
births <- tuesdata$births
deaths <- tuesdata$deaths


# Data Wrangling

births_data <- births %>%
  mutate(category = case_when(
    str_detect(description, "player") |
      str_detect(description, "swimmer") |
      str_detect(description, "footballer") |
      str_detect(description, "sprint") |
      str_detect(description, "cricketer") |
      str_detect(description, "sailor") |
      str_detect(description, "cyclist") ~ "sports",
    str_detect(description, "author")|
      str_detect(description, "writer") ~ "author/writer",
    str_detect(description, "actor") | 
      str_detect(description, "actress")   ~ "actor/actress",
    str_detect(description, "singer") ~ "singer",
    str_detect(description, "politician") |
      str_detect(description, "Minister") |
      str_detect(description, "Duke") ~ "politician",
    str_detect(description, "cartoon") |
      str_detect(description, "composer") |
      str_detect(description, "dancer") |
      str_detect(description, "sculptor") |
      str_detect(description, "illustrator") ~ "artist",
    str_detect(description, "educator") ~ "educator",
    str_detect(description, "academic") |
      str_detect(description, "mathematician") |
      str_detect(description, "statistician") ~ "academician",
    str_detect(description, "bishop") |
      str_detect(description, "priest") |
      str_detect(description, "religious") |
      str_detect(description, "theologian ")  ~ "religion",
    str_detect(description, "lawyer") ~ "lawyer",
  )) %>%
  group_by(category) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(!is.na(category)) %>%
  arrange(desc(n)) %>%
  mutate(y = rep(1:5, each = 2),
         x = rep(c(1, 2.2), times = 5),
         scale_n = n,
         scale_n = ifelse(n < 9,  7, scale_n))


# The Plot

p <- ggplot(births_data) +
  geom_text(aes(x = x, y = y, label = paste0(n, " ", category),
                                             size = scale_n),
            family = "cn", fontface = "bold", color = "chocolate4") +
  # Calendar
  ggchicklet:::geom_rrect(aes(xmin = 0, xmax = 0.5, 
                              ymin = 0, ymax = -2), 
                          fill = "ivory",
                          radius = unit(0.5, units = "cm")) +
  geom_rect(aes(xmin = 0, xmax = 0.5,
                ymin = -2, ymax = -1.5), size = 0.2, fill = "red") +
  geom_text(aes(x = 0.25, y = -1.7, label = "FEBRUARY"), hjust = 0.5,
            family = "cn", size = 10) +
  geom_text(aes(x = 0.25, y = -0.8, label = "29"), hjust = 0.5,
            family = "cn", size = 16) +
  geom_text(aes(x = 1.7, y = -1.2, 
                label = "JOBS OF LEAPERS"),
            family = "cn", fontface = "bold", size = 28) +
  scale_size_identity() +
  labs(caption = "Data Source: Wikipedia | TidyTuesday 2024 - Week 9 | Prepared by: C. YAZICI") +
  scale_x_continuous(limits = c(0, 3)) +
  scale_y_reverse(limits = c(6, -2)) +
  theme(plot.background = element_rect(fill = "#D2B48C", color = NA),
        panel.background = element_rect(fill = "#D2B48C", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(family = "cn", hjust = 1, 
                                    size = 24),
        plot.margin = margin(0.5, 1, 0.5, 1, "cm"))

# Save the Plot

ggsave("Week9.png", p, width = 22, height = 15, dpi = 72)



         