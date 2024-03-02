library(tidyverse)
library(ggchicklet)
library(showtext)

# Font in the Plot

font_add_google('Oswald', 'oswald')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2020, week = 24)
firsts <- tuesdata$firsts
science <- tuesdata$science


tuesdata <- tidytuesdayR::tt_load(2020, week = 25)
blackpast <- tuesdata$blackpast
census <- tuesdata$census
slave_routes <- tuesdata$slave_routes
african_names <- tuesdata$african_names

# Data Wrangling

firsts_data <- firsts %>%
  filter(str_detect(accomplishment, "win|won")) %>%
  separate(accomplishment, into = c("newCol1", "newCol2"),
           sep = "win") %>%
  separate(person, into = c("name", "detail"),
           sep = "\\(") %>%
separate(name, into = c("name", "detail2"),
         sep = "\\[") %>%
separate(name, into = c("name", "detail3"),
         sep = "\\,") %>%
  select(year, newCol1, newCol2, name) %>%
  mutate(medal = ifelse(str_detect(newCol2, "medal") | 
                        str_detect(newCol1, "medal"), 1, 0),
         award = ifelse(str_detect(newCol2, "Award") | 
                          str_detect(newCol1, "Award") |
                          str_detect(newCol2, "award") | 
                          str_detect(newCol1, "award"), 1, 0),
         prize = ifelse(str_detect(newCol2, "Prize") | 
                          str_detect(newCol1, "Prize"), 1, 0),
         champ = ifelse(str_detect(newCol2, "championship") | 
                          str_detect(newCol1, "championship") | 
                          str_detect(newCol2, "Championship") | 
                          str_detect(newCol1, "Championship"), 1, 0),
         oscar = ifelse(str_detect(newCol2, "Oscar") | 
                         str_detect(newCol1, "Oscar"), 1, 0),) %>%
  mutate(sum = medal + award + prize + champ + oscar) %>%
  filter(sum > 0) %>%
  mutate(color = case_when(
      medal == 1 ~ "#F1815D",
      award == 1 ~ "#479312",
      prize == 1 ~ "#2F88AC",
      oscar == 1 ~ "#312FAC",
      champ == 1 ~ "#AC2FAA",
  ),
  V1 = c(1, rep(c(-1, 1), times = 14))) 

firsts_data$name[29] <- "Hannah Beachler"
firsts_data$name[28] <- "Ruth E. Carter"

# Legend Data

legend_data <- tibble(x = rep(0.75, 5),
                         y = seq(1921, 1941, by = 5),
                      type = c("Medal", "Award", "Prize",
                               "Oscar", "Championship"),
                      color = c("#F1815D", "#479312", "#2F88AC",
                                "#312FAC", "#AC2FAA"))
# The Plot

p <- ggplot(firsts_data) +
  geom_rect(aes(ymin = 1900, ymax = 2025,
                xmin = -1, xmax = 1),
            fill = "#F6A376",
            colour = NA,
            size = 0.5)  +
  geom_segment(aes(x = 0, xend = 0, y = 1940, yend = 2025),
               colour = "grey80", size = 2) +
  geom_point(mapping = aes(x = 0, y = year, color = color), size = 3)+
  geom_text_repel(mapping = aes(x = -0.5 * V1,
                          y = year , label = name,
                          family = "oswald"), size = 5, hjust = 0.5) +
  geom_text_repel(mapping = aes(x = -0.1 * V1,
                                y = year , label = year,
                                family = "oswald"), size = 5, hjust = 0) +
  # Legend
  geom_point(data = legend_data, aes(x = x, y = y, color = color), size = 3) +
  geom_text(data = legend_data, aes(x = x + 0.1, y = y, label = type,
                                    family = "oswald", size = 5)) +
  ggchicklet:::geom_rrect(aes(xmin = 0.72, xmax = 0.92, 
                              ymin = 1918, ymax = 1945), 
                          fill = "transparent", colour = "black",
                          radius = unit(0.2, units = "cm")) + 
  scale_color_identity() +
  labs(title = "Juneteenth",
       subtitle = "First African-American who win a medal, oscar, award,prize or championship.",
       caption = "Data Source: Tidy Tuesday - 2020-06-09 | TidyTuesday 2022 - Week 25 | Prepared by: @Cyd_yzc") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#F6A376", color = NA),
        plot.background = element_rect(fill = "#F6A376", color = NA),
        legend.position = "none",
        plot.title = element_text(family = "oswald", size = 40, hjust = 0),
        plot.subtitle = element_text(family = "oswald", size = 20, hjust = 0.5),
        plot.caption = element_text(family = "oswald", size = 15, hjust = 1),
        plot.margin = unit(c(0.5, 2.0, 0.5, 2.0), "cm"))

# Save the Plot

ggsave("Week25_2022.png", p, width = 23, height = 15, dpi = 72)


  

