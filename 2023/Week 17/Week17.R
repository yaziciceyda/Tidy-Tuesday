# Tidy Tuesday - 2023 - Week 17

# Libraries used 

library(tidyverse)
library(geomtextpath)
library(PrettyCols)
library(ggflags)
library(countrycode)
library(bbplot)
library(showtext)

# Font in the Plot

font_add_google("Open Sans", "os")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 17)
winners <- tuesdata$winners
london_marathon <- tuesdata$london_marathon

# Data Preparation 

winners_data <- winners %>%
  arrange(Year) %>%
  mutate(country_code = countrycode(Nationality, 
              origin = 'country.name', destination = 'iso2c')) 

# Data Summary

winners_data_summary <- winners_data %>%
  group_by(Category, Nationality) %>%
  summarise(n = n())
  
# Subtitle in the Plot
  
subtitle_text <- str_wrap("There are four different categories of the marathon
as Women, Men, Wheelchair Women and Wheelchair Men. The winning time which is
decreasing until 1990 especially for Wheelchair Women and Wheelchair Men is
presented with the nationality of the winner. Kenya winners are the most 
frequent winners for Women and Men; while UK winners are the same for Wheelchair
Women and Wheelchair Men.", 110)

# The First Plot

p1 <- ggplot(winners_data) +
  geom_textline(aes(x = Year, y = Time, color = Category,
                    label = Category), linewidth = 3, size = 20, hjust = 1) +
  geom_flag(aes(x = Year, y = Time,
                          country = tolower(country_code)), size = 10) +
  scale_color_pretty_d("Relax") +
  labs(title = "Winners of London Marathon",
       subtitle = subtitle_text,
       caption = "Data Source: {LondonMarathon} | TidyTuesday 2023 - Week 17 | Prepared by: C. YAZICI") +
  bbc_style() +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 1, family = "os", size = 20),
        plot.margin = unit(c(1, 2, 1, 2), "cm"))

# Save the Plot

ggsave("Week17.png", p1, width = 25, height = 15, dpi = 72)


# Data Preparation

london_marathon_data <- london_marathon %>%
  group_by(Year) %>%
  mutate(accepted_percent = round(Accepted / Applicants * 100),
         finished_percent = round(Finishers / Starters  * 100),
         gap = finished_percent - accepted_percent) %>%
  ungroup() %>%
  arrange(Year) 

# Data Summary

london_marathon_summary <- london_marathon_data %>%
  summary(avg_acc = mean(accepted_percent),
          avg_fin = mean(finished_percent))

# Subtitle in the Second Plot

subtitle_text2 <- str_wrap("The percentage of people who are accepted and finished the 
marathon are given for each year. The average acceptance rate is 36% which
reached its maximum value in 1996 (with 58%). On the other hand, the average 
percentage of finishers is 96% among the people who started the race. Since the 
number of accepted people is low in 2020 probably due to the Coronavirus Outbreak, 
both of the percentages are lower than the average.", 110)
  
# The Second Plot

p2 <- ggplot(london_marathon_data, aes(x = accepted_percent, 
                                 xend = finished_percent, 
                                 y = Year, 
                        group = Year)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 4,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  # Accepted
  geom_point(aes(x = 25, y = 2022), color = "#E7A911", size = 5) +
  geom_text(aes(x = 30, y = 2022, label = "Accepted"), 
            size = 7, family = "os") +
  # Finished
  geom_point(aes(x = 75, y = 2022), color = "#26854C", size = 5) +
  geom_text(aes(x = 80, y = 2022, label = "Finished"), 
            size = 7, family = "os") +
  coord_cartesian() +
  labs(title = "London Marathon",
       subtitle = subtitle_text2,
       caption = "Data Source: {LondonMarathon} | TidyTuesday 2023 - Week 17 | Prepared by: C. YAZICI") +
  bbc_style() +
  theme(plot.caption = element_text(hjust = 1, family = "os", size = 20),
        plot.margin = unit(c(1, 2, 1, 2), "cm"))


# Save the Plot

ggsave("Week17_v2.png", p2, width = 25, height = 15, dpi = 72)

