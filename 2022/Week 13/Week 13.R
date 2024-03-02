# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-03-29')
tuesdata <- tidytuesdayR::tt_load(2022, week = 13)


library(forcats)
library(tidyverse)
library(dplyr)
library(showtext)

sports_new <- tuesdata$sports %>%
  select(sports, year, rev_men, rev_women, exp_men, exp_women) %>%
  filter(year %in% c(2015, 2019))%>%
  group_by(sports, year) %>%
  summarise(avg_rev_men = mean(rev_men, na.rm=TRUE),
            avg_rev_women = mean(rev_women, na.rm=TRUE),
            avg_exp_men = mean(exp_men, na.rm=TRUE),
            avg_exp_women = mean(exp_women, na.rm=TRUE)) %>%
  mutate(avg_diff_rev = avg_rev_men - avg_rev_women,
         avg_diff_exp = avg_exp_men - avg_exp_women) %>%
  filter(sports != "Team Handball") 



sports_new_final <- sports_new %>%
  mutate(year = factor(year)) %>%
  filter(!sports %in% c("Synchronized Swimming",
                     "Softball",
                     "Football",
                     "Field Hockey",
                     "Equestrian",
                     "Baseball",
                     "Badminton",
                     "Ice Hockey",
                     "Gymnastics",
                     "Basketball"))

paired <- rep(1:(54/2), each = 2)

sports_new_final <- sports_new_final %>%
cbind(paired = paired) 

# Difference in Revenue

font_add_google("DM Serif Display", "sd")
showtext_auto()

p <- ggplot(sports_new_final, aes(x = avg_diff_exp, y = reorder(sports, abs(avg_diff_exp)), color = "red")) +
  geom_path(aes(x = avg_diff_exp, reorder(sports, abs(avg_diff_exp))),
  arrow = arrow(length = unit(1.5, "mm"), type = "closed"), size = 1) +
  scale_x_continuous(breaks = c(-300000, -200000, -100000, 0, 100000), 
                     labels = c("-300k", "-200k", "-100k", "0", "100k")) +
  coord_cartesian() +
  geom_line(aes(group = paired)) +
  geom_vline(xintercept = 0, linetype ="dotdash") +
  #geom_point(aes(color = as.factor(year)), size = 3) +
  labs(y = "",
       x = "",
       title = "Change in the Average Expenditure between Men and Women from 2015 to 2019",
       caption = "#TidyTuesday Week 13 - 2022\n Data: Equity in Athletics Data Analysis\n Prepared by: @Cyd_yzc") +
  theme_minimal() +
  theme(plot.margin = margin(0.5, 1, 0.5, 1, "cm"),
    text = element_text(family = "sd"),
        legend.position = "",
        #plot.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "#05ECFD"),
        plot.title = element_text(family = "sd", size = 32, hjust = 0),
        axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
        axis.text.y = element_text(hjust = 0, family = "sd", size = 25),
        axis.text.x = element_text(family = "sd", size = 25),
        plot.caption = element_text(family = "sd", hjust = 1, size = 20)) 
 
  
ggsave("Week13.png", p)


