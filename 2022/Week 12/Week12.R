# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-03-22')
tuesdata <- tidytuesdayR::tt_load(2022, week = 12)

babynames <- tuesdata$babynames
str(babynames)

library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(sysfonts)
library(showtext)


prop_summary <- babynames %>%
  summarise(min = min(prop),
            mean = mean(prop),
            max = max(prop),
            q20 = quantile(prop, 0.20)) 

  babynames.data <- babynames %>%
  mutate(num_letter = str_length(name)) %>%
  group_by(year, sex, num_letter) %>%
  summarise(sum_prop = sum(prop),
              sum = sum(n)) %>%
  ungroup() %>%
  mutate(  decade  = case_when(
           year >= 1880 & year < 1890 ~ "1880s",
           year >= 1890 & year < 1900 ~ "1890s",
           year >= 1900 & year < 1910 ~ "1900s",
           year >= 1910 & year < 1920 ~ "1910s",
           year >= 1920 & year < 1930 ~ "1920s",
           year >= 1930 & year < 1940 ~ "1930s",
           year >= 1940 & year < 1950 ~ "1940s",
           year >= 1950 & year < 1960 ~ "1950s",
           year >= 1960 & year < 1970 ~ "1960s",
           year >= 1970 & year < 1980 ~ "1970s",
           year >= 1980 & year < 1990 ~ "1980s",
           year >= 1990 & year < 2000 ~ "1990s",
           year >= 2000 & year < 2010 ~ "2000s",
           year >= 2010 & year < 2020 ~ "2010s"
         ),
         color = case_when(
           sex == "F" ~ "#fcafac",
           sex == "M" ~ "#aceefc"
         ) 
  ) 

  sdata <- babynames.data %>%
  filter(decade == "1980s")
  

  font_add_google("DM Serif Display", "sd")
  showtext_auto()
  

p <- ggplot(babynames.data) +
  geom_density(aes(x = num_letter, color = color), lwd = 1)  +
  facet_wrap(~decade, nrow = 4) +
  coord_cartesian() +
  scale_linetype_manual(values = c("#fcafac","#aceefc")) +
  labs(x = "",
       y = "",
       title =  "The distribution of the length of babynames",
       subtitle = "The distribution of the number of letters are always similar for girls and boys but the longer names become more popular after 1970s.",
       caption = "#TidyTuesday Week 12 - 2022\n Source: Data from {babynames} R package\n Prepared by: @Cyd_yzc") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.background = element_rect("#B6F8A4"),
        panel.background = element_rect("#B6F8A4"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill="#F9F5A4"),
        strip.text.x = element_text(family = "sd", size = 22),
        strip.text = element_text(family = "sd"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(family = "sd", size = 25),
        plot.title = element_text(family = "sd", hjust = 0.5, 
                                  margin = margin(0.5, 1.5, 0.5, 0, "cm"), size = 35),
        plot.subtitle = element_text(family = "sd", hjust = 0, size = 25),
        plot.caption = element_text(family = "sd", hjust = 1, size = 17)) 

p 
ggsave("Week12_2022.png", p)


