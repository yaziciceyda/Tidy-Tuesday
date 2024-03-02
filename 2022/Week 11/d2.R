library(tidyverse)
library(dplyr)
library(sjmisc)
library(lubridate)
library(scales)
library(ggplot2)
library(colorspace)
library(gganimate)
library(extrafont)

cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

Sys.setlocale(locale = "English")

new_data <- cran %>%
  filter(str_detect(cran$package, "clim")) %>%
  # The following code is taken from @tashapiro to tidy the date variable
  mutate(date = case_when(grepl("^[[:digit:]]",(substr(date,4,nchar(date)))) ~ as.Date(date), 
                          TRUE ~ as.Date(substr(date,5,nchar(date)), '%b %d %H:%M:%S %Y')),
  #create columns with date partitioned by year
  year = as.numeric(format(date,'%Y'))) %>%
  filter(package != "climbeR") %>%
  drop_na() %>%
  mutate(date_year = year(date),
         date_month =  month(date, label = T)) %>%
  group_by(date_month)


p <- ggplot(new_data, aes(x = date_month, y = package, color = date_month)) +
  geom_point(size = 2) +
  geom_jitter(alpha = 0.3) +
  scale_fill_continuous_sequential(palette = "Blues") +
  scale_y_discrete(limits = rev) +
  coord_cartesian(expand = F) +
  labs(x = NULL,
       y = NULL,
       title = expression(paste("Number of updates of the packages with ", italic("clim"))),
       caption = "#TidyTuesday Week 11 - 2022\n
       Source:  Robert Flight (https://github.com/rmflight/vignette_analysis)\n
       prepared by: @Cyd_yzc") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    text = element_text(family = "Comic Sans MS", face = "bold"),
    axis.ticks.x = element_blank(),
        plot.background = element_rect("black"),
        axis.ticks.y = element_blank(),
        plot.title = element_text(colour = "#BDAEA8",  size = 14, hjust = 0,
                                  margin = margin(0.5, 1.5, 0.5, 0, "cm")),
        axis.text.y = element_text(family = "Comic Sans MS", size = 12, color = "#BDAEA8", hjust = 0),
        axis.text.x = element_text(family = "Comic Sans MS", color = "#BDAEA8", size = 12),
        panel.background = element_rect("black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Comic Sans MS", color = "#BDAEA8", face = "italic")) +
  transition_reveal(as.integer(date_month)) 

animate(p, res = 150)
anim_save("TidyTuesday_Week11_2022.gif", p)
        

