image_alt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/image_alt.csv')
color_contrast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/color_contrast.csv')
ally_scores <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/ally_scores.csv')
bytes_total <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/bytes_total.csv')
speed_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/speed_index.csv')

library(tidyverse)
library(lubridate)
library(gghighlight)
library(ggmap)
library(showtext)

speed_mobile <- speed_index %>%
  select(date, p50, timestamp, client) %>%
  filter(client == "mobile") %>%
    mutate(date = ymd(date)) %>%
  rename(p50_mobile = p50)

speed_desktop <- speed_index %>%
  select(date, p50, timestamp, client) %>%
  filter(client == "desktop") %>%
  mutate(date = ymd(date)) %>%
  rename(p50_desktop = p50)

diff_data <- speed_desktop %>%
  left_join(speed_mobile, by = c("timestamp", "date")) %>%
  mutate(diff = p50_mobile - p50_desktop,
         date = ymd(date)) 

# Bytes Data

bytes_mobile <- bytes_total %>%
  select(date, p50, timestamp, client) %>%
  filter(client == "mobile") %>%
  mutate(date = ymd(date)) %>%
  rename(p50_mobile = p50)

bytes_desktop <- bytes_total %>%
  select(date, p50, timestamp, client) %>%
  filter(client == "desktop") %>%
  mutate(date = ymd(date)) %>%
  rename(p50_desktop = p50)

diff_data_bytes <- bytes_desktop %>%
  left_join(bytes_mobile, by = c("timestamp", "date")) %>%
  mutate(diff = p50_mobile - p50_desktop,
         date = ymd(date)) 

font_add_google("DM Serif Display", "sd")
showtext_auto()

p <- ggplot(diff_data, aes(x = date, y = diff)) +
  geom_line(colour = "red", size = 2) +
  gghighlight(diff < 1.96,
              unhighlighted_params = list(linewidth = 2, 
              colour = alpha("black", 0.4))) +
  labs(y = "mobile speed - desktop speed",
       title = "\nThe Difference between Mobile & Desktop Speed\n",
       subtitle = "The speed of mobile and desktop are closest in the period highlighted. However, the plot in the bottom which shows the difference\nin the total bytes for the same period indicates that desktops are significantly higher than the mobiles.",
       caption = "Data Source: httpArchive.org | TidyTuesday 2022 - Week 46 | Prepared by: @Cyd_yzc") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#FFFFF0"),
        plot.title = element_text(size = 30, family ="sd"),
        plot.subtitle = element_text(size = 20, family ="sd"),
        plot.caption = element_text(size = 13, family ="sd"),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 15, family ="sd"),
        axis.text = element_text(size = 13, family ="sd"))
 

p2 <- ggplot(diff_data_bytes %>%
               filter(between(date, as.Date('2016-01-01'), 
                              as.Date('2022-10-01'))), 
                      aes(x = date, y = diff)) +
  geom_line(colour = "red", size = 2) +
    gghighlight(between(date, as.Date('2016-10-15'), 
                        as.Date('2017-04-01')),
                unhighlighted_params = list(linewidth = 2, 
                                            colour = alpha("black", 0.4)))  +
  labs(y = "mobile bytes - desktop bytes",
       title = "Total Bytes") +
  theme(plot.title = element_text(size = 20, family ="sd"),
        axis.ticks = element_blank(),
         axis.title = element_text(size = 13, family ="sd"),
         axis.text = element_text(size = 11, family ="sd"))

final_plot <- p +
  inset_element(p2, 0.3, 0.05, 0.8, 0.5) &
  theme(
    plot.background = element_rect(fill = "#FFFFF0", color = NA),
  )
ggsave("Week46_2022.png", final_plot, width = 25, height = 12, dpi = 72)


