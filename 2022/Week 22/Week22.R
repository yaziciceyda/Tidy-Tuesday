tuesdata <- tidytuesdayR::tt_load(2022, week = 22)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggbump)
library(ggimg)
library(magick)
library(showtext)

poll <- tuesdata$poll
reputation <- tuesdata$reputation


tech_comp <- poll %>%
  mutate(year = 2022) %>% 
  distinct(company, industry, year, rank = `2022_rank`, rq = `2022_rq`) %>% 
  bind_rows(select(poll, company, industry, year, rank, rq)) %>%
  arrange(company, year) %>% 
  group_by(company) %>% 
  mutate(change = rank - lag(rank, default = NA)) %>% 
  ungroup() %>% 
  filter(industry == "Tech",
         !company %in% c("Sony",
                         "LG Corporation",
                         "Dell",
                         "Robinhood",
                         "Electronic Arts, Inc.",
                         "TikTok",
                         "Twitter",
                         "Uber")) 

color <- c("black", "#4267B2", "#F4B400", "#006699",
           "#F25022", "#E50914", "#1428a0")

logo <- image_read(c("Apple_logo.svg.png", "Facebook_logo.png",
                     "Google_logo.png","IBM_logo.png",
                     "Microsoft_logo.png", "Netflix_logo.jpg",
                     "Samsung_logo.png"))

tech_comp %>% 
  filter(!is.na(rank))
  
font_add_google("Lato", "gp")
showtext_auto()

  ggplot(tech_comp, aes(year, rank, col = company)) +
  geom_bump(size = 5) +
  geom_point(shape = 21, size = 8, stroke = 1.25, fill = "white") +
  geom_text(aes(label=rank), size = 5) +
  geom_text(data = tech_comp %>% filter(year == 2017),
            aes(label = company, x = year-0.01, y = rank),
            size = 5,
            hjust = 1.2) +
#  geom_text(data = tech_comp %>% filter(year == 2022),
#            aes(label = company, x = year+ 0.01, y = rank),
#            size = 4,
#            hjust = -0.2) +
  geom_vline(xintercept = 2020, linetype = "dotdash") +
  scale_x_continuous(position = "top") +
  scale_y_reverse() +
  coord_cartesian(clip = "off", expand = TRUE) +
  scale_color_manual(values = color) +
     theme(text = element_text(family = "gp"),
       legend.position = "none",
       panel.grid = element_blank(),
       axis.ticks = element_blank(),
       axis.text = element_text(size = 15),
       axis.title.y = element_text(size = 15),
       panel.background = element_rect(fill = "#f4cccc"),
       plot.title = element_text(family = "gp", size = 30, hjust = 0.5,
                                 color = "black"),
       plot.subtitle = element_text(family = "gp", size = 20, hjust = 0.5,
                                    color = "black"),
       plot.caption = element_text(family = "gp", hjust = 1, size = 10)) +
    labs(x = "",
         y = "Rank",
         title = "The Reputation of Tech Companies",
         subtitle = "\nDid Coronavirus Outbreak affect the Rank of the Tech Companies?",
         caption = "Data Source: Axios-Harris Poll | TidyTuesday 2022 - Week 22 |Prepared by: @Cyd_yzc") +
    annotate(
      geom = "curve", x = 2020, y = 80, xend = 2021, yend = 75, 
      curvature = .3, arrow = arrow(length = unit(2, "mm"))
    ) +
    annotate(geom = "text", x = 2021, y = 75, label = "Coronavirus Outbreak",
             hjust = "left", family = "gp", size = 5)

  grid::grid.raster(logo[1], x = 0.96, y = 0.69, just = c('left', 'bottom'),
                    width = unit(0.8, 'cm'), height = unit(0.8, 'cm'))
  grid::grid.raster(logo[2], x = 0.97, y = 0.06, just = c('left', 'bottom'),
                    width = unit(0.8, 'cm'), height = unit(0.8, 'cm'))
  grid::grid.raster(logo[3], x = 0.97, y = 0.59, just = c('left', 'bottom'),
                    width = unit(0.8, 'cm'), height = unit(0.8, 'cm'))
  grid::grid.raster(logo[4], x = 0.96, y = 0.75, just = c('left', 'bottom'),
                    width = unit(1, 'cm'), height = unit(0.8, 'cm')) 
  grid::grid.raster(logo[5], x = 0.96, y = 0.72, just = c('left', 'bottom'),
                    width = unit(1.4, 'cm'), height = unit(0.7, 'cm'))
  grid::grid.raster(logo[6], x = 0.96, y = 0.65, just = c('left', 'bottom'),
                    width = unit(0.8, 'cm'), height = unit(0.8, 'cm'))
  grid::grid.raster(logo[7], x = 0.9, y = 0.79, just = c('left', 'bottom'),
                    width = unit(1.5, 'cm'), height = unit(0.8, 'cm'))  
  