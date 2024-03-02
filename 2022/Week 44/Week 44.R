tuesdata <- tidytuesdayR::tt_load(2022, week = 44)
horror_movies <- tuesdata$horror_movies

library(lubridate)
library(ggplot2)
library(showtext)
library(ggstar)
library(ggtext)

movie_2022 <- horror_movies %>%
  mutate(year = year(release_date)) %>%
  filter(year == 2022) %>%
  arrange(desc(popularity)) %>%
  top_n(1, popularity)

movies <- horror_movies %>%
  select(title, release_date, popularity, vote_average,
         budget, revenue) %>%
  mutate(year = year(release_date)) %>%
  group_by(year) %>%
  top_n(1, popularity) %>%
  arrange(desc(year)) %>%
  filter(year != 2022) %>%
  mutate(colour = ifelse(vote_average > 7.0, "#880808", "#f21a1a"),
         shape = ifelse(vote_average > 7.0, "Above 7 pts", "Below 7 pts"))

font_add_google('Creepster', 'cr')

subtitle_text <- " <span style='color:#880808'>23 films have average vote above 7 pts and
<span style='color:#f21a1a'> 49 films have average vote below 7 pts."

title_text <- "<span style='color:#f21a1a'>The Most Popular Horror Movies"

p <- ggplot(movies, aes(x = year, y = popularity)) + 
  geom_star(mapping = aes(starshape = shape, color = colour,
                          fill = colour), size = 20) +
  scale_starshape_manual(values = c(26, 26)) +
  scale_fill_identity() +
  scale_colour_identity() +
  labs(caption = "Data Source: The Movie Database | TidyTuesday 2022 - Week 44 | Prepared by: @Cyd_yzc") +
  ggtitle(title_text, subtitle_text) +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(colour = "black", fill = "black"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "cr", colour = "#f21a1a", size = 20),
        plot.title = element_markdown(size = 55, family = "cr"),
        plot.subtitle = element_markdown(size = 25, family = "cr"),
        plot.caption = element_markdown(size = 18, family = "cr", colour = "#f21a1a"),
        legend.position = "none"
        ) +
  # Dahmer
  annotate(
    geom = "curve", x = 1994, xend = 2001, y = 388, yend = 380, 
    curvature = .7, arrow = arrow(length = unit(4.5, "mm")), colour = "#f21a1a",
    size = 4
  ) +
  annotate(geom = "text", x = 1993, y = 400, label = "Dahmer", hjust = "left",
           colour = "#f21a1a", family = "cr", size = 8) +
  # Visitor
annotate(
  geom = "curve", x = 2016, xend = 2020, y = 382, yend = 378, 
  curvature = .7, arrow = arrow(length = unit(4.5, "mm")), colour = "#f21a1a",
  size = 4
) +
  annotate(geom = "text", x = 2015, y = 393, label = "Visitor", hjust = "left",
           colour = "#f21a1a", family = "cr", size = 8)

ggsave("Week44_2022.png", p, width = 20, height = 10, dpi = 72)




