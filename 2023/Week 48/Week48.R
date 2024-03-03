library(datardis)
library(tidyverse)
library(lubridate)
library(ggblur)
library(ggstar)
library(ggfx)
library(showtext)
library(ggimage)
library(gganimate)

font_add_google(name = "Space Mono",
                family = "sm")
showtext_auto()


directors <- drwho_directors
episodes <- drwho_episodes
writers <- drwho_writers

data_tardis <- episodes %>%
  mutate(season_number = ifelse(story_number >= 298, 13, season_number)) %>%
  filter(type == "special") %>%
  inner_join(directors, by = "story_number") %>%
  inner_join(writers, by = "story_number") %>%
  mutate(year = year(first_aired),
         uk_viewers = as.numeric(uk_viewers),
         rating = as.numeric(rating)) %>%
  group_by(year) %>%
  mutate(avg_view = mean(uk_viewers),
         avg_rating = mean(rating),
         y_text = avg_view + 0.8,
         y_text = ifelse(year == 2009 & writer == "Russell T Davies",
                         y_text - 3.2, y_text),
         y_text = ifelse(year == 2009 & writer == "Gareth Roberts",
                         y_text - 2.4, y_text),
         y_text = ifelse(year == 2009 & writer == "Phil Ford",
                         y_text - 1.6, y_text),
         y_text = ifelse(year == 2010 & writer == "Russell T Davies",
                         y_text + 0.8, y_text),
         y_text = ifelse(year == 2022 & writer == "Ella Road",
                         y_text + 0.8, y_text),
         writer = str_replace_all(writer, " ", "\n"),
         writer = str_replace_all(writer, "\nT\n", " T\n"),
         img = "C:\\Users\\Ceyda\\Desktop\\Tidy Tuesday\\2023\\Week 48\\tardis.png",
         episode_title = ifelse(story_number == "202a",
                                "The End of Time - Part One", episode_title),
         episode_title = ifelse(story_number == "202b",
                                "The End of Time - Part Two", episode_title),
         )



subtitle_text <- str_wrap("\nDoctor Who is an extremely long-running British
television program has several special episodes. Here, the average number of
U.K. viewers in each year is presented with the writer of the special episode.
The colour of the stars represents the writers. The average number of viewers
declined after 2013 and Steven Moffat is the most popular writer of the special
episodes.\n", 90)

subtitle_text <- ""
p <- ggplot(data_tardis) +
  geom_line(aes(x = year, y = avg_view), linewidth = 2) +
#  geom_point_blur(aes(x = year, y = avg_view)) +
  with_shadow(geom_star(aes(x = year, y = avg_view, fill = writer),
                        size = 25)) +
  scale_starshape_manual(values = c(3, 3)) +
  geom_text(aes(x = year, y = y_text, label = writer),
            family = "sm", size = 5, hjust = 0.5) +
  scale_x_continuous(breaks = c(2005, 2010, 2015, 2020)) +
  coord_cartesian() +
  labs(y = "Average Number of U.K. Viewers (millions)",
       x = "",
       title = "Doctor Who",
       subtitle = subtitle_text,
       caption = "Data Source: {datardis} | TidyTuesday 2023 - Week 48\nPrepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "lightblue", color = NA),
        plot.background = element_rect(fill = "lightblue", color = NA),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_text(family = "sm", size = 15),
        axis.text = element_text(family = "sm", size = 15),
        plot.title.position = "plot",
        plot.title = element_text(family = "sm", size = 40),
        plot.subtitle = element_text(family = "sm", size = 20),
        plot.caption = element_text(family = "sm", size = 20),
        plot.margin = margin(0.3, 0.5, 0.5, 0.5, "cm"))

ggsave("Week48.png", p, width = 22, height = 15, dpi = 72)


ggplot(data_tardis) +
  with_shadow(geom_star(aes(x = year, y = 1, fill = avg_rating),
                        size = 23)) +
  scale_starshape_manual(values = c(3, 3)) +
  scale_fill_gradient(low = "#0092ff", high = "#0c096f") +
  geom_point_blur(aes(x = year, y = 1), color = "ivory", size = 10) +
  geom_text(aes(x = year, y = 1, label = avg_rating))


p <- ggplot(data_tardis, aes(x = year, y = avg_view)) +
  geom_line(linewidth = 2, colour = "darkblue") +
  with_shadow(geom_image(aes(x = year, y = avg_view, image = img,
                             group = seq_along(year)))) +
  with_shadow(geom_image(aes(x = year, y = avg_view, image = img))) +
  geom_text(aes(x = 2005, y = 4.5, label = paste0("Writer: ", writer)),
                family = "sm", size = 4, hjust = 0) +
  geom_text(aes(x = 2005, y = 6, label = paste0("Title: ", episode_title)),
            family = "sm", size = 4, hjust = 0) +
  scale_x_continuous(limits = c(2005, 2030)) +
  transition_reveal(year) +
  view_follow(fixed_x = TRUE,fixed_y = TRUE)
animate(p, rewind = FALSE)
anim_save("dr_who.gif", p)
