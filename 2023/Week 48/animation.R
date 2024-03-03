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
         img = "C:\\Users\\Ceyda\\Desktop\\Tidy Tuesday\\2023\\Week 48\\tardis.png",
         episode_title = ifelse(story_number == "202a",
                                "The End of Time - Part One", episode_title),
         episode_title = ifelse(story_number == "202b",
                                "The End of Time - Part Two", episode_title),
  )

subtitle_text <- str_wrap("\nDoctor Who is an extremely long-running British
television program has several special episodes. Here, the average number of
U.K. viewers of these episodes which declined after 2013 is presented.", 60)

p <- ggplot(data_tardis, aes(x = year, y = avg_view)) +
  geom_line(linewidth = 2, colour = "darkblue") +
  with_shadow(geom_image(aes(x = year, y = avg_view, image = img,
                             group = seq_along(year)))) +
  with_shadow(geom_image(aes(x = year, y = avg_view, image = img), size = 0.1)) +
  geom_text(aes(x = 2005, y = 4.5, label = paste0("Writer: ", writer)),
            family = "sm", size = 6, hjust = 0, colour = "ivory") +
  geom_text(aes(x = 2005, y = 5, label = paste0("Title: ", episode_title)),
            family = "sm", size = 6, hjust = 0, colour = "ivory") +
  scale_x_continuous(limits = c(2005, 2025)) +
  scale_y_continuous(limits = c(2, 15)) +
  transition_reveal(year) +
  view_follow(fixed_x = TRUE,fixed_y = TRUE) +
  labs(y = "Average Number of U.K. Viewers (millions)",
       x = "",
       title = "DOCTOR WHO\n\n",
       subtitle = subtitle_text,
       caption = "Data Source: {datardis}\nTidyTuesday 2023 - Week 48\nPrepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "grey10", color = NA),
        plot.background = element_rect(fill = "grey10", color = NA),
        panel.grid = element_blank(),
        axis.title = element_text(family = "sm", size = 15, colour = "ivory"),
        axis.title.y = element_text(vjust = 7.5),
        axis.text = element_text(family = "sm", size = 15, colour = "ivory"),
        plot.title.position = "plot",
        plot.title = element_text(family = "sm", size = 40, colour = "ivory"),
        plot.subtitle = element_text(family = "sm", size = 17, colour = "ivory"),
        plot.caption = element_text(family = "sm", size = 15, colour = "ivory"),
        plot.margin = margin(1, 1, 1, 1, "cm"))
animate(p, rewind = FALSE)
anim_save("dr_who.gif", p, width = 1000, height = 800)



