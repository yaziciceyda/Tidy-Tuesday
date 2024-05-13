library(tidyverse)
library(showtext)

tuesdata <- tidytuesdayR::tt_load(2024, week = 19)
rolling_stone <- tuesdata$rolling_stone

# Font in the Plot

font_add_google('Fraunces', 'fraunces')
showtext_auto()

data_list <- rolling_stone %>%
  mutate(genre = ifelse(genre == "Blues/Blues ROck",
                        "Blues/Blues Rock", genre)) %>%
  drop_na(genre, rank_2003, rank_2012, rank_2020) %>%
  arrange(desc(differential)) %>%
  top_n(10, differential) %>%
  mutate(x = seq(from = 1, to = 20, by = 2),
         genre = str_replace_all(genre, "/", "\\\\n"))

subtitle_text <-str_wrap("Top 10 albums with the highest increase from 2003 
to 2020 are generally hip-hop and rap albums. Most of them has similar
rankings in 2012 and  2020, but Jay-Z (The Bluprint) and Radiohead (Kid A)
have sharp increase from 2003 to 2012 and then to 2020.", 100)


p <- ggplot(data_list) +
  geom_circle(aes(x0 = x, y0 = 1, r = 0.8), fill = "black") +
  geom_circle(aes(x0 = x, y0 = 1, r = rank_2003/1000), fill = "#34D349",
              color = "#34D349") +
  geom_circle(aes(x0 = x, y0 = 1, r = rank_2012/1000), fill = "brown",
              color = "brown") +
  geom_circle(aes(x0 = x, y0 = 1, r = rank_2020/1000), fill = "grey80",
              color = "grey80") +
  geom_text(aes(x = x, y = 2.4, label = clean_name), family = "fraunces",
            , size = 5) +
  geom_text(aes(x = x, y = 2, label = album), family = "fraunces",
            , size = 5) +
  geom_text(aes(x = x, y = 0, label = gsub("\\\\n", "\n", genre),
                color = genre), fontface = "bold",
            vjust = 1, family = "fraunces", , size = 4.5) +
  scale_colour_manual(values = c("#D36734", "#28AE7B", "#A8AE28",
                                 "#1A1481", "#D427C9", "#A2284B")) +
  coord_equal(ratio = 1) +
  scale_y_continuous(limits = c(-2, 6.5)) +
  scale_x_continuous(limits = c(-1, 20)) +
  annotate("text", x = -1, y = 2.4, label = "Name", 
           family = "fraunces", hjust = 0, fontface = "bold", size = 5.5) +
  annotate("text", x = -1, y = 2, label = "Album", 
           family = "fraunces", hjust = 0, fontface = "bold", size = 5.5) +
  annotate("text", x = -1, y = 0, label = "Genre", 
           family = "fraunces", hjust = 0, fontface = "bold", size = 5.5) +
  
  # "How to read" plot
  
  annotate("point", x = 17, y = 4, size = 25, colour = "black", 
           stroke = 1) +
  annotate("point", x = 17, y = 4, size = 17, colour = "#34D349", 
           stroke = 1) +
  annotate("point", x = 17, y = 4, size = 9, colour = "brown", 
           stroke = 1) +
  annotate("point", x = 17, y = 4, size = 5, colour = "grey80", 
           stroke = 1) +
  # 2003
  annotate("curve", x = 15, xend = 16.8, 
           y = 4.5, yend = 4, color = "#34D349", linewidth = 1.5,
           curvature = -0.2) +
  annotate("text", x = 13, y = 4.7, 
           label = "Radius shows the\nrank in 2003", 
           family = "fraunces", hjust = 0, size = 5) +
  # 2012
  annotate("segment", x = 17, xend = 17, 
           y = 5.2, yend = 4.3, color = "brown", linewidth = 1.5) +
  annotate("text", x = 17, y = 5.5, 
           label = "Radius shows the\nrank in 2012", 
           family = "fraunces", hjust = 0, size = 5) +
  # 2020
  annotate("curve", x = 17, xend = 18, 
           y = 4, yend = 3.8, color = "grey80", linewidth = 1.5,
           curvature = 0.2) +
  annotate("text", x = 18.2, y = 3.9, 
           label = "Radius shows the\nrank in 2020", 
           family = "fraunces", hjust = 0, size = 5) +
  annotate("text", x = 17, y = 6.5, 
           label = "How to read", 
           family = "fraunces", hjust = 0.5, size = 9) +
  labs(caption = "Data Source: Rolling Stone 500\nTidyTuesday 2024 - Week 19\nPrepared by: C. YAZICI",
       title = "Rolling Stone Album Rankings",
       subtitle = subtitle_text)  +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(family = "fraunces", hjust = 1, 
                                    size = 20, vjust = -3),
        plot.title = element_text(family = "fraunces", hjust = 0, 
                                    size = 40),
        plot.subtitle = element_text(family = "fraunces", hjust = 0, 
                                  size = 25),
        plot.margin = margin(0.5, 1, 1.5, 1, "cm"))

# Save the Plot

ggsave("Week19.png", p, width = 25, height = 14, dpi = 72)




