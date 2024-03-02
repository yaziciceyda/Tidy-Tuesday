tuesdata <- tidytuesdayR::tt_load(2022, week = 43)

library(showtext)

challenges <- tuesdata$challenges
bakers <- tuesdata$bakers
ratings <- tuesdata$ratings
episodes <- tuesdata$episodes


star_bakers <- challenges %>%
  filter(result == "STAR BAKER") %>%
  group_by(baker) %>%
  mutate(baker_n = cur_group_id(),
         n_baker = n())  %>%
  filter(n_baker %in% c(5, 4, 3)) %>%
  select(series, episode, baker, n_baker) %>%
ungroup() %>% 
  rowwise() %>%
  group_by(series) %>%
  mutate(min_episode = min(episode),
         max_episode = max(episode)) %>%
  ungroup() %>%
  mutate(food = c("apple",
          "rice_cracker",
          "lemon",
          "baguette_bread",
          "bread",
          "bread",
          "baguette_bread",
          "chocolate_bar",
          "spaghetti",
          "cake",
          "rose",
          "orange",
          "bread",
          "cake",
          "cake",
          "rose",
          "chocolate_bar",
          "chocolate_bar",
          "apple",
          "fish_cake",
          "cake",
          "chocolate_bar",
          "chocolate_bar",
          "bread",
          "lemon",
          "cake",
          "cake",
          "strawberry",
          "cheese"),
         color_food = c(
         "#9A0A0A",
           "#ED7211",
           "#F5C71B",
           "#ED7211",
           "#ED7211",
           "#ED7211",
           "#ED7211",
           "#622F22",
           "#D2AE18",
           "#F15398",
           "#D61742",
           "#FFA500",
           "#ED7211",
           "#F15398",
           "#F15398",
           "#D61742",
           "#622F22",
           "#622F22",
           "#9A0A0A",
           "#ED7211",
           "#F15398",
           "#622F22",
           "#622F22",
           "#ED7211",
           "#F5C71B",
           "#F15398",
           "#F15398",
           "#fc5a8d",
           "#ffa600"
         ))

font_add_google(name = "Kanit",   
                family = "knt")

p <- ggplot(star_bakers) +
  geom_segment(mapping= aes(x=min_episode, xend=max_episode, y=series, yend=series), 
               color = "grey20", size=2) +
  geom_segment(mapping= aes(x=1, xend=min_episode, y=series, yend=series), 
               color = "grey50", size=1) +
  geom_segment(mapping= aes(x=max_episode, xend=9, y=series, yend=series), 
               color = "grey50", size=1) +
  
  geom_text(mapping = aes(x = episode, y = series + 0.06, label = emoji(food), color = color_food),
            family = "EmojiOne",
            size = 14, 
            lineheight = 0.05) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5",
                              "6", "7", "8", "9")) +
  scale_y_discrete(limits = c("1", "2", "3", "4", "5", 
                              "6", "7", "8", "9", "10", "11"),
                   breaks = c("3", "4", "5", "6", "7", "8", "9", "10")) +
  scale_colour_identity() +
  labs(title = "Signature of Star Bakers",
       x = "Episodes",
       y = "Series",
    subtitle = "\nThe type of food or one of the ingredient shows the signature of the star baker. Series 6 includes the highest\n with 6 star bakers. But, series 9 has only two of them.\n",
      caption = "Data Source: {bakeoff} | TidyTuesday 2022 - Week 43 | Prepared by: @Cyd_yzc") +
  theme(plot.background = element_rect(fill = "#FFFFF0"),
        panel.background = element_rect(fill = "#FFFFF0"),
      plot.title = element_text(size = 30, hjust = 0.5, family = "knt"),
      plot.subtitle = element_text(size = 19, hjust = 0.5, family = "knt"),
      plot.caption = element_text(size = 12, family = "knt"),
      axis.title.y = element_text(angle = 0, vjust = 1.0, hjust = 1.2, size = 15, family = "knt"),
      axis.title.x = element_text( vjust = 1.5, hjust = 0.5, size = 15, family = "knt"),
      axis.text = element_text(size = 12, family = "knt"),
      axis.ticks = element_blank())
p
ggsave("Week43_2022.png", p, width = 20, height = 10, dpi = 72)






               
               
