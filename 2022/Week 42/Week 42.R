tuesdata <- tidytuesdayR::tt_load(2022, week = 42)

library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(showtext)

episodes <-tuesdata$episodes
stranger_things_all_dialogue <- tuesdata$stranger_things_all_dialogue


dialogue_episodes <- stranger_things_all_dialogue %>%
  select(season, episode, dialogue) %>%
  right_join(episodes, by = c("season", "episode")) %>%
  select(season, episode, dialogue, written_by) %>%
  group_by(season, episode) %>%
  na.omit() %>%
  mutate(episode_dialogue = paste0(dialogue, collapse = " ")) %>%
  select(-dialogue) %>%
  distinct() %>%
  ungroup() 
  
afinn_dict <- get_sentiments("afinn")
dialogues_score <- c()

for(i in 1:nrow(dialogue_episodes))
{
text1 <- as_tibble(dialogue_episodes$episode_dialogue[i])
k <- text1 %>%
  unnest_tokens(word, value) %>%
 anti_join(stop_words) %>%
  inner_join(afinn_dict, by = "word")
 # left_join(get_sentiments("afinn"), by = "word")
dialogues_score <- c(dialogues_score, sum(k$value))
}

dialogue_episodes <- dialogue_episodes %>%
  mutate(dialogues_score,
         season_name = paste0("Season ", season))

# cc <- scales::seq_gradient_pal( "red")(seq(0,1,length.out=11))

font_add_google('Creepster', 'cr')
showtext_auto()

p <- ggplot(dialogue_episodes) +
  geom_rect(aes(ymin = 0.5, ymax = -400,
                xmin = 0, xmax = 10),
            fill = "#d3c399",
            colour = NA,
            size = 0.5) +
  annotation_custom(grid::rasterGrob(paste0("#0F0E0E", as.hexmode(1:240)), 
                                     width=unit(1,"npc"), 
                                     height = unit(1,"npc"), 
                                     interpolate = TRUE), 
                    xmin = 0, xmax = 10, 
                    ymin = -Inf, ymax = 10) +
 geom_point(aes(x = episode, y = dialogues_score, color = written_by),
                size = 5) +
    geom_line(aes(x = episode, y = dialogues_score)) +
  facet_wrap(vars(season_name), ncol = 1) +
#  scale_colour_manual(values = cc) +
  scale_colour_brewer(palette = "Spectral") +
  coord_cartesian(xlim = c(1, 9)) +
  labs(title = "\nStranger Things",
       subtitle = "\nSentiment score of each episode in Stranger Things.\n As the seasons continue, the dialogues become more negative\n and the most darkest writers are\nThe Duffer Brothers and Curtis Gwinn.",
       colour = "Written by",
       x = "Episodes",
       y = "Sentiment Score",
       caption = "Data Source: 8flix.com | TidyTuesday 2022 - Week 42 | Prepared by: @Cyd_yzc") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5",
                              "6", "7", "8", "9"),
                   breaks = c("1", "2", "3", "4", "5",
                              "6", "7", "8", "9")) +
  theme(legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 15, family = "cr"),
        legend.title = element_text(size = 15, family = "cr"),
        plot.background = element_rect(fill = "#FFFFF0"),
        panel.background = element_rect(fill = "#FFFFF0"),
        legend.background = element_rect(fill = "#FFFFF0"),
        legend.key = element_rect(fill = "#FFFFF0"),
        strip.background = element_rect(fill="#FFFFF0"),
        strip.text = element_text(family = "cr"),
        strip.text.x = element_text(family = "cr", size = 22),
        plot.title = element_text(size = 40, colour = "red", family = "cr",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 25, colour = "red", family = "cr",
                                  hjust = 0.5,
        margin = margin(r = 10)),
        plot.caption = element_text(size = 15, colour = "black", family = "cr",
                                     hjust = 1),
        axis.ticks = element_blank(),
        axis.title = element_text(family = "cr", size = 15),
        axis.text = element_text(family = "cr", size = 15),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20))) +
   guides(colour = guide_legend(override.aes = list(size = 5)))
ggsave("Week42_2022.png", p, width = 20, height = 15, dpi = 72)



    
  



