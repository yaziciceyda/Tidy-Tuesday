library(ggforce)
library(PrettyCols)
library(ggnewscale)
library(showtext)

# Font in the Plot

font_add_google('Arvo', 'arvo', db_cache = FALSE)
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 4)

ratings <- tuesdata$ratings
details <- tuesdata$details

# Data Wrangling

details_data <- details %>%
  filter(yearpublished < 0) %>%
  left_join(ratings, by = "id") %>%
  arrange(yearpublished) %>%
  select(primary, yearpublished, minplayers, playingtime, average) %>%
  mutate(x = c(rep(c(1, 2, 3, 4), times = 2), c(1, 2, 3)),
         y = (rep(c(3, 2, 1), each = 4))[-12],
         label = str_replace_all(yearpublished, "-", ""),
         label = paste0(label, " B.C.", sep = ""))

# Plot Tag

tag <- str_wrap("The board games published B.C. is presented as rings. The colour of the outer ring shows the 
year it is published. The colour in the middle presents the playing time and the average rating 
of the game is shown in the middle.", 40)

# Plot

p <- ggplot(details_data) +
  geom_rect(mapping = aes(ymin = 0, ymax = 4,
                          xmin = 0, xmax = 5),
            fill = "antiquewhite",
            colour = NA,
            size = 0.5) +
  annotation_custom(grid::rasterGrob(paste0("#0F0E0E", as.hexmode(1:240)), 
                                     width = unit(1,"npc"), 
                                     height = unit(1,"npc"), 
                                     interpolate = TRUE), 
                    xmin = 0, xmax = 5, 
                    ymin = 0, ymax = 4) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.45, fill = yearpublished)) +
  scale_fill_pretty_c("Neon", direction = -1) +
  labs(fill = "Year Published") +
  new_scale_fill() +
  geom_circle(aes(x0 = x, y0 = y, r = 0.35, fill = playingtime)) +
  scale_fill_pretty_c("Teals", direction = -1) +
  labs(fill = "Playing Time",
       title = "BOARD GAMES",
       tag = tag,
       caption = "Data Source: Kaggle | TidyTuesday 2022 - Week 4 | Prepared by: @Cyd_yzc") +
  geom_text(aes(x = x, y = y, label = average), family = "arvo", size = 8) +
  geom_text(aes(x = x, y = y + 0.5, label = primary), family = "arvo", 
            size = 8) +
  geom_text(aes(x = x, y = y + 0.2, label = label), family = "arvo", size = 7) +
  coord_cartesian() +
  theme(panel.background = element_rect(fill = "#0F0E0E", color = NA),
        plot.background = element_rect(fill = "#0F0E0E", color = NA),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(colour = "antiquewhite", size = 30,
                                  family = "arvo", hjust = 0.5),
        plot.caption = element_text(colour = "antiquewhite", size = 15,
                                  family = "arvo", hjust = 1),
        plot.tag = element_text(colour = "antiquewhite", size = 16,
                                family = "arvo"),
        plot.tag.position = c(0.73, 0.25),
        legend.text = element_text(family = "arvo"),
        legend.title = element_text(family = "arvo"),
        legend.background = element_rect(fill = "antiquewhite"),
        plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "cm"))


# Save the Plot

ggsave("Week4_2022.png", p, width = 25, height = 15, dpi = 72)



