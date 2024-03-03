# Tidy Tuesday - 2023 - Week 8

# Libraries used

library(tidytext)
library(wordcloud)
library(tidyverse)
library(ggchicklet)
library(ggforce)
library(ggimage)
library(showtext)
library(ggtext)

# Font in the Plot

font_add_google("Laila", "laila")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 8)
bob_ross <- tuesdata$bob_ross

# Calculate the frequency of the words used in the titles of the paintings

data(stop_words)
titles_data <- bob_ross$painting_title %>%
  as_tibble() %>%
  rename(text = value) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) # --> Mountainis the most popular word
  
# The episodes with "Mountain" in the title (not Mountains)

bob_ross_data <- bob_ross %>%
  filter(str_detect(painting_title, " mountain | Mountain")) %>%
  mutate(colors = str_remove_all(colors, "\\["),
         colors = str_remove_all(colors, "\\]"),
         colors = str_remove_all(colors, "'"),
         colors = str_remove_all(colors, "\\\\r"),
         colors = str_remove_all(colors, "\\\\n"),
         color_hex = str_remove_all(color_hex, "\\["),
         color_hex = str_remove_all(color_hex, "\\]"),
         color_hex = str_remove_all(color_hex, "'")) %>%
  select(painting_title, season, episode, num_colors, colors, color_hex,
         img_src) %>%
  mutate(color_hex = strsplit(color_hex, ","),
         colors = strsplit(colors, ","),
         x = rep(c(1:5), each = 4),
         y = rep(c(1, 2.5, 4, 5.5), times = 5),
         x_painting = c(rep(-0.1, times = 5),
                        rep(1, times = 2),
                        rep(2, times = 2),
                        rep(3, times = 2),
                        rep(4, times = 2),
                        rep(5, times = 2),
                        rep(6.2, times = 5)),
         y_painting = ifelse(x_painting == -0.1, c(1, 2.5, 4, 5.5, 7), -0.7),
         y_painting = ifelse(x_painting == 6.2, c(1, 2.5, 4, 5.5, 7), 
                             y_painting),
         y_painting = ifelse(x_painting == 1, c(-0.7, 7), y_painting),
         y_painting = ifelse(x_painting == 2, c(-0.7, 7), y_painting),
         y_painting = ifelse(x_painting == 3, c(-0.7, 7), y_painting),
         y_painting = ifelse(x_painting == 4, c(-0.7, 7), y_painting),
         y_painting = ifelse(x_painting == 5, c(-0.7, 7), y_painting)
         ) %>%
  unnest(c(colors, color_hex)) %>%
  group_by(painting_title) %>%
  mutate(xx = x - c(0.2, 0, -0.2, -0.4)) %>%
  ungroup() %>%
  mutate(color_hex = str_replace_all(color_hex, " ", ""))

list_painting <- unique(bob_ross_data$painting_title)
n_list <- length(unique(bob_ross_data$painting_title))

yy <- c()
for(i in 1:n_list) # title 
{
  data_subset <- bob_ross_data %>%
    filter(painting_title == list_painting[i])
  
  for(j in 1:data_subset$num_colors[1]) # num of colors
  {
    yy_add <- c(rep(data_subset$y[1] - 0.2, times = 4), 
                rep(data_subset$y[1] - 0, times = 4),
                rep(data_subset$y[1] - -0.2, times = 4), 
                rep(data_subset$y[1] - -0.4, times = 4))
    yy_add <- yy_add[1:data_subset$num_colors[1]]
  }
  yy <- c(yy, yy_add)
}

# The final data to prepare the plot

bob_ross_data <- bob_ross_data %>%
  mutate(yy = yy)

# Subtitle of the plot

subtitle_text <- "<br>Bob Ross used  **Mountain** at most in the 
titles of the paintings. Here, randomly selected paintings with  <br>**Mountain**
in the title are shown with their colour palettes.<br>"


# Plot

p <- ggplot(bob_ross_data) +
  ggchicklet:::geom_rrect(aes(xmin = x - 0.4, xmax = x + 0.55, 
                              ymin = y - 0.4, ymax = y + 0.8), 
                          fill = "#EFD79C",
                          radius = unit(0.3, units = "cm")) +
  geom_circle(aes(x0 = xx, y0 = yy, r = 0.1, fill = color_hex)) +
  geom_text(aes(x = x + 0.10, y = y + 0.7, label = painting_title),
            size = 4.5, family = "laila") +
  geom_image(aes(x = x_painting, y = y_painting, image = img_src),
             size = 0.12) +
  geom_text(aes(x = x_painting + 0.05, y = y_painting + 0.6, 
                label = painting_title),
            family = "laila", size = 4.5) +
  scale_fill_identity() +
  coord_cartesian(xlim = c(-0.2, 6.5),
                  ylim = c(-1, 7.5)) +
  labs(title = "BOB ROSS PAINTINGS",
       subtitle = subtitle_text,
       caption = "Data Source: {BobRossColors} | TidyTuesday 2023 - Week 8 | Prepared by: C. YAZICI") +
  theme(plot.background = element_rect(fill = "ivory", color = NA),
        panel.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "laila", size = 40, hjust = 0),
        plot.subtitle = element_markdown(family = "laila", size = 24, hjust = 0),
        plot.caption = element_text(family = "laila", size = 20, hjust = 1),
        plot.margin = unit(c(1, 2, 1, 2), "cm"))

# Save the Plot

ggsave("Week8.png", p, width = 23.5, height = 15, dpi = 72)

  


  



