tuesdata <- tidytuesdayR::tt_load('2022-05-17')
library(tidyverse)
library(dplyr)
library(ggplot2)
library(magick)
library(cowplot)

eurovision <- tuesdata$eurovision
votes <- tuesdata$`eurovision-votes`

vote2022 <- votes %>%
  filter(year == 2022,
         semi_final == "f",
         jury_or_televoting == "J",
         to_country == "Ukraine",
         points != 0) %>%
  select(jury_or_televoting, from_country, points) %>%
  arrange(jury_or_televoting, desc(points)) 

fm <- tibble(
    jury_or_televoting = rep(NA, 3),
    from_country = rep(NA, 3), 
    points = rep(NA, 3)
  )

vote2022 <- vote2022 %>%
  rbind(fm) %>%
  arrange(desc(points)) %>%
  mutate(id = seq(1, 29))
  
# Get the name and the y position of each label
label_data <- vote2022
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id - 0.5) /number_of_bar    
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


  
p <- ggplot(vote2022, aes(x = reorder(from_country, -points), y = points)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", fill=alpha("#a64d79", 0.3)) +
  coord_polar(start = 0) +
  geom_text(data=label_data, aes(x=id, y=points+1, label=from_country, hjust=hjust),
            color="grey80", fontface="bold",alpha=0.6, size = 3.5,
            angle= label_data$angle, inherit.aes = FALSE) +
    geom_text(data=label_data, aes(x=id, y=points-1, label=points, hjust=hjust),
            color= "#E37737", fontface="bold",alpha=0.6, size = 3.5,
            angle= label_data$angle, inherit.aes = FALSE) +
  labs(title = "\nThe Jury Points of the Winner of Eurovision 2022",
       caption = "#TidyTuesday Week 20 - 2022\n Data: Eurovision 2022\n Prepared by: @Cyd_yzc") +
   theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#161125"),
    plot.title = element_text(color = "grey80", face = "bold", hjust = 0.5,
                              size = 15),
    plot.caption = element_text(size = 10, color = "grey80"))
    #  plot.margin = unit(rep(-1,4), "cm") 

p2 <-  ggdraw() +
   draw_plot(p) +
  draw_image("https://upload.wikimedia.org/wikipedia/tr/4/4b/2022_Eurovision_%C5%9Eark%C4%B1_Yar%C4%B1%C5%9Fmas%C4%B1_Resmi_logosu.jpg",
                          scale = 0.19, x = -0.15, y = 0.45, height = 0.8) 
ggsave("Week20.png", p2, width = 20, height = 10, dpi = 72)  




