# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-02-22')
tuesdata <- tidytuesdayR::tt_load(2022, week = 8)

freedom <- tuesdata$freedom

library(tidyverse)
library(ggplot2)
library(fontawesome)
library(emojifont)
library(showtext)
library(cowplot)
library(grid)


# 193 countiries
# 5 Regions --> Africa, Americas, Asia, Europe, Oceania
# 7 Groups
# 3 Status

# Region 1: Africa

fa <- fontawesome('fa-twitter')

font_add_google(name = "Pacifico",   
                family = "pacifico")

nr_countries_Africa <- freedom %>%
  filter(Region_Name == "Africa") %>%
  group_by(year) %>%
  distinct(country) %>%
  count() 

freedom_data_Africa <- freedom %>%
  filter(is_ldc == 1, Region_Name == "Africa") %>%
  group_by(year, Status) %>%
  count() %>%
  arrange(year, Status) %>%
  mutate(freedom_new = ifelse(year == 2020, fa, NA),
         freq_country = ifelse(year %in% c(1995:2010), 53, 54),
         prop = round(n/freq_country * 100), 1)


p1 <- ggplot(freedom_data_Africa, aes(x = year, y = prop, color = Status, group = Status)) +
  geom_line(stat = "identity", size = 1) +
  geom_point(shape = "circle open") +
  ylim(c(0, 40)) +
  geom_text(freedom_data_Africa, mapping = aes(label = freedom_new), family ='fontawesome-webfont') +
  scale_colour_manual(values = c("F" = "#1DF771", "PF" = "#F7DC1D", "NF" = "red"),
                      labels = c("Free", "Partially Free", "Not Free")) +
  coord_cartesian() +
    labs(x = "",
       y = "Proportion",
       title = "Africa") +
  theme_minimal() +
  theme(text = element_text(family = "pacifico"),
        axis.title = element_text(size = 15),
        legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15, vjust = -0.5))

# Region 2: Asia

nr_countries_Asia <- freedom %>%
  filter(Region_Name == "Asia") %>%
  group_by(year) %>%
  distinct(country) %>%
  count() 

freedom_data_Asia <- freedom %>%
  filter(is_ldc == 1, Region_Name == "Asia") %>%
  group_by(year, Status) %>%
  count() %>%
  arrange(year, Status) %>%
  mutate(freedom_new = ifelse(year == 2020, fa, NA),
         freq_country = ifelse(year %in% c(1995:1998), 46, 47),
         prop = round(n/freq_country * 100), 1)

p2 <- ggplot(freedom_data_Asia, aes(x = year, y = prop, color = Status, group = Status)) +
  geom_line(stat = "identity", size = 1) +
  geom_point(shape = "circle open") +
  ylim(c(0, 40)) +
  geom_text(freedom_data_Asia, mapping = aes(label = freedom_new), family ='fontawesome-webfont') +
  scale_colour_manual(values = c("F" = "#1DF771", "PF" = "#F7DC1D", "NF" = "red")) +
  coord_cartesian() +
  labs(x = "",
       y = "",
       title = "Asia") +
  theme_minimal() +
  theme(text = element_text(family = "pacifico"),
        axis.title = element_text(size = 15),
        legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15, vjust = -0.5))

# Region 3: Americas

nr_countries_Americas <- freedom %>%
  filter(Region_Name == "Americas") %>%
  group_by(year) %>%
  distinct(country) %>%
  count() 

freedom_data_Americas <- freedom %>%
  filter(is_ldc == 1, Region_Name == "Americas") %>%
  group_by(year, Status) %>%
  count() %>%
  arrange(year, Status) %>%
  mutate(freedom_new = ifelse(year == 2020, fa, NA),
         freq_country = 35,
         prop = round(n/freq_country * 100), 1) 
 

p3 <- ggplot(freedom_data_Americas, aes(x = year, y = prop, color = Status, group = Status)) +
  geom_line(stat = "identity", size = 1) +
  geom_point(shape = "circle open") +
  ylim(c(0, 40)) +
  geom_text(freedom_data_Americas, mapping = aes(label = freedom_new), family ='fontawesome-webfont') +
  scale_colour_manual(values = c("F" = "#1DF771", "PF" = "#F7DC1D", "NF" = "red")) +
  coord_cartesian() +
  labs(x = "",
       y = "Proportion",
       title = "Americas") +
  theme_minimal() +
  theme(text = element_text(family = "pacifico"),
        axis.title = element_text(size = 15),
        legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15, vjust = -0.5))

# Region 4: Oceania

nr_countries_Oceania <- freedom %>%
  filter(Region_Name == "Oceania") %>%
  group_by(year) %>%
  distinct(country) %>%
  count() 

freedom_data_Oceania <- freedom %>%
  filter(is_ldc == 1, Region_Name == "Oceania") %>%
  group_by(year, Status) %>%
  count() %>%
  arrange(year, Status) %>%
  mutate(freedom_new = ifelse(year == 2020 || (year == 2015 & Status == "PF"), fa, NA),
             freq_country = 14,
         prop = round(n/freq_country * 100), 1)


p4 <- ggplot(freedom_data_Oceania, aes(x = year, y = prop, color = Status, group = Status)) +
  geom_line(stat = "identity", size = 1) +
  geom_point(shape = "circle open") +
  ylim(c(0, 40)) +
  geom_text(freedom_data_Oceania, mapping = aes(label = freedom_new), family ='fontawesome-webfont') +
  scale_colour_manual(values = c("F" = "#1DF771", "PF" = "#F7DC1D", "NF" = "red")) +
  coord_cartesian() +
  labs(x = "",
       y = "",
       title = "Oceania") +
  theme_minimal() +
  theme(text = element_text(family = "pacifico"),
        axis.title = element_text(size = 15),
        legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15, vjust = -0.5))

legend<- get_legend(p1 + 
    guides(color = guide_legend(ncol = 1)) +
    theme(legend.position = "right")
)

plots <- align_plots(p1, p2, p3, p4,legend, align = 'r', axis = 'r')

title <- ggdraw() + 
  draw_label(
    "Least  ",
    fontface = 'bold',
    fontfamily = "pacifico",
    hjust = 0.5,
    x = 0.1,
    size = 25,
    color = "red"
  ) + 
  draw_label(
    "Developed ",
    fontface = 'bold',
    fontfamily = "pacifico",
    hjust = 0.5,
    x = 0.205,
    size = 25,
    color = "#F7DC1D"
  ) +
  draw_label(
    "Countries ",
    fontface = 'bold',
    fontfamily = "pacifico",
    hjust = 0.5,
    x = 0.325,
    size = 25,
    color = "#1DF771"
  ) +
  draw_label(
    "The proportion of the least developed countries according to their status differs in each region.\n Even though in Americas Region, there is only one Partially Free country, \n Africa and Asia include Free, Partially Free and Not Free countries.",
    fontface = 'bold',
    fontfamily = "pacifico",
    hjust = 0,
    x = 0.1,
    y = 0.25,
    size = 18,
    color = "black"
  )

top_row <-  plot_grid(
  plots[[1]], plots[[2]], legend,
  labels = "",
  rel_widths = c(1, 1, 0.3),
  nrow = 1
)

bottom_row <- plot_grid(
  plots[[3]], plots[[4]], get_legend(p1 + scale_shape(guide = FALSE)),
  labels = "",
  rel_widths = c(1, 1, 0.3), 
  nrow = 1
)
plot_grid(title, top_row, bottom_row, labels = "", ncol = 1)


ggsave("Week82.png", final_plot)

