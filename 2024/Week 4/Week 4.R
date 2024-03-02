library(showtext)
library(ggtext)
library("RColorBrewer")

# Font in the Plot

font_add_google('Noto Sans', 'ns')
showtext_auto()


# library to use fontawesome icons
font_add('fa-solid', 'Font Awesome 6 Free-Solid-900.otf')

# Data Import

english_education <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-23/english_education.csv')

# Data Wrangling

data_education <- english_education %>%
  select(size_flag, coastal, job_density_flag, university_flag,
         education_score) %>%
  filter(size_flag %in% c("Small Towns", "Medium Towns",
                          "Large Towns")) %>%
  group_by(coastal, size_flag, university_flag) %>%
  summarise(avg_att = median(education_score),
            n = n()) %>%
  ungroup() %>%
  mutate(town_y = case_when(
    size_flag == "Small Towns" ~ 21,
    size_flag == "Medium Towns" ~ 14,
    size_flag == "Large Towns" ~ 7
  ),
  coastal_x = case_when(
    coastal == "Coastal" ~ -2.5,
    coastal == "Non-coastal" ~ 2.5,
    )) %>%
  arrange(coastal, size_flag, university_flag) %>%
  mutate(label_book = ifelse(university_flag == "No university",
           "<span style='font-family:fa-solid'>&#xf02d;</span>",
           "<span style='font-family:fa-solid'>&#xf19d;</span>"))

# Data for the wave

y <- seq(0, 15/2 * pi, by = 0.5 * pi)
x <- rep(c(0, 1, 0, -1), times = 4)

a <- tibble(x = rep(c(0, 1, 0, -1), times = 4),
            y = seq(0, 15/2 * pi, by = 0.5 * pi))

a_left <- a %>%
  tibble::add_row(x = c(-5, -5),
                  y = c(23.56, 0)) %>%
  mutate(x = ifelse(x == -1, -0.7, x),
         x = ifelse(x == 1, 0.8, x))

a_right <- a %>%
  tibble::add_row(x = c(5, 5),
                  y = c(23.56, 0)) %>%
  mutate(x = ifelse(x == -1, -0.7, x),
         x = ifelse(x == 1, 0.6, x))

a <- a %>%
  mutate(x = ifelse(y > 23, x + 0.5, x),
         y = ifelse(y > 23, y - 0.2, y),
         x = ifelse(y == 0, x + 0.5, x),
         y = ifelse(y == 0, y + 0.5, y))

# Subtitle of the Plot

subtitle_text <- str_wrap("The educational attainment of pupils in English
towns is shaped by many factors such as the size of the town, whether
the town is coastal or not and the existence of a university in it.
Non-coastal towns has higher  average attainment than the coastal
ones and the existence of a university has a positive effect. Generally,
there is a negative relationship between the size of the town and
attainment.", 140)

# The Plot

p <- ggplot(a) +
  geom_polygon(data = a_left, mapping = aes(x = x,
                                            y = y), fill = "#1da2d8") +
  geom_polygon(data = a_right, mapping = aes(x = x,
                                             y = y), fill = "#fff5be") +
  geom_bspline(aes(x = x, y = y), color = "#fff5be", size = 4.5) +
  geom_richtext(data = data_education %>%
                   filter(university_flag == "No university"),
                            mapping = aes(x = coastal_x - 1,
                              y = town_y,
                              label = label_book,
                              color = avg_att),
                family = 'fontawesome-webfont', size = 30, fill = NA,
                label.colour = NA) +
  geom_text(data = data_education %>%
                 filter(university_flag == "No university"),
               mapping = aes(x = coastal_x - 1,
                             y = as.numeric(town_y) - 3,
                            label = round(avg_att, 1)),
            family = "ns", size = 8) +
  geom_richtext(data = data_education %>%
                  filter(university_flag == "University"),
                mapping = aes(x = coastal_x + 1,
                              y = town_y,
                              label = label_book,
                              color = avg_att),
                family = 'fontawesome-webfont', size = 30, fill = NA,
                label.colour = NA) +
  geom_text(data = data_education %>%
              filter(university_flag == "University"),
            mapping = aes(x = coastal_x + 1, y = as.numeric(town_y) - 3,
                          label = round(avg_att, 1)),
            family = "ns", size = 8) +
  scale_colour_gradient(low = "#FBCAD6",
                        high = "#F01B52") +
  scale_x_continuous(limits = c(-6.5, 5)) +
  labs(colour = "Average Attainment",
       title = "Educational Attainment in English Towns\n",
       subtitle = subtitle_text,
       caption = "Data Source: The UK Office for National Statistics\nTidyTuesday 2024 - Week 4 | Prepared by: C. YAZICI") +
  annotate("text", x = -5.55, y = 21, label = "Small\nTowns",
           family = "ns", size = 8) +
  annotate("text", x = -5.55, y = 14, label = "Medium\nTowns",
           family = "ns", size = 8) +
  annotate("text", x = -5.55, y = 7, label = "Large\nTowns",
           family = "ns", size = 8) +
  annotate("text", x = -3.5, y = 25, label = "No university",
           family = "ns", size = 10) +
  annotate("text", x = -1.5, y = 25, label = "University",
           family = "ns", size = 10) +
  annotate("text", x = 3.5, y = 25, label = "University",
           family = "ns", size = 10) +
  annotate("text", x = 1.5, y = 25, label = "No university",
           family = "ns", size = 10) +
  annotate("text", x = -2.5, y = 1.5, label = "COASTAL",
           family = "ns", size = 10) +
  annotate("text", x = 2.5, y = 1.5, label = "NON-COASTAL",
           family = "ns", size = 10) +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.background = element_rect(fill = "ivory"),
        legend.title = element_text(family = "ns", size = 18),
        legend.text =  element_text(family = "ns", size = 16),
        legend.position = "top",
        legend.key.height = unit(0.8, 'cm'),
        legend.key.width = unit(1.5, 'cm'),
        plot.caption = element_text(family = "ns", size = 20,
                                    hjust = 1),
        plot.title = element_text(family = "ns", size = 30,
                                    hjust = 0),
        plot.subtitle = element_text(family = "ns", size = 20,
                                  hjust = 0),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  guides(colour = guide_colorbar(title.position = "top", title.hjust = 0.5))

# Save the Plot

ggsave("Week4.png", p, width = 25, height = 12, dpi = 72)




