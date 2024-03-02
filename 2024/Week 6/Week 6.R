library(tidyverse)
library(janitor)
library(ggchicklet)
library(cowplot)
library(showtext)
library(ggforce)


# Font in the Plot

font_add_google('Roboto Mono', 'rm')
showtext_auto()

heritage <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv')


heritage_data <- heritage %>%
  clean_names() %>%
  pivot_longer(cols = -country, values_to = "frequency",
               names_to = "year") %>%
  arrange(year, frequency) %>%
  mutate(xmin = -1 * frequency / 2,
         xmax = frequency / 2,
         ymin = -1 * frequency / 2,
         ymax = frequency / 2,
         color_country = case_when(
           country == "Sweden" ~ "#3274d8",
           country == "Denmark" ~ "#f05440",
           country == "Norway" ~ "#283250"
         ),
         country_label = case_when(
           country == "Sweden" ~ "SE",
           country == "Denmark" ~ "DK",
           country == "Norway" ~ "NO"
         ))

heritage_data_2004 <- heritage_data %>%
  filter(year == "x2004")

heritage_data_2022 <- heritage_data %>%
  filter(year == "x2022")



p_2004 <- ggplot(heritage_data_2004) +
  ggchicklet:::geom_rrect(data = heritage_data_2004 %>%
                            filter(country == "Sweden"),
                          mapping = aes(xmin = xmin, xmax = xmax,
                                        ymin = ymin, ymax = ymax,
                                        color = color_country,
                                        fill = color_country),
                          radius = unit(0.5, units = "cm")) +
  ggchicklet:::geom_rrect(data = heritage_data_2004 %>%
                            filter(country == "Norway"),
                          mapping = aes(xmin = xmin, xmax = xmax,
                                        ymin = ymin, ymax = ymax,
                                        color = color_country,
                                        fill = color_country),
                          radius = unit(0.5, units = "cm")) +
  ggchicklet:::geom_rrect(data = heritage_data_2004 %>%
                            filter(country == "Denmark"),
                          mapping = aes(xmin = xmin, xmax = xmax,
                                        ymin = ymin, ymax = ymax,
                                        color = color_country,
                                        fill = color_country),
                          radius = unit(0.5, units = "cm")) +
  scale_colour_identity() +
  scale_fill_identity() +
  labs(title = "2004") +
  theme(panel.background = element_rect(fill = "#f0ead6", color = NA),
        plot.background = element_rect(fill = "#f0ead6", color = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "rm", hjust = 0.5,
                                  size = 25))





p_2022 <- ggplot(heritage_data_2022) +
ggchicklet:::geom_rrect(data = heritage_data_2022 %>%
                          filter(country == "Sweden"),
             mapping = aes(xmin = xmin, xmax = xmax,
                            ymin = ymin, ymax = ymax,
                        color = color_country,
                        fill = color_country),
                        radius = unit(0.5, units = "cm")) +
  ggchicklet:::geom_rrect(data = heritage_data_2022 %>%
                            filter(country == "Denmark"),
                          mapping = aes(xmin = xmin, xmax = xmax,
                                        ymin = ymin, ymax = ymax,
                                        color = color_country,
                                        fill = color_country),
                          radius = unit(0.5, units = "cm")) +
  ggchicklet:::geom_rrect(data = heritage_data_2022 %>%
                            filter(country == "Norway"),
                          mapping = aes(xmin = xmin, xmax = xmax,
                                        ymin = ymin, ymax = ymax,
                                        color = color_country,
                                        fill = color_country),
                          radius = unit(0.5, units = "cm")) +
  scale_colour_identity() +
  scale_fill_identity() +
  labs(title = "2022") +
  theme(panel.background = element_rect(fill = "#f0ead6", color = NA),
        plot.background = element_rect(fill = "#f0ead6", color = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "rm", hjust = 0.5,
                                  size = 25))

legend_data <- tibble(x = 1:3,
                      y = rep(1, 3),
                      country = c("Sweden", "Norway", "Denmark"),
                      color_country = c("#3274d8", "#283250",
                                        "#f05440"))
p_legend <- ggplot(legend_data) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.2, fill = color_country,
                  colour = color_country)) +
  geom_text(aes(x = x, y = y + 0.3, label = country), family = "rm",
            size = 5, fontface = "bold") +
  scale_fill_identity() +
  scale_colour_identity() +
  coord_fixed() +
  theme(panel.background = element_rect(fill = "#f0ead6", color = NA),
        plot.background = element_rect(fill = "#f0ead6", color = NA),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(0.1, 1.5, 0.5, 2.0, "cm"))



# The Final Plot

plots <- align_plots(p_legend , p_2004, p_2022,
                     align = 'r', axis = 'r')

title1 <- ggdraw() +
  draw_label(
    "Heritage Sites: 2004 vs 2022",
    fontface = 'bold',
    fontfamily = "rm",
    hjust = 0.5,
    x = 0.5,
    size = 40,
    color = "black"
  )

subtitle_text <- str_wrap("The number of UNESCO World Heritage sites
is the highest in Sweden in both years (13 in 2004 and 15 in 2022),
but the maximum increase occured in Denmark (150%).\n\n\n", 80)


title2 <- ggdraw() +
  draw_label(subtitle_text,
             fontface = 'bold',
             fontfamily = "rm",
             hjust = 0.5,
             x = 0.5,
             y = 0.5,
             size = 20,
             color = "black"
  )
caption <- ggdraw() +
  draw_label(
    "Data Source: UNESCO World Heritage Sites | TidyTuesday 2024 - Week 6 | Prepared by: C. YAZICI",
    fontface = 'bold',
    fontfamily = "rm",
    hjust = 0.5,
    x = 0.5,
    y = 0.5,
    size = 18,
    color = "black"
  )
bottom_row <-  plot_grid(
  plots[[2]], plots[[3]],
  labels = "",
  rel_widths = c(0.8, 0.8),
  nrow = 1
)

top_row <- plot_grid(plots[[1]],
                        labels = "")

final_plot <- plot_grid(title1, title2, top_row, bottom_row, caption,
                        labels = "", ncol = 1,
                        rel_heights = c(0.1, 0.12, 0.2, 0.6, 0.1),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "#f0ead6", colour = NA),
        panel.background = element_rect(fill = "#f0ead6", colour = NA),
        plot.margin = margin(1.2, 2.5, 0.5, 2.5, "cm"))

final_plot

# Save the Plot

 ggsave("Week6.png", final_plot, width = 20, height = 12, dpi = 72)


