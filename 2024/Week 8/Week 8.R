library(ggforce)
library(ggimage)
library(scales)
library(showtext)
library(patchwork)

# Font in the Plot

font_add_google(name = "Ubuntu Mono",
                family = "um")

# Data Import

tuesdata <- tidytuesdayR::tt_load(2024, week = 8)
isc_grants <- tuesdata$isc_grants

# Fall Data

grant_data_fall <- isc_grants %>%
  group_by(year, group) %>%
  summarise(total_funded = sum(funded)) %>%
  ungroup() %>%
  arrange(year, group) %>%
  filter(group == 2) %>%
  mutate(x = c(0.5, 1, 0.5, 0, -0.5, -1, -0.5),
         y = c(0.5, 0, -0.5, -1, -0.5, 0, 0.5),
         img_r = paste0(here::here(),
                        "/2024/Week 8/",
                        "Rlogo", ".jpg"))
# Plot of fall

p_fall <- ggplot(grant_data_fall) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.3, fill = total_funded,
                  colour = total_funded)) +
  geom_text(aes(x = x, y = y, label = format(total_funded,
                                             big.mark = ",")),
            family = "um", size = 10, hjust = 0.5) +
  geom_text(aes(x = x, y = y + 0.4, label = year), family = "um",
            fontface = "bold", size = 10, hjust = 0.5) +
  geom_image(aes(x = 0,
                 y = 0,
                 image = img_r), size = 0.18, by = 'height') +
  annotate("text", x = 0, y = 1.7, label = "FALL",
           family = "um", fontface = "bold", size = 13, hjust = 0.5) +
  coord_fixed() +
  scale_colour_gradient(low = "#4AA4DE",
                        high = "#1F65CC") +
  scale_fill_gradient(low = "#4AA4DE",
                      high = "#1F65CC") +
  theme(panel.background = element_rect(fill = "#fafafa", colour = NA),
        plot.background = element_rect(fill = "#fafafa", colour = NA),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

#Spring Data

grant_data_spring <- isc_grants %>%
  group_by(year, group) %>%
  summarise(total_funded = sum(funded)) %>%
  ungroup() %>%
  arrange(year, group) %>%
  filter(group == 1) %>%
  mutate(x = c(0.5, 1, 0.5, 0, -0.5, -1, -0.5, 0),
         y = c(0.5, 0, -0.5, -1, -0.5, 0, 0.5, 1),
         img_r = paste0(here::here(),
                          "/2024/Week 8/",
                          "Rlogo", ".jpg"))

# Plot of spring

p_spring <- ggplot(grant_data_spring) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.3, fill = total_funded,
                  colour = total_funded)) +
  geom_text(aes(x = x, y = y, label = format(total_funded,
                                             big.mark = ",")),
            family = "um", size = 10, hjust = 0.5) +
  geom_text(aes(x = x, y = y + 0.4, label = year), family = "um",
            fontface = "bold", size = 10, hjust = 0.5) +
  geom_image(aes(x = 0,
                 y = 0,
                 image = img_r), size = 0.18, by = 'height') +
  annotate("text", x = 0, y = 1.7, label = "SPRING",
           family = "um", fontface = "bold", size = 13, hjust = 0.5) +
  coord_fixed() +
  scale_colour_gradient(low = "#4AA4DE",
                      high = "#1F65CC") +
  scale_fill_gradient(low = "#4AA4DE",
                        high = "#1F65CC") +
  theme(panel.background = element_rect(fill = "#fafafa", colour = NA),
        plot.background = element_rect(fill = "#fafafa", colour = NA),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

# The final plot

final_plot <- p_fall + p_spring + plot_layout(nrow = 1, ncol = 2,
                               heights = c(1., 1.0)) +
 plot_annotation(
    title = "R Consortium ISC Grants",
    subtitle = "\nTotal amount of funding(in $) for the projects in fall and spring cycles.",
    caption = "Data Source: R Consortium ISC Funded Projects\nTidyTuesday 2024 - Week 8\nPrepared by: C. YAZICI") &
  theme(plot.title = element_text(family = "ps", hjust = 0,
                                  size = 40,
                                  face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "ps", hjust = 0,
                                  size = 30),
        plot.caption = element_text(family = "ps", size = 22,
                                    hjust = 1),
        plot.background = element_rect(color = "#fafafa", fill = "#fafafa"),
        panel.background = element_rect(color = "#fafafa", fill = "#fafafa"),
        plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"))


# Save the Plot

ggsave("Week8.png", final_plot, width = 22, height = 15, dpi = 72)
