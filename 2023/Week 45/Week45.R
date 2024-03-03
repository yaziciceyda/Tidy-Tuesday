library(tidyverse)
library(ggplot2)
library(maps)
library(usmap)
library(geofacet)
library(ggforce)
library(ggfx)
library(showtext)
library(cowplot)

# Font in the Plot

font_add_google("Montserrat", "ms")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 45)
house <- tuesdata$house

# Data Wrangling

house_data <- house %>%
    filter(year == 2022,
         stage == "GEN",
         party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  group_by(state, party) %>%
  summarize(party_votes = round(sum(candidatevotes) / sum(totalvotes) * 100),
            2) %>%
  ungroup() %>%
  mutate(state = str_to_title(state)) %>%
  full_join(geofacet::us_state_grid2, by = c("state" = "name")) %>%
  filter(code != "DC")

#Plot for Democrats

plot_d <- ggplot(house_data %>% filter(party == "DEMOCRAT")) +
  with_shadow(geom_circle(aes(x0 = col, y0 = 9 - row, r = 0.4,
                              fill = party_votes)),
              sigma = 5) +
  geom_text(aes(x = col, y = 9 - row, label = code)) +
 scale_fill_gradient(low = "#b2b2ff",
                     high = "#0000ff",
                     breaks = c(30, 40, 50, 60, 70)) +
  labs(fill = "Percentage\nof\nVotes",
       title = "DEMOCRATS") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "ms", size = 22, hjust = 0.5,
                                  color = "#0000ff"),
        legend.background = element_rect(fill = "ivory"),
        legend.title = element_text(family = "ms", size = 13),
        legend.text = element_text(family = "ms", size = 12),
        legend.key.height = unit(1.2, 'cm'),
        plot.margin = margin(0.5, 0.5, 0.5, 1.2, "cm"))

# Plot for Republicans

plot_r <- ggplot(house_data %>% filter(party == "REPUBLICAN")) +
  with_shadow(geom_circle(aes(x0 = col, y0 = 9 - row, r = 0.4,
                              fill = party_votes)),
              sigma = 5) +
  geom_text(aes(x = col, y = 9 - row, label = code)) +
  scale_fill_gradient(low = "#f8cccc",
                      high = "#DE0100",
                      breaks = c(30, 40, 50, 60, 70)) +
  labs(fill = "Percentage\nof\nVotes",
       title = "REPUBLICANS") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "ms", size = 22, hjust = 0.5,
                                  color = "#DE0100"),
        legend.background = element_rect(fill = "ivory"),
        legend.title = element_text(family = "ms", size = 13),
        legend.text = element_text(family = "ms", size = 12),
        legend.key.height = unit(1.2, 'cm'),
        plot.margin = margin(0.5, 0.5, 0.5, 1.2, "cm"))

# The Final Plot

plots <- align_plots(plot_d, plot_r, align = 'v', axis = 'r')

title1 <- ggdraw() +
  draw_label(
    "US House Election Results in 2022",
    fontface = 'bold',
    fontfamily = "ms",
    hjust = 0.5,
    x = 0.5,
    size = 35
  )

caption <- ggdraw() +
  draw_label(
    "Data Source: MIT Election Data and Science Lab | TidyTuesday 2023 - Week 45 | Prepared by: C. YAZICI",
    fontface = 'bold',
    fontfamily = "ms",
    hjust = 0.5,
    x = 0.5,
    y = 0.5,
    size = 15
  )

bottom_row <- plot_grid(
  plots[[1]], plots[[2]],
  labels = "",
  rel_heights = c(0.8, 0.8),
  ncol = 1
)

p <- final_plot <- plot_grid(title1, bottom_row, caption,
                         ncol = 1,
                        rel_heights = c(0.2, 0.8, 0.2),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "ivory", colour = NA),
        plot.margin = margin(0.3, 1.0, 0.5, 0.5, "cm"),
        aspect.ratio = 8/9)

final_plot

# Save the Plot

ggsave("Week45.png", p, width = 17, height = 15, dpi = 72)

