library(tidyverse)
library(lubridate)
library(statebins)
library(RColorBrewer)
library(showtext)
library(cowplot)


# Font in the Plot

font_add_google('Roboto Mono', 'rm')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2024, week = 3)
polling_places <- tuesdata$polling_places

# Data Wrangling

data_polling <- polling_places %>%
 # filter(location_type == "polling_location") %>%
  mutate(year = year(election_date),
         state_name = state.name[sapply(state, \(x) which(x == state.abb))]) %>%
  group_by(state_name, year) %>%
  summarise(n = n()) %>%
  ungroup()

state_both <- data_polling %>%
  filter(year %in% c(2012, 2020)) %>%
  group_by(state_name) %>%
  count(state_name) %>%
  ungroup() %>%
  filter(n == 2)

data_2020 <- data_polling %>%
  filter(state_name %in% state_both$state_name,
         year == 2020) %>%
  arrange(state_name, year) %>%
  mutate(n_2020 = n,
         year_2020 = year) %>%
  select(-c(year, n))

data_2012 <- data_polling %>%
  filter(state_name %in% state_both$state_name,
         year == 2012) %>%
  arrange(state_name, year) %>%
  mutate(n_2012 = n,
         year_2012 = year,
         state_name_2012 = state_name) %>%
  select(-c(year, n, state_name))


data_difference <- data_2020 %>%
  cbind(data_2012) %>%
  mutate(percent_change = round((n_2020 - n_2012) / n_2012 * 100),
         percent_change = as.numeric(percent_change)) %>%
  select(state_name, percent_change)

# The plot of the states

p_year <- ggplot(data_polling) +
  geom_statebins(aes(state = state_name, fill = n),
                 lbl_size = 3, border_col = "grey80", family = "rm") +
  facet_wrap(~year) +
  scale_fill_viridis_c(option = "plasma") +
  labs(fill = "Number of Polling Places") +
  theme(panel.background = element_rect(fill = "grey80", color = NA),
        plot.background = element_rect(fill = "grey80", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.background = element_rect(fill = "grey80"),
        legend.text = element_text(family = "rm", size = 15),
        legend.title = element_text(family = "rm", size = 18),
        legend.position = "top",
        legend.key.height = unit(0.7, 'cm'),
        legend.key.width = unit(1.8, 'cm'),
        strip.background = element_rect(fill = "grey50"),
        strip.text = element_text(family = "rm", size = 20,
                                  color = "black")) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

# The plot of the changes

p_change <- ggplot(data_difference) +
  geom_statebins(aes(state = state_name, fill = percent_change),
                 lbl_size = 8, border_col = "grey50", family = "rm") +
  scale_fill_gradient2(midpoint = 0,
                       low = "blue", mid = "white",
                        high = "red", space ="Lab") +
  labs(fill = "Change (%)") +
  theme(panel.background = element_rect(fill = "grey80", color = NA),
        plot.background = element_rect(fill = "grey80", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.background = element_rect(fill = "grey80"),
        legend.text = element_text(family = "rm", size = 15),
        legend.title = element_text(family = "rm", size = 18),
        legend.position = "top",
        legend.key.height = unit(0.8, 'cm'),
        legend.key.width = unit(1.5, 'cm')) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

# The Final Plot

plots <- align_plots(p_year, p_change,
                     align = 'r', axis = 'r')

title1 <- ggdraw() +
  draw_label(
    "US POLLING PLACES\n",
    fontface = 'bold',
    fontfamily = "rm",
    hjust = 0.5,
    x = 0.5,
    size = 40,
    color = "black"
  )

subtitle_text <- str_wrap("\nThe total number of polling places and
the change (in %) in them between 2012 - 2020 are shown here. Several
states (Colorado, Hawaii, Oregon, Washington and Utah) vote primarily
by mail and have little or no data. Among the others, Maryland shows
the maximum percentage of decrease while Nevada has the highest
percentage of increase.", 100)

title2 <- ggdraw() +
  draw_label(subtitle_text,
    fontface = 'bold',
    fontfamily = "rm",
    hjust = 0.5,
    x = 0.5,
    y = 0.5,
    size = 18,
    color = "black"
  )
caption <- ggdraw() +
  draw_label(
    "Data Source: Center for Public Integrity | TidyTuesday 2024 - Week 3 | Prepared by: C. YAZICI",
    fontface = 'bold',
    fontfamily = "rm",
    hjust = 0.5,
    x = 0.5,
    y = 0.5,
    size = 18,
    color = "black"
  )
top_row <-  plot_grid(
  plots[[2]], plots[[1]],
  labels = "",
  rel_widths = c(0.8, 1.0),
  nrow = 1
)


final_plot <- plot_grid(title1, title2, top_row, caption,
                        labels = "", ncol = 1,
                        rel_heights = c(0.1, 0.2, 0.9, 0.1),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "grey80", colour = NA),
        plot.margin = margin(1.2, 2, 0.5, 2, "cm"))

# final_plot

# Save the Plot

ggsave("Week3.png", final_plot, width = 25, height = 12, dpi = 72)







