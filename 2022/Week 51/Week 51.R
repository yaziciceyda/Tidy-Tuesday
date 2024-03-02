
library(ggchicklet)
library(colorspace)
library(showtext)
library(sysfonts)
library(cowplot)
library(scales)

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 51)
weather_forecasts <- tuesdata$weather_forecasts
cities <- tuesdata$cities
outlook_meanings <- tuesdata$outlook_meanings

# Colors used

cl <- c("#F97C2C")
color_seq <- seq(0, 1, length.out = 23)[-23]
color_list <- c(cl, lighten(cl, color_seq))

# Fonts in the Plot

sysfonts::font_add_google("Rubik Bubbles", "rb", db_cache = FALSE)
showtext::showtext_auto()

# Data Wrangling for high weather

data_weather_12_high <- weather_forecasts %>%
  filter(possible_error == "none",
         forecast_hours_before == 12,
         high_or_low == "high") %>%
  mutate(diff = observed_temp - forecast_temp) %>%
  drop_na(forecast_outlook) %>%
  group_by(forecast_outlook) %>%
  summarise(mean_diff = mean(diff, na.rm = TRUE),
            sd_diff = sd(diff, na.rm = TRUE),
            mean_precip = mean(observed_precip, na.rm = TRUE),
            sd_precip = sd(observed_precip, na.rm = TRUE),
            mean_minus_sd = mean_diff - sd_diff,
            mean_plus_sd = mean_diff + sd_diff,
            range = mean_plus_sd - mean_minus_sd) %>%
  ungroup() %>%
  arrange(desc(mean_diff)) %>%
  mutate(ymin = row_number()) %>%
  left_join(outlook_meanings, by = c("forecast_outlook")) %>%
  mutate(color = rev(color_list))

# Plot for high weather

p1 <- ggplot(data_weather_12_high) +
  ggchicklet:::geom_rrect(aes(xmin = mean_minus_sd, xmax = mean_plus_sd, 
                             ymin = ymin, ymax = ymin + 0.5, fill = color), 
                         radius = unit(0.2, units = "cm")) +
  geom_text(aes(x = -10, y = ymin, label = meaning, color = color), hjust = 0, 
            family = "rb", size = 7)  +
  geom_vline(xintercept = 0, color = "white", linetype = "dashed", size = 1) +
  geom_text(aes(x = -3.1, y = 26), label = "mean - sd", color = "red", family = "rb",
            size = 6) +
  geom_segment(aes(x = -2.8, y = 25.5, xend = -2.7, yend = 23.4), color = "red",
               size = 2, arrow = arrow(length = unit(0.5, 'cm'))) +
  geom_text(aes(x = 1, y = 26), label = "mean + sd", color = "red", family = "rb",
            size = 6) +
  geom_segment(aes(x = 1.3, y = 25.5, xend = 0.8, yend = 23.4), color = "red",
               size = 2, arrow = arrow(length = unit(0.5, 'cm'))) +
  geom_text(aes(x = -1, y = 26), label = "mean", color = "red", family = "rb",
            size = 6) +
  geom_segment(aes(x = -1, y = 25.5, xend = -0.95, yend = 23.4), color = "red",
               size = 2, arrow = arrow(length = unit(0.5, 'cm'))) +
   scale_fill_identity() +
   scale_color_identity() +
   geom_point(aes(x = mean_diff, y = ymin + 0.3), color = "red", size = 6) +
   scale_x_discrete(limits = c(-5, 0)) +
   labs(subtitle = "\nHIGH") +
   theme(plot.background = element_rect(fill = "black", color = NA),
         panel.background = element_rect(fill = "black"),
         panel.grid = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.text.x = element_text(family = "rb", size = 15),
         plot.title = element_text(family = "rb", size = 30, color = "white"),
         plot.subtitle = element_text(family = "rb", size = 20, color = "white"),
         plot.caption = element_text(family = "rb", size = 18, color = "white", hjust = 1))

# Data Wrangling for low weather
 
 data_weather_12_low <- weather_forecasts %>%
   filter(possible_error == "none",
          forecast_hours_before == 12,
          high_or_low == "low") %>%
   mutate(diff = observed_temp - forecast_temp) %>%
   drop_na(forecast_outlook) %>%
   group_by(forecast_outlook) %>%
   summarise(mean_diff = mean(diff, na.rm = TRUE),
             sd_diff = sd(diff, na.rm = TRUE),
             mean_precip = mean(observed_precip, na.rm = TRUE),
             sd_precip = sd(observed_precip, na.rm = TRUE),
             mean_minus_sd = mean_diff - sd_diff,
             mean_plus_sd = mean_diff + sd_diff,
             range = mean_plus_sd - mean_minus_sd) %>%
   ungroup() %>%
   arrange(desc(mean_diff)) %>%
   mutate(ymin = row_number()) %>%
   left_join(outlook_meanings, by = c("forecast_outlook")) %>%
   mutate(color = rev(color_list))
 
# Plot for high weather
 
p2 <- ggplot(data_weather_12_low) +
   ggchicklet:::geom_rrect(aes(xmin = mean_minus_sd, xmax = mean_plus_sd, 
                               ymin = ymin, ymax = ymin + 0.5, fill = color), 
                           radius = unit(0.2, units = "cm")) +
   geom_text(aes(x = -10, y = ymin, label = meaning, color = color), hjust = 0, 
             family = "rb", size = 7)  +
   geom_vline(xintercept = 0, color = "white", linetype = "dashed", size = 1) +
   scale_fill_identity() +
   scale_color_identity() +
  geom_point(aes(x = mean_diff, y = ymin + 0.3), color = "red", size = 6) +
   scale_x_discrete(limits = c(-5, 0, 5)) +
   labs(subtitle = "LOW") +
   theme(plot.background = element_rect(fill = "black",color = NA),
         panel.background = element_rect(fill = "black"),
         panel.grid = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.text.x = element_text(family = "rb", size = 15),
         plot.title = element_text(family = "rb", size = 30, color = "white"),
         plot.subtitle = element_text(family = "rb", size = 20, color = "white"),
         plot.caption = element_text(family = "rb", size = 18, color = "white", hjust = 1),
         plot.margin = unit(c(1.5, 0, 0, 0), "cm"))

# Combination

plots <- align_plots(p1, p2)

subtitle <- ggdraw() + 
  draw_label(
    "Weather Forecast (made before 12 hours) Errors within one standard deviation for high and\nlow temperatures show that fog, drizzle and dust are the common outlooks for the higher forecast errors.", 
    fontface = 'bold',
    fontfamily = "rb",
    hjust = 0,
    x = 0.05,
    y = 0.6, 
    size = 25,
    color = "white"
  ) 
title <- ggdraw() + 
  draw_label(
    "Weather Forecast Error", 
    fontface = 'bold',
    fontfamily = "rb",
    hjust = 0,
    x = 0.3,
    y = 0.5, 
    size = 40,
    color = "white"
  ) 
caption <- ggdraw() + 
  draw_label(
   "Data Source: Weather Forecast Capstone Project | TidyTuesday 2022 - Week 51 | Prepared by: @Cyd_yzc", 
    fontface = 'bold',
    fontfamily = "rb",
    hjust = 0,
    x = 0.4,
    y = 0.5, 
    size = 16,
    color = "white"
  ) 

bottom_row <- plot_grid(
  plots[[1]], plots[[2]], 
  labels = "",
  rel_widths = c(0.5, 0.5), 
  nrow = 1
)

final_plot <- plot_grid(title, subtitle, bottom_row, caption, labels = "", ncol = 1,
                        rel_heights = c(0.2, 0.2, 1, 0.2)) +
  theme(plot.background = element_rect(fill = "black", colour = "white"))
final_plot

# Save the Plot

ggsave("Week51_2022.png", final_plot, width = 27, height = 15, dpi = 72)


