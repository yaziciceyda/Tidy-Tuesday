library(tidyverse)
library(ggstar)
library(PrettyCols)
library(showtext)
library(scales)
library(patchwork)


# Font in the Plot

font_add_google(name = "Electrolize",
                family = "electrolize")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 52)
cran_20221122 <- tuesdata$cran_20221122
external_calls <- tuesdata$external_calls
internal_calls <- tuesdata$internal_calls

# Data Wrangling
# Filtering for the *gg* libraries
# calculating the min, max and median

data_cran <- cran_20221122 %>%
  select(package, loc_R, loc_vignettes, loc_tests,
         blank_lines_R, blank_lines_vignettes, blank_lines_tests,
         comment_lines_R, comment_lines_vignettes,
         comment_lines_tests) %>%
  drop_na(loc_vignettes, loc_tests) %>%
  mutate(first_two = substr(package, start = 1, stop = 2)) %>%
  filter(first_two == "gg") %>%
  summarise(min_loc_r = min(loc_R),
         max_loc_r = max(loc_R),
         median_loc_r = median(loc_R),
         min_loc_vig = min(loc_vignettes),
         max_loc_vig = max(loc_vignettes),
         median_loc_vig = median(loc_vignettes),
         min_loc_test = min(loc_tests),
         max_loc_test = max(loc_tests),
         median_loc_test = median(loc_tests),
         min_blank_r = min(blank_lines_R),
         max_blank_r = max(blank_lines_R),
         median_blank_r = median(blank_lines_R),
         min_blank_vig = min(blank_lines_vignettes),
         max_blank_vig = max(blank_lines_vignettes),
         median_blank_vig = median(blank_lines_vignettes),
         min_blank_test = min(blank_lines_tests),
         max_blank_test = max(blank_lines_tests),
         median_blank_test = median(blank_lines_tests),
         min_comment_r = min(comment_lines_R),
         max_comment_r = max(comment_lines_R),
         median_comment_r = median(comment_lines_R),
         min_comment_vig = min(comment_lines_vignettes),
         max_comment_vig = max(comment_lines_vignettes),
         median_comment_vig = median(comment_lines_vignettes),
         min_comment_test = min(comment_lines_tests),
         max_comment_test = max(comment_lines_tests),
         median_comment_test = median(comment_lines_tests)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(group = case_when(
    str_detect(rownames(data_cran), "loc_r") ~ 1,
    str_detect(rownames(data_cran), "loc_vig") ~ 2,
    str_detect(rownames(data_cran), "loc_test") ~ 3,
    str_detect(rownames(data_cran), "blank_r") ~ 4,
    str_detect(rownames(data_cran), "blank_vig") ~ 5,
    str_detect(rownames(data_cran), "blank_test") ~ 6,
    str_detect(rownames(data_cran), "comment_r") ~ 7,
    str_detect(rownames(data_cran), "comment_vig") ~ 8,
    str_detect(rownames(data_cran), "comment_test") ~ 9,
  ),
  group_name = case_when(
    group == 1 ~ "Code across all files in the /R directory",
    group == 2 ~ "Code across all files in the /vignettes directory",
    group == 3 ~ "Code across all files in the /tests directory",
    group == 4 ~ "Blank lines across all files in the /R directory",
    group == 5 ~ "Blank lines across all files in the /vignettes directory",
    group == 6 ~ "Blank lines across all files in the /tests directory",
    group == 7 ~ "Comment lines across all files in the /R directory",
    group == 8 ~ "Comment lines across all files in the /vignettes directory",
    group == 9 ~ "Comment lines across all files in the /tests directory",
  )
  ) %>%
  rename(value = V1) %>%
  mutate(level_value = case_when(
    str_detect(rownames(data_cran), "min") ~ "min",
    str_detect(rownames(data_cran), "max") ~ "max",
    str_detect(rownames(data_cran), "median") ~ "median",
  ),
    start = ifelse(level_value == "min", value, 0),
         end = ifelse(level_value == "max", value, 0),
  group_name = reorder(group_name, end))

# Subtitle of the Plot

subtitle_text <- str_wrap("The number of lines in all files, tests and
vignettes with the blank lines and comments of 68 libraries starting
with gg show that the number of lines of code and comment are higher
than the rest. Most of them have median values closer to their minimum
value.\n", 90)

# The Main Plot

p <- ggplot(data_cran) +
  geom_vline(xintercept = c(0, 5000, 10000, 15000),
             linetype = "dashed", colour = "grey80", linewidth = 1.2) +
  geom_segment(aes(x = start, xend = end,
                   y = group_name, yend = group_name,
                   color = group_name),
               linetype = "dashed", linewidth = 1.5) +
  geom_star(aes(x = value, y = group_name, group = group_name,
                color = group_name, fill = group_name),
            starshape = "hexagon", size = 20) +
  scale_fill_pretty_d(name = "Autumn") +
  scale_colour_pretty_d(name = "Autumn") +
  scale_x_continuous(labels = label_number(suffix = "K",
                                           scale = 1e-3)) +
  labs(x = "Number of Lines",
       y = "",
       title = "Number of Lines in gg Packages",
       subtitle = subtitle_text,
       caption = "\nData Source: {pkgstats} | TidyTuesday 2023 - Week 52 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust = 0, family = "electrolize",
                                   size = 25),
        axis.text.x = element_text(hjust = 0, family = "electrolize",
                                   size = 25),
        axis.title = element_text(family = "electrolize",
                                   size = 25),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "electrolize", size = 45),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "electrolize", size = 28),
        plot.caption = element_text(family = "electrolize", size = 20,
                                    hjust = 1))

# The data for the "How to read" plot

p2_data <- data_frame(x = c(0, 20, 50),
                      stats = c("min", "median", "max"))

# The "How to read" plot

p2 <- ggplot(p2_data) +
  geom_segment(aes(x = 0, xend = 50, y = 1, yend = 1),
               linetype = "dashed", linewidth = 1.2) +
  geom_star(aes(x = x, y = 1, colour = "brown", fill = "brown"),
            starshape = "hexagon", size = 12) +
  geom_text(aes(x = x , y = 1.2, label = stats),
            family = "electrolize", size = 10) +
  scale_x_continuous(limits = c(-5, 55)) +
  scale_y_continuous(limits = c(0.5, 1.3)) +
  labs(title = "How to read") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        plot.title = element_text(family = "electrolize",
                                  hjust = 0.5, size = 30),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

# The Final Plot
 final_plot <- p +
  inset_element(p2, left = 0.3, bottom = 0.005,
                right = 0.8, top = 0.35) &
   theme(plot.background = element_rect(fill = "ivory", color = NA),
         plot.margin = margin(0.4, 0.6, 0.4, 0.6, "cm"))


 # Save the Plot

 ggsave("Week52.png", final_plot, width = 26, height = 15, dpi = 72)



