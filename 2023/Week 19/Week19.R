# Tidy Tuesday - 2023 - Week 19

# Libraries used 

library(statebins)
library(PrettyCols)
library(showtext)
library(cowplot)

# Font in the Plot

font_add_google('PT Serif', 'ptserif')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 19)
childcare_costs <- tuesdata$childcare_costs
counties <- tuesdata$counties

# Data Wrangling

childcare_data <- childcare_costs %>%
  select(county_fips_code, study_year, pr_f, mcsa, households, 
         h_under6_both_work, h_under6_f_work, h_under6_m_work, 
         h_under6_single_m) %>%
  filter(study_year == 2018) %>%
  mutate(both_percent = h_under6_both_work / households * 100,
         only_f = h_under6_f_work / households * 100,
         only_m = h_under6_m_work / households * 100,
         single_mom  = h_under6_single_m / households * 100) %>%
  right_join(counties, by = "county_fips_code") %>%
  group_by(state_name) %>%
  mutate(avg_only_f = mean(only_f),
         avg_only_m = mean(only_m),
         avg_both = mean(both_percent),
         avg_pr = mean(pr_f),
         avg_mcsa = mean(mcsa),
         avg_single_mom = mean(single_mom)
         )
  ungroup()
  
# Both parents are working
  
both <- ggplot(childcare_data) +
    geom_statebins(aes(state = state_abbreviation, fill = avg_both),
                   border_col = "ivory", lbl_size = 4, family = "ptserif") +
    scale_fill_pretty_c("Tangerines", direction = -1) +
    labs(fill = "Percentage",
         title = "Both Parents") +
    theme(panel.background = element_rect(fill = "ivory", color = NA),
          plot.background = element_rect(fill = "ivory", color = NA),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          legend.background = element_rect(fill = "ivory"),
          legend.title = element_text(family = "ptserif", size = 18),
          legend.key.height = unit(1.0, 'cm'),
          legend.text = element_text(family = "ptserif", size = 15),
          plot.title = element_text(family = "ptserif", hjust = 0.5, size = 20),
          plot.margin = unit(c(0.5, 1.0, 0.5, 1.0), "cm"))
  
 
# Only mother is working

mother <- ggplot(childcare_data) +
    geom_statebins(aes(state = state_abbreviation, fill = avg_only_m),
                   border_col = "ivory", lbl_size = 4, family = "ptserif") +
    scale_fill_pretty_c("Teals", direction = -1) +
    labs(fill = "Percentage",
         title = "Only Mother") +
    theme(panel.background = element_rect(fill = "ivory", color = NA),
          plot.background = element_rect(fill = "ivory", color = NA),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          legend.background = element_rect(fill = "ivory"),
          legend.title = element_text(family = "ptserif", size = 18),
          legend.key.height = unit(1.0, 'cm'),
          legend.text = element_text(family = "ptserif", size = 15),
          plot.title = element_text(family = "ptserif", hjust = 0.5, size = 20),
          plot.margin = unit(c(0.5, 1.0, 0.5, 1.0), "cm"))

# Only father is working

father <- ggplot(childcare_data) +
    geom_statebins(aes(state = state_abbreviation, fill = avg_only_f),
                   border_col = "ivory", lbl_size = 4, family = "ptserif") +
    scale_fill_pretty_c("Greens", direction = -1) +
    labs(fill = "Percentage",
         title = "Only Father") +
    theme(panel.background = element_rect(fill = "ivory", color = NA),
          plot.background = element_rect(fill = "ivory", color = NA),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          legend.background = element_rect(fill = "ivory"),
          legend.title = element_text(family = "ptserif", size = 18),
          legend.key.height = unit(1.0, 'cm'),
          legend.text = element_text(family = "ptserif", size = 15),
          plot.title = element_text(family = "ptserif", hjust = 0.5, size = 20),
          plot.margin = unit(c(0.5, 1.0, 0.5, 1.0), "cm"))

# Single Moms 

single_mom <- ggplot(childcare_data) +
  geom_statebins(aes(state = state_abbreviation, fill = avg_single_mom),
                 border_col = "ivory", lbl_size = 4, family = "ptserif") +
  scale_fill_pretty_c("Blues", direction = -1) +
  labs(fill = "Percentage",
       title = "Single Mom") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.background = element_rect(fill = "ivory"),
        legend.title = element_text(family = "ptserif", size = 18),
        legend.key.height = unit(1.0, 'cm'),
        legend.text = element_text(family = "ptserif", size = 15),
        plot.title = element_text(family = "ptserif", hjust = 0.5, size = 20),
        plot.margin = unit(c(0.5, 1.0, 0.5, 1.0), "cm"))

# The Final Plot

plots <- align_plots(both, mother, father, single_mom,
                     align = 'r', axis = 'r')

title1 <- ggdraw() + 
  draw_label(
    "Childcare Costs",
    fontface = 'bold',
    fontfamily = "ptserif",
    hjust = 0.5,
    x = 0.5,
    size = 40,
    color = "black"
  )  
subtitle_text <- str_wrap("The % of households with children under 6 years old 
    with both parents, only mother, only father working and the % of households 
    with a single mom are shown. The percentage of only mother working is 
    smaller than the other categories in all states. The states in the north 
    have higher rates of both parents working and the ones with a coast or 
    border have higher rates of only mother working. However, the states have 
    similar percentages for the other two categories.", 125)

title2 <- ggdraw() + 
  draw_label(
    subtitle_text,
    fontfamily = "ptserif",
    hjust = 0,
    x = 0.1,
    y = 0.3, 
    size = 18,
    color = "black"
  ) 
caption <- ggdraw() + 
  draw_label(
    "Data Source: National Database of Childcare Prices\nTidyTuesday 2023 - Week 19 | Prepared by: @C. YAZICI", 
    fontfamily = "ptserif",
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 18,
    color = "black"
  ) 
top_row <-  plot_grid(
  plots[[1]], plots[[2]], 
  labels = "",
  rel_widths = c(1.0, 1.0), 
  nrow = 1
)

bottom_row <- plot_grid(
  plots[[3]], plots[[4]],
  labels = "",
  rel_widths = c(1.0, 1.0), 
  nrow = 1
)

final_plot <- plot_grid(title1, title2, top_row, bottom_row, caption, 
                        labels = "", ncol = 1,
                        rel_heights = c(0.1, 0.4, 0.9, 0.9, 0.1),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "ivory", colour = NA),
        plot.margin = margin(0.5, 2, 0.5, 2, "cm"))

final_plot


# Save the Plot

ggsave("Week19.png", final_plot, width = 25, height = 12, dpi = 72)


