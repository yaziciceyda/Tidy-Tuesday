library(tidyverse)
library(ggstar)
library(tidytext) 
library(tidyverse)
library(magtrittr)
library(showtext)

# Font in the Plot

font_add_google(name = "Ubuntu Mono",
                family = "um")


tuesdatashowtexttuesdata <- tidytuesdayR::tt_load(2024, week = 16)

shiny_revdeps <- tuesdata$shiny_revdeps
package_details <- tuesdata$package_details


shiny_data <- shiny_revdeps %>%
  filter(dependency_type %in% c("imports", "suggests")) %>%
  group_by(dependency_type, parent) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  group_by(dependency_type) %>%
  top_n(10, n) %>%
  arrange(desc(n)) %>%
  ungroup() 
  

subtitle_text <- str_wrap("The top 10 imported or suggested packages 
are similar, but their frequencies are very different. For instance, 
dplyr is imported 830 times; while suggested 3407 times.", 70)


p <-  ggplot(shiny_data) +
  geom_col(aes(x = n, y = reorder_within(parent, n, dependency_type)), 
           width = 0.2, 
           color = "#1F65CC") +
  geom_star(aes(x = n + 100, y = reorder_within(parent, n, 
                                                dependency_type)), 
            starshape = 14,
            size = 12, fill = "black") +
  facet_wrap(~dependency_type, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = 2000, linewidth = 1.5, color ="grey80",
             linetype = "dashed") +
   geom_vline(xintercept = 4000, linewidth = 1.5, color ="grey80",
              linetype = "dashed") +
   geom_vline(xintercept = 6000, linewidth = 1.5, color ="grey80",
              linetype = "dashed") +
   geom_vline(xintercept = 8000, linewidth = 1.5, color ="grey80",
              linetype = "dashed") +
  scale_y_reordered() +
  scale_x_continuous(expand = c(0,0), limits = c(0, 8400)) +
   coord_cartesian() +
  labs(y = "",
       title = "Shiny Packages",
       subtitle = subtitle_text, 
       caption = "Data Source: CRAN\nTidyTuesday 2024 - Week 16\nPrepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "um", size = 18,
                                 face = "bold"),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0,
                                   family = "um", size = 50),
        plot.subtitle = element_text(hjust = 0,
                                  family = "um", size = 30),
        plot.caption = element_text(hjust = 1,
                                  family = "um", size = 20),
        strip.text = element_text(family = "um", color = "#1F65CC",
                                  size = 25),
        strip.background = element_rect(fill = "grey90"),
        plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm")
        )


# Save the Plot

  ggsave("Week16.png", p, width = 22, height = 15, dpi = 72)

