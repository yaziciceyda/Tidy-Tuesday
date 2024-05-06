library(tidyverse)
library(janitor)
library(rnaturalearth)
library(sf)
library(showtext)
library(cowplot)


# Font in the Plot

font_add_google('Space Grotesk', 'sg')
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2024, week = 17)

outer_space_objects <- tuesdata$outer_space_objects


space_data <- outer_space_objects %>%
  clean_names() %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(decade = case_when(
    year < 1960 ~"1950s",
    year >= 1960 & year < 1970 ~ "1960s",
    year >= 1970 & year < 1980 ~ "1970s",
    year >= 1980 & year < 1990 ~ "1980s",
    year >= 1990 & year < 2000 ~ "1990s",
    year >= 2000 & year < 2010 ~ "2000s",
    year >= 2010 & year < 2020 ~ "2010s",
    year >= 2020 & year < 2030 ~ "2020s",
  ),
  size = ifelse(decade %in% c("1950s", "1960s", "1970s", "1980s"),
                "left", "right"),
  y = case_when(
    year %% 10 == 1 ~ 5,
    year %% 10 == 2 ~ 4,
    year %% 10 == 3 ~ 3,
    year %% 10 == 4 ~ 2,
    year %% 10 == 5 ~ 1,
    year %% 10 == 6 ~ 5,
    year %% 10 == 7 ~ 4,
    year %% 10 == 8 ~ 3,
    year %% 10 == 9 ~ 2,
    year %% 10 == 0 ~ 1,
  ),
  x = case_when(
    decade == "1950s" & year %% 10 <= 5 ~ 1,
    decade == "1950s" & year %% 10 > 5 ~ 2,
    decade == "1960s" & year %% 10 == 0 ~ 2,
    
    decade == "1960s" & year %% 10 <= 5 ~ 4,
    decade == "1960s" & year %% 10 > 5 ~ 5,
    decade == "1970s" & year %% 10 == 0 ~ 5,
    
    
    decade == "1970s" & year %% 10 <= 5 ~ 7,
    decade == "1970s" & year %% 10 > 5 ~ 8,
    decade == "1980s" & year %% 10 == 0 ~ 8,
    
    decade == "1980s" & year %% 10 <= 5 ~ 10,
    decade == "1980s" & year %% 10 > 5 ~ 11,
    decade == "1990s" & year %% 10 == 0 ~ 11,
    
    
    decade == "1990s" & year %% 10 <= 5 ~ 16,
    decade == "1990s" & year %% 10 > 5 ~ 17,
    decade == "2000s" & year %% 10 == 0 ~ 17,
    
  
    decade == "2000s" & year %% 10 <= 5 ~ 19,
    decade == "2000s" & year %% 10 > 5 ~ 20,
    decade == "2010s" & year %% 10 == 0 ~ 20,
    
    
    decade == "2010s" & year %% 10 <= 5 ~ 22,
    decade == "2010s" & year %% 10 > 5 ~ 23,
    decade == "2020s" & year %% 10 == 0 ~ 23,
    
    
    decade == "2020s" & year %% 10 <= 5 ~ 25,
    decade == "2020s" & year %% 10 > 5 ~ 28,
      ))


triangle_data <- tibble(x = c(11, 13, 14, 16),
                        y = c(8, 7, 7, 8))

subtitle_text <- str_wrap("\nObjects are defined here as satellites,
probes, landers, crewed spacecrafts, and space station flight elements 
launched into Earth orbit or beyond. The  annual number of objects 
launched into outer space shows that there is an increasing trend.", 90)

p <- ggplot(space_data) +
  annotate("rect", xmin = 1.5, xmax = 25.5,
           ymin = 0.5, ymax = 5.5, fill ="grey20") +
  geom_rect(aes(xmin = x - 0.4, xmax = x + 0.4,
                ymin = y - 0.4, ymax = y + 0.4, fill = n)) +
  scale_fill_gradient(low = "#D6EAF8",
                      high = "#1B4F72") +
  geom_text(aes(x = x, y = y, label = year),
            family = "sg", size = 5, hjust = 0.5) +
  annotate("rect", xmin = 12.5, xmax = 14.5,
           ymin = 0, ymax = 6.5, fill ="#8CA9AF") +
  # Lower Part
  annotate("rect", xmin = 12.5, xmax = 14.5,
           ymin = 6, ymax = 6.5, fill ="#EDE072") +
  annotate("rect", xmin = 12.5, xmax = 14.5,
           ymin = 0, ymax = -0.5, fill ="#EDE072") +
  annotate("rect", xmin = 13, xmax = 14,
           ymin = -0.5, ymax = -0.75, fill = "#8CA9AF") +
  annotate("rect", xmin = 13, xmax = 14,
           ymin = -0.75, ymax = -1, fill ="#EDE072") +
  # Upper Part
  annotate("rect", xmin = 12.75, xmax = 14.25,
           ymin = 6.5, ymax = 6.75, fill = "#8CA9AF") +
  annotate("rect", xmin = 12.75, xmax = 14.25,
           ymin = 6.75, ymax = 7, fill ="#EDE072") +
  geom_polygon(triangle_data,
               mapping = aes(x = x, y = y),
                             fill = "#8CA9AF") +
  annotate("rect", xmin = 11, xmax = 16,
           ymin = 8, ymax = 8.25, fill ="#EDE072") +
  annotate("point", x = 13.5, y = 9, color = "#8CA9AF",
           size = 5) +
  annotate("segment", x = 13.5, xend = 13.5,
                   y = 8.25, yend = 9, color = "#8CA9AF",
           linewidth = 1.5) +
  annotate("segment", x = 12.5, xend = 13.5,
           y = 8.25, yend = 9, color = "#8CA9AF",
           linewidth = 1.5) +
  annotate("segment", x = 14.5, xend = 13.5,
           y = 8.25, yend = 9, color = "#8CA9AF",
           linewidth = 1.5) +
  # half circle
  annotate("curve", x = 12, xend = 15, 
           y = 9.5, yend = 9.5, color = "#8CA9AF", size = 2,
           curvature = -0.2) +
  annotate("curve", x = 11, xend = 16, 
           y = 10, yend = 10, color = "#8CA9AF", size = 2,
           curvature = -0.2) +
  annotate("curve", x = 10, xend = 17, 
           y = 10.5, yend = 10.5, color = "#8CA9AF", size = 2,
           curvature = -0.2) +
 # annotate("point", x = 13.5, y = 2, color = "#EDE072", size = 8) +
#  annotate("point", x = 13.5, y = 4, color = "#EDE072", size = 8) +
  scale_y_continuous(expand = c(0, 0), limits = c(-2, 11.5)) +
  coord_cartesian() +
  labs(fill = "Object\nCount",
       title = "Objects Launched into Space",
       subtitle = subtitle_text,
       caption = "Data Source: Our World in Data\nTidyTuesday 2024 - Week 17 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "grey80", color = NA),
        plot.background = element_rect(fill = "grey80", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.5, 0.35),
        legend.background = element_rect(fill = "#8CA9AF"),
        legend.text = element_text(family = "sg", size = 15),
        legend.title = element_text(family = "sg", size = 18),
        legend.key.height = unit(1.2, "cm"),
        plot.title = element_text(family = "sg", hjust = 0, 
                                  size = 45),
        plot.subtitle = element_text(family = "sg", hjust = 0, 
                                  size = 25),
        plot.caption = element_text(family = "sg", hjust = 1, 
                                  size = 20),
        plot.margin = margin(0.5, 1.5, 0.5, 1.5, "cm"))



# Save the Plot

ggsave("Week17.png", p, width = 22, height = 15, dpi = 72) 
  

  
  
  
  
  

  
      