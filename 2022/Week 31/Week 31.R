
library(tidyverse)
remotes::install_github("nrennie/usefunc")
library(usefunc)
library(showtext)

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 31)
frogs <- tuesdata$frogs


# library to use fontawesome icons
font_add('fa-regular', 'Font Awesome 6 Free-Regular-400.otf')

font_add_google('Manrope', 'manrope')
showtext_auto()

# Data Wrangling

data_frog <- frogs %>%
  select(Detection, Female) %>%
    group_by(Female, Detection) %>%
    summarise(cnt = n()) %>%
    mutate(freq = round(cnt / sum(cnt), 2),
           percent_detect = round(freq * 100)) %>% 
    arrange(desc(freq)) %>%
  ungroup() %>%
  pivot_longer(cols = 5, values_to = "perc", names_to = "type") %>%
  mutate(perc = ifelse(Female == 1 & Detection == "No visual", 82, perc)) %>%
  arrange(Female) %>%
  mutate(Female = ifelse(Female == 0, "Female", "Male"))

# Data Preparation for plot

plot_data <- rep_df(expand.grid(x = rep(1:10), y = rep(1:10)), length(unique(data_frog$Female))) %>%
  mutate(gender = rep(unique(data_frog$Female), each = 100),
         label = "<span style='font-family:fa-solid'>&#xf52e;</span>", 
         type = rep(data_frog$Detection, times = data_frog$perc))

# Subtitle

subtitle_text <- "Radio-telemetry has been used to study late-season movement and habitat use by Oregon spotted frogs at Crane Prairie Reservoir in Oregon<br>
between September and late November of 2018. 87 female and 224 male frogs were found. The percentage of 
<span style='color:red'>captured,
<span style='color:green'>visual and
<span style='color:grey80'>no visual <span style='color:black'>types<br>of detections showed that for both genders a high percentage of no visual type occured."

# Plot

p <- ggplot() +
  geom_richtext(data = plot_data,
                mapping = aes(x = x,
                              y = y,
                              label = label,
                              colour = type),
                family = 'fontawesome-webfont', size = 12, fill = NA, label.colour = NA) +
  scale_colour_manual("", values = c("red", "grey80", "green")) +
  facet_wrap(~gender) + 
  labs(title = "Oregon Spotted Frog",
       subtitle = subtitle_text,
       caption = "Data Source: USGS | TidyTuesday 2022 - Week 31 | Prepared by: @Cyd_yzc") +
  theme(panel.background = element_rect(fill = "#9B7653", color = NA),
        plot.background = element_rect(fill = "#9B7653", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(family = "manroto", hjust = 0, size = 30),
        plot.subtitle = element_markdown(family = "manroto", size = 20),
        plot.caption = element_text(family = "manroto", hjust = 1, size = 18),
        legend.position = "none",
        strip.background = element_rect(fill = "#9B7653", color = "#9B7653"),
        strip.text = element_text(family = "manroto", size = 25),
        panel.spacing = unit(3, "lines"),
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"))

ggsave("Week31_2022.png", p, width = 25, height = 12, dpi = 72)


