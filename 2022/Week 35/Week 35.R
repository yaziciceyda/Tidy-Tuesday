
library(dplyr)
library(janitor)
library(statebins)
library(PrettyCols)
library(showtext)

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 35)
pell <- tuesdata$pell

# The font in the plot

font_add_google("Poppins", "pp")
showtext_auto()

# Data Wrangling 

pell_data <- pell %>%
  clean_names() %>%
  drop_na() %>%
  filter(award != 0) %>%
  mutate(award_per_recipient = award / recipient) %>%
  group_by(state) %>%
  summarize(mean_apr = mean(award_per_recipient)) %>%
  filter(!state %in% c("VI", "PR")) %>%
  ungroup()

# The Plot

final_plot <- ggplot(pell_data, aes(state = state, fill = mean_apr)) + 
  geom_statebins(border_col = "#357783", lbl_size = 9, family = "pp") +
    scale_fill_pretty_c("Teals",
                        direction = -1) +
  labs(title = "Pell Grants",
       fill = "Average award\nper recipient ($)",
       subtitle = "Average award per recipient ($) for each state between 1999 - 2017",
       caption = "Data Source:  US Department of Education | TidyTuesday 2022 - Week 35 | Prepared by: @Cyd_yzc") +
      theme(plot.background = element_rect(fill = "#F8FA9C"),
            panel.background = element_rect(fill = "#F8FA9C"),
  panel.grid = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  plot.title = element_text(family = "pp", size = 30),
  plot.subtitle = element_text(family = "pp", size = 20),
  plot.caption = element_text(family = "pp", size = 18),
  legend.background = element_rect(fill = "#F8FA9C"),
  legend.title = element_text(family = "pp", size = 13),
  legend.text = element_text(family = "pp", size = 13))

# Save the Plot

ggsave("Week35_2022.png", final_plot, width = 25, height = 12, dpi = 72)




