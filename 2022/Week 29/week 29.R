library(tidyverse)
library(gganimate)
library(showtext)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2022, week = 29)
technology <- tuesdata$technology

font_add_google('Kanit', 'kanit')
showtext_auto()

tech <- technology %>%
  filter(iso3c == "TUR",
         category == "Agriculture") %>%
  count(label)

tech <- technology %>%
  filter(iso3c == "TUR",
  category == "Agriculture",
  label %in%  c("Aggregate kg of fertilizer consumed", 
                "Area equipped to provide water to crops",
                "Milking machines in use",
                "Agricultural tractors in use")) %>%
  group_by(label) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(pct_change = round((value/lag(value) - 1) * 100))
 
subtitle_text <- "The % change in agricultural technology adoption in Turkey between 1961 - 2018 shows that<br>
<span style='color:#3939F0'>fertilizer consumed (kg) <span style='color:black'>has ups and downs,
<span style='color:#5DF039'>tractors in use <span style='color:black'>reached its constant level <br>around  1980s,
<span style='color:#F039A2'>area equipped to provide water to crops  <span style='color:black'> stayed constant after 2000 and<br>
<span style='color:#CC39F0'>milking machines in use <span style='color:black'> generally increased except for 1979."


p <- ggplot(tech, aes(year, pct_change, color = label)) +
  geom_line() +
  geom_point(aes(group = year), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey20") +
  scale_colour_manual("", values = c("#3939F0", "#5DF039", "#F039A2", "#CC39F0")) +
  labs(title = "Technology Adoption",
       caption = "Data Source: data.nber.org | TidyTuesday 2022 - Week 29 | Prepared by: @Cyd_yzc",
       color = "Agricultural Technology",
       x = "Year",
       y = "% Change",
       subtitle = subtitle_text) +
  theme(plot.caption = element_text(size = 19, hjust = 1, family = "kanit"),
        plot.title = element_text(family = "kanit", size = 40, hjust = 0),
        plot.subtitle = element_markdown(family = "kanit", size = 25),
        legend.text = element_text(family = "kanit", size = 20),
        legend.title = element_text(family = "kanit", size = 25),
        legend.key = element_rect(colour = "transparent", fill = "ivory"),
        legend.background = element_rect(fill = "ivory", color = NA),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "kanit", size = 15),
        axis.title = element_text(family = "kanit", size = 20),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
  guides(color = guide_legend(override.aes = list(size = 5))) 

ggsave("Week29_2022.png", p, width = 25, height = 12, dpi = 72)
