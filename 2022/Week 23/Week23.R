tuesdata <- tidytuesdayR::tt_load(2022, week = 23)

install.packages("ggpol")
library(ggpol)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)

font_add_google("PT Serif", "an")
showtext_auto()

pride <- tuesdata$pride_aggregates %>%
  filter(Company != "Grand Total") %>%
  summarise(mean_cont = mean(`Total Contributed`),
            mean_nr_politicians = mean(`# of Politicians Contributed to`))
  
sbtl <- "\nOn the average, the 30 Pride sponsors donate to 11 politicians with an amount of $53,481 million\n\nto the Anti-LQBTQ Campaigns. The 6 companies with donations to the largest number of politicians are\n\npresented here. Each dot represent the politician that they have donated."

pride <- tuesdata$pride_aggregates 
pride <- pride %>%
  filter(Company != "Grand Total") %>%
  rename("Total_contribution" = `Total Contributed`,
         "Nr_politicians" = `# of Politicians Contributed to`,
         "Nr_states" = `# of States Where Contributions Made`)%>%
  mutate(Cont_per_politician = Total_contribution/Nr_politicians) %>%
  arrange(desc(Nr_politicians)) %>%
  slice(1:6) %>%
  mutate(colors = c("#E50000", "#FF8D00", "#FFEE00", "#028121",
            "#004CFF", "#770088"),
         label = c("AT&T - Total Contribution of $307,137.5 in 6 States", 
                   "Comcast - Total Contribution of $121,350 in 4 States", 
                   "State Farm - Total Contribution of $79,550 in 3 States", 
                   "Enterprise - Total Contribution of $41,300 in 6 States",
                   "FedEx - Total Contribution of $79,700 in 2 States", 
                   "Budweiser - Total Contribution of $45,250 in 4 States"),
         x = c(2.8, 1.7, -1.5, -2.2, -2.8, -1),
         y = c(2, 2.9, 2.95, 2.5, 1.9, 0.2),
         hjust = rep(0.5, 6),
         vjust = rep(0.5, 6)) 

p <- ggplot(pride) + 
  geom_parliament(aes(seats = Nr_politicians, fill = Company), color = "#FFFFF0") + 
  scale_fill_manual(values = pride$colors, labels = pride$Company) +
  geom_textbox(aes(label = label, x = x, y = y, hjust = hjust, vjust = vjust,
                   colour = colors), fill = "#FFFFF0", family = "an", size = 4) +
  scale_colour_identity() +
  theme(text = element_text(family = "an"),
    panel.background = element_rect(fill = "#FFFFF0"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
    plot.caption = element_text(size = 15),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 18),
    plot.margin = unit(c(1, 5, 0.5, 1), "cm")) +
  labs(x = "",
       y = "",
       title = "Pride sponsors who have donated to Anti-LQBTQ Campaigns",
       subtitle = sbtl,
       caption = "Data Source: Data For Progress | TidyTuesday 2022 - Week 23 | Prepared by: @Cyd_yzc") 


ggsave("Week23_2022.png", p, width = 20, height = 10, dpi = 72)

