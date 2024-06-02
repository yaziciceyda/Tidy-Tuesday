library(stringr)
library(tidyverse)
library(ggchicklet)
library(showtext)
library(cowplot)

# Font in the Plot

font_add_google('Geologica', 'ge', db_cache = FALSE)
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2024, week = 18)

wwbi_data <- tuesdata$wwbi_data
wwbi_series <- tuesdata$wwbi_series
wwbi_country <- tuesdata$wwbi_country


ind_data <- wwbi_series %>%
  filter(str_detect(indicator_name, "Medical | Health"))
# BI.PWK.AGES.PB.HE.MD
# Median age of public paid employees, by industry: Health 
# BI.PWK.AGES.PB.MW.MD
# Median age of public paid employees, by occupation: Medical workers
# BI.PWK.AGES.PV.HE.MD
# Median age of private paid employees, by industry: Health
# BI.PWK.AGES.PV.MW.MD
# Median age of private paid employees, by occupation: Medical workers

# Health

final_data_health <- wwbi_data %>%
  filter(indicator_code %in% c("BI.PWK.AGES.PB.HE.MD",
                              "BI.PWK.AGES.PV.HE.MD")) %>%
  group_by(indicator_code, year) %>%
  summarise(avg_age = mean(value)) %>%
  ungroup() %>%
  mutate(indicator_code = ifelse(indicator_code == "BI.PWK.AGES.PB.HE.MD",
                                  "Public", "Private")) %>%
  group_by(year) %>%
  mutate(min_age = min(avg_age),
         max_age = max(avg_age)) %>%
  ungroup()



p1 <- ggplot(final_data_health) +
  geom_segment(aes(x = year, xend = year,
                             y = min_age, yend = max_age)) +
  ggchicklet:::geom_rrect(aes(xmin = year - 0.45, xmax = year  + 0.45,
                            ymin = avg_age - 0.5, ymax = avg_age + 0.5,
                            fill = indicator_code),
                           radius = unit(0.5, units = "cm")) +
  geom_text(aes(x = year, y = avg_age, 
                label = paste0(round(avg_age, 2))),
            family = "ge", size = 5) +
  scale_fill_manual(values = c("#86F4E5", "#1EB9A4")) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2019)) +
  labs(x = "",
       y = "Median Age",
       fill = "",
       title = "Annual Median age of employees, by industry: Health") +
  theme(panel.background = element_rect(fill = "#fafafa", color = NA),
        plot.background = element_rect(fill = "#fafafa", color = NA),
        panel.grid = element_blank(),
        axis.title = element_text(family = "ge", size = 17),
        axis.text = element_text(family = "ge", size = 15, hjust = 0.5),
        axis.ticks = element_blank(),
        legend.text.position = "top",
        legend.position = "top",
        legend.background = element_rect(fill = "#fafafa"), 
        legend.text = element_text(family = "ge", size = 12),
        legend.key.width = unit(2, "cm"),
        plot.title = element_text(family = "ge", hjust = 0.5,
                                  size = 20)) 
  
# Medical Workers

final_data_medical <- wwbi_data %>%
  filter(indicator_code %in% c("BI.PWK.AGES.PB.MW.MD",
                               "BI.PWK.AGES.PV.MW.MD")) %>%
  group_by(indicator_code, year) %>%
  summarise(avg_age = mean(value)) %>%
  ungroup() %>%
  mutate(indicator_code = ifelse(indicator_code == "BI.PWK.AGES.PB.MW.MD",
                                 "Public", "Private")) %>%
  group_by(year) %>%
  mutate(min_age = min(avg_age),
         max_age = max(avg_age)) %>%
  ungroup()



p2 <- ggplot(final_data_medical) +
  geom_segment(aes(x = year, xend = year,
                   y = min_age, yend = max_age)) +
  ggchicklet:::geom_rrect(aes(xmin = year - 0.45, xmax = year  + 0.45,
                              ymin = avg_age - 0.5, ymax = avg_age + 0.5,
                              fill = indicator_code),
                          radius = unit(0.5, units = "cm")) +
  geom_text(final_data_medical %>%
              filter(year != 2019),
            mapping = aes(x = year, y = avg_age, 
                label = paste0(round(avg_age, 2))),
            family = "ge", size = 5) +
  scale_fill_manual(values = c("#86F4E5", "#1EB9A4")) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2019)) +
  annotate("text", x = 2019, y = 35.6, label = "35.33",
           family = "ge", size = 5) +
  annotate("text", x = 2019, y = 35, label = "35",
           family = "ge", size = 5) +
  labs(x = "",
       y = "Median Age",
       title = "Annual Median age of employees, by occupation: Medical workers") +
  theme(panel.background = element_rect(fill = "#fafafa", color = NA),
        plot.background = element_rect(fill = "#fafafa", color = NA),
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "ge", size = 17, 
                                    margin = margin(r = 20)),
        axis.text = element_text(family = "ge", size = 15, hjust = 0.5),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "ge", hjust = 0.5,
                                  size = 20))



# The Final Plot

plots <- align_plots(p1, p2, align = "v")

title1 <- ggdraw() +
  draw_label(
    "Worldwide Bureaucracy Indicators\n",
    fontface = 'bold',
    fontfamily = "ge",
    hjust = 0.5,
    x = 0.5,
    size = 40)

subtitle_text <- str_wrap("\nThe Worldwide Bureaucracy Indicators 
(WWBI) database is a unique cross-national dataset on public sector 
employment and wages that aims to fill an information gap. Here, the 
average median age of employees, by industry (Health) and
by occupation (Medical workers) shows that private employees are
younger. But, the difference between the median ages decreased in
2019.", 90)

title2 <- ggdraw() +
  draw_label(subtitle_text,
             fontface = 'bold',
             fontfamily = "ge",
             hjust = 0,
             x = 0.05,
             y = 0.7,
             size = 20)

caption <- ggdraw() +
  draw_label(
    "Data Source: World Bank | TidyTuesday 2024 - Week 18 | Prepared by: C. YAZICI",
    fontface = 'bold',
    fontfamily = "ge",
    hjust = 0.5,
    x = 0.5,
    y = 0.5,
    size = 18)

top_row <-  plot_grid(
  plots[[1]], plots[[2]],
  labels = "",
  rel_heights = c(1.0, 1.0),
  nrow = 2
)


final_plot <- plot_grid(title1, title2, top_row, caption,
                        labels = "", ncol = 1,
                        rel_heights = c(0.1, 0.1, 1.0, 0.1),
                        align = "hv") +
  theme(plot.background = element_rect(fill = "#fafafa", colour = NA),
        plot.margin = margin(1.0, 1.0, 0.5, 0.5, "cm"))

# final_plot

# Save the Plot

ggsave("Week18.png", final_plot, width = 20, height = 18, dpi = 72)









