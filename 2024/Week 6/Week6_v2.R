heritage_data <- heritage %>%
  clean_names() %>%
  pivot_longer(cols = -country, values_to = "frequency",
               names_to = "year") %>%
  arrange(year, frequency) %>%
  group_by(year) %>%
  mutate(y = 1:3) %>%
  ungroup() %>%
  mutate(year = as.factor(year),
         year = str_replace_all(year, "x", ""))


subtitle_text <- str_wrap("The number of UNESCO World Heritage sites
is the highest in Sweden in both years,
but the maximum increase occured in Denmark (150%).\n\n\n", 70)

p2 <- ggplot(heritage_data) +
  geom_text(aes(x = 0, y = y, label = paste0(country, " (", frequency,
                ")"), alpha = frequency),
            color = "white", size = 20) +
  facet_wrap(~year) +
  labs(title = "Heritage Sites: 2004 vs 2022\n",
       subtitle = subtitle_text,
       caption = "Data Source: UNESCO World Heritage Sites\nTidyTuesday 2024 - Week 6 | Prepared by: C. YAZICI") +
  scale_x_continuous(limits = c(-0.3, 0.3)) +
  scale_y_continuous(limits = c(0, 3.5)) +
  annotate("segment", x = Inf, xend = -Inf,y = Inf, yend = Inf,
           color = "white", lwd = 3) +
  theme(panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(colour = "white", size = 55),
        plot.title = element_text(colour = "white", size = 60), 
        plot.subtitle = element_text(colour = "white", size = 29), 
        plot.caption = element_text(colour = "white", hjust = 1, 
                                    size = 25), 
        panel.spacing = unit(3, "lines"),
        plot.margin = margin(1.2, 1.0, 0.5, 1.0, "cm"),
        aspect.ratio = 9/9) 

# Save the Plot

ggsave("Week6_v2.png", p2, width = 20, height = 15, dpi = 72)

