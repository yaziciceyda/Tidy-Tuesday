tuesdata <- tidytuesdayR::tt_load(2022, week = 41)

yarn <- tuesdata$yarn

library(ggchicklet)
library(ggfx)
library(ggforce)

yarn_washable <- yarn %>%
  filter(machine_washable == TRUE,
         discontinued == FALSE,
         yarn_weight_name == "Sport",
         rating_count > 30) %>%
  drop_na(rating_average, yardage, max_gauge, min_gauge) %>%
  arrange(desc(rating_average)) %>%
  slice_max(rating_average, n = 5) %>%
  mutate(group_nr = row_number(),
         gauge_inc = (max_gauge - min_gauge)/min_gauge) %>%
  select(name, yarn_company_name, rating_average, yardage,
         max_gauge, min_gauge, rating_count, group_nr, gauge_inc) 


df_yarn <- map_dfr(1:nrow(yarn_washable), ~{
  tibble(
    x = 3*yarn_washable$gauge_inc[.x]*sin(seq(0, 2*pi, length = 200)) + 
      2.5*yarn_washable$group_nr[.x],
    
    y = 3*yarn_washable$gauge_inc[.x]*cos(seq(0, 2*pi, length = 200)) + 
      yarn_washable$rating_average[.x] - 3*yarn_washable$gauge_inc[.x],
    
    id = runif(200),
    yarn_company_name = yarn_washable$yarn_company_name[.x]
  )
}) %>%
  mutate(new_x =
    case_when(yarn_company_name == "Turtlepurl Yarns" ~ rescale(x, to = c(0, 0.1)),
      yarn_company_name == "Lollipop Yarn" ~ rescale(x, to = c(0.07, 0.5)),
      yarn_company_name == "Fibernymph Dye Works" ~ rescale(x, to = c(0.2, 0.3)),
      yarn_company_name == "Polka Dot Sheep" ~ rescale(x, to = c(0.3, 0.4)),
      yarn_company_name == "Tempted Hand Painted" ~ rescale(x, to = c(0.4, 0.5))),
    
    new_y =
      case_when(yarn_company_name == "Turtlepurl Yarns" ~ rescale(y, to = c(0, 3.75)),
                yarn_company_name == "Lollipop Yarn" ~ rescale(y, to = c(0, 3.30)),
                yarn_company_name == "Fibernymph Dye Works" ~ rescale(y, to = c(0, 3.28)),
                yarn_company_name == "Polka Dot Sheep" ~ rescale(y, to = c(0, 3.28)),
                yarn_company_name == "Tempted Hand Painted" ~ rescale(y, to = c(0, 2.25))),
    
    new_colour = 
      case_when(yarn_company_name == "Turtlepurl Yarns" ~ "red",
                yarn_company_name == "Lollipop Yarn" ~ "blue",
                yarn_company_name == "Fibernymph Dye Works" ~ "yellow",
                yarn_company_name == "Polka Dot Sheep" ~ "black",
                yarn_company_name == "Tempted Hand Painted" ~ "pink"),
    
   # y2 = y,
   y2 = -new_y) %>%
  # new_y = -y) %>%
   #      y = -y) %>%
   arrange(id) 



df_yarn_summary <- df_yarn %>%
  group_by(yarn_company_name) %>%
  summarise(min_x = min(new_x),
            max_x = max(new_x),
            min_y = min(new_y),
              max_y = max(new_y))


font_add_google("Dancing Script", "ds")

showtext_auto()

p <- df_yarn %>%
filter(yarn_company_name == "Turtlepurl Yarns") %>%
ggplot() +
  with_blur(geom_bspline0(aes(x, y, colour = "purple"), 
                              size = 0.6), sigma = 3) +
  with_blur(geom_bspline0(aes(x, y-2.75, colour = "purple"), 
                          size = 0.6), sigma = 3) +
 ggchicklet:::geom_rrect(aes(xmin=6.7, xmax=8.3, ymin=1.5, ymax=2.5), 
                          fill="#FFFFF0",
                          radius = unit(0.7, units = "cm")) +
  geom_text(aes(x = 7.5, y = 2), label = "Turtlepurl Yarns\n produces the longest, washable,\n sport yarn rated by\n more than 30 people", 
            family = "ds", color = "black", size = 8) +
  scale_colour_identity() +
  coord_cartesian(xlim = c(6, 9), ylim = c(-1, 5)) +
  labs(title = "\nRavelry Yarn",
       caption = "Data Source: ravelry.com | TidyTuesday 2022 - Week 41 | Prepared by: @Cyd_yzc") +
  theme_void() +
  theme(plot.title = element_text(family = "ds", size = 40, hjust = 0.5,
                                  colour = "#FFFFF0"),
        plot.caption = element_text(family = "ds", hjust = 1, size = 15,
                                    colour = "#FFFFF0"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"))


ggsave("Week41_2022.png", p, width = 12, height = 16, dpi = 72)
