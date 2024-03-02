poll2 <- poll %>%
  filter(year == 2020) %>%
  arrange(rank)

library(tidyverse)
library(igraph)
library(showtext)


# font_add_google("DM Serif Display", "sd")
# showtext_auto()

axios_poll <- tibble(from = c("2022", "2022", "2022",
                              "2. HEB Grocery - Retail\nRQ Score: 82", "2. HEB Grocery - Retail\nRQ Score: 82", "2. HEB Grocery - Retail\nRQ Score: 82", 
                              "2021", "2021", "2021", 
                              "2. Honda Motor Company - Automotive\nRQ Score: 81.6", "2. Honda Motor Company - Automotive\nRQ Score: 81.6", "2. Honda Motor Company - Automotive\nRQ Score: 81.6", 
                              "2020", "2020", "2020"),
                     to = c("3. Patagonia - Retail\nRQ Score: 81.8", "2. HEB Grocery - Retail\nRQ Score: 82", "1. Trader Joe's - Retail\nRQ Score: 82.4",
                            "2021", "2021", "2021",
                            "2. Honda Motor Company - Automotive\nRQ Score: 81.6", "4. Chick-fil-A - Food & Beverage\nRQ Score: 81.4", "1. Patagonia - Retail\nRQ Score: 82.7",
                            "2020", "2020", "2020", 
                            "6. Wegmans - Food & Beverage\nRQ Score: 80.4", "4. Publix Supermarkets - Food & Beverage\nRQ Score: 81.2", "3. Amazon.com - Ecommerce\nRQ Score: 81.4"))

g = graph_from_data_frame(axios_poll, directed = TRUE)
coords = layout_as_tree(g)
colnames(coords) = c("x", "y")

output_df = as_tibble(coords) %>%
  mutate(step = vertex_attr(g, "name"),
         x = x*-1,
         type = factor(c(1, 2, 1, 2, 1, 2, 2, 2, 2,
                         2, 2, 2)),
         type2 = factor(c(1, 3, 1, 4, 1, 3,
                        3, 5, 3, 2, 2, 6)),
         label = step)
output_df[2,1] <- 0
output_df[6,1] <- 1
output_df[3,1] <- 0
output_df[9,1] <- -1
output_df[8,1] <- 0
output_df[4,1] <- 1
output_df[4,1] <- 0
output_df[8,1] <- 1
output_df[5,1] <- 0
output_df[12,1] <- -1
output_df[11,1] <- 0
output_df[10,1] <- 1
plot_nodes = output_df %>%
  mutate(xmin = x - 0.45,
         xmax = x + 0.45,
         ymin = y - 0.35,
         ymax = y + 0.35)

plot_edges = goldilocks %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c("from", "to"),
               names_to = "s_e",
               values_to = "step") %>%
  left_join(plot_nodes, by = "step") %>%
  select(-c(label, type2, y, xmin, xmax)) %>%
  mutate(y = ifelse(s_e == "from", ymin, ymax)) %>%
  select(-c(ymin, ymax)) 

plot_nodes <- plot_nodes %>%
  mutate(size_text = c(15, 8, 15, 8, 15, 8, 
                       8, 8, 8, 8, 8, 8))
p <-  ggplot() +
  geom_rect(data = plot_nodes,
            mapping = aes(xmin = xmin,
                          ymin = ymin,
                          xmax = xmax,
                          ymax = ymax,
                          fill = type,
                          colour = type),
            alpha = 0.5,
            linejoin = "round") +
  geom_text(data = plot_nodes,
            mapping = aes(x = x,
                          y = y,
                          label = label,
                          size = size_text - 3,
                          fontface = "bold"),
            family = "sd",
            color = "#585c45") +
  geom_path(data = plot_edges,
            mapping = aes(x = x,
                          y = y,
                          group = id),
            colour = "#585c45",
            arrow = arrow(length = unit(0.2, "cm"),  type = "closed"), size = 1) +
  scale_fill_manual(values = c("#EC541F", "#9E84F7")) +
  scale_colour_manual(values = c("#EC541F", "#9E84F7"))+
   scale_size_identity() +
  labs(title = "2022 Axios-Harris Poll",
       subtitle = "\nThe top three companies with their RQ Score for the last three years \nare given. The data does not include the first and the second\ncompanies in 2020 and third company in 2021.",
       caption = "Data Source: Axios-Harris Poll | TidyTuesday 2022 - Week 22 | Prepared by: @Cyd_yzc") +
  theme_void() +
  theme( plot.margin = unit(c(1, 5, 0.5, 5), "cm"),
        legend.position = "none",
        plot.background = element_rect(colour = "#DDF3F2", fill = "#DDF3F2"),
        panel.background = element_rect(colour = "#DDF3F2", fill = "#DDF3F2"),
        plot.title = element_text(family = "sd", hjust = 0.5, face = "bold",
                                  size = 40, color = "#585c45",
                                  margin = margin(t = 5, r = 0, b = 5, l = 10)),
        plot.caption = element_text(family = "sd", hjust = 1,
                                    size = 13, color = "#585c45"),
        plot.subtitle = element_text(family = "sd", hjust = 0, face = "bold",
                                     size = 30, color = "#585c45")) 
 

ggsave("Week22_2022.png", p, width = 20, height = 10, dpi = 72)
