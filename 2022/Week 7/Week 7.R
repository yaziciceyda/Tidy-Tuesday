library(tidyverse)
library(ggforce)
library(ggchicklet)
library(showtext)

# The Font in the Plot

font_add_google(name = "Cormorant Garamond", family = "corgar")
showtext_auto()

# Data Import

illiteracy <- readr::read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge06/data.csv')

# Data Wrangling

data_ill <- illiteracy %>%
  rename(ill_rate = `Iliteracy Rate`) %>%
  add_row(Date = 1950, ill_rate = 0) %>%
  mutate(y = rescale(ill_rate, to = c(0, 10)),
         xnew = ill_rate - 5)

data_ill2 <- data_ill %>% 
  filter(ill_rate < 99 & ill_rate > 0)

# The Plot

p <- ggplot(data_ill) +
  geom_segment(aes(x = ill_rate, xend = ill_rate,
                   y = 0, yend = y), linewidth = 8) +
  scale_x_reverse(breaks = data_ill$ill_rate[1:5], 
                  labels = sprintf("%s%%", as.character(data_ill$ill_rate[1:5]))) +
  geom_segment(data_ill %>% 
               filter(ill_rate > 0), 
               mapping = aes(x = 100, xend = ill_rate,
               y = y, yend = y), linewidth = 8, colour = "black", 
               lineend = 'round') +
  geom_segment(data_ill %>% 
                 filter(ill_rate > 0), 
               mapping = aes(x = 99.9, xend = ill_rate,
                             y = y, yend = y), linewidth = 6, 
               colour = "#D5C3AF", lineend = 'round') +
  geom_rect(data_ill %>% 
              filter(ill_rate > 0), 
            mapping = aes(xmin = 101, xmax = 99, ymin = y - 0.12, ymax = y + 0.12),
            color = "black", fill = "#D5C3AF", size = 1.4) +
  geom_segment(data_ill %>% 
                 filter(ill_rate > 0), 
               mapping = aes(x = 99, xend = 99,
                             y = y - 0.11, yend = y + 0.1), linewidth = 2, 
               colour = "#D5C3AF") +
  geom_polygon(data.frame(x = c(98.45, 98.45, 99.8), 
                              y = c(10.07, 9.8, 9.8)), 
                              mapping = aes(x = x, y = y), fill = "black") +
  geom_polygon(data.frame(x = c(91.58, 91.58, 92.9), 
                         y = c(9.37, 9.18, 9.18)), 
               mapping = aes(x = x, y = y), fill = "black") +
  geom_polygon(data.frame(x = c(81.1, 81.1, 82.4), 
                          y = c(8.31, 8.08, 8.08)), 
               mapping = aes(x = x, y = y), fill = "black") +
  geom_polygon(data.frame(x = c(66.65, 66.65, 68), 
                          y = c(6.9, 6.66, 6.66)), 
               mapping = aes(x = x, y = y), fill = "black")  +
  geom_polygon(data.frame(x = c(49.45, 49.45, 50.8), 
                          y = c(5.16, 4.92, 4.92)), 
               mapping = aes(x = x, y = y), fill = "black") +
  scale_y_continuous(breaks = data_ill$y[1:5], 
                   labels = sprintf(as.character(data_ill$Date[1:5]))) +
  labs(y = "",
       x = "", 
       title = "ILLITERACY.") +
annotate(geom = "text", x = 105, y = 0.5, label = "PERCENT OF\nILLITERACY",
         family = "Courier", size = 5) +
  theme(panel.background = element_rect(fill = "#D5C3AF", color = NA),
        plot.background = element_rect(fill = "#D5C3AF", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(hjust = 50, family = "Courier", size = 20),
        axis.text.x = element_text(family = "Courier", size = 20),
        plot.title = element_text(hjust = 0.5, family = "Courier", size = 30),
        plot.margin = margin(0.5, 5, 0.5, 0.5, "cm")) 

p

# Save the Plot

ggsave("Week7.png", p, width = 25, height = 12, dpi = 72)






