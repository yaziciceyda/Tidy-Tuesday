
hidden_gems <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-26/hidden_gems.csv')

library(tidyverse)
library(calendR)
library(lubridate)
library(cowplot)
library(showtext)

# Font in the Plot

font_add_google('Golos Text', 'gt', db_cache = FALSE)
showtext_auto()


Sys.setlocale("LC_ALL", "English") 

# Data Wrangling

gems_data <- hidden_gems %>%
mutate(year = year(date),
       day =  as.numeric(strftime(date, format = "%j")))

# Function to calculate the freq of each day of a regular year

year_fn <- function(dt)
{
  tt <- table(dt$day)
  ttm <- as.matrix(tt)
  
  first = 1:365
  second = rep(0, 365)
  matrix_year <- cbind(first, second)
  
  for(i in 1:365)
  {
    for(j in 1:dim(ttm)[1])
      if(i == as.numeric(rownames(ttm)[j]))
        matrix_year[i, 2] = ttm[j]
  }
  return(matrix_year)
}

# Function to calculate the freq of each day of a leap year

leap_year_fn <- function(dt)
{
  tt <- table(dt$day)
  ttm <- as.matrix(tt)
  
  first = 1:366
  second = rep(0, 366)
  matrix_year <- cbind(first, second)
  
  for(i in 1:366)
  {
    for(j in 1:dim(ttm)[1])
      if(i == as.numeric(rownames(ttm)[j]))
        matrix_year[i, 2] = ttm[j]
  }
  return(matrix_year)
}

# The freq of 2020

data20 <- gems_data %>%
  filter(year == 2020)

# The Plot of 2020

p20 <- calendR(year = 2020,
               start = "M",
               special.days = leap_year_fn(data20)[,2],
               gradient = TRUE,
               special.col = "#EE39F1",
               low.col = "#F4BAF5")   

# The freq of 2021

data21 <- gems_data %>%
  filter(year == 2021)

# The Plot of 2021

p21 <- calendR(year = 2021,
               start = "M",
               special.days = year_fn(data21)[,2],
               gradient = TRUE,
               special.col = "#EE39F1",
               low.col = "#F4BAF5")                    

# The freq of 2022

data22 <- gems_data %>%
  filter(year == 2022)

# The Plot of 2022

p22 <- calendR(year = 2022,
               start = "M",
               special.days = year_fn(data21)[,2],
               gradient = TRUE,
               special.col = "#EE39F1",
               low.col = "#F4BAF5")                    


# The Final Plot

plots <- align_plots(p20, p21, p22,  align = 'r', axis = 'r')

title1 <- ggdraw() + 
  draw_label(
    "Kaggle Hidden Gems",
    fontface = 'bold',
    fontfamily = "gt",
    hjust = 0.5,
    x = 0.5,
    size = 30,
    color = "#EE39F1"
  )  
title2 <- ggdraw() + 
  draw_label(
    "The dates when the Hidden Gems episodes were published",
    fontface = 'bold',
    fontfamily = "gt",
    hjust = 0.5,
    x = 0.5,
    y = 0.4, 
    size = 20,
    color = "#EE39F1"
  ) 
caption <- ggdraw() + 
  draw_label(
    "Data Source: Kaggle | TidyTuesday 2022 - Week 17 | Prepared by: @Cyd_yzc", 
    fontface = 'bold',
    fontfamily = "gt",
    hjust = 0.5,
    x = 0.5,
    y = 0.5, 
    size = 18,
    color = "#EE39F1"
  ) 

bottom_row <- plot_grid(
  plots[[1]], plots[[2]],  plots[[3]],
  labels = "",
  rel_widths = c(1.0, 1.0, 2.0), 
  nrow = 2
)

final_plot <- plot_grid(title1, title2, bottom_row, caption, 
                        labels = "", ncol = 1,
                        rel_heights = c(0.1, 0.1, 2.5, 0.1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        plot.margin = unit(c(2.0, 0.5, 2.0, 0.5), "cm"))

final_plot

# Save the Plot

ggsave("Week17_2022.png", final_plot, width = 25, height = 15, dpi = 72)


