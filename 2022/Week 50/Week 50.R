# Data Import

state_retail <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/state_retail.csv',  col_types = "cciciiccc")
coverage_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/coverage_codes.csv')

install.packages("CGPfunctions")
library(CGPfunctions)
library(tidyverse)
library(lubridate)
library(statebins)
library(cowplot)
library(PrettyCols)
library(showtext)

font_add_google("Poppins", "pp")
showtext_auto()

# Data Wrangling

dt_all <- state_retail %>%
filter(state_abbr != "USA",
       year != 2022,
       subsector != "total") %>%
  mutate(
    ym = ymd(paste0(year, "-", month, "-01")),
    change_yoy = as.numeric(change_yoy),
    change_yoy_se = as.numeric(change_yoy_se),
    year = as.factor(year),
    subsector = as.factor(subsector)) %>%
  drop_na() %>%
  group_by(year, subsector) %>%
  summarise(mean_subsector = round(mean(change_yoy)), 1) %>%
  ungroup()

# The first plot

slope_graph <- newggslopegraph(dataframe = dt_all,
                Times = year,
                Measurement = mean_subsector,
                Grouping = subsector,
                Title = "Average year-over-year percent change in retail sales value",
                SubTitle = "\nThe Monthly State Retail Sales (MSRS) is the Census Bureau's new experimental data product featuring modeled state-level retail sales.\n
                The percent change in 2019 and 2020 can take negative values, however it is positive and higher in 2021.",
                Caption = "",
                TitleTextSize = 25,
                SubTitleTextSize = 15,
                LineThickness = 3,
                DataTextSize = 5,
                YTextSize = 5,
                XTextSize = 15,
                DataLabelPadding = 0.5,
                DataLabelLineSize = 0.5,
                DataLabelFillColor = "#F4CE58") +
  theme(plot.title = element_text(family = "pp", hjust = 0.5, size = 20),
        plot.subtitle = element_text(family = "pp", hjust = 0.5, size = 15),
        plot.background = element_rect(fill = "#FCF8D7", colour = "#FCF8D7"),
        panel.background = element_rect(fill = "#FCF8D7", colour = "#FCF8D7"),
        axis.title = element_blank(),
        axis.text.x = element_text(family = "pp", size = 25))

# Year 2019

dt_2019 <- state_retail %>%
  filter(state_abbr != "USA",
         year == 2019,
         subsector != "total") %>%
  mutate(
    ym = ymd(paste0(year, "-", month, "-01")),
    change_yoy = as.numeric(change_yoy),
    change_yoy_se = as.numeric(change_yoy_se),
    year = as.factor(year),
    subsector = as.factor(subsector)) %>%
  drop_na() %>%
  group_by(state_abbr) %>%
  summarise(mean_subsector = round(mean(change_yoy)), 1) %>%
  ungroup()

p1 <- ggplot(dt_2019, aes(state = state_abbr, fill = mean_subsector)) + 
  geom_statebins(border_col = "#DC3D0B", lbl_size = 4, family = "pp") +
  scale_fill_pretty_c("Tangerines",
                      direction = -1) +
  labs(fill = "Average %\nchange in\nretail sales\nvalue\n",
       title = "2019") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(family = "pp"),
        legend.background = element_rect(fill = "#FCF8D7"),
        plot.background = element_rect(fill = "#FCF8D7"),
        panel.background = element_rect(fill = "#FCF8D7"),
        plot.title = element_text(family = "pp", hjust = 0.5, size = 15))

# Year 2020

dt_2020 <- state_retail %>%
  filter(state_abbr != "USA",
         year == 2020,
         subsector != "total") %>%
  mutate(
    ym = ymd(paste0(year, "-", month, "-01")),
    change_yoy = as.numeric(change_yoy),
    change_yoy_se = as.numeric(change_yoy_se),
    year = as.factor(year),
    subsector = as.factor(subsector)) %>%
  drop_na() %>%
  group_by(state_abbr) %>%
  summarise(mean_subsector = round(mean(change_yoy)), 1) %>%
  ungroup()


 p2 <- ggplot(dt_2020, aes(state = state_abbr, fill = mean_subsector)) + 
  geom_statebins(border_col = "#DC3D0B", lbl_size = 4) +
  scale_fill_pretty_c("Tangerines",
                      direction = -1) +
   labs(fill = "Average %\nchange in\nretail sales\nvalue\n",
        title = "2020") +
   theme(panel.grid = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         legend.title = element_text(family = "pp"),
         legend.background = element_rect(fill = "#FCF8D7"),
         plot.background = element_rect(fill = "#FCF8D7"),
         panel.background = element_rect(fill = "#FCF8D7"),
         plot.title = element_text(family = "pp", hjust = 0.5, size = 15))

# Year 2021

dt_2021 <- state_retail %>%
  filter(state_abbr != "USA",
         year == 2021,
         subsector != "total") %>%
  mutate(
    ym = ymd(paste0(year, "-", month, "-01")),
    change_yoy = as.numeric(change_yoy),
    change_yoy_se = as.numeric(change_yoy_se),
    year = as.factor(year),
    subsector = as.factor(subsector)) %>%
  drop_na() %>%
  group_by(state_abbr) %>%
  summarise(mean_subsector = round(mean(change_yoy)), 1) %>%
  ungroup()

p3 <- ggplot(dt_2021, aes(state = state_abbr, fill = mean_subsector)) + 
  geom_statebins(border_col = "#DC3D0B", lbl_size = 4) +
  scale_fill_pretty_c("Tangerines",
                      direction = -1) +
  labs(fill = "Average %\nchange in\nretail sales\nvalue\n",
       title = "2021") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(family = "pp"),
        legend.background = element_rect(fill = "#FCF8D7"),
        plot.background = element_rect(fill = "#FCF8D7"),
        panel.background = element_rect(fill = "#FCF8D7"),
        plot.title = element_text(family = "pp", hjust = 0.5, size = 15))

# Combination

plots <- align_plots(slope_graph, p1, p2, p3)

   
caption <- ggdraw() + 
  draw_label(
    "Data Source: United States Census Bureau's Monthly State Retail Sales | TidyTuesday 2022 - Week 50 | \nPrepared by: @Cyd_yzc", 
    fontface = 'bold',
    fontfamily = "pp",
    hjust = 0,
    x = 0.5,
    y = 0.5, 
    size = 12,
    color = "#723608"
  ) 

bottom_row <- plot_grid(
  plots[[2]], plots[[3]], plots[[4]],
  labels = "",
  rel_widths = c(0.5, 0.5, 0.5), 
  nrow = 1
)

final_plot <- plot_grid(slope_graph, bottom_row, caption, labels = "", ncol = 1,
                        rel_heights = c(0.8, 0.5, 0.1)) +
  theme(plot.background = element_rect(fill = "#FCF8D7", colour = "#FCF8D7"))

final_plot
ggsave("Week50_2022.png", final_plot, width = 25, height = 12, dpi = 72)

