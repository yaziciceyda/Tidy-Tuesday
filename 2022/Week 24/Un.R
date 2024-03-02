
library(tidyverse)
library(ggh4x)
library(lubridate)


drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')

# clean

drought_cln <- 
  drought %>%
  janitor::clean_names() %>%
  # formatting
  mutate(date = str_remove(date,"d_"),
         date = ymd(date),
         state = str_to_title(state)) %>%
  # pivot
  pivot_longer(cols = c(d0:w4), names_to = "drought_code", values_to = "drought_level") %>%
  select(-x0) %>%
  # filter out
  filter(drought_level != 0,
         drought_code  != "x9") %>%
  # labels
  mutate(drought_label = case_when(drought_code == "d4" ~ "Exceptional Dry" ,
                                   drought_code == "d3" ~ "Extreme Dry"     ,
                                   drought_code == "d2" ~ "Severe Dry"      ,
                                   drought_code == "d1" ~ "Moderate Dry"    ,
                                   drought_code == "d0" ~ "Abnormally Dry"  ,
                                   drought_code == "w0" ~ "Abnormally Wet"  ,
                                   drought_code == "w1" ~ "Moderate Wet"    ,
                                   drought_code == "w2" ~ "Severe Wet"      ,
                                   drought_code == "w3" ~ "Extreme Wet"     ,
                                   drought_code == "w4" ~ "Exceptional Wet" )) %>%
  mutate(type = if_else(drought_label %in% c("Abnormally Wet", "Moderate Wet", "Severe Wet", "Extreme Wet", "Exceptional Wet"), "Wet", "Dry"),
  state_abbr = state.abb[match(state, state.name)]) %>%
  # count
  count(date, type, drought_label, state, state_abbr) %>%
  group_by(date, state, state_abbr) %>%
  mutate(pc = n/sum(n)) %>%
  ungroup() %>%
  # fill
  mutate(fill          = case_when(drought_label == "Exceptional Dry"   ~ "#C7EF34FF",
                                   drought_label == "Extreme Dry"       ~ "#FABA39FF",
                                   drought_label == "Severe Dry"        ~ "#F66B19FF",
                                   drought_label == "Moderate Dry"      ~ "#CB2A04FF",
                                   drought_label == "Abnormally Dry"    ~ "#7A0403FF",
                                   drought_label == "Abnormally Wet"    ~ "#36AAF9FF",
                                   drought_label == "Moderate Wet"      ~ "#1AE4B6FF",
                                   drought_label == "Severe Wet"        ~ "#30123BFF",
                                   drought_label == "Extreme Wet"       ~ "#4662D7FF",
                                   drought_label == "Exceptional Wet"   ~ "#72FE5EFF")) %>%
  # create up and down
  mutate(pc = if_else(drought_label %in% c("Abnormally Wet", "Moderate Wet", "Severe Wet", "Extreme Wet", "Exceptional Wet"), -pc, pc))

library(ggh4x)
library(showtext)

labels <- rev(c("Exceptional Dry", "Extreme Dry", "Severe Dry", "Moderate Dry", "Abnormally Dry", "Abnormally Wet", "Moderate Wet"   , "Severe Wet", "Extreme Wet", "Exceptional Wet"))

explan  <- "\nThe drought and wet conditions are collected by US Drought Monitor site since 1895. 
The US National Oceanic and Atmospheric Aministration (NOAA) collect data on drought conditions going back to 1895. This meteroalogical data is bucketed into drought categories e.g. 'exceptional dry', 'extreme dry' etc. using the Standardized Precipitation Index (SPI).\n
This graphic shows the percentage of each SPI drought category in the US since 1895, split out by wet (bottom) and dry (top)."

cap <- "Graphic: @Cyd_yzc | Data Source: National Integrated Drought Information System. | #TidyTuesday - Week 24 - 2022"

mygrid = us_state_grid1 %>% filter(!code %in% c("DC","HI","AK"))
  

font_add_google("DM Serif Display", "sd")
showtext_auto()

  ggplot(drought_cln, aes(x = date, y = pc)) +
  geom_col(aes(fill = fill)) +
#  scale_x_date(expand = c(0,0), breaks = "5 year", labels = scales::date_format("%Y")) +
  scale_y_continuous(expand = c(0,0), breaks = c(-1, -0.5,0,0.5,1), labels = scales::percent(c(1,0.5, 0, 0.5, 1))) +
  scale_fill_viridis_d(NULL, option = "turbo", direction = 1, guide = "stringlegend", labels = labels) + # labels = levels(drought_cln$drought_label)
  facet_geo(~state_abbr, grid = mygrid) + 
    coord_cartesian(ylim = c(-1, 1), clip = "off") +
  guides(fill  = guide_stringlegend(nrow = 1, size = 12)) +
    labs(x = "", 
         y = "",
         title    = "US Historical Drought Conditions",
         subtitle = explan,
         caption = cap) +
    theme(text = element_text(family = "sd"),
         legend.position = c(0.5, -0.05),
         plot.title.position = "plot", 
         plot.title = element_text(size = 20),
         plot.background = element_rect(fill = "#FFFFF0"),
         panel.background = element_rect(fill = "#FFFFF0"),
         strip.background = element_rect(fill = "#FFFFF0"),
         axis.ticks = element_blank())



ggsave(plot = plot_drought, here::here("Week 24 : US Drought/drought_con.png"), width = 18, height = 10, dpi = 360)

