library(tidyverse)
library(scales)
library(sf)
library(showtext)
library(cowplot)
library(colorspace)

# Data Import

tuesdata <- tidytuesdayR::tt_load(2022, week = 47)

museums <- tuesdata$museums

# Data Wrangling

allgroups <- museums %>%
  group_by(Subject_Matter) %>%
  count()

museums_2022 <- museums %>%
  separate(col = Year_opened, into = c('opened1', 'opened2'), sep = ':') %>%
  separate(col = Year_closed, into = c('closed1', 'closed2'), sep = ':') %>%
  mutate(new_subject =
           case_when(
             str_detect(Subject_Matter, "Archaeology") ~ "Archaeology",
             str_detect(Subject_Matter, "Arts") ~ "Arts",
             str_detect(Subject_Matter, "Belief_and_identity") ~ "Belief_and_identity",
             str_detect(Subject_Matter, "Buildings") ~ "Buildings",
             str_detect(Subject_Matter, "Communications") ~ "Communications",
             str_detect(Subject_Matter, "Food_and_drink") ~ "Food_and_drink",
             str_detect(Subject_Matter, "Industry_and_manufacture") ~ "Industry_and_manufacture",
             str_detect(Subject_Matter, "Leisure_and_sport") ~ "Leisure_and_sport",
             str_detect(Subject_Matter, "Local_Histories") ~ "Local_Histories",
             str_detect(Subject_Matter, "Medicine_and_health") ~ "Medicine_and_health",
             str_detect(Subject_Matter, "Mixed") ~ "Mixed",
             str_detect(Subject_Matter, "Natural_world") ~ "Natural_world",
             str_detect(Subject_Matter, "Personality") ~ "Personality",
             str_detect(Subject_Matter, "Rural_Industry") ~ "Rural_Industry",
             str_detect(Subject_Matter, "Science_and_technology") ~ "Science_and_technology",
             str_detect(Subject_Matter, "Sea_and_seafaring") ~ "Sea_and_seafaring",
             str_detect(Subject_Matter, "Services") ~ "Services",
             str_detect(Subject_Matter, "Transport") ~ "Transport",
             str_detect(Subject_Matter, "Utilities") ~ "Utilities",
             str_detect(Subject_Matter, "War_and_conflict") ~ "War_and_conflict",
           )) 

museums_group <- museums_2022%>%
  group_by(new_subject) %>%
  summarise(min_opened = min(opened1)) %>%
  ungroup() %>%
  arrange(min_opened)

 museums_2022_final <- museums_group %>%
  inner_join(museums_2022, by = c('new_subject'='new_subject', 
                            'min_opened'='opened1')) %>%
  select(new_subject, min_opened, Name_of_museum, `Village,_Town_or_City`,
         Governance, Size, Accreditation, closed2, Longitude, Latitude) %>%
  mutate(opened_scaled = rescale(as.numeric(min_opened), to = c(0, 100)))


england = rnaturalearth::ne_countries(country = 'united kingdom',
                                      type='map_units',scale = 'medium',
                                      returnclass = "sf") %>% 
  filter(geounit %in% c("England", "Scotland"))
england2 = st_transform(england, 7405)

df2 = sf_project(from = st_crs(4326), to = st_crs(7405), 
                 museums_2022_final[, c("Longitude", "Latitude")]) %>%
  as.data.frame()

museums_2022_final2 <- museums_2022_final %>%
  cbind(df2) %>%
  mutate(text_info = ifelse(min_opened == 1887 | min_opened == 1888, 
                            paste0(Name_of_museum,  
                            " \n", "(", str_replace_all(new_subject, "_", " "),
                            ") was opened in ", `Village,_Town_or_City`,
                                                             "."),
           paste0(Name_of_museum, " \n", "(", str_replace_all(new_subject, "_", " "),
                            ") was opened in ", `Village,_Town_or_City`,
                            ".")),
         size_new = case_when(
           Size == "small" ~ 1,
           Size == "medium" ~ 2,
           Size == "large" ~ 3,
           Size == "huge" ~ 4
         )) %>%
  drop_na(new_subject) %>%
  arrange(`Village,_Town_or_City`) %>%
  mutate(V1_new = rep(c(-1, 1), times = 11),
         opened_scaled = ifelse(min_opened >= 1800,
                                opened_scaled - 15, opened_scaled),
         opened_scaled = ifelse(min_opened == 1835,
                                opened_scaled + 1, opened_scaled),
         opened_scaled = ifelse(min_opened == 1872,
                                opened_scaled + 1, opened_scaled),
         opened_scaled = ifelse(Name_of_museum == 
                                  "Anatomy Museum, University Of Edinburgh",
                                   opened_scaled + 1, opened_scaled),
         opened_scaled = ifelse(Name_of_museum == "The Hunterian",
                                opened_scaled - 3, opened_scaled),
         opened_scaled = ifelse(min_opened >= 1887,
                                opened_scaled + 3, opened_scaled),
         V1_new = ifelse(min_opened == 1887, 1, V1_new),
         V1_new = ifelse(Name_of_museum == "Sheppys House Of Cider & Fine Foods",
                         -1, V1_new),
         V1_new = ifelse(min_opened == 1920,
                         -1, V1_new),
         V1 = ifelse(new_subject == "Arts", V1 - 10000, V1),
         V1 = ifelse(new_subject == "Science_and_technology", V1 - 10000, V1),
         V1 = ifelse(new_subject == "Transport", V1 - 10000, V1),
         V1 = ifelse(new_subject == "Leisure_and_sport", V1 - 10000, V1),
         V1 = ifelse(new_subject == "Communications", V1 - 20000, V1),
         V2 = ifelse(new_subject == "War_and_conflict", V2 - 20000, V2),
         V2 = ifelse(new_subject == "Belief_and_identity", V2 - 10000, V2),
         V2 = ifelse(new_subject == "Natural_world", V2 - 10000, V2),
         V2 = ifelse(new_subject == "Leisure_and_sport", V2 - 10000, V2)) %>%
  arrange(min_opened)
         
font_add_google('atma', 'atma')
showtext_auto()

new_cl <- c()
nr_year <- unique(as.numeric(museums_2022_final2$min_opened))
cl <- "#723608"
color_list <- seq(0, 1, length.out = (length(nr_year) + 5))
lcl <- length(color_list)
color_list_final <- cbind(nr_year, lighten(cl, 
                                           color_list[-lcl:-(lcl - 4)]))
for(i in 1:length(museums_2022_final2$min_opened))
{
  for(j in 1:nrow(color_list_final))
  {
    if(museums_2022_final2$min_opened[i] == color_list_final[j, 1])
      new_cl <- c(new_cl, color_list_final[j, 2])
  }
}

 museums_2022_final2 <- museums_2022_final2 %>% 
  mutate(new_color = new_cl)
  
 p1 <- ggplot(museums_2022_final2) +
  geom_sf(data = england2) +
  geom_point(mapping = aes(V1, V2, color = new_color), 
             shape = 19, size = 3, alpha = 0.5) +
  scale_color_identity(name = "Year Opened",
                       breaks = c("#723608", "#7A3D18", "#814523",
                                  "#894C2D", "#915436", "#995B3F",
                                  "#A16347", "#AA6B50",
                                  "#B27258", "#BA7A60", "#C38269",
                                  "#CB8A71", "#D49279", "#DC9A82",
                                  "#E5A38A", "#EEAB93", "#F7B39B",
                                  "#FFBCA4", "#FFC7B5"),
                       
                       labels = c("1621", "1653", "1660", "1676",
                                  "1683", "1800", "1814", "1833",
                                  "1835", "1857", "1869", "1872",
                                  "1887", "1888", "1900", "1920",
                                  "1928", "1932", "1945"),
                       guide = "legend") +
   theme_void() +
   theme(panel.background = element_rect(fill = "#F4F2EF",
                                         colour = NA),
         legend.position = "left",
         legend.title = element_text(family = "atma", size = 18),
         legend.text  = element_text(family = "atma", size = 15),
         legend.key.size = unit(0.8, "cm"))

p2 <- ggplot(museums_2022_final2) +
    geom_rect(aes(ymin = -3, ymax = 100,
                  xmin = -10, xmax = 10),
              fill = "#d3c399",
              colour = NA,
              size = 0.5) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 100),
               colour = "grey80", size = 2) +
  geom_point(mapping = aes(x = 0, y = opened_scaled, color = new_color),
             size = 3) +
  geom_text(mapping = aes(x = 0.5 * V1_new,
                          y = opened_scaled , label = min_opened,
                          family = "atma"), size = 5) +
  geom_text(mapping = aes(x = V1_new * 5,
                          y = opened_scaled, label = str_wrap(text_info, 70),
                          family = "atma", size = 4)) +
    scale_color_identity() +
    theme_void() +
    theme(legend.position = "none",
    panel.background = element_rect(fill = "#F4F2EF", colour = NA))
 
  
  
  plots <- align_plots(p1, p2,  align = 'r', axis = 'r')
  
  title1 <- ggdraw() + 
    draw_label(
      "UK Museums",
      fontface = 'bold',
      fontfamily = "atma",
      hjust = 0.5,
      x = 0.5,
      size = 40,
      color = "#723608"
    )  
  title2 <- ggdraw() + 
    draw_label(
      "The first museum in each subject with its location.\n There are two Medicine & Health and Food & Drink museums opened in the same year in different locations.",
      fontface = 'bold',
      fontfamily = "atma",
      hjust = 0.5,
      x = 0.5,
      y = 0.5, 
      size = 20,
      color = "#723608"
    ) 
  caption <- ggdraw() + 
    draw_label(
      "Data Source: https://museweb.dcs.bbk.ac.uk | TidyTuesday 2022 - Week 47 | Prepared by: C. YAZICI", 
      fontface = 'bold',
      fontfamily = "atma",
      hjust = 0.5,
      x = 0.5,
      y = 0.5, 
      size = 18,
      color = "#723608"
    ) 
  
  bottom_row <- plot_grid(
    plots[[2]], plots[[1]],
    labels = "",
    rel_widths = c(1.4, 0.5), 
    nrow = 1
  )

  final_plot <- plot_grid(title1, title2, bottom_row, caption, labels = "", ncol = 1,
                          rel_heights = c(0.1, 0.1, 0.9, 0.1)) +
    theme(plot.background = element_rect(fill = "#F4F2EF", colour = "#F4F2EF"))
  
  final_plot
  ggsave("Week47_2022.png", final_plot, width = 25, height = 12, dpi = 72)
  
