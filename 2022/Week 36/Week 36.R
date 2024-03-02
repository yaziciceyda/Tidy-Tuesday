# Data Import 

inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
colors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
themes <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')



font_add_google("Dancing Script", "ds")
showtext_auto()

# Data Wrangling

birthday <- themes %>%
  rename(name_theme = name,
         id_theme = id) %>%
  left_join(sets %>% 
              rename(name_sets = name), 
            by = c("id_theme" = "theme_id")) %>%
  left_join(inventories %>% rename(inv_set_num = set_num) %>%
              select(-version),
            by = c("set_num" = "inv_set_num")) %>%
  select(-parent_id) %>%
  left_join(inventory_parts %>%
              rename(inv_parts_img_url = img_url) %>%
              select(- is_spare),
              by = c("id" = "inventory_id")) %>%
  left_join(colors %>%
              select(-is_trans) %>%
              rename(color_data_id = id,
                     color_name = name), by = c("color_id" = "color_data_id")) %>%
  mutate(rgb = paste("#", rgb, sep = "")) %>%
  filter(str_detect(name_sets, "birthday cake|Birthday Cake|Birthday cake")) %>%
  arrange(year)%>%
  drop_na(rgb, color_name) 
  
  ggplot(birthday, aes(x = year, y = num_parts, color = rgb, quantity)) +
    geom_point(aes(color = rgb, size = quantity)) +
    geom_jitter() +
    scale_color_identity()
  
birthday_data <- birthday %>%
  select(color_name, quantity, rgb) %>%
  group_by(color_name, rgb) %>%
  summarise(sum_q = sum(quantity)) %>%
  ungroup() %>%
  mutate(id = c(1:14))
 

packing <- circleProgressiveLayout(birthday_data$sum_q, sizetype='area')
birthday_data <- cbind(birthday_data, packing)
  dat.gg <- circleLayoutVertices(packing, npoints=50)
  
  dat.gg2 <- dat.gg %>%
    group_by(id) %>%
    summarize(m_x = mean(x),
              m_y = mean(y)) %>%
    ungroup()
  
  dat.gg3 <- full_join(dat.gg, birthday_data, by = ("id")) %>%
    left_join(dat.gg2, by = ("id"))
  
  # The plot
  
  final_plot <-  ggplot() + 
    geom_polygon(data = dat.gg3, aes(x, y, group = id, fill = rgb)) +
    scale_fill_identity() +
    geom_text(dat.gg3, mapping = aes(x = m_x, y = m_y, size = sum_q + 5, 
                                     label = color_name,
                                     fill = "red", family = "ds")) +
    scale_size_identity() +
    labs(title = "LEGO - Birthday Cakes",
         subtitle = "The colours of parts in the sets that include **Birthday Cakes** in the names are mostly 
         <span style='color:#FFFFFF'>white, <span style='color:#C870A0'>dark pink, <span style='color:#582A12'>reddish brown and 
         <span style='color:#F2CD37'>yellow.",
         caption = "Data Source: {rebrickable} | TidyTuesday 2022 - Week 36 | Prepared by: @Cyd_yzc") +
    theme(plot.subtitle = element_markdown(family = "ds", size = 25),
          plot.title = element_text(family = "ds", size = 40),
          plot.caption = element_text(family = "ds", size = 20),
          panel.background = element_rect(fill = "red"),
          plot.background = element_rect(fill = "red"),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  final_plot
  
  # Save the Plot
  ggsave("Week36_2022.png", final_plot, width = 25, height = 12, dpi = 72)
  

   
  
  