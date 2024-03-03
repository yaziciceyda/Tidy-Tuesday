# Tidy Tuesday - 2023 - Week 16

# Libraries used

library(tidyverse)
library(rnaturalearth)
library(ggimage)
library(showtext)
library(patchwork)

# Font in the Plot

font_add_google('Glass Antiqua', 'ga')
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 16)
founder_crops <- tuesdata$founder_crops

# Data Wrangling - Part 1

crop_data <- founder_crops %>%
  filter(source == "ORIGINS",
         site_name != "Kastros",
         !is.na(founder_crop)) %>%
  select(source, site_name, longitude, latitude, n, prop, founder_crop)

# Site Names

crop_data_site <- crop_data %>%
  group_by(site_name) %>%
  mutate(avg_long = mean(longitude),
         avg_lat = mean(latitude)) %>%
  ungroup() %>%
  select(site_name, avg_long, avg_lat) %>%
  distinct()

# World Map

world <- ne_countries(scale = "medium", returnclass = "sf")

# Data Wrangling - Part 2

crop_data_summary <- crop_data %>%
  filter(n > 50,
         site_name != "Kastros") %>%
  group_by(founder_crop) %>%
  slice_max(prop, n = 1) %>%
  ungroup() %>%
  mutate(x = case_when(
    site_name == "Hac?lar" ~ 23.5,
    site_name == "Tell Ain el-Kerkh" ~ 25,
    site_name == "Yiftah'el" ~ 29.5,
    site_name == "?ay?n?" ~ 40,
    site_name == "Mureybet" ~ 34,
    site_name == "Sabi Abyad II" ~ 39,
    site_name == "Yarim Tepe" ~ 44
  ),
  y = case_when(
    site_name == "Hac?lar" ~ 42,
    site_name == "Tell Ain el-Kerkh" ~ 34,
    site_name == "Yiftah'el" ~ 31,
    site_name == "?ay?n?" ~ 43.5,
    site_name == "Mureybet" ~ 26,
    site_name == "Sabi Abyad II" ~ 26,
    site_name == "Yarim Tepe" ~ 26
  ),
  img = case_when(
    founder_crop == "barley" ~ paste0(here::here(), "/Barley.png"),
    founder_crop == "bitter vetch" ~ paste0(here::here(), "/Bitter_vetch.png"),
    founder_crop == "chickpea" ~ paste0(here::here(), "/Chickpea.png"),
    founder_crop == "einkorn wheat" ~ paste0(here::here(), "/Einkorn_wheat.png"),
    founder_crop == "emmer wheat" ~ paste0(here::here(), "/Emmer_wheat.png"),
    founder_crop == "flax" ~ paste0(here::here(), "/Flax.png"),
    founder_crop == "lentil" ~ paste0(here::here(), "/Lentil.png"),
    founder_crop == "pea" ~ paste0(here::here(), "/Pea.png"),
    founder_crop == "wheat" ~ paste0(here::here(), "/Wheat.png")
  ),
  x = ifelse(founder_crop == "bitter vetch", 46, x),
  x = ifelse(founder_crop == "barley", 49, x)) 

# Subtitle of the Plot

subtitle_text <- str_wrap("\nEight 'founder crops' ? emmer wheat, einkorn wheat, barley, 
lentil, pea, chickpea, bitter vetch, and flax ? have long been thought to have 
been the bedrock of Neolithic economies. Here, the locations of the sites for 
each founder crop is given. The names of these sites is written for the ones 
with the highest proportion of the sample that contains this crop.", 90)
  

# The plot of the locations of the crops

p1 <-  ggplot() +
  geom_sf(data = world %>% filter(name %in% c("Turkey", "Iraq", "Iran",
                                              "Syria", "Cyprus", "Lebanon",
                                              "Israel", "Jordan")),
          colour = "#A16E47",
          fill = alpha("#E3C575", 0.3))  +
   geom_point(data = crop_data,
              mapping = aes(x = longitude,
                            y = latitude,
                            colour = founder_crop),
              size = 3) +
   geom_image(data = crop_data_summary, 
              mapping = aes(x = x, y = y, image = img),
              size = 0.13) +
   # Barley
   annotate("segment", x = 42.36, xend = 49, 
            y = 36.28, yend = 29,
            colour = "#D9A420", 
            size = 1, arrow = arrow()) +
   annotate("text", x = 49, y = 28.85, label = "Barley", size = 8,
            family = "ga") +
   # Bitter Vetch
   annotate("segment", x = 39.72, xend = 43.5, 
            y = 38.22, yend = 42,
            colour = "#D9A420", 
            size = 1, arrow = arrow()) +
   annotate("text", x = 45.5, y = 46.35, label = "Bitter Vetch", size = 8,
            family = "ga") + 
   # Chickpea
   annotate("segment", x = 36.46, xend = 27, 
            y = 35.82, yend = 34,
            colour = "#D9A420", 
            size = 1, arrow = arrow()) +
   annotate("text", x = 24.8, y = 37, label = "Chickpea", size = 8,
            family = "ga") +
   # Einkorn Wheat
   annotate("segment", x = 38.15, xend = 34, 
            y = 36.02, yend = 29,
            colour = "#D9A420", 
            size = 1, arrow = arrow()) +
   annotate("text", x = 34, y = 28.75, label = "Einkorn Wheat", size = 8,
            family = "ga") +
   # Emmer Wheat
   annotate("segment", x = 42.36, xend = 44, 
            y = 36.28, yend = 29,
            colour = "#D9A420", 
            size = 1, arrow = arrow()) +
   annotate("text", x = 44, y = 28.95, label = "Emmer Wheat", size = 8,
            family = "ga") +
   # Flax
   annotate("segment", x = 39.08, xend = 39, 
            y = 36.5, yend = 29.5,
            colour = "#D9A420", 
            size = 1, arrow = arrow()) +
   annotate("text", x = 39, y = 28.85, label = "Flax", size = 8,
            family = "ga") +
   # Lentil
   annotate("segment", x = 35.23, xend = 31.5, 
            y = 32.75, yend = 31,
            colour = "#D9A420", 
            size = 1, arrow = arrow()) +
   annotate("text", x = 30, y = 34, label = "Lentil", size = 8,
            family = "ga") +
   # Pea 
   annotate("segment", x = 30.07, xend = 25.5, 
            y = 37.57, yend = 42,
            colour = "#D9A420", 
            size = 1, arrow = arrow()) +
   annotate("text", x = 23.5, y = 44.85, label = "Pea", size = 8,
            family = "ga") +
   # Wheat
   annotate("segment", x = 39.72, xend = 40, 
            y = 38.22, yend = 42,
            colour = "#D9A420", 
            size = 1, arrow = arrow()) +
   annotate("text", x = 40, y = 45.5, label = "Wheat", size = 8,
            family = "ga") +
   geom_text_repel(data = crop_data_summary[-c(1, 9),], 
                   mapping = aes(x = longitude, y = latitude,
                                 label = site_name), family = "ga", size = 6) +
   scale_colour_manual(values = c("barley" = "red",
                                  "bitter vetch" = "green",
                                  "chickpea" = "blue",
                                  "einkorn wheat" = "brown",
                                  "emmer wheat" = "black",
                                  "flax" = "pink",
                                  "lentil" = "dark green",
                                  "pea" = "#ff485b",
                                  "wheat" = "orange")) +
   
   coord_sf(xlim = c(20, 52), ylim = c(23, 47), expand = FALSE) +
   labs(title = "NEOLITHIC FOUNDER CROPS",
        caption = "Data Source: The Neolithic Founder Crops in Southwest Asia: Research Compendium\nTidyTuesday 2023 - Week 16 | Prepared by: C. YAZICI",
        subtitle = subtitle_text,
        colour = "Founder Crop") +
   theme(panel.background = element_rect(fill = "white", color = NA),
         plot.background = element_rect(fill = "white", color = NA),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         panel.grid = element_blank(),
         legend.title = element_text(family = "ga", size = 18),
         legend.text = element_text(family = "ga", size = 17),
         legend.key.height = unit(1.0, 'cm'),
         plot.title = element_text(family = "ga", size = 40, hjust = 0.5),
         plot.subtitle = element_text(family = "ga", size = 22, hjust = 0),
         plot.caption = element_text(family = "ga", size = 20, hjust = 1),
         plot.margin = unit(c(1, 2, 1, 2), "cm"))


p1

# Preparation for the World Globe

world <- rgeoboundaries::gb_adm0()

long_min <- min(founder_crops$longitude)
long_max <- max(founder_crops$longitude)
lat_min <- min(founder_crops$latitude)
lat_max <- max(founder_crops$latitude)

world_cropped <- world %>% 
  st_make_valid() %>% 
  st_crop(xmin = long_min - 2,
          xmax = long_max + 2,
          ymin = lat_min - 1,
          ymax = lat_max + 1)

world_ortho <- world %>% 
  st_transform(crs = "+proj=ortho +lon_0=45 +lat_0=40")

rect <- st_as_sf(data.frame(lat = 35, long = 36), coords = c("lat", "long"), 
                 crs = 4326)

ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = "+proj=ortho +lon_0=45 +lat_0=40")

rect <- tibble(lon = c(long_min, long_max), lat = c(lat_min, 
                                                    lat_max)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

# World Globe

p2 <- ggplot(world_ortho) +
  geom_sf(data = ocean, color = NA, fill = "#1da2d8") +
  geom_sf(fill = "#E3C575", color = "#A16E47", linewidth = 0.1) +
  geom_sf(data = rect, fill = NA, color = "red2", linewidth = 1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA)
  )

# The final plot

final_plot <- p1 +
  inset_element(p2, left = -0.6, bottom = -0.05, right = 0.9, top = 0.3) +
  theme(plot.margin = unit(c(1, 2, 1, 2), "cm"))

final_plot

# Save the Plot

ggsave("Week16.png", final_plot, width = 28, height = 15, dpi = 72)


 


  
