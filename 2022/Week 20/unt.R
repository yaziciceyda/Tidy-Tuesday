library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world_map <- ne_countries(scale = 50, returnclass = 'sf')

european_union <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                    "Czech Rep.","Denmark","Estonia","Finland","France",
                    "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                    "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                    "Portugal","Romania","Slovakia","Slovenia","Spain",
                    "Sweden","United Kingdom", "Ukraine")
european_union_map <-  world_map %>% 
  filter(name %in% european_union)

bbox_europe <- st_bbox(c(xmin = -10, ymin = 20, xmax = 50, ymax = 80), crs = st_crs(european_union_map))
european_union_map_cropped <- st_crop(european_union_map, bbox_europe)

df <- tibble(country = european_union,
         some_value = runif(length(european_union)))

map <- european_union_map_cropped %>% 
  left_join(df, by = c("name" = "country"))

ggplot(data = map) +
  geom_sf(mapping = aes(fill = some_value)) +
  scale_fill_gradient(name = "Growth Rate", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50") +
  labs(title = "(Fictitious) growth rate.") +
  theme(plot.title.position = "plot")
