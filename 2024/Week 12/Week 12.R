library(tidyverse)
library(janitor)
library(ggrepel)
library(showtext)

# Font in the Plot

font_add_google(name = "Vast Shadow",
                family = "vs", db_cache = FALSE)
showtext_auto()

# Data Import 

tuesdata <- tidytuesdayR::tt_load(2024, week = 12)
mutant_moneyball <- tuesdata$mutant_moneyball

# Data Wrangling

moneyball_data <- mutant_moneyball %>%
  clean_names() %>%
  select(member, total_value60s_ebay, total_value70s_ebay, 
         total_value80s_ebay,
         total_value90s_ebay, x60s_appearance_percent, 
         x70s_appearance_percent, x80s_appearance_percent,
         x90s_appearance_percent) %>%
  mutate(x60s_appearance_percent = str_replace_all(x60s_appearance_percent,
                                                   "%", ""),
         x60s_appearance_percent =  as.numeric(x60s_appearance_percent),
         
         x70s_appearance_percent = str_replace_all(x70s_appearance_percent,
                                                   "%", ""),
         x70s_appearance_percent =  as.numeric(x70s_appearance_percent),
         
         x80s_appearance_percent = str_replace_all(x80s_appearance_percent,
                                                   "%", ""),
         x80s_appearance_percent =  as.numeric(x80s_appearance_percent),
         
         x90s_appearance_percent = str_replace_all(x90s_appearance_percent,
                                                   "%", ""),
         x90s_appearance_percent =  as.numeric(x90s_appearance_percent)) %>%
  
  pivot_longer(-member, values_to = "values", 
               names_to = "year") %>%
  mutate(year_numeric = case_when(
    year == "total_value60s_ebay" | year == "x60s_appearance_percent" ~ 1960,
    year == "total_value70s_ebay" | year == "x70s_appearance_percent" ~ 1970,
    year == "total_value80s_ebay" | year == "x80s_appearance_percent" ~ 1980,
    year == "total_value90s_ebay" | year == "x90s_appearance_percent" ~ 1990),
    type = case_when(
      year == "total_value60s_ebay" | year == "total_value70s_ebay" |
        year == "total_value80s_ebay" | year == "total_value90s_ebay" ~ 
        "value",
      .default = "appearance"
    )) 

appearance_data <- moneyball_data %>%
  filter(type == "appearance")%>%
  rename("values_appearance" = values) %>%
  select(-c(year, type)) %>%
  mutate(y = case_when(
    year_numeric == 1960 ~ 1,
    year_numeric == 1970 ~ 2,
    year_numeric == 1980 ~ 3,
    year_numeric == 1990 ~ 4,
  )) 

label_data <- appearance_data %>%
  filter(y == 4) %>%
  arrange(values_appearance) %>%
  mutate(x_label = row_number()) %>%
  select(c(member, x_label))


appearance_data_label <- appearance_data %>%
  left_join(label_data, by = c("member"))

# Subtitle of the Plot

subtitle_text <- "The percentage of each X-Men member appeared in an issue published in\n60s, 70s, 80s and 90s are shown. "

# The Plot

p <- ggplot(appearance_data_label) +
  geom_point(aes(x = reorder(member, x_label), y = y, 
                 size = values_appearance/3.5), 
             color = "lightgreen") +
  geom_text(aes(x = reorder(member, x_label), y = y, 
                 label = round(values_appearance)), color = "black",
            family = "vs", size = 6) +
  scale_size_identity() +
  annotate("text", x = 13, y = 1.5, label = "60s",
           family = "vs", size = 10, colour = "grey70",
           hjust = 0.5, vjust = 0.5) +
  annotate("text", x = 13, y = 2.5, label = "70s",
           family = "vs", size = 10, colour = "grey70",
           hjust = 0.5, vjust = 0.5) +
  annotate("text", x = 13, y = 3.5, label = "80s",
           family = "vs", size = 10, colour = "grey70",
           hjust = 0.5, vjust = 0.5) +
  annotate("text", x = 13, y = 4.5, label = "90s",
           family = "vs", size = 10, colour = "grey70",
           hjust = 0.5, vjust = 0.5) +
  labs(title = "X-Men Mutant Moneyball",
       subtitle = subtitle_text,
       caption = "Data Source: Mutant Moneyball Data\nTidyTuesday 2024 - Week 12 | Prepared by: C. YAZICI") +
          theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                           hjust = 1,
                                   family = "vs", size = 13),
                axis.text.y = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(),
                panel.background = element_rect(fill = "ivory",
                                                color = NA),
                plot.background = element_rect(fill = "ivory", 
                                               color = NA),
                plot.title = element_text(family = "vs", hjust = 0, 
                                          size = 40),
                plot.subtitle = element_text(family = "vs", hjust = 0, 
                                          size = 25, vjust = 0),
                plot.caption = element_text(family = "vs", hjust = 1, 
                                          size = 18,
                                          vjust = 1),
                plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"))

  
# Save the Plot

ggsave("Week12.png", p, width = 24, height = 16, dpi = 72)

  
                      