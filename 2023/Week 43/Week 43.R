library(janitor)
library(ggchicklet)
library(ggnewscale)
library(showtext)

# The Font in the plot

font_add_google(name = "Rubik",
                family = "rubik")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2023, week = 43)
patient_risk_profiles <- tuesdata$patient_risk_profiles

# Data Wrangling

patient_data <- patient_risk_profiles %>%
  clean_names() %>%
  select(person_id, sex_female, sex_male, starts_with("age_group"),
         type_1_diabetes_and_no_prior_specific_non_t1dm_diabetes_in_prior_year,
         type_2_diabetes_mellitus_dm_with_no_type_1_or_secondary_dm_in_prior_year,
         predicted_risk_of_acute_pancreatitis_with_no_chronic_or_hereditary_or_common_causes_of_pancreatitis) %>%
  mutate(sex = ifelse(sex_female == 1, "female", "male")) %>%
  rename(risk = predicted_risk_of_acute_pancreatitis_with_no_chronic_or_hereditary_or_common_causes_of_pancreatitis,
         type1 = type_1_diabetes_and_no_prior_specific_non_t1dm_diabetes_in_prior_year,
         type2 = type_2_diabetes_mellitus_dm_with_no_type_1_or_secondary_dm_in_prior_year) %>%
  select(-c(sex_female, sex_male)) %>%
  pivot_longer(age_group_10_14:age_group_90_94,
               names_to = "age",
               values_to = "age_group") %>%
  filter(age_group == 1) %>%
  group_by(sex, age) %>%
  mutate(max_risk = max(risk, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(risk == max_risk) %>%
  mutate(age_numeric = str_sub(age, 11, 15),
         age_min = str_sub(age, 11, 12),
         age_min = ifelse(age_min == "0_", "0", age_min),
         age_min = ifelse(age_min == "5_", "5", age_min),
         age_min = as.numeric(age_min),
         diabetes = type1 + type2,
         diabetes = as.character(diabetes)) %>%
  arrange(age_min, sex)


data_artificial <- tibble(sex = rep(c("female", "male"), 19),
                          age_min = rep(seq(from = 0, to = 90, by = 5),
                                        each = 2))
patient_data <- patient_data %>%
  right_join(data_artificial, by = c("sex", "age_min")) %>%
  arrange(age_min, sex) %>%
  mutate(y = c(20:1, 20:3),
         x = c(rep(1, 20), rep(1.5, 18)))

# The subtitle in the plot

subtitle_text <- str_wrap("Predicted 1-year risk of having acute pancreatitis for 34
patients. Patient's ID is shown at the end of each rectangle and Type 1 or
Type 2 diabetes existence is written in the middle.", 75)

# The Plot

p <- ggplot(patient_data %>% filter(!is.na(max_risk))) +
  # Pancreatitis
  ggchicklet:::geom_rrect(aes(xmin = x - 0.2, xmax = x,
                              ymin = y - 0.3, ymax = y + 0.3,
                          fill = max_risk),
                          radius = unit(0.2, units = "cm")) +
  scale_fill_gradient(low = "#F5ADB2", high = "#A00511") +
  labs(title = "Patient Risk Profiles",
       subtitle = subtitle_text,
       fill = "Risk",
       caption = "Data Source: Jenna Reps | TidyTuesday 2023 - Week 43 | Prepared by: C. YAZICI") +
  new_scale_fill() +
  geom_text(aes(x = x - 0.24, y = y,
                label = str_to_sentence(sex), color = sex),
            show.legend = FALSE, family = "rubik") +
  scale_colour_manual(values = c("brown", "purple")) +
  new_scale_colour() +
  geom_text(patient_data %>% filter(y %% 2 == 1),
            mapping = aes(x = x - 0.30, y = y + 0.5,
                label = paste0(age_min, " - ", age_min + 4)),
            family = "rubik", size = 5) +
  # Only Type 1
  geom_text(patient_data %>% filter(type1 == 1, type2 == 0),
           mapping = aes(x = x - 0.1, y = y, label = "Type 1"),
           color = "#2722E1", family = "rubik", size = 5) +
  # Only Type 2
  geom_text(patient_data %>% filter(type1 == 0, type2 == 1),
            mapping = aes(x = x - 0.1, y = y, label = "Type 2"),
            color = "#EDF163", family = "rubik", size = 5) +
  #Both Type 1 & Type 2
  geom_text(patient_data %>% filter(type1 == 1, type2 == 1),
            mapping = aes(x = x - 0.1, y = y, label = "Type 1 & Type 2"),
            color = "#18B913", family = "rubik", size = 5) +
  geom_text(aes(x = x + 0.03, y = y, label = paste0("[", person_id, "]")),
            family = "rubik") +
  coord_cartesian() +
  theme(aspect.ratio = 11/9,
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.background = element_rect(fill = "ivory", color = NA),
        legend.background = element_rect(fill = "ivory"),
        legend.key.height = unit(1.5, 'cm'),
        legend.title = element_text(family = "rubik", size = 15),
        legend.text = element_text(family = "rubik", size = 12),
        plot.caption = element_text(family = "rubik", size = 13, hjust = 1),
        plot.title = element_text(family = "rubik", size = 30),
        plot.subtitle = element_text(family = "rubik", size = 15),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# Save the Plot

ggsave("Week43.png", p, width = 17, height = 15, dpi = 72)






