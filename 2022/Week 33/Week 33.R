tuesdata <- tidytuesdayR::tt_load(2022, week = 33)

characters <- tuesdata$characters
myers_briggs <- tuesdata$myers_briggs
psych_stats <- tuesdata$psych_stats

library(readr)
pych2 <- read.csv("psych_stats.csv", sep = ";")



psych <- pych2 %>%
  filter(uni_name == "Sherlock") %>%
  group_by(char_name) %>%
  arrange(desc(avg_rating)) %>%
  slice(1:8) %>%
  ungroup() %>%
  group_by(question, personality) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

filter(question %in% c("loyal/traitorous",
                       "animalistic/human",
                       "cruel/kind",
                       "high IQ/low IQ",
                       ""))  
  
