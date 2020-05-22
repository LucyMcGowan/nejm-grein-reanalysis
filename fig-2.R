library(tidyverse)
d <- read_csv("data/data-fig-2.csv")

long_dat <- d %>%
  pivot_longer(day_1:day_36)

cats <- tibble(
  value = 1:6,
  cat = factor(c("Ambient air", "Low-flow oxygen", "High-flow oxygen", "NIPPV", 
                 "Mechanical ventilation", "ECMO"),
               levels = c("ECMO", "Mechanical ventilation", "NIPPV", 
                          "High-flow oxygen", "Low-flow oxygen", "Ambient air"))
)
long_dat %>%
  left_join(cats, by = "value") %>%
  filter(!is.na(value)) %>%
  mutate(day_oxy = as.numeric(gsub("day_", "", name)) - 1,
         day_oxy = ifelse(day_oxy > 28, 28, day_oxy),
         day = ifelse(day > 28, 28, day),
         patient = factor(patient, levels = 53:1),
         status = factor(status, labels = c("Improved", "Not Changed", "Worsened")),
         event = ifelse(event == "censor", NA, event)
  ) %>%
  ggplot(aes(x = patient, y = day_oxy, fill = cat)) +
  geom_segment(aes(x = patient, xend = patient,
                   y = 0, yend = day - 0.5), lty = 3) +
  geom_tile(width = 0.5) + 
  scale_fill_manual("Oxygen support",
                    values = c("#7D3A2C", "#AA3B2F", "#D36446", "#DEA568", 
                               "#F5D280", "#FCEEBC")) +
  geom_point(aes(x = patient, y = day - 0.5, shape = event)) +
  scale_shape_manual("Event", values = c(15, 5),
                     labels = c("Death", "Discharge", "")) +
  guides(fill = guide_legend(override.aes = list(shape = NA), order = 1)) +
  coord_flip() +
  labs(y = "day", x = "") +
  theme_classic()

