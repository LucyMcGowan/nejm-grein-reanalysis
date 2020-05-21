library(tidyverse)

d <- read_csv("data/data-fig-2.csv")

dat_long <- d %>%
  pivot_longer(day_1:day_36) %>%
  mutate(day_oxy = as.numeric(gsub("day_", "", name))) %>%
  select(-name)

dat_long <- dat_long %>%
  filter(!is.na(value)) %>%
  group_by(patient) %>%
  mutate(first = first(value),
         last = last(value),
         improve = first >= last + 2 | event == "discharged") %>%
  ungroup()

dat_long %>%
  filter(improve) %>%
  group_by(patient) %>%
  mutate(improve1 = first >= value + 2) %>%
  filter(improve1) %>%
  mutate(t = first(day_oxy),
         event = 1) %>%
  slice(1) %>%
  ungroup() %>%
  select(patient, t, event) -> events_improved

dat_long %>%
  filter(improve) %>%
  anti_join(events_improved, by = "patient") %>%
  group_by(patient) %>%
  mutate(t = day,
         event = 1) %>%
  slice(1) %>%
  ungroup() %>%
  select(patient, t, event) -> events_discharged
  

dat_long %>%
  filter(!improve) %>%
  distinct(patient, day) %>%
  mutate(event = 0) %>%
  select(patient, t = day, event) -> non_events

data <- bind_rows(events_improved, events_discharged, non_events)

library(survival)

x <- survfit(Surv(time = data$t, event = data$event) ~ 1, data = data)
plot(x, fun = "event")
library(cmprsk)
s <- cuminc(data$t, data$event)
