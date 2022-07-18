
library(tidyverse)
library(hrbrthemes)


red_10 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_3years_10percent_sample.csv", sep = ",", fileEncoding="UTF-8-BOM") %>% 
  mutate(changeBMI = (final_BMI - BMI_est)/BMI_est)

red_15 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_3years_15percent_sample.csv", sep = ",", fileEncoding="UTF-8-BOM") %>% 
  mutate(changeBMI = (final_BMI - BMI_est)/BMI_est)

red_20 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_3years_20percent_sample.csv", sep = ",", fileEncoding="UTF-8-BOM") %>% 
  mutate(changeBMI = (final_BMI - BMI_est)/BMI_est)

red_25 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_3years_25percent_sample.csv", sep = ",", fileEncoding="UTF-8-BOM") %>% 
  mutate(changeBMI = (final_BMI - BMI_est)/BMI_est)

ggplot(red_10, aes(x = BMI_est, y = changeBMI)) + 
  geom_point() +
  theme_ipsum() +
  labs(x = "Initial BMI",
       y = "Percentage Change in BMI",
       title = "10% Intake Reduction")

ggplot(red_20, aes(x = BMI_est, y = changeBMI)) + 
  geom_point() +
  theme_ipsum()+
  labs(x = "Initial BMI",
       y = "Percentage Change in BMI",
       title = "20% Intake Reduction")

ggplot(red_25, aes(x = BMI_est, y = changeBMI)) + 
  geom_point() +
  theme_ipsum()+
  labs(x = "Initial BMI",
       y = "Percentage Change in BMI",
       title = "25% Intake Reduction")


all <- rbind(red_10, red_15, red_20, red_25)

ggplot(all, aes(x = BMI_est, y = changeBMI)) + 
  facet_wrap(~ CalRedShare) +
  geom_point() +
  theme_ipsum()+
  labs(x = "Initial BMI",
       y = "Percentage Change in BMI",
       title = "Share of Intake Reduction")


all %>% 
  group_by(BMI_class, CalRedShare) %>%
  summarise(mean = mean(changeBMI)*100)
