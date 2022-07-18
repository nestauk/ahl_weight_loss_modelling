library(tidyverse)
library(hrbrthemes)
library(survey)
library(ggridges)

# read scenarios files

sc3 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_scenario_3.csv") %>% 
  mutate(Age_group_2 = cut(Age_est, breaks = c(16, 20, 30, 40, 50, 60, 200), right = F,labels = c("16-19", "20-29", "30-39", "40-49", "50-59", "60 +"))) 

sc4 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_scenario_4.csv") %>% 
  mutate(Age_group_2 = cut(Age_est, breaks = c(16, 20, 30, 40, 50, 60, 200), right = F,labels = c("16-19", "20-29", "30-39", "40-49", "50-59", "60 +"))) 

# combine files

df <- rbind(sc3 %>% select(id, BMI_est, sample_weight, Sex, Age_group_2, IMD_q) %>% mutate(scenario = "Current") %>% rename(BMI = BMI_est),
            sc3 %>% select(id, final_BMI, sample_weight, Sex, Age_group_2, IMD_q) %>% mutate(scenario = "Scenario 3") %>% rename(BMI = final_BMI),
            sc4 %>% select(id, final_BMI, sample_weight, Sex, Age_group_2, IMD_q) %>% mutate(scenario = "Scenario 4") %>% rename(BMI = final_BMI)) 

# df$IMD_q <- factor(df$IMD_q, levels = c("1", "2", "3", "4", "5"))


ggplot(df, aes(x = BMI, y = Sex, group = Sex, fill = Sex)) + 
  geom_density_ridges_gradient(rel_min_height = 0.01, scale = 3) +
  scale_fill_viridis_d(option = "C") +
  facet_grid(scenario ~ .) +
  xlim(25,45) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_vline(xintercept = 30, color = "red", size = 1.5)


ggplot(df, aes(x = BMI, y = Age_group_2, group = Age_group_2, fill = Age_group_2)) + 
  geom_density_ridges_gradient(rel_min_height = 0.01, scale = 3) +
  scale_fill_viridis_d(option = "C") +
  facet_grid(scenario ~ .) +
  xlim(25,45) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(y = "Age") +
  geom_vline(xintercept = 30, color = "red", size = 1.5)

ggplot(df, aes(x = BMI, y = IMD_q, group = IMD_q, fill = IMD_q)) + 
  geom_density_ridges_gradient(rel_min_height = 0.01, scale = 3) +
  scale_fill_viridis_c(option = "C") +
  facet_grid(scenario ~ .) +
  xlim(25,45) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(y = "Deprivation Quintiles (1 = low, 5 = high)") +
  scale_y_continuous( breaks = seq(1,5,1)) +
  geom_vline(xintercept = 30, color = "red", size = 1.5)




ggplot(df, aes(x = BMI, y = Sex, group = Sex, fill = Sex)) + 
  geom_density_ridges_gradient(stat = "binline", binwidth = 0.1, draw_baseline = F) +
  scale_fill_viridis_d(option = "C") +
  facet_grid(scenario ~ .) +
  xlim(20,45) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_vline(xintercept = 30, color = "red", size = 1.5)

ggplot(df, aes(x = BMI, y = IMD_q, group = IMD_q, fill = IMD_q)) + 
  geom_density_ridges_gradient(stat = "binline", binwidth = 0.1, draw_baseline = F) +
  scale_fill_viridis_c(option = "C") +
  facet_grid(scenario ~ .) +
  xlim(20,45) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(y = "Deprivation Quintiles (1 = low, 5 = high)") +
  scale_y_continuous( breaks = seq(1,5,1)) +
  geom_vline(xintercept = 30, color = "red", size = 1.5)

ggplot(df, aes(x = BMI, y = Age_group_2, group = Age_group_2, fill = Age_group_2)) + 
  geom_density_ridges_gradient(stat = "binline", binwidth = 0.1, draw_baseline = F) +
  scale_fill_viridis_d(option = "C") +
  facet_grid(scenario ~ .) +
  xlim(20,45) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(y = "Age") +
  geom_vline(xintercept = 30, color = "red", size = 1.5)

ggplot(df, aes(x = BMI, group = Sex, fill = Sex)) +
  geom_density(alpha = 0.5, adjust = 2,trim = T) +
  facet_grid(scenario ~ .) +
  xlim(20,45) +
  theme_minimal() +
  geom_vline(xintercept = 30, color = "red", size = 1.5)
