library(tidyverse)
library(reshape2)


df_f <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_women_8pp.csv")

# median intake

before_f_i <- df_f %>% summarise(intakem = wtd.quantile(baseIntake, probs = 0.5, weight = sample_weight)) 

after_f_i <- df_f %>% summarise(intakem = wtd.quantile(newIntake, probs = 0.5, weight = sample_weight)) 

after_f - before_f

after_f/before_f - 1

# median weight

df_f %>% 
  summarise(weightm = wtd.quantile(finalWeight, probs = 0.5, weight = sample_weight)) 

# median bmi

df_f %>% 
  summarise(bmim = wtd.quantile(bmi_final, probs = 0.5, weight = sample_weight)) 

# intake

df_f %>% 
  select(X, baseIntake, newIntake, sample_weight) %>%
  melt(., id.vars = c("X", "sample_weight")) %>% 
  mutate(label = ifelse(variable == "baseIntake", "Before", "After")) %>% 
  ggplot(., aes(x = value, fill = label)) +
  geom_density(aes(weight = sample_weight)) +
  theme_ipsum() +
  labs(x = "Intake", 
       fill = "",
       title = "Distribution of Intake - Obese Women")

df_m <- read.csv( "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_men_8.9pp.csv")

# median intake

before_m_i <- df_m %>% summarise(intakem = wtd.quantile(baseIntake, probs = 0.5, weight = sample_weight)) 

after_m_i <- df_m %>% summarise(intakem = wtd.quantile(newIntake, probs = 0.5, weight = sample_weight)) 

after_m_i - before_m_i

after_m_i/before_m_i - 1

# median weight

df_m %>% 
  summarise(weightm = wtd.quantile(finalWeight, probs = 0.5, weight = sample_weight)) 

# median bmi

df_m %>% 
  summarise(bmim = wtd.quantile(bmi_final, probs = 0.5, weight = sample_weight)) 

# intake

df_m %>% 
  select(X, baseIntake, newIntake, sample_weight) %>%
  melt(., id.vars = c("X", "sample_weight")) %>% 
  mutate(label = ifelse(variable == "baseIntake", "Before", "After")) %>% 
  ggplot(., aes(x = value, fill = label)) +
  geom_density(aes(weight = sample_weight)) +
  theme_ipsum() +
  labs(x = "Intake", 
       fill = "",
       title = "Distribution of Intake - Obese Men")

# combine

df <- rbind(df_m,df_f)

df %>% 
  mutate(final_BMI_class = factor(final_BMI_class, c("normal", "overweight", "obese"))) %>% 
  group_by(final_BMI_class) %>% 
  count(., wt = sample_weight) %>% 
  ungroup() %>% 
  mutate(freq = n/sum(n))  %>% 
  ggplot(., aes(x = final_BMI_class, y = freq)) +
  geom_bar(stat = "identity")


library(tidyverse)
library(Hmisc)
library(hrbrthemes)
library(ggridges)


df <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_women_obese_3_year_split.csv")

df %>% 
  group_by(time_period) %>% 
  summarise(intakem = wtd.quantile(newIntake, probs = 0.5, weight = sample_weight))

df %>% 
  group_by(time_period) %>% 
  summarise(intakem = wtd.quantile(baseIntake, probs = 0.5, weight = sample_weight))

df %>% 
  group_by(time_period) %>% 
  summarise(intakem = wtd.quantile(bmi_final, probs = 0.5, weight = sample_weight))

df %>% 
  group_by(time_period) %>% 
  summarise(intakem = wtd.quantile(BMI_est, probs = 0.5, weight = sample_weight))

df <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_men_obese_3_year_split.csv")

df %>% 
  group_by(time_period) %>% 
  summarise(intakem = wtd.quantile(newIntake, probs = 0.5, weight = sample_weight))

df %>% 
  group_by(time_period) %>% 
  summarise(intakem = wtd.quantile(baseIntake, probs = 0.5, weight = sample_weight))

df %>% 
  group_by(time_period) %>% 
  summarise(intakem = wtd.quantile(bmi_final, probs = 0.5, weight = sample_weight))

df %>% 
  group_by(time_period) %>% 
  summarise(intakem = wtd.quantile(BMI_est, probs = 0.5, weight = sample_weight))

df_stack <- rbind(df %>% 
                    select(X, sample_weight, time_period, newIntake) %>% 
                    rename(intake = newIntake),
                  df %>% 
                    filter(time_period == 1) %>% 
                    select(X, sample_weight, baseIntake) %>% 
                    rename(intake = baseIntake) %>% 
                    mutate(time_period = 0) %>% 
                    distinct()) %>% 
  mutate(label = case_when(time_period == 0 ~ "Start",
                           time_period == 1 ~ "3 years",
                           time_period == 2 ~ "6 years",
                           time_period == 3 ~ "9 years"))

df_stack$label <- factor(df_stack$label, levels = c("Start", "3 years", "6 years", "9 years"))

ggplot(df_stack, aes(x = time_period, group = time_period, y = intake)) + 
  geom_boxplot(aes(weight = sample_weight)) +
  geom_point() +
  theme_ipsum()

ggplot(df_stack, aes(x = intake, y = label, group = label, fill = label)) + 
  geom_density_ridges_gradient(rel_min_height = 0.01, scale = 3) +
  scale_fill_viridis_d(option = "C") +
  theme_ipsum()  +
  labs(y = "",
       group = "",
       fill = "",
       title = "Intake Distribution - Obese Men")

ggplot(df_stack, aes(x = time_period, y = intake)) +
  geom_smooth()
