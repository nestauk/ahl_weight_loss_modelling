rm(list = ls())

library(Hmisc)
library(lubridate)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(reshape2)
library(survey)
library(networkD3)

# read file and add under weight

full <- read_csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse_full_weight_loss_3_steps.csv") %>% 
  mutate(bmi_class = case_when(bmival <= 18.5 ~ "underweight",
                               bmival > 18.5 & bmival < 25 ~ "normal",
                               bmival >= 25 & bmival < 30 ~ "overweight",
                               bmival >= 30 ~ "obese",
                               TRUE ~ "NA")) %>% 
  mutate(final_bmi_class = case_when(bmi_final <= 18.5 ~ "underweight",
                                     bmi_final > 18.5 & bmi_final < 25 ~ "normal",
                                     bmi_final >= 25 & bmi_final < 30 ~ "overweight",
                                     bmi_final >= 30 ~ "obese",
                                     TRUE ~ "NA"))

obese <- read_csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse_obese_weight_loss_3_steps.csv") %>% 
  mutate(bmi_class = case_when(bmival <= 18.5 ~ "underweight",
                               bmival > 18.5 & bmival < 25 ~ "normal",
                               bmival >= 25 & bmival < 30 ~ "overweight",
                               bmival >= 30 ~ "obese",
                               TRUE ~ "NA")) %>% 
  mutate(final_BMI_class = case_when(bmi_final <= 18.5 ~ "underweight",
                                     bmi_final > 18.5 & bmi_final < 25 ~ "normal",
                                     bmi_final >= 25 & bmi_final < 30 ~ "overweight",
                                     bmi_final >= 30 ~ "obese",
                                     TRUE ~ "NA"))


merge(  obese  %>% 
mutate(time_period = paste0("starting", time_period))  %>% 
group_by(time_period, bmi_class, sex_label) %>% 
  summarise(intakeM = round(exp(log(wtd.mean(baseIntake, weight = wt_int))),0)) %>% 
  dcast(., bmi_class + sex_label ~ time_period),
obese  %>% 
mutate(time_period = paste0("ending", time_period))  %>% 
group_by(time_period, bmi_class, sex_label) %>% 
  summarise(intakeM = round(exp(log(wtd.mean(intake_final, weight = wt_int))),0)) %>% 
  dcast(., bmi_class + sex_label ~ time_period))  %>% 
mutate(diff = ending3 - starting1,
      perc = round(diff/starting1*100,1))  %>% 
arrange(sex_label) 


names(full)

rbind(full  %>% 
      dplyr::select(sex_label, X, baseIntake, wt_int)  %>% 
      unique()  %>% 
      rename(intake = baseIntake)  %>% 
      mutate(time_period = 0), 
      full  %>% 
      dplyr::select(sex_label, X, intake_final, time_period, wt_int)  %>% 
      rename(intake = intake_final))  %>% 
ggplot(., aes(x = time_period, y = intake, group = time_period)) + 
facet_grid(sex_label ~.) +
geom_boxplot() +
theme_ipsum()



