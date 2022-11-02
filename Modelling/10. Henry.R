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

under <- read.csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse2019_clean_no_outliers.csv", sep = ",", fileEncoding="UTF-8-BOM") %>% 
  filter(bmi_class == "underweight") %>% 
  mutate(finalWeight = wtval,
         startWeight = wtval,
         height = htval,
         age = age_est,
         Sex_letter = ifelse(sex_label == "female", "F", "M"),
         sex = Sex_letter,
         baseIntake = ifelse(sex == "M", -.0971*(wtval^2) + 40.853*wtval + 323.59, .0278*(wtval^2) + 9.2893*wtval + 1528.9),
         newIntake = baseIntake,
         TEE_final = baseIntake,
         weight_final = wtval,
         intake_final = newIntake,
         bmi_final = bmival,
         value = 0,
         target = wtval,
         calRed = 0)

full <- plyr::rbind.fill(under, read_csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse_full_weight_loss.csv")) %>% 
  mutate(bmi_class = case_when(bmival <= 18.5 ~ "underweight",
                               bmival > 18.5 & bmival < 25 ~ "normal",
                               bmival >= 25 & bmival < 30 ~ "overweight",
                               bmival >= 30 & bmival < 40 ~ "obese",
                               bmival >= 40 ~ "morb obese",
                               TRUE ~ "NA")) %>% 
  mutate(final_BMI_class = case_when(bmi_final <= 18.5 ~ "underweight",
                                     bmi_final > 18.5 & bmi_final < 25 ~ "normal",
                                     bmi_final >= 25 & bmi_final < 30 ~ "overweight",
                                     bmi_final >= 30 & bmi_final < 40 ~ "obese",
                                     bmi_final >= 40 ~ "morb obese",
                                     TRUE ~ "NA"))

dat_sim <- full %>% 
  mutate(age_plus = age_est + 10,
         baseIntakeH = 1.63*case_when((sex_label == "male" & age_est >= 18 & age_est < 30) ~ 113 + 14.4*wtval + 313*htval/100,
                                     (sex_label == "male" & age_est >= 30 & age_est < 60) ~ -137 + 11.4*wtval + 541*htval/100,
                                     (sex_label == "male" & age_est >= 60) ~ -256 + 11.4*wtval + 541*htval/100,
                                     (sex_label == "female" & age_est >= 18 & age_est < 30) ~ 282 + 10.4*wtval + 615*htval/100,
                                     (sex_label == "female" & age_est >= 30 & age_est < 60)~ -11.6 + 8.18*wtval + 502*htval/100,
                                     (sex_label == "female" & age_est >= 60) ~ 10.7 + 8.52*wtval + 421*htval/100,
                                     TRUE ~ 0), 
         newIntakeH = 1.63*case_when((sex_label == "male" & age_plus >= 18 & age_plus < 30) ~ 113 + 14.4*target + 313*htval/100,
                                    (sex_label == "male" & age_plus >= 30 & age_plus < 60) ~ -137 + 11.4*target + 541*htval/100,
                                    (sex_label == "male" & age_plus >= 60) ~ -256 + 11.4*target + 541*htval/100,
                                    (sex_label == "female" & age_plus >= 18 & age_plus < 30) ~ 282 + 10.4*target + 615*htval/100,
                                    (sex_label == "female" & age_plus >= 30 & age_plus < 60)~ -11.6 + 8.18*target + 502*htval/100,
                                    (sex_label == "female" & age_plus >= 60) ~ 10.7 + 8.52*target + 421*htval/100,
                                    TRUE ~ 0)) %>% 
  mutate(rmrH = baseIntakeH/1.63)
