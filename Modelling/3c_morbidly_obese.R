rm(list = ls())

require("bw")
require("survey")
library(tidyverse)
library(here)
library(reshape2)
library(furrr)
library(tictoc)
library(beepr)

source(here("Modelling/find_EI.R"))


# load 2019 data
df_2019 <- read_csv(here("outputs/data/hse_2019_clean.csv")) %>% 
  mutate(sex = ifelse(sex == 2 ,"female", "male"))

# mean weight change
w_change <- read.csv(here("outputs/reports/weight_change_table_mean.csv")) %>% 
  dplyr::select(bmi_class, female, male) %>% 
  melt(., id.vars = "bmi_class")

# merge data to obtain weight target
dat <- merge(df_2019, w_change, by.x = c("bmi_class", "sex"), by.y = c("bmi_class", "variable")) %>% 
  mutate(target = weight*(1 - value/100))

# split data by bmi class
dat_split <- split(dat, dat$bmi_class)

# 3 years
all_morb3 <- list(id = dat_split$`morbidly obese`$id,
                  bw = dat_split$`morbidly obese`$weight,
                  ht = dat_split$`morbidly obese`$height/100,
                  age = dat_split$`morbidly obese`$age,
                  sex = dat_split$`morbidly obese`$sex,
                  weight_goal = dat_split$`morbidly obese`$target,
                  days = 365*3,
                  ei_limit = 1000,
                  pal = 1.6) %>% 
  pmap_dfr(., find_EI) 


# combine all

all_morb <- all_morb3 %>% 
  mutate(year = days/365)

write_csv(all_morb, here("outputs/data/morb_3_year.csv"))
beep(3)

