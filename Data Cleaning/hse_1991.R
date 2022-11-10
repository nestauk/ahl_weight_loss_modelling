rm(list=ls())

library(here)
library(tidyverse)

# load 1991-92 data

df1991 <- read.table(here("inputs/data/1991_2009hse.tab"), sep = "\t", header = TRUE) %>% 
  filter(year == "1992") %>% # filter to year
  filter(!is.na(wtvalid) & !is.na(htvalid)) %>% # remove missing height and weight
  mutate(wt_int = 1) %>% 
  dplyr::select(wtvalid, htvalid, age, sex, bmivalid, wt_int) %>%  # select variables needed
  rename(weight = wtvalid,
         height = htvalid,
         bmi = bmivalid) %>% 
  mutate(pal = 1.5,
         rmr = case_when(sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
                         TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161)))  %>% # sex = 2 female Miffin & St.Jeor
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 & bmi < 40 ~ "obese",
                               bmi >= 40 ~ "morbidly obese",
                               TRUE ~ "NA")) %>% 
  mutate(id = n())

# save in outputs/data

write_csv(df1991, here("outputs/data/hse_1991_clean.csv")) 

