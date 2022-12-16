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
  mutate(pal = 1.6,
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

# load 2019 data

df2019 <- read.table(here("inputs/data/hse_2019_eul_20211006.tab"), sep = "\t", header = TRUE) %>% 
  filter(WtVal>0 & HtVal>0 & Age35g >=7 ) %>% # remove missing height and weight and children
  mutate(age = case_when(Age35g == 7 ~ (16+19)/2,
                         Age35g == 8 ~ (20+24)/2,
                         Age35g == 9 ~ (25+29)/2,
                         Age35g == 10 ~ (30+34)/2,
                         Age35g == 11 ~ (35+39)/2,
                         Age35g == 12 ~ (40+44)/2,
                         Age35g == 13 ~ (45+49)/2,
                         Age35g == 14 ~ (50+54)/2,
                         Age35g == 15 ~ (55+59)/2,
                         Age35g == 16 ~ (60+64)/2,
                         Age35g == 17 ~ (65+69)/2,
                         Age35g == 18 ~ (70+74)/2,
                         Age35g == 19 ~ (75+79)/2,
                         Age35g == 20 ~ (80+84)/2,
                         Age35g == 21 ~ (85+89)/2,
                         Age35g == 22 ~ (90),
                         TRUE ~ 0)) %>% 
  rename(weight = WtVal,
         height = HtVal,
         sex = Sex,
         bmi = BMIVal,
         id = SerialA) %>% 
  dplyr::select(id, weight, height, age, sex, bmi, wt_int)  %>% # select variables needed
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
                         TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161))) %>% # sex = 2 female Miffin & St.Jeor
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 & bmi < 40 ~ "obese",
                               bmi >= 40 ~ "morbidly obese",
                               TRUE ~ "NA"))


# save in outputs/data

write_csv(df2019, here("outputs/data/hse_2019_clean.csv")) 

# load 1999

df1999 <- read.table(here("inputs/data/1991_2009hse.tab"), sep = "\t", header = TRUE) %>% 
  filter(year == "1999") %>% # filter to year
  filter(!is.na(wtvalid) & !is.na(htvalid)) %>% # remove missing height and weight
  mutate(wt_int = 1) %>% 
  dplyr::select(wtvalid, htvalid, age, sex, bmivalid, wt_int) %>%  # select variables needed
  rename(weight = wtvalid,
         height = htvalid,
         bmi = bmivalid) %>% 
  mutate(pal = 1.6,
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

write_csv(df1999, here("outputs/data/hse_1999_clean.csv")) 

# load 2008

df2008 <- read.table(here("inputs/data/1991_2009hse.tab"), sep = "\t", header = TRUE) %>% 
  filter(year == "2008") %>% # filter to year
  filter(!is.na(wtvalid) & !is.na(htvalid)) %>% # remove missing height and weight
  mutate(wt_int = 1) %>% 
  dplyr::select(wtvalid, htvalid, age, sex, bmivalid, wt_int) %>%  # select variables needed
  rename(weight = wtvalid,
         height = htvalid,
         bmi = bmivalid) %>% 
  mutate(pal = 1.6,
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

write_csv(df2008, here("outputs/data/hse_2008_clean.csv")) 


# load 2013

df2013 <- read.table(here("inputs/data/hse2013ai.tab"), sep = "\t", header = TRUE) %>% 
  filter(Wtval>0 & Htval>0 & Age >=16 ) %>% # remove missing height and weight and children
  rename(weight = Wtval,
         height = Htval,
         age = Age,
         sex = Sex,
         bmi = BMIval,
         id = pserial) %>% 
  dplyr::select(id, weight, height, age, sex, bmi, wt_int)  %>% # select variables needed
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
                         TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161))) %>% # sex = 2 female Miffin & St.Jeor
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 & bmi < 40 ~ "obese",
                               bmi >= 40 ~ "morbidly obese",
                               TRUE ~ "NA"))

# save in outputs/data

write_csv(df2013, here("outputs/data/hse_2013_clean.csv")) 
