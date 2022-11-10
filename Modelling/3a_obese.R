rm(list = ls())

require("bw")
require("survey")
library(tidyverse)
library(here)
library(reshape2)
library(furrr)
library(tictoc)
library(beepr)


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

# define function
find_EI <- function(id, bw, ht, age, sex,weight_goal, days, ei_limit){
  
  deltaNA <- rep(0,days)
  
  #Build a function to uniroot solve
  weight_solve_energy <- function(x){
  
  #Matrix of energy intake change for 365 days
  deltaEI <- rep(x, days)
  
  #Estimate weight change. 
  wtrajectory  <- adult_weight(bw, ht, age, sex, deltaEI, deltaNA, days = days)
  
  #Return weight by the end
  wtrajectory$Body_Weight[days] -weight_goal
  
  }

  #Energy interval for the solution
  EI              <- c(-ei_limit, ei_limit)
  return(data.frame(id, days, ei = uniroot(weight_solve_energy, interval = EI)$root))
}

# 1 year
all_obese1 <- list(id = dat_split$obese$id,
                   bw = dat_split$obese$weight,
                   ht = dat_split$obese$height/100,
                   age = dat_split$obese$age,
                   sex = dat_split$obese$sex,
                   weight_goal = dat_split$obese$target,
                   days = 365*1,
                   ei_limit = 1000) %>% 
  pmap_dfr(., find_EI)

# 3 years
all_obese3 <- list(id = dat_split$obese$id,
                   bw = dat_split$obese$weight,
                   ht = dat_split$obese$height/100,
                   age = dat_split$obese$age,
                   sex = dat_split$obese$sex,
                   weight_goal = dat_split$obese$target,
                   days = 365*3,
                   ei_limit = 1000) %>% 
  pmap_dfr(., find_EI) 

# 5 years
all_obese5 <- list(id = dat_split$obese$id,
                   bw = dat_split$obese$weight,
                   ht = dat_split$obese$height/100,
                   age = dat_split$obese$age,
                   sex = dat_split$obese$sex,
                   weight_goal = dat_split$obese$target,
                   days = 365*5,
                   ei_limit = 1000) %>% 
  pmap_dfr(., find_EI) 

# 10 years
all_obese10 <- list(id = dat_split$obese$id,
                   bw = dat_split$obese$weight,
                   ht = dat_split$obese$height/100,
                   age = dat_split$obese$age,
                   sex = dat_split$obese$sex,
                   weight_goal = dat_split$obese$target,
                   days = 365*10,
                   ei_limit = 1000) %>% 
  pmap_dfr(., find_EI) 

# combine all

all_obese <- rbind(all_obese1, all_obese3, all_obese5, all_obese10) %>% 
  mutate(year = days/365)

write_csv(all_obese, here("outputs/data/obese_all_years.csv"))
beep(3)

