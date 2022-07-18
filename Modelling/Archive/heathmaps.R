library(tidyverse)
library(hrbrthemes)
library(plotly)
library(survey)

# read scenarios files

sc3 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_scenario_3.csv") %>% 
  mutate(Age_group_2 = cut(Age_est, breaks = c(16, 20, 30, 40, 50, 60, 200), right = F,labels = c("16-19", "20-29", "30-39", "40-49", "50-59", "60 +")))

sc4 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_scenario_4.csv") %>% 
  mutate(Age_group_2 = cut(Age_est, breaks = c(16, 20, 30, 40, 50, 60, 200), right = F,labels = c("16-19", "20-29", "30-39", "40-49", "50-59", "60 +")))

# Declare survey design

hse3 <- svydesign(id = ~id,
                 weights = ~sample_weight,
                 data = sc3)

hse4 <- svydesign(id = ~id,
                 weights = ~sample_weight,
                 data = sc4)


# calculate proportion remaining obese by categories

funHeat <- function(formula, var1, var2){
  
  hse3 <- svydesign(id = ~id,
                    weights = ~sample_weight,
                    data = sc3)
  
  hse4 <- svydesign(id = ~id,
                    weights = ~sample_weight,
                    data = sc4)
  
  lab_y <- case_when(!! ensym(var2) == "IMD_q" ~ "Deprivation Quintiles (1 = low, 5 = high)",
                     !! ensym(var2) == "Sex" ~ "Gender",
                     !! ensym(var2) == "Ethnicity" ~ "Ethnicity",
                     !! ensym(var2) == "Age_group_2" ~ "Age")
  
  lab_x <- case_when(!! ensym(var1) == "IMD_q" ~ "Deprivation Quintiles (1 = low, 5 = high)",
                     !! ensym(var1) == "Sex" ~ "Gender",
                     !! ensym(var1) == "Ethnicity" ~ "Ethnicity",
                     !! ensym(var1) == "Age_group_2" ~ "Age")
  
  sc3_pr <- svytable(formula, design = hse3) %>% 
    as.data.frame() %>% 
    group_by(!! ensym(var1), !! ensym(var2)) %>% 
    mutate(total = sum(Freq),
           remain = Freq/total*100) %>% 
    filter(final_BMI_class == "obese") %>% 
    mutate(scenario = "Scenario 3")
  
  sc4_pr <- svytable(formula, design = hse4) %>% 
    as.data.frame() %>% 
    group_by(!! ensym(var1), !! ensym(var2)) %>% 
    mutate(total = sum(Freq),
           remain = Freq/total*100) %>% 
    filter(final_BMI_class == "obese") %>% 
    mutate(scenario = "Scenario 4")
  
  pr <- rbind(sc3_pr, sc4_pr)
  
  ggplot(pr, aes( x = !! ensym(var1), y = !! ensym(var2), fill = remain)) + 
    geom_tile() +
    facet_wrap( ~ scenario) +
    scale_fill_distiller(palette = 8, direction = 1) +
    theme_ipsum() +
    labs( fill = "Share \nRemaining \nObese (%)",
          y = lab_y,
          x = lab_x)  
}


p_imd_sex <- funHeat( ~ final_BMI_class + Sex + IMD_q, Sex, IMD_q)
p_imd_sex

p_imd_ethn <- funHeat( ~ final_BMI_class + IMD_q + Ethnicity,  Ethnicity, IMD_q)
p_imd_ethn

p_age_sex <- funHeat( ~ final_BMI_class + Age_group_2 + Sex, Sex, Age_group_2)
p_age_sex

p_imd_age <- funHeat( ~ final_BMI_class + Age_group_2 + IMD_q, Age_group_2, IMD_q)
p_imd_age
