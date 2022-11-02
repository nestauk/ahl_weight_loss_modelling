library(deSolve)
library(tidyverse)
library(plotly)
library(reshape2)
library(beepr)

# Read flat file

df <- read.csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse2019_clean_no_outliers.csv", sep = ",", fileEncoding="UTF-8-BOM") 

# Recode sex

df$Sex_letter <- ifelse(df$sex_label == "female", "F", "M")

# Define target weight by gender and BMI class


# mean weight change

w_change <- read.csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/weight_change_table_median.csv") %>% 
  dplyr::select(bmi_class, female, male) %>% 
  melt(.)

dat <- merge(df, w_change, by.x = c("bmi_class", "sex_label"), by.y = c("bmi_class", "variable")) %>% 
  mutate(target = wtval*(1 - value/100)) %>% 
  mutate(bmi = wtval/htval/htval*10000,
         EI0 = if_else(Sex_letter == "M",
                        -.0971*(wtval^2) + 40.853*wtval + 323.59,
                        .0278*(wtval^2) + 9.2893*wtval + 1528.9),
         RMR0 = if_else(Sex_letter == "M", 
                        293*wtval^(0.4330)-5.92*age_est,
                        248*wtval^(0.4356)-5.09*age_est),
         DIT0 = 0.075*EI0,
         SPA0 = 0.326*EI0, 
         PA0 = if_else(EI0-DIT0-RMR0-SPA0 > 0, 
                        EI0-DIT0-RMR0-SPA0,
                        0)) %>% 
  mutate(estPA = EI0/RMR0) 

ggplot(dat, aes(y = estPA, x  = age_est)) + geom_point() + labs(title = "Implied PA") + theme_minimal()

ggplot(dat, aes(y = PA0/EI0, x  = age_est)) + geom_point() + labs(title = "Share of PA") + theme_minimal()

dat %>% 
  group_by(bmi_class) %>% 
  summarise(mean = mean(estPA))

mean(dat$estPA)

hist(dat$estPA)

library(patchwork)

pa <- ggplot(dat, aes(y = PA0, x  = age_est)) + geom_point() + labs(title = "PA") + theme_minimal()
rmr <- ggplot(dat, aes(y = RMR0, x  = age_est)) + geom_point() + labs(title = "RMR") + theme_minimal()
dit <- ggplot(dat, aes(y = DIT0, x  = age_est)) + geom_point() + labs(title = "DIT0") + theme_minimal()
spa <- ggplot(dat, aes(y = SPA0, x  = age_est)) + geom_point() + labs(title = "SPA") + theme_minimal()

pa + rmr + dit + spa + plot_layout(widths = c(2,2))
 