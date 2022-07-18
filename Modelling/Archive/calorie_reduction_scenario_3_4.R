library(tidyverse)
library(reshape2)


# In this script I produce additional summary statistics for scenario3 3 and 4

# scenario 3

sc3 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_scenario_3.csv")

# Summary statistics

sc3 %>% 
  group_by(Sex) %>% 
  summarise(intakeBefore = round(mean(baseIntake), -1),
            intakeAfter = round(mean(newIntake), -1),
            intakeBeforeSe = round(sd(baseIntake), -1),
            intakeAfterSe = round(sd(newIntake), -1))

sc3 %>% 
  group_by(Sex, rnd) %>% 
  summarise(intakeBefore = round(mean(baseIntake), -1),
            intakeAfter = round(mean(newIntake), -1),
            intakeBeforeSe = round(sd(baseIntake), -1),
            intakeAfterSe = round(sd(newIntake), -1)) %>% 
  filter(rnd == 1)


sc3 %>% 
  group_by(Sex, rnd) %>% 
  summarise(intakeBefore = round(mean(baseIntake), -1),
            intakeAfter = round(mean(newIntake), -1),
            intakeBeforeSe = round(sd(baseIntake), -1),
            intakeAfterSe = round(sd(newIntake), -1)) %>% 
  filter(rnd == 0)


sc3_diff <- sc3 %>% 
  select(id, baseIntake, newIntake, rnd, Sex) %>% 
  melt(., id.vars = c("id", "rnd", "Sex"))

sc3_diff %>% filter(Sex == "female" & rnd == 0) %>% t.test(value ~ variable, data = ., paired = T)
sc3_diff %>% filter(Sex == "female" & rnd == 1) %>% t.test(value ~ variable, data = ., paired = T)
sc3_diff %>% filter(Sex == "male" & rnd == 0) %>% t.test(value ~ variable, data = ., paired = T)
sc3_diff %>% filter(Sex == "male" & rnd == 1) %>% t.test(value ~ variable, data = ., paired = T)

sc3 %>% filter(Sex == "female" & rnd == 0) %>% mutate(dif = newIntake - baseIntake) %>% summarise(sd = sd(dif))
sc3 %>% filter(Sex == "female" & rnd == 1) %>% mutate(dif = newIntake - baseIntake) %>% summarise(sd = sd(dif))
sc3 %>% filter(Sex == "male" & rnd == 0) %>% mutate(dif = newIntake - baseIntake) %>% summarise(sd = sd(dif))
sc3 %>% filter(Sex == "male" & rnd == 1) %>% mutate(dif = newIntake - baseIntake) %>% summarise(sd = sd(dif))

# function to calculate se of mean difference

funSE <- function(sex, trg){
  
  dat <- sc3_diff %>% filter(Sex == sex & rnd == trg)
  base <- dat %>% filter(variable == "baseIntake")
  new <- dat %>% filter(variable == "newIntake")

var.pooled <- function(df1,df2,SD1,SD2){
  (df1*SD1^2 + df2*SD2^2)/(df1+df2)
}

dfs <- c(nrow(base), nrow(new))
SDs <- c(sd(base$value), sd(new$value))

var.pool <-var.pooled(df1 = dfs[1]-1,
                      df2 = dfs[2]-1,
                      SD1 = SDs[1],
                      SD2 = SDs[2])

SE.diff <- function(var.pool, n1,n2){
  sqrt(var.pool*(1/n1 + 1/n2))
}

se.dif <- SE.diff(var.pool,
                  n1 = dfs[1],
                  n2 = dfs[2]) 

return(se.dif)

}

funSE(sex = "female", trg = 0)
funSE(sex = "female", trg = 1)
funSE(sex = "male", trg = 0)
funSE(sex = "male", trg = 1)

# plot of calorie reduction share

sc3 %>% 
  filter(rnd == 1) %>% 
  ggplot(., aes(x = calRed)) + 
  geom_density(adjust = 3, colour = "blue", size = 2) +
  theme_bw() +
  scale_x_continuous(labels = scales::label_percent(1)) +
  labs(x = "Daily Calorie Reduction")

# calorie reduction by initial BMI

sc3 %>% 
  filter(rnd == 1) %>% 
  ggplot(., aes(x = BMI_est, y = calRed)) +
  geom_smooth()


sc3 %>% 
  filter(rnd == 0) %>% 
  ggplot(., aes(x = BMI_est, y = calRed)) +
  geom_smooth()


# scenario 4

sc4 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_scenario_4.csv")


# Summary statistics

sc4 %>% 
  group_by(Sex) %>% 
  summarise(intakeBefore = round(mean(baseIntake), -1),
            intakeAfter = round(mean(newIntake), -1),
            intakeBeforeSe = round(sd(baseIntake), -1),
            intakeAfterSe = round(sd(newIntake), -1))

sc4 %>% 
  group_by(Sex, rnd) %>% 
  summarise(intakeBefore = round(mean(baseIntake), -1),
            intakeAfter = round(mean(newIntake), -1),
            intakeBeforeSe = round(sd(baseIntake), -1),
            intakeAfterSe = round(sd(newIntake), -1)) %>% 
  filter(rnd == 1)


sc4 %>% 
  group_by(Sex, rnd) %>% 
  summarise(intakeBefore = round(mean(baseIntake), -1),
            intakeAfter = round(mean(newIntake), -1),
            intakeBeforeSe = round(sd(baseIntake), -1),
            intakeAfterSe = round(sd(newIntake), -1)) %>% 
  filter(rnd == 0)


sc4_diff <- sc4 %>% 
  select(id, baseIntake, newIntake, rnd, Sex) %>% 
  melt(., id.vars = c("id", "rnd", "Sex"))

sc4_diff %>% filter(Sex == "female" & rnd == 0) %>% t.test(value ~ variable, data = ., paired = T)
sc4_diff %>% filter(Sex == "female" & rnd == 1) %>% t.test(value ~ variable, data = ., paired = T)
sc4_diff %>% filter(Sex == "male" & rnd == 0) %>% t.test(value ~ variable, data = ., paired = T)
sc4_diff %>% filter(Sex == "male" & rnd == 1) %>% t.test(value ~ variable, data = ., paired = T)

sc4 %>% filter(Sex == "female" & rnd == 0) %>% mutate(dif = newIntake - baseIntake) %>% summarise(sd = sd(dif))
sc4 %>% filter(Sex == "female" & rnd == 1) %>% mutate(dif = newIntake - baseIntake) %>% summarise(sd = sd(dif))
sc4 %>% filter(Sex == "male" & rnd == 0) %>% mutate(dif = newIntake - baseIntake) %>% summarise(sd = sd(dif))
sc4 %>% filter(Sex == "male" & rnd == 1) %>% mutate(dif = newIntake - baseIntake) %>% summarise(sd = sd(dif))


# plot of calorie reduction share

sc4 %>% 
  filter(rnd == 1) %>% 
  ggplot(., aes(x = calRed)) + 
  geom_density(adjust = 3, colour = "blue", size = 2) +
  theme_bw() +
  scale_x_continuous(labels = scales::label_percent()) +
  labs(x = "Daily Calorie Reduction")
