library(Hmisc)
library(lubridate)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(reshape2)
library(survey)

# read file and add under weight

under <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Data\\calorie_deficit_scenarios_filter.csv", sep = ",", fileEncoding="UTF-8-BOM") %>% 
  filter(BMI_class == "underweight") %>% 
  mutate(finalWeight = Wt_est,
         startWeight = Wt_est,
         height = Ht_est,
         age = Age_est,
         Sex_letter = ifelse(Sex == "female", "F", "M"),
         sex = Sex_letter,
         baseIntake = ifelse(sex == "M", -.0971*(Wt_est^2) + 40.853*Wt_est + 323.59, .0278*(Wt_est^2) + 9.2893*Wt_est + 1528.9),
         newIntake = baseIntake,
         TEE_final = baseIntake,
         weight_final = Wt_est,
         intake_final = newIntake,
         bmi_final = BMI_est,
         value = 0,
         target = Wt_est,
         calRed = 0)

full <- plyr::rbind.fill(under, read_csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_full_weight_loss.csv")) %>% 
  mutate(BMI_class = case_when(BMI_est <= 18.5 ~ "underweight",
                               BMI_est > 18.5 & BMI_est < 25 ~ "normal",
                               BMI_est >= 25 & BMI_est < 30 ~ "overweight",
                               BMI_est >= 30 ~ "obese",
                               TRUE ~ "NA")) %>% 
  mutate(final_BMI_class = case_when(bmi_final <= 18.5 ~ "underweight",
                                     bmi_final > 18.5 & bmi_final < 25 ~ "normal",
                                     bmi_final >= 25 & bmi_final < 30 ~ "overweight",
                                     bmi_final >= 30 ~ "obese",
                                     TRUE ~ "NA"))

rbind(full %>% dplyr::select(BMI_class, sex, baseIntake, sample_weight) %>% mutate(type = "base") %>% rename(intake = baseIntake),
      full %>% dplyr::select(BMI_class, sex, intake_final, sample_weight) %>% mutate(type = "final") %>% rename(intake = intake_final)) %>% 
  group_by(BMI_class, sex, type) %>% 
  summarise(intakeM = wtd.mean(intake, weight = sample_weight)) %>% 
  dcast(., BMI_class + sex ~ type) %>% 
  arrange(sex) %>% 
  mutate(diff = final - base,
         perc = diff/base*100)

full %>% 
  group_by(BMI_class, sex) %>% 
  summarise(year = days(round(mean(day_final),0)) %/% years(1),
            months = days(round(mean(day_final),0)) %% years(1) %/% months(1)) %>% 
  arrange(sex)


dclus2 <- svydesign(id=~X, weights = ~sample_weight, data=full)

d_obese_f <- subset(dclus2,  sex == "F" & BMI_class == "obese")

tt<-svyttest(I(intake_final - baseIntake)~0, d_obese_f)
tt
confint(tt, level=0.95)

d_over_f <- subset(dclus2,  sex == "F" & BMI_class == "overweight")

tt<-svyttest(I(intake_final - baseIntake)~0, d_over_f)
tt
confint(tt, level=0.95)


d_norm_f <- subset(dclus2,  sex == "F" & BMI_class == "normal")

tt<-svyttest(I(intake_final - baseIntake)~0, d_norm_f)
tt
confint(tt, level=0.95)


d_obese_m <- subset(dclus2,  sex == "M" & BMI_class == "obese")

tt<-svyttest(I(intake_final - baseIntake)~0, d_obese_m)
tt
confint(tt, level=0.95)

d_over_m <- subset(dclus2,  sex == "M" & BMI_class == "overweight")

tt<-svyttest(I(intake_final - baseIntake)~0, d_over_m)
tt
confint(tt, level=0.95)


d_norm_m <- subset(dclus2,  sex == "M" & BMI_class == "normal")

tt<-svyttest(I(intake_final - baseIntake)~0, d_norm_m)
tt
confint(tt, level=0.95)


# BMI distribution

rects <- data.frame(xstart = c(10, 18.5, 25, 30), 
                    xend = c(18.5, 25, 30, 45), 
                    col = c("Underweight", "Healthy Weight", "Overweight", "Obese"))

rects$col <- factor(rects$col, levels = c("Underweight", "Healthy Weight", "Overweight", "Obese"))


full %>% 
  dplyr::select(BMI_est, bmi_final, sex, sample_weight) %>% 
  melt(., id.vars = c("sex", "sample_weight")) %>% 
  mutate(label = ifelse(variable == "BMI_est", "Current", "50% Reduction")) %>% 
  ggplot(., aes(x = value, group = label, color = label)) +
  stat_density(aes(weight = sample_weight), adjust = 3.5, lwd = 2, geom = "line", position = "identity") +
  theme_ipsum() +
  geom_rect(data = rects, aes(ymin=0,ymax=0.1, xmin=xstart,xmax=xend, fill=col),  alpha =0.3, inherit.aes=FALSE) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ylim(0,0.1)   +
  labs(x = "BMI",
       color = "",
       title = "BMI Distribution",
       fill = "") +
  guides(size = "none",
         fill = guide_legend(override.aes = list(size = 0, color = "white", alpha = 0.3))) +
  theme(text = element_text(size = 18))

full %>% 
  dplyr::select(baseIntake, intake_final, Sex, sample_weight, BMI_class) %>% 
  melt(., id.vars = c("Sex", "sample_weight", "BMI_class")) %>% 
  mutate(label = ifelse(variable == "baseIntake", "Current", "50% Reduction")) %>% 
  ggplot(., aes(x = value, group = label, color = label)) +
  stat_density(aes(weight = sample_weight), adjust = 3, lwd = 2, geom = "line", position = "identity") +
  facet_grid(Sex ~ .) +
  theme_ipsum() +
  scale_color_viridis_d() +
  labs(x = "kcal/day",
       color = "",
       title = "Daily Intake Distribution") +
  theme(text = element_text(size = 18))

full %>% 
  filter(Sex_letter == "F") %>% 
  dplyr::select(baseIntake, intake_final, sex, sample_weight, BMI_class) %>% 
  melt(., id.vars = c("sex", "sample_weight", "BMI_class")) %>% 
  mutate(label = ifelse(variable == "baseIntake", "Current", "50% Reduction")) %>% 
  ggplot(., aes(x = value, group = label, color = label)) +
  stat_density(aes(weight = sample_weight), adjust = 3, lwd = 2, geom = "line", position = "identity") +
  theme_ipsum() +
  scale_color_viridis_d() +
  labs(x = "kcal/day",
       color = "",
       title = "Women Daily Intake Distribution") +
  theme(text = element_text(size = 18))

full %>% 
  filter(Sex_letter == "M") %>% 
  dplyr::select(baseIntake, intake_final, sex, sample_weight, BMI_class) %>% 
  melt(., id.vars = c("sex", "sample_weight", "BMI_class")) %>% 
  mutate(label = ifelse(variable == "baseIntake", "Current", "50% Reduction")) %>% 
  ggplot(., aes(x = value, group = label, color = label)) +
  stat_density(aes(weight = sample_weight), adjust = 3, lwd = 2, geom = "line", position = "identity") +
  theme_ipsum() +
  scale_color_viridis_d() +
  labs(x = "kcal/day",
       color = "",
       title = "Men Daily Intake Distribution") +
  theme(text = element_text(size = 18))



full %>% 
  mutate(BMI_class = factor(BMI_class, levels = c("normal", "overweight", "obese"))) %>% 
  filter(Sex_letter == "M") %>% 
  dplyr::select(calRed, sex, sample_weight, BMI_class) %>% 
  ggplot(., aes(x = calRed, fill = BMI_class)) +
  stat_density(aes(weight = sample_weight), adjust = 2.5, lwd = 2, geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent) +
  theme_ipsum() +
  facet_grid(BMI_class ~ .) +
  labs(x = "kcal/day",
       color = "",
       title = "Men: Distribution of Daily Intake Difference as %") +
  theme(text = element_text(size = 18))

full %>% 
  mutate(BMI_class = factor(BMI_class, levels = c("normal", "overweight", "obese"))) %>% 
  filter(Sex_letter == "F") %>% 
  dplyr::select(calRed, sex, sample_weight, BMI_class) %>% 
  ggplot(., aes(x = calRed, fill = BMI_class)) +
  stat_density(aes(weight = sample_weight), adjust = 3, lwd = 2, geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent) +
  theme_ipsum() +
  facet_grid(BMI_class ~ .) +
  labs(x = "kca/day",
       color = "",
       title = "Women: Distribution of Daily Intake Difference as %") +
  theme(text = element_text(size = 18))

