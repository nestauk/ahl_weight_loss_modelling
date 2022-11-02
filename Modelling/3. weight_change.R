rm(list = ls())

library(tidyverse)
library(plotly)
library(reshape2)
library(hrbrthemes)
library(fitdistrplus)
library(fitur)
library(actuar)
library(Hmisc)
library(survey)
library(spatstat)
library(gridExtra)

# Read flat file

df <- read.csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse2019_clean.csv", sep = ",", fileEncoding="UTF-8-BOM") 
# add morbidly obese category

df <- df %>% mutate(bmi_class = ifelse(bmival >= 40, "morb obese", bmi_class))

# out_2019 <- df %>% filter(bmival >= 40)
# 
# # describe morbidly obese
# 
# count(out_2019, sex_label) %>% mutate(perc = n/sum(n))
# 
# mean(out_2019$age_est)
# 
# mean(out_2019$wtval)
# 
# mean(out_2019$bmival)
# 
# nrow(out_2019)/nrow(df)
# 
# # subset data
# 
# df <- df %>% filter(bmival < 40)

write_csv(df, "/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse2019_clean_no_outliers.csv")

# describe sample

# total observations
sum(df$wt_int)

# sex distribution
df %>% 
  count(sex_label, wt = wt_int) %>% 
  mutate(perc = n/sum(n))

# bmi class distribution
df %>% 
  count(bmi_class, wt = wt_int) %>% 
  mutate(perc = n/sum(n)*100)

# age, weight, height, bmi
df %>% 
  summarise(meanAge = wtd.mean(age_est, weights = wt_int),
            meanW = wtd.mean(wtval, weights = wt_int),
            meanH = wtd.mean(htval, weights = wt_int),
            meanBMI = wtd.mean(bmival, weights = wt_int),
            sdAge = sqrt(wtd.var(age_est, weights = wt_int)),
            sdW =sqrt( wtd.var(wtval, weights = wt_int)),
            sdH = sqrt(wtd.var(htval, weights = wt_int)),
            sdBMI = sqrt(wtd.var(bmival, weights = wt_int)))

df_m <- df %>% filter(sex_label == "male")
df_f <- df %>% filter(sex_label == "female")

# Read 1991 data

df_1991 <- read.csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse1991_clean.csv") %>% 
  mutate(bmi_class = case_when(bmivalid <= 18.5 ~ "underweight",
                               bmivalid > 18.5 & bmivalid < 25 ~ "normal",
                               bmivalid >= 25 & bmivalid < 30 ~ "overweight",
                               bmivalid >= 30 & bmivalid < 40 ~ "obese",
                               bmivalid >= 40 ~ "morb obese",
                               TRUE ~ "NA"))


# out_1991 <- df_1991 %>% filter(bmivalid >= 40)
# 
# # describe morbidly obese
# 
# count(out_1991, sex) %>% mutate(perc = n/sum(n))
# 
# mean(out_1991$age)
# 
# mean(out_1991$wtvalid)
# 
# mean(out_1991$bmivalid)
# 
# nrow(out_1991)/nrow(df_1991)
# 
# df_1991 <- df_1991 %>% filter(bmivalid < 40)

write_csv(df_1991, "/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse1991_clean_no_outliers.csv")

# describe sample

nrow(df_1991)

# sex distribution
df_1991 %>% 
  count(sex) %>% 
  mutate(perc = n/sum(n))

# bmi distribution
df_1991 %>% 
  count(bmi_class) %>% 
  mutate(perc = n/sum(n)*100)

# age, weight, height, bmi
df_1991 %>% 
  summarise(meanAge = mean(age),
            meanW = mean(wtvalid),
            meanH = mean(htvalid),
            meanBMI = mean(bmivalid),
            sdAge = sd(age),
            sdW = sd(wtvalid),
            sdH = sd(htvalid),
            sdBMI = sd(bmivalid))


df_1991_m <- df_1991 %>% filter(sex == "1")
df_1991_f <- df_1991 %>% filter(sex == "2")



# MALES

# calculate intake for 1991 males
old_m <- df_1991_m %>% 
  dplyr::select(bmivalid, wtvalid, htvalid) %>% 
  mutate(wt_int = 1) %>% 
  rename(height = htvalid,
         weight = wtvalid,
         bmi = bmivalid) %>% 
  mutate(intake = -.0971*(weight^2) + 40.853*weight + 323.59) %>% 
  mutate(sex = "Male")

# calculate intake for 2019 males
actual_m <- df_m %>% 
  dplyr::select(htval, bmival, wtval, wt_int) %>% 
  rename(height = htval,
         bmi = bmival,
         weight = wtval) %>% 
  mutate(intake = -.0971*(weight^2) + 40.853*weight + 323.59) %>% 
  mutate(sex = "Male")

# combine
all_m <- rbind(old_m %>% mutate(type = "1991-92"),
               actual_m %>% mutate(type = "2019"))

# BMI distribution

data_breaks <- data.frame(start = c(-Inf, 18.5, 25, 30, 40),  # Create data with breaks
                          end = c(18.5, 25, 30, 40, Inf),
                          colors = gray.colors(5),
                          labels = c("Underweight", "Normal", "Overweight", "Obese", "Morbidly Obese"),
                          pos_x = c(15, 22, 27, 35, 45),
                          pos_y = rep(0.15,5))

data_breaks

ggplot() +
  geom_rect(data = data_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = all_m, aes(x = bmi, group = type, color = type, weight = wt_int), size = 3) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution - Males") +
  theme_ipsum(base_size = 15, axis_title_size = 15) +
  scale_color_manual(values=c("#0000FF", "#F6A4B7")) +
  scale_fill_manual(values = rev(gray.colors(5))) +
  guides(fill = "none")
  
# FEMALES

# calculate intake for 1991 females
old_f <- df_1991_f %>% 
  dplyr::select(bmivalid, wtvalid, htvalid) %>% 
  mutate(wt_int = 1) %>% 
  rename(height = htvalid,
         weight = wtvalid,
         bmi = bmivalid) %>% 
  mutate(intake = .0278*(weight^2) + 9.2893*weight + 1528.9) %>% 
  mutate(sex = "Female")

# calculate intake for 2019 females
actual_f <- df_f %>% 
  dplyr::select(htval, bmival, wtval, wt_int) %>% 
  rename(height = htval,
         bmi = bmival,
         weight = wtval) %>% 
  mutate(intake = .0278*(weight^2) + 9.2893*weight + 1528.9) %>% 
  mutate(sex = "Female")

# combine
all_f <- rbind(old_f %>% mutate(type = "1991-92"),
               actual_f %>% mutate(type = "2019"))

# BMI distribution
ggplot() +
  geom_rect(data = data_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = colors),
            alpha=0.5) +
  geom_text(data = data_breaks, aes(x = pos_x, y = pos_y, label = labels)) + 
  geom_density(data = all_f, aes(x = bmi, group = type, color = type, weight = wt_int), size = 3) +
  xlim(12,50) +
  labs(x = "BMI",
       y = "Density",
       color = "",
       title = "BMI Distribution - Females") +
  theme_ipsum(base_size = 15, axis_title_size = 15) +
  scale_color_manual(values=c("#0000FF", "#F6A4B7")) +
  scale_fill_manual(values = rev(gray.colors(5))) +
  guides(fill = "none")

# combine all old

all_o <- rbind(old_f, old_m)

# combine all 2019 and generate bmi class

all_a <- rbind(actual_f, actual_m) %>% 
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 & bmi < 40 ~ "obese",
                               bmi >= 40 ~ "morb obese",
                               TRUE ~ "NA"))

# proportions of bmi classes in 2019

prop_a <- all_a  %>% 
  count(bmi_class, wt = wt_int) %>% 
  mutate(freq = n/sum(n))

morb <- prop_a[which(prop_a$bmi_class == "morb obese"),]$freq
obese <- prop_a[which(prop_a$bmi_class == "obese"),]$freq
over <- prop_a[which(prop_a$bmi_class == "overweight"),]$freq
normal <- prop_a[which(prop_a$bmi_class == "normal"),]$freq
under <- prop_a[which(prop_a$bmi_class == "underweight"),]$freq

cut_1 <- wtd.quantile(all_o$bmi, probs = under, all_o$wt_int)   
cut_2 <- unname( wtd.quantile(all_o$bmi, probs = 1 - morb - obese - over, all_o$wt_int))
cut_3 <- unname( wtd.quantile(all_o$bmi, probs = 1 - morb - obese, all_o$wt_int))
cut_4 <- unname( wtd.quantile(all_o$bmi, probs = 1 - morb , all_o$wt_int))

# add alternative bmi categories to 1991

all_o <- all_o %>% 
  mutate(bmi_class = case_when(bmi <= cut_1 ~ "underweight",
                               bmi > cut_1 & bmi < cut_2 ~ "normal",
                               bmi >= cut_2 & bmi < cut_3 ~ "overweight",
                               bmi >= cut_3 & bmi < cut_4 ~ "obese",
                               bmi >= cut_4 ~ "morb obese",
                               TRUE ~ "NA"))

# check distribution

all_o %>% count(bmi_class) %>% mutate(freq = n/sum(n))


# combine all sexes and years

all <- rbind(all_o %>% mutate(type = "1991-92"), all_a %>% mutate(type = "2019")) %>% mutate(id = seq_len(n()))

# intake plot

plot1 <- all %>% 
  mutate(sex = ifelse(sex == "Female", "Females", "Males")) %>% 
ggplot(., aes(x = intake, group = type, color = type)) + 
  geom_density(aes(weight = wt_int), size = 3) +
  facet_grid(sex ~ .) +
  labs(x = "kcal/day",
       color = "",
       title = "Calculated kcal/day") +
  xlim(1800,4000) +
  theme_ipsum(base_size = 15, axis_title_size = 15) +
  scale_color_manual(values=c("#0000FF", "#F6A4B7")) 


density_diff <- rbind(
  all %>% 
    filter(sex == "Female") %>% 
    group_by(type) %>% 
  # calculate densities for each group over same range; store in list column
  summarise(d = list(density(intake, from = min(.$intake), to = max(.$intake)))) %>% 
  # make a new data.frame from two density objects
  do(data.frame(x = .$d[[1]]$x,    # grab one set of x values (which are the same)
                y = .$d[[2]]$y - .$d[[1]]$y)) %>%    # and subtract the y values
    mutate(sex = "Females"),
  all %>% 
    filter(sex == "Male") %>% 
    group_by(type) %>% 
    # calculate densities for each group over same range; store in list column
    summarise(d = list(density(intake, from = min(.$intake), to = max(.$intake)))) %>% 
    # make a new data.frame from two density objects
    do(data.frame(x = .$d[[1]]$x,    # grab one set of x values (which are the same)
                  y = .$d[[2]]$y - .$d[[1]]$y)) %>%    # and subtract the y values
    mutate(sex = "Males"))

options(scipen=100)

plot2 <- ggplot(density_diff, aes(x, y)) + 
  geom_line(color = "#FF6E47", size = 3) +
  geom_hline(yintercept = 0) +
  facet_grid(sex ~ . ) +
  labs(x = "kcal/day",
       color = "",
       title = "Difference in Daily Intake (2019 - 1991/92)",
       y = "Density difference") +
  xlim(1800,4000) +
  theme_ipsum(base_size = 15, axis_title_size = 15) 

plot2

grid.arrange(plot1, plot2, ncol=2)

# median intake

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(intakem = round(wtd.quantile(intake, probs = 0.5, weight = wt_int),0)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "intakem") %>% 
  mutate(diff_f = `2019_Female` - `1991-92_Female`,
         diff_m = `2019_Male` - `1991-92_Male`,
         perc_f = round(diff_f/`1991-92_Female`*100,1),
         perc_m = round(diff_m/`1991-92_Male`*100,1))

# IQR

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(intake25 = round(wtd.quantile(intake, probs = 0.25, weight = wt_int),0),
            intake75 = round(wtd.quantile(intake, probs = 0.75, weight = wt_int),0)) %>% 
  mutate(iqr = intake75 - intake25) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "iqr")


# mean intake

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(intakem = round(wtd.mean(intake, weight = wt_int),0)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "intakem") %>% 
  mutate(diff_f = `2019_Female` - `1991-92_Female`,
         diff_m = `2019_Male` - `1991-92_Male`,
         perc_f = round(diff_f/`1991-92_Female`*100,1),
         perc_m = round(diff_m/`1991-92_Male`*100,1))

# standard deviation

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(intakem = round(sqrt(wtd.var(intake, weight = wt_int)),0)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "intakem")

# confidence interval

dclus2 <- svydesign(id=~id, weights = ~wt_int, data=all)

funDiff <- function(bmi_class_c, sex_c){
  sub <- subset(dclus2, sex == sex_c & bmi_class == bmi_class_c)
  tt<-svyttest(intake~type, sub)
  return(data.frame(sex = sex_c, bmi_class = bmi_class_c, round(confint(tt, level=0.95),1)))
}

list <- expand.grid(sex_c = unique(all$sex), bmi_class_c = unique(all$bmi_class))

pmap_dfr(list, funDiff)  %>% arrange(sex)

# mean weight

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(weightm = round(wtd.mean(weight, weight = wt_int),1)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "weightm") %>% 
  mutate(diff_f = `2019_Female` - `1991-92_Female`,
         diff_m = `2019_Male` - `1991-92_Male`,
         perc_f = round(diff_f/`1991-92_Female`*100,1),
         perc_m = round(diff_m/`1991-92_Male`*100,1))

# median weight

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(weightm = round(wtd.quantile(weight, probs = 0.5, weight = wt_int),0)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "weightm") %>% 
  mutate(diff_f = `2019_Female` - `1991-92_Female`,
         diff_m = `2019_Male` - `1991-92_Male`,
         perc_f = round(diff_f/`1991-92_Female`*100,1),
         perc_m = round(diff_m/`1991-92_Male`*100,1))


# standard deviation

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(weightm = round(sqrt(wtd.var(weight, weight = wt_int)),1)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "weightm")

# IQR

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(w25 = round(wtd.quantile(weight, probs = 0.25, weight = wt_int),0),
            w75 = round(wtd.quantile(weight, probs = 0.75, weight = wt_int),0)) %>% 
  mutate(iqr = w75 - w25) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "iqr")


# output

weight_change_median <- all %>% 
  group_by(bmi_class , type, sex) %>% 
  summarise(weight_m = quantile(weight, prob = 0.5)) %>% 
  dcast(., bmi_class  ~ type + sex, value.var = "weight_m") %>% 
  mutate(female = 100*(`2019_Female`/`1991-92_Female`-1),
         male = 100*(`2019_Male`/`1991-92_Male`-1))

weight_change_mean <- all %>% 
  group_by(bmi_class , type, sex) %>% 
  summarise(weight_m = mean(weight)) %>% 
  dcast(., bmi_class  ~ type + sex, value.var = "weight_m") %>% 
  mutate(female = 100*(`2019_Female`/`1991-92_Female`-1),
         male = 100*(`2019_Male`/`1991-92_Male`-1))

weight_change_gmean <- all %>% 
  group_by(bmi_class , type, sex) %>% 
  summarise(weight_m = exp(mean(log(weight)))) %>% 
  dcast(., bmi_class  ~ type + sex, value.var = "weight_m") %>% 
  mutate(female = 100*(`2019_Female`/`1991-92_Female`-1),
         male = 100*(`2019_Male`/`1991-92_Male`-1))

write_csv(weight_change_median,"/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/weight_change_table_median.csv")

write_csv(weight_change_mean,"/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/weight_change_table_mean.csv")



