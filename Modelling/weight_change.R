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

# Read flat file

df <- read.csv("C:/Users/Elena.Mariani/Documents/Projects/ahl_weight_loss_modelling/Data/calorie_deficit_scenarios_w_imd.csv", sep = ",", fileEncoding="UTF-8-BOM") 

# BMI restriction

bmi_out <- df  %>% filter(BMI_est >= 45 | BMI_est <= 15)

df1 <- df  %>% filter(!X %in% bmi_out$X)

# weight restriction

outlier <- boxplot.stats(df1$Wt_est)$out

wgt_out <- df1  %>% filter(Wt_est %in% outlier)

df_low <- df1[which(!df1$Wt_est %in% outlier),] 

write_csv(df_low, "C:/Users/Elena.Mariani/Documents/Projects/ahl_weight_loss_modelling/Data/calorie_deficit_scenarios_filter.csv")


df_m <- df_low %>% filter(Sex == "male")
df_f <- df_low %>% filter(Sex == "female")

# Read 1991 data

df_1991 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_1991.csv") # there are no outliers

# MEN

df_1991_m <- df_1991 %>% filter(sex == 1)

old_m <- df_1991_m %>% 
  dplyr::select(bmivalid, wtvalid, htvalid) %>% 
  mutate(sample_weight = 1) %>% 
  rename(height = htvalid,
         weight = wtvalid,
         bmi = bmivalid) %>% 
  mutate(intake = -.0971*(weight^2) + 40.853*weight + 323.59) %>% 
  mutate(sex = "Male")

actual_m <- df_m %>% 
  dplyr::select(Ht_est, BMI_est, Wt_est, sample_weight) %>% 
  rename(height = Ht_est,
         bmi = BMI_est,
         weight = Wt_est) %>% 
  mutate(intake = -.0971*(weight^2) + 40.853*weight + 323.59) %>% 
  mutate(sex = "Male")

all_m <- rbind(old_m %>% mutate(type = "1991"),
               actual_m %>% mutate(type = "2019"))


# WOMEN

df_1991_f <- df_1991 %>% filter(sex == 2)

old_f <- df_1991_f %>% 
  dplyr::select(bmivalid, wtvalid, htvalid) %>% 
  mutate(sample_weight = 1) %>% 
  rename(height = htvalid,
         weight = wtvalid,
         bmi = bmivalid) %>% 
  mutate(intake = .0278*(weight^2) + 9.2893*weight + 1528.9) %>% 
  mutate(sex = "Female")

actual_f <- df_f %>% 
  dplyr::select(Ht_est, BMI_est, Wt_est, sample_weight) %>% 
  rename(height = Ht_est,
         bmi = BMI_est,
         weight = Wt_est) %>% 
  mutate(intake = .0278*(weight^2) + 9.2893*weight + 1528.9) %>% 
  mutate(sex = "Female")

all_f <- rbind(old_f %>% mutate(type = "1991"),
               actual_f %>% mutate(type = "2019"))


# combine all old

all_o <- rbind(old_f, old_m)

# combine all actual

all_a <- rbind(actual_f, actual_m) %>% 
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 ~ "obese",
                               TRUE ~ "NA"))

# assign quantiles within BMI category and gender

findQuan <- function(sex_c, bmi_class_c, x){
  dat_n <- all_a %>% dplyr::filter(sex == sex_c & bmi_class == bmi_class_c)
  dat_n$quant <- factor(gtools::quantcut(dat_n$bmi, q = x, labels = c(1:x)), levels = c(1:x))
  dat_n$bmi_class_fine <- factor(paste0(bmi_class_c, "_", dat_n$quant), levels = c(paste0(bmi_class_c, "_", c(1:x))))
  return(dat_n)
}

list <- expand.grid(sex_c = unique(all_a$sex), bmi_class_c = unique(all_a$bmi_class), x= 5)

all_aq <- pmap_dfr(list, findQuan)

# proportions of actual

prop_a <- all_a  %>% 
  count(bmi_class, wt = sample_weight) %>% 
  mutate(freq = n/sum(n))

obese <- prop_a[which(prop_a$bmi_class == "obese"),]$freq
over <- prop_a[which(prop_a$bmi_class == "overweight"),]$freq
normal <- prop_a[which(prop_a$bmi_class == "normal"),]$freq
under <- prop_a[which(prop_a$bmi_class == "underweight"),]$freq

cut_1 <- wtd.quantile(all_o$bmi, probs = under, all_o$sample_weight)   
cut_2 <- unname( wtd.quantile(all_o$bmi, probs = 1 - obese - over, all_o$sample_weight))
cut_3 <- unname( wtd.quantile(all_o$bmi, probs = 1 - obese, all_o$sample_weight))

# add alternative bmi categories to simulated

all_o <- all_o %>% 
  mutate(bmi_class = case_when(bmi <= cut_1 ~ "underweight",
                               bmi > cut_1 & bmi < cut_2 ~ "normal",
                               bmi >= cut_2 & bmi < cut_3 ~ "overweight",
                               bmi >= cut_3 ~ "obese",
                               TRUE ~ "NA"))

all_o %>% count(bmi_class) %>% mutate(freq = n/sum(n))

# add fine grained bmi category to simulated

findQuanO <- function(sex_c, bmi_class_c, x){
  dat_n <- all_o %>% dplyr::filter(sex == sex_c & bmi_class == bmi_class_c)
  dat_n$quant <- factor(gtools::quantcut(dat_n$bmi, q = x, labels = c(1:x)), levels = c(1:x))
  dat_n$bmi_class_fine <- factor(paste0(bmi_class_c, "_", dat_n$quant), levels = c(paste0(bmi_class_c, "_", c(1:x))))
  return(dat_n)
}

all_oq <- pmap_dfr(list, findQuanO)

# combine all genders

all <- rbind(all_oq %>% mutate(type = "1991"), all_aq %>% mutate(type = "2019")) %>% mutate(id = seq_len(n()))

# distribution of weight

all %>% ggplot(., aes(x = type, y = weight)) + geom_boxplot() + facet_grid(sex ~ bmi_class)

# remove outliers

outlier <- boxplot.stats(all$weight)$out

all <- all[which(!all$weight %in% outlier),]

# median intake

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(intakem = wtd.quantile(intake, probs = 0.5, weight = sample_weight)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "intakem")

# mean intake

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(intakem = wtd.mean(intake, weight = sample_weight)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "intakem") %>% 
  mutate(diff_f = `2019_Female` - `1991_Female`,
         diff_m = `2019_Male` - `1991_Male`,
         perc_f = diff_f/`1991_Female`*100,
         perc_m = diff_m/`1991_Male`*100)

dclus2 <- svydesign(id=~id, weights = ~sample_weight, data=all)

funDiff <- function(bmi_class_c, sex_c){
  sub <- subset(dclus2, sex == sex_c & bmi_class == bmi_class_c)
  tt<-svyttest(intake~type, sub)
  return(data.frame(sex = sex_c, bmi_class = bmi_class_c, confint(tt, level=0.95)))
}

list <- expand.grid(sex_c = unique(all$sex), bmi_class_c = unique(all$bmi_class))

pmap_dfr(list, funDiff)  %>% arrange(sex)



# mean weight by category

weight_change_mean <- all %>% 
  group_by(bmi_class , type, sex) %>% 
  summarise(weight_m = wtd.mean(weight, weight = sample_weight)) %>% 
  dcast(., bmi_class  ~ type + sex, value.var = "weight_m") %>% 
  mutate(female = 100*(`2019_Female`/`1991_Female`-1),
         male = 100*(`2019_Male`/`1991_Male`-1))

ggplot(all, aes(x = type, y = weight)) + geom_boxplot() + facet_grid(sex ~ .)

write_csv(weight_change_mean, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Data\\weight_change_table_mean.csv")


# mean weight by fine grained category

weight_change <- all %>% 
  group_by(bmi_class_fine , type, sex) %>% 
  summarise(weight_m = wtd.mean(weight, weight = sample_weight)) %>% 
  dcast(., bmi_class_fine  ~ type + sex, value.var = "weight_m") %>% 
  mutate(female = 100*(`2019_Female`/`1991_Female`-1),
         male = 100*(`2019_Male`/`1991_Male`-1))

weight_change

write_csv(weight_change, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Data\\weight_change_table_fine.csv")




