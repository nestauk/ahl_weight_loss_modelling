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

# Read flat file 2019

df <- read.csv("/Users/elenamariani/Documents/working/weight_loss_modelling/hse2019_clean.csv", sep = ",", fileEncoding="UTF-8-BOM") 

# flag outliers

outlier <- boxplot.stats(df$wtval)$out

df_low <- df %>% mutate(outlier = wtval %in% outlier)

df_m <- df_low %>% filter(sex_label == "male")
df_f <- df_low %>% filter(sex_label == "female")

sum(df_low$wt_int)

df_low %>% 
  count(sex_label, wt = wt_int) %>% 
  mutate(perc = n/sum(n))

df_low %>% 
  count(bmi_class, wt = wt_int) %>% 
  mutate(perc = n/sum(n)*100)

df_low %>% 
  summarise(meanAge = wtd.mean(age_est, weights = wt_int),
            meanW = wtd.mean(wtval, weights = wt_int),
            meanH = wtd.mean(htval, weights = wt_int),
            meanBMI = wtd.mean(bmival, weights = wt_int),
            sdAge = sqrt(wtd.var(age_est, weights = wt_int)),
            sdW =sqrt( wtd.var(wtval, weights = wt_int)),
            sdH = sqrt(wtd.var(htval, weights = wt_int)),
            sdBMI = sqrt(wtd.var(bmival, weights = wt_int)))


df_m <- df_low %>% filter(sex_label == "male")
df_f <- df_low %>% filter(sex_label == "female")

# Read 1991 data

df_1991 <- read.csv("/Users/elenamariani/Documents/working/weight_loss_modelling/hse1991_clean.csv")

df_1991 %>% 
  group_by(sex) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = n/sum(n))

df_1991 %>% 
  mutate(bmi_class = case_when(bmivalid <= 18.5 ~ "underweight",
                               bmivalid > 18.5 & bmivalid < 25 ~ "normal",
                               bmivalid >= 25 & bmivalid < 30 ~ "overweight",
                               bmivalid >= 30 ~ "obese",
                               TRUE ~ "NA")) %>% 
  count(bmi_class) %>% 
  mutate(perc = n/sum(n)*100)


df_1991 %>% 
  summarise(meanAge = mean(age),
            meanW = mean(wtvalid),
            meanH = mean(htvalid),
            meanBMI = mean(bmivalid),
            sdAge = sd(age),
            sdW = sd(wtvalid),
            sdH = sd(htvalid),
            sdBMI = sd(bmivalid))

# MEN

df_1991_m <- df_1991 %>% filter(sex == 1)

old_m <- df_1991_m %>% 
  dplyr::select(bmivalid, wtvalid, htvalid) %>% 
  mutate(wt_int = 1) %>% 
  rename(height = htvalid,
         weight = wtvalid,
         bmi = bmivalid) %>% 
  mutate(intake = -.0971*(weight^2) + 40.853*weight + 323.59) %>% 
  mutate(sex = "Male")

actual_m <- df_m %>% 
  dplyr::select(htval, bmival, wtval, wt_int) %>% 
  rename(height = htval,
         bmi = bmival,
         weight = wtval) %>% 
  mutate(intake = -.0971*(weight^2) + 40.853*weight + 323.59) %>% 
  mutate(sex = "Male")

all_m <- rbind(old_m %>% mutate(type = "1991"),
               actual_m %>% mutate(type = "2019"))

ggplot(all_m, aes(x = bmi, group = type, color = type)) + 
  geom_density(aes(weight = wt_int)) +
  labs(x = "BMI",
       color = "",
       title = "Males") +
  theme_ipsum()


ggplot(all_m, aes(x = intake, group = type, color = type)) + 
  geom_density(aes(weight = wt_int)) +
  labs(x = "kcal/day",
       color = "",
       title = "Males") +
  theme_ipsum()


# WOMEN

df_1991_f <- df_1991 %>% filter(sex == 2)

old_f <- df_1991_f %>% 
  dplyr::select(bmivalid, wtvalid, htvalid) %>% 
  mutate(wt_int = 1) %>% 
  rename(height = htvalid,
         weight = wtvalid,
         bmi = bmivalid) %>% 
  mutate(intake = .0278*(weight^2) + 9.2893*weight + 1528.9) %>% 
  mutate(sex = "Female")

actual_f <- df_f %>% 
  dplyr::select(htval, bmival, wtval, wt_int) %>% 
  rename(height = htval,
         bmi = bmival,
         weight = wtval) %>% 
  mutate(intake = .0278*(weight^2) + 9.2893*weight + 1528.9) %>% 
  mutate(sex = "Female")

all_f <- rbind(old_f %>% mutate(type = "1991"),
               actual_f %>% mutate(type = "2019"))

ggplot(all_f, aes(x = bmi, group = type, color = type)) + 
  geom_density(aes(weight = wt_int)) +
  labs(x = "BMI",
       color = "",
       title = "Females") +
  theme_ipsum()


ggplot(all_f, aes(x = intake, group = type, color = type)) + 
  geom_density(aes(weight = wt_int)) +
  labs(x = "kcal/day",
       color = "",
       title = "Females") +
  theme_ipsum()


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
  count(bmi_class, wt = wt_int) %>% 
  mutate(freq = n/sum(n))

obese <- prop_a[which(prop_a$bmi_class == "obese"),]$freq
over <- prop_a[which(prop_a$bmi_class == "overweight"),]$freq
normal <- prop_a[which(prop_a$bmi_class == "normal"),]$freq
under <- prop_a[which(prop_a$bmi_class == "underweight"),]$freq

cut_1 <- wtd.quantile(all_o$bmi, probs = under, all_o$wt_int)   
cut_2 <- unname( wtd.quantile(all_o$bmi, probs = 1 - obese - over, all_o$wt_int))
cut_3 <- unname( wtd.quantile(all_o$bmi, probs = 1 - obese, all_o$wt_int))

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

all <- rbind(all_oq %>% mutate(type = "1991-92"), all_aq %>% mutate(type = "2019")) %>% mutate(id = seq_len(n()))

# distribution of weight

all %>% ggplot(., aes(x = type, y = weight)) + geom_boxplot(aes(weight = wt_int)) + facet_grid(sex ~ bmi_class)

all %>% 
  ggplot(., aes(x = type, y = weight)) + 
  geom_boxplot(aes(weight = wt_int)) + 
  facet_grid(sex ~ .) +
  theme_ipsum()

all %>% 
  ggplot(., aes(x = type, y = bmi)) + 
  geom_boxplot(aes(weight = wt_int)) + 
  facet_grid(sex ~ .) +
  theme_ipsum()


all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(weightm = round(wtd.quantile(weight, probs = 0.5, weight = wt_int),1)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "weightm") %>% 
  mutate(diff_f = `2019_Female` - `1991-92_Female`,
         diff_m = `2019_Male` - `1991-92_Male`,
         perc_f = round(diff_f/`1991-92_Female`*100,1),
         perc_m = round(diff_m/`1991-92_Male`*100,1))


# remove outliers

# outlier <- boxplot.stats(all$weight)$out
# 
# all <- all[which(!all$weight %in% outlier),]

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

# difference

dclus2 <- svydesign(id=~id, weights = ~wt_int, data=all)


funDiff <- function(bmi_class_c, sex_c){
  sub <- subset(dclus2, sex == sex_c & bmi_class == bmi_class_c)
  tt<-svyttest(intake~type, sub)
  return(data.frame(sex = sex_c, bmi_class = bmi_class_c, round(confint(tt, level=0.95),1)))
}

list <- expand.grid(sex_c = unique(all$sex), bmi_class_c = unique(all$bmi_class))

pmap_dfr(list, funDiff)  %>% arrange(sex)


# median bmi

all %>% 
  group_by(bmi_class, type) %>% 
  summarise(bmim = wtd.quantile(bmi, probs = 0.5, weight = wt_int)) %>% 
  dcast(., bmi_class ~ type , value.var = "bmim")

all %>% 
  filter(bmi_class == "obese") %>% 
  ggplot(., aes(x = intake, group = type, fill = type)) + 
  geom_density(aes(weight = wt_int)) + 
  facet_grid(sex ~ .) +
  labs(x = "kcal/day",
       fill = "",
       title = "Daily Intake") +
  theme_ipsum()

all %>% 
  filter(bmi_class == "obese") %>% 
  ggplot(., aes(x = bmi, group = type, color = type)) + geom_density(aes(weight = wt_int))

all %>% 
  group_by(type, bmi_class) %>% 
  count(., wt = wt_int) %>% 
  group_by(type) %>% 
  mutate(total = sum(n)) %>% 
  mutate(perc = n/total*100)

all %>% 
  filter(type == "2019") %>% 
  group_by(bmi_class) %>% 
  count(., wt = wt_int) %>% 
  ungroup() %>% 
  mutate(total = sum(n)) %>% 
  mutate(perc = n/total*100)


df_low %>% 
  count(bmi_class, wt = wt_int) %>% 
  mutate(perc = n/sum(n)*100)

df_low %>% 
  group_by(bmi_class) %>% 
  count(., wt = wt_int) %>% 
  ungroup() %>% 
  mutate(total = sum(n)) %>% 
  mutate(perc = n/total*100)

test <- all %>% 
  filter(type == "2019")

test %>% 
  group_by(bmi_class) %>% 
  summarise(min = min(bmi),
            max = max(bmi))

df_low %>% 
  group_by(bmi_class) %>% 
  summarise(min = min(bmival),
            max = max(bmival))

all %>% 
  mutate(label = ifelse(type == "A", "Actual 2019", "Simulated")) %>% 
  ggplot(., aes(y = intake, x = bmi_class, fill = label)) + 
  geom_boxplot(aes(weight = wt_int)) +
  labs(title = "Distribution of Calorie Intake",
       fill = "") +
  theme_ipsum(base_size = 20)

all %>% 
  filter(sex == "Female") %>% 
  mutate(label = ifelse(type == "A", "Actual 2019", "Simulated")) %>% 
  ggplot(., aes(x = intake, group = label, color = label)) +
  stat_ecdf(geom = "step", pad = F) +
  facet_grid(bmi_class ~ .) +
  theme_ipsum(base_size = 15) +
  labs(title = "Cumulative Intake Distribution - Women")

all %>% 
  filter(sex == "Male") %>% 
  mutate(label = ifelse(type == "A", "Actual 2019", "Simulated")) %>% 
  ggplot(., aes(x = intake, group = label, color = label)) +
  stat_ecdf(geom = "step", pad = F) +
  facet_grid(bmi_class ~ .) +
  theme_ipsum(base_size = 15) +
  labs(title = "Cumulative Intake Distribution - Men")

##############################################################################################################
# analysis by BMI class

# obese

obese_all <- all %>% 
  filter(bmi_class == "obese")

cdf_s_f <- ecdf(obese_all[which(obese_all$type == "S" & obese_all$sex == "Female"),]$intake)
cdf_a_f <- ecdf(obese_all[which(obese_all$type == "A" & obese_all$sex == "Female"),]$intake)

quan_s_f <- wtd.quantile(obese_all[which(obese_all$type == "S" & obese_all$sex == "Female"),]$intake, seq(0.1, 0.9, 0.1), weight = obese_all[which(obese_all$type == "S" & obese_all$sex == "Female"),]$wt_int)
quan_a_f <- quantile(obese_all[which(obese_all$type == "A" & obese_all$sex == "Female"),]$intake, seq(0.1, 0.9, 0.1),weight = obese_all[which(obese_all$type == "S" & obese_all$sex == "Female"),]$wt_int)

quan_all_f <- quan_s_f/quan_a_f - 1

mean(quan_all_f)

cdf_s_f(3000)
cdf_a_f(3000)

obese_all %>% 
  group_by(type, sex) %>% 
  summarise(meanI = mean(intake),
            medianI = median(intake),
            quant10 = quantile(intake, 0.1),
            quant20 = quantile(intake, 0.2),
            quant30 = quantile(intake, 0.3),
            quant40 = quantile(intake, 0.4),
            quant50 = quantile(intake, 0.5),
            quant60 = quantile(intake, 0.6),
            quant70 = quantile(intake, 0.7),
            quant80 = quantile(intake, 0.8),
            quant90 = quantile(intake, 0.9))

quant_o <- 
  rbind(obese_all %>% 
          filter(sex == "Female") %>% 
          group_by(type) %>% 
          summarise(quant10 = quantile(intake, 0.1),
                    quant20 = quantile(intake, 0.2),
                    quant30 = quantile(intake, 0.3),
                    quant40 = quantile(intake, 0.4),
                    quant50 = quantile(intake, 0.5),
                    quant60 = quantile(intake, 0.6),
                    quant70 = quantile(intake, 0.7),
                    quant80 = quantile(intake, 0.8),
                    quant90 = quantile(intake, 0.9)) %>% 
          melt(.),
  )

quan_s_f <- quantile(obese_all[which(obese_all$type == "S" & obese_all$sex == "Female"),]$intake, seq(0,1, 0.1))
quan_a_f <- quantile(obese_all[which(obese_all$type == "A" & obese_all$sex == "Female"),]$intake, seq(0,1, 0.1))

quan_f <- data.frame(quan = seq(0,1, 0.1), simulated = quan_s_f, actual = quan_a_f) %>% 
  mutate(diff = (simulated - actual)/actual*100)

ggplot(quan_f, aes(x = quan, y = diff)) + 
  geom_bar(stat = "identity") +
  theme_ipsum() +
  labs(x = "Percentile",
       y = "kcal/day % reduction") +
  geom_vline(xintercept = c(0.1, 0.5, 0.9))


quan_s_m <- quantile(obese_all[which(obese_all$type == "S" & obese_all$sex == "Male"),]$intake, seq(0,1, 0.1))
quan_a_m <- quantile(obese_all[which(obese_all$type == "A" & obese_all$sex == "Male"),]$intake, seq(0,1, 0.1))

quan_m <- data.frame(quan = seq(0,1, 0.1), simulated = quan_s_m, actual = quan_a_m) %>% 
  mutate(diff = (simulated - actual)/actual*100)

ggplot(quan_m, aes(x = quan, y = diff)) + 
  geom_bar(stat = "identity") +
  theme_ipsum() +
  labs(x = "Percentile",
       y = "kcal/day % reduction") +
  geom_vline(xintercept = c(0.1, 0.5, 0.9))

# overweight

over_all <- all %>% 
  filter(bmi_class == "overweight")

over_all %>% 
  group_by(type, sex) %>% 
  summarise(meanI = mean(intake),
            medianI = median(intake),
            quant10 = quantile(intake, 0.1),
            quant20 = quantile(intake, 0.2),
            quant30 = quantile(intake, 0.3),
            quant40 = quantile(intake, 0.4),
            quant50 = quantile(intake, 0.5),
            quant60 = quantile(intake, 0.6),
            quant70 = quantile(intake, 0.7),
            quant80 = quantile(intake, 0.8),
            quant90 = quantile(intake, 0.9))

# Simulate distribution based on 1991 parameters for men and women

# MEN

set.seed(333)

newDistM <- rinvgamma(nrow(df_m),  shape = 46.2399, scale = 1161.3295)

dist0 <- ecdf(newDistM)

# overweight + obese

1 - dist0(25)

# obese

1 - dist0(30)

# generate random height based on distribution

height_m <- df_m$htval

hist(height_m)

boxplot(df_m$bmival)

descdist( data = height_m , discrete = FALSE)


normal_ = fitdist(height_m, "norm")
weibull_ = fitdist(height_m, "weibull")
gamma_ = fitdist(height_m, "gamma")
invweibull_ = fitdist(height_m, "invweibull")


plot(normal_)
plot(weibull_)
plot(gamma_)
plot(invweibull_)

## Step (3) Estimate parameters

print(normal_)
print(weibull_)
print(gamma_)
print(invweibull_)

summary(normal_)
summary(weibull_)
summary(gamma_)
summary(invweibull_)

gofstat(list(normal_, weibull_, gamma_, invweibull_), 
        fitnames=c("normal", "Weibull", "gamma", "inverseWeibull"))



# height is normally distributed with a mean of 175.22 and sd of 7.53

set.seed(333)

newH <- rnorm(nrow(df_m), mean = 175.214482, sd = 7.531631)

est_m <- data.frame(height = newH, bmi = newDistM) %>% 
  mutate(wt_int = 1) %>% 
  mutate(weight = bmi * height/100 * height/100) %>% 
  mutate(intake = -.0971*(weight^2) + 40.853*weight + 323.59) %>% 
  mutate(sex = "Male")

actual_m <- df_m %>% 
  dplyr::select(htval, bmival, wtval, wt_int) %>% 
  rename(height = htval,
         bmi = bmival,
         weight = wtval) %>% 
  mutate(intake = -.0971*(weight^2) + 40.853*weight + 323.59) %>% 
  mutate(sex = "Male")

all_m <- rbind(est_m %>% mutate(type = "E"),
               actual_m %>% mutate(type = "A"))

ggplot(all_m, aes(x = intake, group = type)) + geom_density(aes(weight = wt_int))

wtd.quantile(est_m$intake, c(0.25, 0.75), weights = est_m$wt_int)
wtd.quantile(actual_m$intake, c(0.25, 0.75), weights = actual_m$wt_int)

# WOMEN

set.seed(333)

newDistF <- rinvgamma(nrow(df_f),  shape = 33.76421, scale = 827.86971)

dist0 <- ecdf(newDistF)

# overweight + obese

1 - dist0(25)

# obese

1 - dist0(30)

# generate random height based on distribution

height_f <- df_f$htval

hist(height_f)

boxplot(df_m$bmival)

descdist( data = height_f , discrete = FALSE)


normal_ = fitdist(height_f, "norm")
weibull_ = fitdist(height_f, "weibull")
gamma_ = fitdist(height_f, "gamma")
invweibull_ = fitdist(height_f, "invweibull")


plot(normal_)
plot(weibull_)
plot(gamma_)
plot(invweibull_)

## Step (3) Estimate parameters

print(normal_)
print(weibull_)
print(gamma_)
print(invweibull_)

summary(normal_)
summary(weibull_)
summary(gamma_)
summary(invweibull_)

gofstat(list(normal_, weibull_, gamma_, invweibull_), 
        fitnames=c("normal", "Weibull", "gamma", "inverseWeibull"))



# height is normally distributed with a mean of 161.57 and sd of 6.94

set.seed(333)

newH_f <- rnorm(nrow(df_f), mean = 161.57155, sd = 6.93519)

est_f <- data.frame(height = newH_f, bmi = newDistF) %>% 
  mutate(wt_int = 1) %>% 
  mutate(weight = bmi * height/100 * height/100) %>% 
  mutate(intake = .0278*(weight^2) + 9.2893*weight + 1528.9) %>% 
  mutate(sex = "Female")

actual_f <- df_f %>% 
  dplyr::select(htval, bmival, wtval, wt_int) %>% 
  rename(height = htval,
         bmi = bmival,
         weight = wtval) %>% 
  mutate(intake = .0278*(weight^2) + 9.2893*weight + 1528.9) %>% 
  mutate(sex = "Female")

all_f <- rbind(est_f %>% mutate(type = "E"),
               actual_f %>% mutate(type = "A"))

ggplot(all_f, aes(x = intake, group = type)) + geom_density(aes(weight = wt_int))

wtd.quantile(est_f$intake, c(0.25, 0.75), weight = est_f$wt_int)
wtd.quantile(actual_f$intake, c(0.25, 0.75), weight = actual_f$wt_int)

# combine all simulated

all_s <- rbind(est_f, est_m)

# combine all actual

all_a <- rbind(actual_f, actual_m) %>% 
  mutate(bmi_class = cut(bmi, c(0, 18.5, 24.9, 29.9, 100), labels = c("underweight", "normal", "overweight", "obese")))

# proportions of actual

prop_a <- all_a  %>% 
  group_by(bmi_class) %>% 
  count(., wt = wt_int) %>% 
  ungroup() %>% 
  mutate(freq = n/sum(n))

obese <- prop_a[which(prop_a$bmi_class == "obese"),]$freq
over <- prop_a[which(prop_a$bmi_class == "overweight"),]$freq
normal <- prop_a[which(prop_a$bmi_class == "normal"),]$freq
under <- prop_a[which(prop_a$bmi_class == "underweight"),]$freq

cut_1 <- wtd.quantile(all_s$bmi, probs = under, all_s$wt_int)   
cut_2 <- unname( wtd.quantile(all_s$bmi, probs = 1 - obese - over, all_s$wt_int))
cut_3 <- unname( wtd.quantile(all_s$bmi, probs = 1 - obese, all_s$wt_int))

# add alternative bmi categories to simulated

all_s <- all_s %>% 
  mutate(bmi_class = cut(bmi, c(0, cut_1, cut_2, cut_3, 100), labels = c("underweight", "normal", "overweight", "obese"))) 


# combine all genders

all <- rbind(all_s %>% mutate(type = "S"), all_a %>% mutate(type = "A")) 

# median intake

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(intakem = wtd.quantile(intake, probs = 0.5, weight = wt_int)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "intakem")

# median weight

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(weightm = wtd.quantile(weight, probs = 0.5, weight = wt_int)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "weightm")

# median bmi

all %>% 
  group_by(bmi_class, type) %>% 
  summarise(bmim = wtd.quantile(bmi, probs = 0.5, weight = wt_int)) %>% 
  dcast(., bmi_class ~ type , value.var = "bmim")

all %>% 
 filter(bmi_class == "obese") %>% 
  ggplot(., aes(x = intake, group = type, color = type)) + geom_density(aes(weight = wt_int)) + facet_grid(sex ~ .)

all %>% 
  filter(bmi_class == "obese") %>% 
  ggplot(., aes(x = bmi, group = type, color = type)) + geom_density(aes(weight = wt_int))

all %>% 
  group_by(type, bmi_class) %>% 
  count(., wt = wt_int) %>% 
  group_by(type) %>% 
  mutate(total = sum(n)) %>% 
  mutate(perc = n/total*100)

all %>% 
  mutate(label = ifelse(type == "A", "Actual 2019", "Simulated")) %>% 
  ggplot(., aes(y = intake, x = bmi_class, fill = label)) + 
  geom_boxplot(aes(weight = wt_int)) +
  labs(title = "Distribution of Calorie Intake",
       fill = "") +
  theme_ipsum(base_size = 20)

all %>% 
  filter(sex == "Female") %>% 
  mutate(label = ifelse(type == "A", "Actual 2019", "Simulated")) %>% 
  ggplot(., aes(x = intake, group = label, color = label)) +
  stat_ecdf(geom = "step", pad = F) +
  facet_grid(bmi_class ~ .) +
  theme_ipsum(base_size = 15) +
  labs(title = "Cumulative Intake Distribution - Women")

all %>% 
  filter(sex == "Male") %>% 
  mutate(label = ifelse(type == "A", "Actual 2019", "Simulated")) %>% 
  ggplot(., aes(x = intake, group = label, color = label)) +
  stat_ecdf(geom = "step", pad = F) +
  facet_grid(bmi_class ~ .) +
  theme_ipsum(base_size = 15) +
  labs(title = "Cumulative Intake Distribution - Men")

##############################################################################################################
# analysis by BMI class

# obese

obese_all <- all %>% 
  filter(bmi_class == "obese")

cdf_s_f <- ecdf(obese_all[which(obese_all$type == "S" & obese_all$sex == "Female"),]$intake)
cdf_a_f <- ecdf(obese_all[which(obese_all$type == "A" & obese_all$sex == "Female"),]$intake)

quan_s_f <- wtd.quantile(obese_all[which(obese_all$type == "S" & obese_all$sex == "Female"),]$intake, seq(0.1, 0.9, 0.1), weight = obese_all[which(obese_all$type == "S" & obese_all$sex == "Female"),]$wt_int)
quan_a_f <- quantile(obese_all[which(obese_all$type == "A" & obese_all$sex == "Female"),]$intake, seq(0.1, 0.9, 0.1),weight = obese_all[which(obese_all$type == "S" & obese_all$sex == "Female"),]$wt_int)

quan_all_f <- quan_s_f/quan_a_f - 1

mean(quan_all_f)

cdf_s_f(3000)
cdf_a_f(3000)

obese_all %>% 
  group_by(type, sex) %>% 
  summarise(meanI = mean(intake),
            medianI = median(intake),
            quant10 = quantile(intake, 0.1),
            quant20 = quantile(intake, 0.2),
            quant30 = quantile(intake, 0.3),
            quant40 = quantile(intake, 0.4),
            quant50 = quantile(intake, 0.5),
            quant60 = quantile(intake, 0.6),
            quant70 = quantile(intake, 0.7),
            quant80 = quantile(intake, 0.8),
            quant90 = quantile(intake, 0.9))

quant_o <- 
  rbind(obese_all %>% 
          filter(sex == "Female") %>% 
          group_by(type) %>% 
          summarise(quant10 = quantile(intake, 0.1),
            quant20 = quantile(intake, 0.2),
            quant30 = quantile(intake, 0.3),
            quant40 = quantile(intake, 0.4),
            quant50 = quantile(intake, 0.5),
            quant60 = quantile(intake, 0.6),
            quant70 = quantile(intake, 0.7),
            quant80 = quantile(intake, 0.8),
            quant90 = quantile(intake, 0.9)) %>% 
  melt(.),
  )

quan_s_f <- quantile(obese_all[which(obese_all$type == "S" & obese_all$sex == "Female"),]$intake, seq(0,1, 0.1))
quan_a_f <- quantile(obese_all[which(obese_all$type == "A" & obese_all$sex == "Female"),]$intake, seq(0,1, 0.1))

quan_f <- data.frame(quan = seq(0,1, 0.1), simulated = quan_s_f, actual = quan_a_f) %>% 
  mutate(diff = (simulated - actual)/actual*100)

ggplot(quan_f, aes(x = quan, y = diff)) + 
  geom_bar(stat = "identity") +
  theme_ipsum() +
  labs(x = "Percentile",
       y = "kcal/day % reduction") +
  geom_vline(xintercept = c(0.1, 0.5, 0.9))


quan_s_m <- quantile(obese_all[which(obese_all$type == "S" & obese_all$sex == "Male"),]$intake, seq(0,1, 0.1))
quan_a_m <- quantile(obese_all[which(obese_all$type == "A" & obese_all$sex == "Male"),]$intake, seq(0,1, 0.1))

quan_m <- data.frame(quan = seq(0,1, 0.1), simulated = quan_s_m, actual = quan_a_m) %>% 
  mutate(diff = (simulated - actual)/actual*100)

ggplot(quan_m, aes(x = quan, y = diff)) + 
  geom_bar(stat = "identity") +
  theme_ipsum() +
  labs(x = "Percentile",
       y = "kcal/day % reduction") +
  geom_vline(xintercept = c(0.1, 0.5, 0.9))

# overweight

over_all <- all %>% 
  filter(bmi_class == "overweight")

over_all %>% 
  group_by(type, sex) %>% 
  summarise(meanI = mean(intake),
            medianI = median(intake),
            quant10 = quantile(intake, 0.1),
            quant20 = quantile(intake, 0.2),
            quant30 = quantile(intake, 0.3),
            quant40 = quantile(intake, 0.4),
            quant50 = quantile(intake, 0.5),
            quant60 = quantile(intake, 0.6),
            quant70 = quantile(intake, 0.7),
            quant80 = quantile(intake, 0.8),
            quant90 = quantile(intake, 0.9))

