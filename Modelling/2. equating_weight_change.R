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
library(extrafont)
library(magrittr)
library(here)

# Read flat file

df_2019 <- read_csv(here("outputs/data/hse_2019_clean.csv"))

# describe sample

# total observations
sum(df_2019$wt_int)

# sex distribution
df_2019 %>% 
  count(sex, wt = wt_int) %>% 
  mutate(perc = n/sum(n))

# bmi class distribution
df_2019 %>% 
  count(bmi_class, wt = wt_int) %>% 
  mutate(perc = n/sum(n)*100)

# age, weight, height, bmi
df_2019 %>% 
  summarise(meanAge = wtd.mean(age, weights = wt_int),
            meanW = wtd.mean(weight, weights = wt_int),
            meanH = wtd.mean(height, weights = wt_int),
            meanBMI = wtd.mean(bmi, weights = wt_int),
            sdAge = sqrt(wtd.var(age, weights = wt_int)),
            sdW =sqrt( wtd.var(weight, weights = wt_int)),
            sdH = sqrt(wtd.var(height, weights = wt_int)),
            sdBMI = sqrt(wtd.var(bmi, weights = wt_int)))

df_2019_m <- df_2019 %>% filter(sex == 1)
df_2019_f <- df_2019 %>% filter(sex == 2)

# Read 1991 data

df_1991 <- read_csv(here("outputs/data/hse_1991_clean.csv"))

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
            meanW = mean(weight),
            meanH = mean(height),
            meanBMI = mean(bmi),
            sdAge = sd(age),
            sdW = sd(weight),
            sdH = sd(height),
            sdBMI = sd(bmi))


df_1991_m <- df_1991 %>% filter(sex == "1")
df_1991_f <- df_1991 %>% filter(sex == "2")



# MALES

# combine
all_m <- rbind(df_1991_m %>% mutate(type = "1991-92"),
               df_2019_m %>% mutate(type = "2019")) %>% 
  mutate(intake = pal * rmr)

# BMI distribution (males)

data_breaks <- data.frame(start = c(-Inf, 18.5, 25, 30, 40),  # Create data with breaks
                          end = c(18.5, 25, 30, 40, Inf),
                          colors = gray.colors(5),
                          labels = c("Underweight", "Normal", "Overweight", "Obese", "Morbidly Obese"),
                          pos_x = c(15, 22, 27, 35, 45),
                          pos_y = rep(0.15,5))


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
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#0000FF", "#F6A4B7")) +
  scale_fill_manual(values = rev(gray.colors(5))) +
  guides(fill = "none")

ggsave(here("outputs/figures/bmi_male.png"), width = 10, bg='#ffffff')
dev.off()

  
# FEMALES

all_f <- rbind(df_1991_f %>% mutate(type = "1991-92"),
               df_2019_f %>% mutate(type = "2019")) %>% 
  mutate(intake = pal * rmr)

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
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#0000FF", "#F6A4B7")) +
  scale_fill_manual(values = rev(gray.colors(5))) +
  guides(fill = "none")

ggsave(here("outputs/figures/bmi_female.png"), width = 10, bg='#ffffff')
dev.off()


# equipercentile equating
# proportions of bmi classes in 2019

prop_a <- df_2019  %>% 
  count(bmi_class, wt = wt_int) %>% 
  mutate(freq = n/sum(n))

morb <- prop_a[which(prop_a$bmi_class == "morbidly obese"),]$freq
obese <- prop_a[which(prop_a$bmi_class == "obese"),]$freq
over <- prop_a[which(prop_a$bmi_class == "overweight"),]$freq
normal <- prop_a[which(prop_a$bmi_class == "normal"),]$freq
under <- prop_a[which(prop_a$bmi_class == "underweight"),]$freq

cut_1 <- wtd.quantile(df_1991$bmi, probs = under, df_1991$wt_int)   
cut_2 <- unname( wtd.quantile(df_1991$bmi, probs = 1 - morb - obese - over, df_1991$wt_int))
cut_3 <- unname( wtd.quantile(df_1991$bmi, probs = 1 - morb - obese, df_1991$wt_int))
cut_4 <- unname( wtd.quantile(df_1991$bmi, probs = 1 - morb , df_1991$wt_int))

# add alternative bmi categories to 1991

df_1991_cutoff <- df_1991 %>% 
  mutate(bmi_class = case_when(bmi <= cut_1 ~ "underweight",
                               bmi > cut_1 & bmi < cut_2 ~ "normal",
                               bmi >= cut_2 & bmi < cut_3 ~ "overweight",
                               bmi >= cut_3 & bmi < cut_4 ~ "obese",
                               bmi >= cut_4 ~ "morbidly obese",
                               TRUE ~ "NA"))


# combine all sexes and years with synthetic cutoffs

all <- rbind(df_1991_cutoff %>% 
               mutate(type = "1991-92"), 
             df_2019 %>% 
               mutate(type = "2019")) %>% 
  mutate(id = seq_len(n())) %>% 
  mutate(intake = pal * rmr)

# intake plot

plot1 <- all %>% 
  mutate(sex = ifelse(sex == 2, "Females", "Males")) %>% 
ggplot(., aes(x = intake, group = type, color = type)) + 
  geom_density(aes(weight = wt_int), size = 3) +
  facet_grid(sex ~ .) +
  labs(x = "kcal/day",
       color = "",
       title = "Calculated kcal/day") +
  xlim(1500,4000) +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#0000FF", "#F6A4B7")) 


density_diff <- rbind(
  all %>% 
    filter(sex == 2) %>% 
    group_by(type) %>% 
  # calculate densities for each group over same range; store in list column
  summarise(d = list(density(intake, from = min(.$intake), to = max(.$intake)))) %>% 
  # make a new data.frame from two density objects
  do(data.frame(x = .$d[[1]]$x,    # grab one set of x values (which are the same)
                y = .$d[[2]]$y - .$d[[1]]$y)) %>%    # and subtract the y values
    mutate(sex = "Females"),
  all %>% 
    filter(sex == 1) %>% 
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
  xlim(1500,4000) +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") 

png(here("outputs/figures/intake_difference.png"), width = 1000)
grid.arrange(plot1, plot2, ncol=2)
dev.off()


# create tables for report

all$bmi_class <- factor(all$bmi_class, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))

# median intake

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(intakem = round(wtd.quantile(intake, probs = 0.5, weight = wt_int),0)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "intakem") %>% 
  mutate(diff_f = `2019_2` - `1991-92_2`,
         diff_m = `2019_1` - `1991-92_1`,
         perc_f = round(diff_f/`1991-92_2`*100,1),
         perc_m = round(diff_m/`1991-92_1`*100,1)) %>% 
  dplyr::select(bmi_class, `1991-92_1`, `2019_1`, `diff_m`, `perc_m`,`1991-92_2`, `2019_2`, `diff_f`, `perc_f`) %T>% 
  write_csv(here("outputs/reports/median_intake.csv"))

# IQR

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(intake25 = round(wtd.quantile(intake, probs = 0.25, weight = wt_int),0),
            intake75 = round(wtd.quantile(intake, probs = 0.75, weight = wt_int),0)) %>% 
  mutate(iqr = intake75 - intake25) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "iqr") %>% 
  dplyr::select(bmi_class, `1991-92_1`, `2019_1`,`1991-92_2`, `2019_2`) %T>% 
  write_csv(here("outputs/reports/iqr_intake.csv"))


# mean intake

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(intakem = round(wtd.mean(intake, weight = wt_int),0)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "intakem") %>% 
  mutate(diff_f = `2019_2` - `1991-92_2`,
         diff_m = `2019_1` - `1991-92_1`,
         perc_f = round(diff_f/`1991-92_2`*100,1),
         perc_m = round(diff_m/`1991-92_2`*100,1)) %>% 
  dplyr::select(bmi_class, `1991-92_1`, `2019_1`, `diff_m`, `perc_m`,`1991-92_2`, `2019_2`, `diff_f`, `perc_f`) %T>% 
  write_csv(here("outputs/reports/mean_intake.csv"))

# standard deviation

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(intakem = round(sqrt(wtd.var(intake, weight = wt_int)),0)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "intakem") %>% 
  dplyr::select(bmi_class, `1991-92_1`, `2019_1`,`1991-92_2`, `2019_2`) %T>% 
  write_csv(here("outputs/reports/sd_intake.csv"))

# confidence interval

dclus2 <- svydesign(id=~id, weights = ~wt_int, data=all)

funDiff <- function(bmi_class_c, sex_c){
  sub <- subset(dclus2, sex == sex_c & bmi_class == bmi_class_c)
  tt<-svyttest(intake~type, sub)
  return(data.frame(sex = sex_c, bmi_class = bmi_class_c, round(confint(tt, level=0.95),1)))
}

list <- expand.grid(sex_c = unique(all$sex), bmi_class_c = unique(all$bmi_class))

pmap_dfr(list, funDiff)  %>% 
  arrange(sex) %T>% 
  write_csv(here("outputs/reports/ci_intake.csv"))

# mean weight

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(weightm = round(wtd.mean(weight, weight = wt_int),1)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "weightm") %>% 
  mutate(diff_f = `2019_2` - `1991-92_2`,
         diff_m = `2019_1` - `1991-92_1`,
         perc_f = round(diff_f/`1991-92_2`*100,1),
         perc_m = round(diff_m/`1991-92_1`*100,1)) %>% 
  dplyr::select(bmi_class, `1991-92_1`, `2019_1`, `diff_m`, `perc_m`,`1991-92_2`, `2019_2`, `diff_f`, `perc_f`) %T>% 
  write_csv(here("outputs/reports/mean_weight.csv"))

# median weight

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(weightm = round(wtd.quantile(weight, probs = 0.5, weight = wt_int),0)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "weightm") %>% 
  mutate(diff_f = `2019_2` - `1991-92_2`,
         diff_m = `2019_1` - `1991-92_1`,
         perc_f = round(diff_f/`1991-92_2`*100,1),
         perc_m = round(diff_m/`1991-92_1`*100,1)) %>% 
  dplyr::select(bmi_class, `1991-92_1`, `2019_1`, `diff_m`, `perc_m`,`1991-92_2`, `2019_2`, `diff_f`, `perc_f`) %T>% 
  write_csv(here("outputs/reports/median_weight.csv"))


# standard deviation

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(weightm = round(sqrt(wtd.var(weight, weight = wt_int)),1)) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "weightm") %>% 
  dplyr::select(bmi_class, `1991-92_1`, `2019_1`,`1991-92_2`, `2019_2`) %T>% 
  write_csv(here("outputs/reports/sd_weight.csv"))

# IQR

all %>% 
  group_by(bmi_class, type, sex) %>% 
  summarise(w25 = round(wtd.quantile(weight, probs = 0.25, weight = wt_int),0),
            w75 = round(wtd.quantile(weight, probs = 0.75, weight = wt_int),0)) %>% 
  mutate(iqr = w75 - w25) %>% 
  dcast(., bmi_class ~ type + sex, value.var = "iqr") %>% 
  dplyr::select(bmi_class, `1991-92_1`, `2019_1`,`1991-92_2`, `2019_2`) %T>% 
  write_csv(here("outputs/reports/iqr_weight.csv"))


# output

weight_change_median <- all %>% 
  group_by(bmi_class , type, sex) %>% 
  summarise(weight_m = wtd.quantile(weight, prob = 0.5, weight = wt_int)) %>% 
  dcast(., bmi_class  ~ type + sex, value.var = "weight_m") %>% 
  mutate(female = 100*(`2019_2`/`1991-92_2`-1),
         male = 100*(`2019_1`/`1991-92_1`-1))

weight_change_mean <- all %>% 
  group_by(bmi_class , type, sex) %>% 
  summarise(weight_m = wtd.mean(weight, weight = wt_int)) %>% 
  dcast(., bmi_class  ~ type + sex, value.var = "weight_m") %>% 
  mutate(female = 100*(`2019_2`/`1991-92_2`-1),
         male = 100*(`2019_1`/`1991-92_1`-1))

write_csv(weight_change_median, here("outputs/reports/weight_change_table_median.csv"))

write_csv(weight_change_mean,here("outputs/reports/weight_change_table_mean.csv"))



