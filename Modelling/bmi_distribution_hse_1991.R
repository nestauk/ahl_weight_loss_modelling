library(tidyverse)
library(hrbrthemes)
library(fitdistrplus)
library(fitur)
library(actuar)

df <- read.table("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Data\\UKDA-7025-tab\\tab\\1991_2009hse.tab", sep = "\t", header = T)

df_1991 <- df %>% filter(year == "1992")
nrow(df_1991)

df_valid <- df_1991 %>% filter(!is.na(bmivalid))
nrow(df_valid)

df_low <- df_valid %>% filter(bmivalid < 50)
nrow(df_low)

write_csv(df_low, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_1991.csv")

# Describe sample

df_low %>% 
  group_by(sex) %>% 
  summarise(count = n())

df_low %>% 
  mutate(age_gr = cut(age, c(0,16,20,30,40,50,60,70,200), right = F, include.lowest = F)) %>% 
  group_by(age_gr) %>% 
  summarise(count = n())

# Alternative method, to run after variable bmi90 is in the environment
# library(fitur)
# library(actuar)
# fitur::fit_dist_addin()


bmi90 <- df_low[,"bmivalid"] %>% na.omit() %>% as.numeric()

## Step (1) Plot 
descdist( data = bmi90 , discrete = FALSE)
descdist(data = bmi90, discrete = FALSE, boot=1000)


## Step (2) Fit

normal_ = fitdist(bmi90, "norm")
weibull_ = fitdist(bmi90, "weibull")
gamma_ = fitdist(bmi90, "gamma")
invweibull_ = fitdist(bmi90, "invweibull")
invgamma_ = fitdist(bmi90, "invgamma")


plot(normal_)
plot(weibull_)
plot(gamma_)
plot(invweibull_)
plot(invgamma_)

## Step (3) Estimate parameters

print(normal_)
print(weibull_)
print(gamma_)
print(invweibull_)

summary(normal_)
summary(weibull_)
summary(gamma_)
summary(invweibull_)
summary(invgamma_)

gofstat(list(normal_, weibull_, gamma_, invweibull_, invgamma_), 
        fitnames=c("normal", "Weibull", "gamma", "inverseWeibull", "inverseGamma"))

# the best fitting distribution is the inverse gamma with parameters
# shape: 37.63875
# scale: 933.47771

# simulate distribution with the same parameters to check accuracy

set.seed(333)

newDist <- rinvgamma(6532, shape = 37.63875, scale = 933.47771)
 
hist(newDist)

# combine

dat <- rbind(data.frame(type = "real", bmi = bmi90),
             data.frame(type = "simulated", bmi = newDist))

ggplot(dat, aes(x = bmi, colour = type)) + 
  geom_density(size = 2) +
  theme_ipsum() +
  labs(title = "Actual and simulated distribution from HSE 1991",
       subtitle = "Inverse Gamma distribution with shape of 37.6 and scale of 933.4",
       colour = "")


dist0 <- ecdf(newDist)

# overweight + obese

1 - dist0(25)

# obese

1 - dist0(30)
