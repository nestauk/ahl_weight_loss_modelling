library(tidyverse)
library(hrbrthemes)
library(fitdistrplus)
library(fitur)
library(actuar)
library(here)

# load cleaned 1991 file

df <- read_csv(here("outputs/data/hse_1991_clean.csv"))

# Explore BMI distribution 

bmi90 <- df$bmi %>% as.numeric()

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
# shape: 36.15893
# scale: 897.80151

# simulate distribution with the same parameters to check accuracy

newDist <- rinvgamma(nrow(df), shape = 36.15893, scale = 897.80151)
 
hist(newDist)

# combine

dat <- rbind(data.frame(type = "real", bmi = bmi90),
             data.frame(type = "simulated", bmi = newDist))

ggplot(dat, aes(x = bmi, colour = type)) + 
  geom_density(size = 2) +
  theme_ipsum() +
  labs(title = "Actual and simulated distribution from HSE 1991",
       subtitle = "Inverse Gamma distribution with shape of 36.2 and scale of 897.8",
       colour = "")

# Density function

dist0 <- ecdf(newDist)

# overweight + obese

1 - dist0(25)

# obese

1 - dist0(30)
