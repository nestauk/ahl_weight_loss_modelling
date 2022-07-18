library(tidyverse)
library(hrbrthemes)


dat <- expand.grid(sex = c("M", "F"), w0 = seq(40,200,1))

dat$h <- case_when(dat$sex == "M" ~ 175,
                   TRUE ~ 162)

dat$bmi <- dat$w0/dat$h/dat$h*10000


dat$EI0 <- if_else(dat$sex == "M", 
               -.0971*(dat$w0^2) + 40.853*dat$w0 + 323.59,
               .0278*(dat$w0^2) + 9.2893*dat$w0 + 1528.9) # at baseline


ggplot(dat, aes(x = w0, y= EI0, colour = sex, group = sex)) + 
  geom_line(size = 2) +
  theme_ipsum() +
  labs(x = "Weight",
       y = "Total Energy Expenditure/Intake",
       color = "Gender")

ggplot(dat, aes(x = bmi, y= EI0, colour = sex, group = sex)) + 
  geom_line(size = 2) +
  theme_ipsum() +
  labs(x = "BMI",
       y = "Total Energy Expenditure/Intake",
       color = "Gender")
