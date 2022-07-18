library(deSolve)
library(tidyverse)
library(plotly)
library(reshape2)
library(beepr)

# Read flat file

<<<<<<< HEAD
<<<<<<< HEAD
df <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Data\\calorie_deficit_scenarios_filter.csv", sep = ",", fileEncoding="UTF-8-BOM") %>% 
  filter(BMI_class != "underweight")
=======
df <- read.csv("/Users/fausto/Downloads/calorie_deficit_scenarios_w_imd.csv", sep = ",", fileEncoding="UTF-8-BOM")
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
df <- read.csv("/Users/fausto/Downloads/calorie_deficit_scenarios_w_imd.csv", sep = ",", fileEncoding="UTF-8-BOM")
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18

# Recode sex

df$Sex_letter <- ifelse(df$Sex == "female", "F", "M")

<<<<<<< HEAD
<<<<<<< HEAD
# Define target weight by gender and BMI class


# mean weight change

w_change <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Data\\weight_change_table_mean.csv") %>% 
  dplyr::select(bmi_class, female, male) %>% 
  melt(.)

dat <- merge(df, w_change, by.x = c("BMI_class", "Sex"), by.y = c("bmi_class", "variable")) %>% 
  mutate(target = Wt_est*(1 - value/100))



funScenario <- function(id, w0, height, sex, age, w1, days){
  
  bmi <- w0/height/height*10000
  
  EI0 <- if_else(sex == "M",
=======
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
funScenario <- function(id, w0, height, sex, age, w1, days, calibration){
  
  bmi <- w0/height/height*10000
  
  EI0 <- if_else(sex == "M", 
<<<<<<< HEAD
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
                 -.0971*(w0^2) + 40.853*w0 + 323.59,
                 .0278*(w0^2) + 9.2893*w0 + 1528.9) # at baseline
  
  a1 <- if_else(sex == "M",293,248)
  y1 <- if_else(sex == "M",5.92,5.09)
  p <- if_else(sex == "M",.4330,.4356)
  
<<<<<<< HEAD
<<<<<<< HEAD
  RMR0 <- a1*(w0^(p))-y1*(age)
=======
  RMR0 <- a1*(w0^(p))-y1*(age + days/365)
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
  RMR0 <- a1*(w0^(p))-y1*(age + days/365)
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
  
  a <- 0.02                       # adaption parameter
  
  DIT0 <- 0.075*EI0
  
  SPA0 <- 0.326*EI0 
  
  PA0 <- if_else(EI0-DIT0-RMR0-SPA0 > 0, 
                 EI0-DIT0-RMR0-SPA0,
                 0)
  
  polyc <- case_when(sex == "M" ~ c(-w0 -71.73349 - 0.38273e-1*age + 0.6555023*height,    # 1's terms
                                    1.0 + 3.5907722 - 0.2296e-2*age - 0.13308e-1*height, # x terms 
                                    0.332e-4*age - 0.7195e-1 + 0.2721e-3*height,         # x^2 terms 
                                    0.6841e-3 - 0.187e-5*height,                       # x^3 terms
                                    - 0.162e-5 ), 
                     TRUE ~ c(-w0 - 72.055453 + 0.6555023*height - 0.38273e-1*age,   # 1's terms
                              1.0 + 2.4837412 - 0.2296e-2*age - 0.13308e-1*height,   # x terms 
                              -0.390627e-1 +0.332e-4*age +0.2721e-3*height,          # x^2 terms
                              0.2291e-3 -0.187e-5*height,                          # x^3 terms
                              3.5*10^(-7) ))
  res <- polyroot(polyc)
  F0 <-  Re(res[1])
  FFM0 <- w0 - F0
  
<<<<<<< HEAD
<<<<<<< HEAD
  
  tout <- (seq(0, days, by = days/30)) # calculate 2 month intervals (approximately)
  
  CalRedShare <- seq(0.01,0.30,by=0.01)  # List of potential calorie reduction values
=======
  if (calibration == T){  
  CalRedShare <- seq(0.05,0.70,by=0.01)  # List of potential calorie reduction values
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
  if (calibration == T){  
  CalRedShare <- seq(0.05,0.70,by=0.01)  # List of potential calorie reduction values
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18

  funSelect <- function(x) {
    Cr <- EI0*x            # Updated calorie intake
    
    NI1 <- EI0 - Cr                  # New energy intake 
    
    C = F0 * 1/exp(FFM0/10.4)        # Estimate the constant of the Forbes Equation
    
    CC <- SPA0 - 2*(RMR0+PA0+DIT0)   # Determine constant of integration (note that s = 0.67 )
    
    DIT1 <- .075 * NI1               # New DIT
    
    cl <- 1020                       # energy in 1kg of lean muscle
    cf <- 9500                       # energy in 1kg of fat
    
<<<<<<< HEAD
<<<<<<< HEAD
=======
    # initial fat mass   
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
    # initial fat mass   
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
    SolveFF <- function(pars, times=tout) { 
      derivs <- function(t, state, parameters){
        with(as.list(c(state, parameters)), {
          mff <- 10.4 * log(f / C)                    # fat free mass according to Forbes equation
          RMR <- a1 * (mff + f)^p - y1*(A + t/365)   # change in RMR with new FFM and FM
          PA <- PA0 / W0 * (mff + f)                 # change in PA with new FFM and FM
          SPA1 <- 2*((1-a)*RMR + DIT1 + PA) + CC     # change in SPA  
          
          # rate of change
          df <- if_else(SPA1 <= 0,
                        (NI1 - RMR - DIT1 - PA) / (cl * 10.4 / f + cf),
                        (NI1 - RMR - DIT1 - PA - SPA1) / (cl * 10.4 / f + cf)) 
          
          
          #return the rate of change
          return(list(c(df)))
        }) 
      }
      state <- c(f=F0) 
      return(ode(y = state,
                 times = times,
                 func = derivs,
                 parms = pars,
                 method = rk4))
    }
    
<<<<<<< HEAD
<<<<<<< HEAD
    
=======
    tout <- (seq(0, days, by = 1))
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
    tout <- (seq(0, days, by = 1))
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
    out_pars <- c(C = C,
                  CC = CC,
                  EI0 = EI0,
                  p = p,
                  PA0 = PA0,
                  W0 = w0,
                  DIT1 = DIT1,
                  NI1 = NI1,
                  A = age,
                  a1 = a1,
                  y1 = y1,
                  DIT0 = DIT0
    )
    
<<<<<<< HEAD
<<<<<<< HEAD
    
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
    out <- as.data.frame(SolveFF(out_pars, tout)) %>% 
      mutate(FFM = 10.4*log(f/C),
             weight = f + FFM,
           startWeight = w0,
           cal = x, 
           tarW = w1)
    
    tail(out,1)
  }
  
<<<<<<< HEAD
<<<<<<< HEAD

    optimCal <- pmap_dfr(list(CalRedShare), funSelect) %>%
      filter(abs(weight - tarW) == min(abs(weight - tarW)))

    finalCal <- optimCal$cal

=======
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
    optimCal <- do.call("rbind", lapply(CalRedShare, funSelect)) %>%
      filter(abs(weight - tarW) == min(abs(weight - tarW)))   
    finalCal <- optimCal$cal
  } else {
    finalCal <- ifelse(w0 == w1, 0.05, -1.274392 + 0.0597815956*bmi+-0.0004652386*bmi*bmi)
  }
<<<<<<< HEAD
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18


  Cr <- EI0*finalCal               # Calorie reduction
  NI1 <- EI0 - Cr                  # New energy intake 
  
<<<<<<< HEAD
<<<<<<< HEAD
  
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
  C = F0 * 1/exp(FFM0/10.4)        # Estimate the constant of the Forbes Equation
  
  CC <- SPA0 - 2*(RMR0+PA0+DIT0)   # Determine constant of integration (note that s = 0.67 )
  
  DIT1 <- .075 * NI1               # New DIT
  
  cl <- 1020                       # energy in 1kg of lean muscle
  cf <- 9500                       # energy in 1kg of fat
  
<<<<<<< HEAD
<<<<<<< HEAD
=======
  # initial fat mass   
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
  # initial fat mass   
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
  SolveFF <- function(pars, times=tout) { 
    derivs <- function(t, state, parameters){
      with(as.list(c(state, parameters)), {
        mff <- 10.4 * log(f / C)                    # fat free mass according to Forbes equation
        RMR <- a1 * (mff + f)^p - y1*(A + t/365)   # change in RMR with new FFM and FM
        PA <- PA0 / W0 * (mff + f)                 # change in PA with new FFM and FM
        SPA1 <- 2*((1-a)*RMR + DIT1 + PA) + CC     # change in SPA  
        
        # rate of change
        df <- if_else(SPA1 <= 0,
                      (NI1 - RMR - DIT1 - PA) / (cl * 10.4 / f + cf),
                      (NI1 - RMR - DIT1 - PA - SPA1) / (cl * 10.4 / f + cf)) 
        
        
        #return the rate of change
        return(list(c(df)))
      }) 
    }
    state <- c(f=F0) 
    return(ode(y = state,
               times = times,
               func = derivs,
               parms = pars,
               method = rk4))
  }
  
<<<<<<< HEAD
<<<<<<< HEAD
  
=======
  tout <- (seq(0, days, by = 1))
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
  tout <- (seq(0, days, by = 1))
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
  out_pars <- c(C = C,
                CC = CC,
                EI0 = EI0,
                p = p,
                PA0 = PA0,
                W0 = w0,
                DIT1 = DIT1,
                NI1 = NI1,
                A = age,
                a1 = a1,
                y1 = y1,
                DIT0 = DIT0
  )
  
<<<<<<< HEAD
<<<<<<< HEAD
  
  # try and merge the two functions so that they report all output variables at once
  
  out <- as.data.frame(SolveFF(out_pars, tout)) %>% 
    # tail(n=1) %>% 
    mutate(X = id,
           FFM = 10.4*log(f/C),
           finalWeight = f + FFM,
           startWeight = w0,
           sex = sex,
           height = height,
           age = age)
=======
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
  out <- as.data.frame(SolveFF(out_pars, tout)) %>% 
    mutate(FFM = 10.4*log(f/C),
           weight = f + FFM,
           calRed = finalCal) %>% 
    filter(time == days)
<<<<<<< HEAD
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
  
  derivs <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      mff <- 10.4 * log(f / C)                    # fat free mass according to Forbes equation
      RMR <- a1 * (mff + f)^p - y1*(A + t/365)   # change in RMR with new FFM and FM
      PA <- PA0 / W0 * (mff + f)                 # change in PA with new FFM and FM
      SPA1 <- 2*((1-a)*RMR + DIT1 + PA) + CC     # change in SPA  
      
      # rate of change
      df <- if_else(SPA1 <= 0,
                    (NI1 - RMR - DIT1 - PA) / (cl * 10.4 / f + cf),
                    (NI1 - RMR - DIT1 - PA - SPA1) / (cl * 10.4 / f + cf)) 
      
      #return the rate of change
      return(list(change = c(df), 
                  baseIntake = EI0, 
                  newIntake = NI1, 
                  RMR = RMR, 
                  PA = PA, 
                  SPA = SPA1, 
                  DIT = DIT1,
                  EExp = RMR + DIT1 + SPA1 + PA))
    }) 
  }
  
<<<<<<< HEAD
<<<<<<< HEAD
  
  
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
  state <- c(f=F0) 
  finalPar <- ode(y = state,
                  times = tout,
                  func = derivs,
                  parms = out_pars,
                  method = rk4)
  
<<<<<<< HEAD
<<<<<<< HEAD
  
  final <- merge(out, finalPar, by = c("time", "f"))   %>%          # merge
    mutate(diff = abs(EExp - newIntake))  %>%                         # difference between new intake and TEE
    mutate(thisMin = diff == min(diff))  %>%                          # minimum value of the difference, this is the time when weight is achieved
    arrange(time)  %>%                                                # sort by time
    mutate(cum = cumsum(thisMin))  %>%                                # flag all times after weight is achieved
    mutate(TEE_final = ifelse(time == 0, baseIntake, ifelse(cum == 1, newIntake, EExp)))             # update TEE so that is becomes stable after achieving target weight
  
  newWeight <- (final  %>% filter(thisMin == T))$finalWeight             # target weight
  dayAchieve <- (final  %>% filter(thisMin == T))$time              # day target weight is achieved
  
  finalOut <- final  %>% 
    mutate(weight_final = ifelse(cum == 1, cum*newWeight, startWeight))  %>%  # add target weight
    mutate(intake_final = ifelse(time == 0, baseIntake, newIntake))  %>% # update intake at day 0 it is the TEE
    mutate(day_final = dayAchieve)  %>% 
    mutate(bmi_final = weight_final/height/height*10000) %>% 
    tail(1)
  
  return(finalOut)
  
  # return(final)
}



###############################################################################
# FULL SAMPLE
###############################################################################

dat_obese <- dat %>% filter(BMI_class == "obese") 

list <- list(id = dat_obese$X,
             w0 =dat_obese$Wt_est, 
             height = dat_obese$Ht_est, 
             sex = dat_obese$Sex_letter, 
             age = dat_obese$Age_est, 
             w1 = dat_obese$target, 
             days = 365*5 )


start_time <- Sys.time()
obese <- pmap_dfr(list, funScenario) %>% 
  merge(., dat_obese, by = "X") %>% 
  mutate(final_BMI_class = cut(bmi_final, 
                               c(0, 18.5, 24.9, 29.9, 100), 
                               labels = c("underweight", "normal", "overweight", "obese"))) %>% 
  mutate(calRed = (newIntake - baseIntake)/baseIntake)
=======
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
  final <- merge(out, finalPar, by = c("time", "f"))  %>% mutate(id = id)
  
  return(final)
}


##############################################################################
# CALIBRATION
##############################################################################

# Determine relationship between BMI and calorie reduction for obese people

# For people losing weight
set.seed(3)

dat <- df %>% 
  filter(BMI_class == "obese") %>% 
  group_by(round(BMI_est,0)) %>% 
  sample_n(3, replace = T) %>% 
  unique()

obese <- dat %>% 
  filter(X %in% dat$X) %>% 
  mutate(trg = 29*(Ht_est/100)^2) %>% 
  mutate(rnd = 1) %>% 
  mutate(target = ifelse(rnd == 1, trg, Wt_est))

start_time <- Sys.time()
relationship <- do.call("rbind", mapply(funScenario, 
                                        id = obese$X,
                                        w0 =obese$Wt_est, 
                                        height = obese$Ht_est, 
                                        sex = obese$Sex_letter, 
                                        age = obese$Age_est, 
                                        w1 = obese$target, 
                                        days = 365*3, 
                                        calibration = T,
                                        SIMPLIFY = F)) %>% 
  merge(., obese, by.x = "id", by.y = "X") %>% 
  mutate(final_BMI = weight/Ht_est/Ht_est*10000,
         final_BMI_class = cut(final_BMI, 
                               c(0, 18.5, 24.9, 29.9, 100), 
                               labels = c("underweight", "normal", "overweight", "obese")))

end_time <- Sys.time()

end_time - start_time

beep(3)

ggplot(relationship, aes(x = BMI_est, y = calRed)) + geom_point()

relationship$BMI2 <- relationship$BMI_est*relationship$BMI_est

mod <- lm(calRed ~ BMI_est + BMI2, relationship)

# these are the coefficients that will be used for the main sample
summary(mod)$coefficients


# For weight stable

set.seed(3)

dat <- df %>% 
  filter(BMI_class == "obese") %>% 
  group_by(round(BMI_est,0)) %>% 
  sample_n(3, replace = T) %>% 
  unique()

obese <- dat %>% 
  filter(X %in% dat$X) %>% 
  mutate(trg = 29*(Ht_est/100)^2) %>% 
  mutate(rnd = 0) %>% 
  mutate(target = ifelse(rnd == 1, trg, Wt_est))

start_time <- Sys.time()
relationship_stb <- do.call("rbind", mapply(funScenario, 
                                        id = obese$X,
                                        w0 =obese$Wt_est, 
                                        height = obese$Ht_est, 
                                        sex = obese$Sex_letter, 
                                        age = obese$Age_est, 
                                        w1 = obese$target, 
                                        days = 365*3, 
                                        calibration = T,
                                        SIMPLIFY = F)) %>% 
  merge(., obese, by.x = "id", by.y = "X") %>% 
  mutate(final_BMI = weight/Ht_est/Ht_est*10000,
         final_BMI_class = cut(final_BMI, 
                               c(0, 18.5, 24.9, 29.9, 100), 
                               labels = c("underweight", "normal", "overweight", "obese")))

end_time <- Sys.time()

end_time - start_time

beep(3)

ggplot(relationship_stb, aes(x = BMI_est, y = calRed)) + geom_point()

relationship_stb$BMI2 <- relationship_stb$BMI_est*relationship_stb$BMI_est

mod <- lm(calRed ~ BMI_est + BMI2, relationship_stb)

# these are the coefficients that will be used for the main sample

summary(mod)$coefficients


# Scenario 3
# 50% obesity reduction in 3 years

set.seed(3)

obese <- df %>% 
  filter(BMI_class == "obese") %>% 
  mutate(trg = 29*(Ht_est/100)^2) %>% 
  mutate(rnd = sample(0:1, n(), replace = TRUE)) %>% 
  mutate(target = ifelse(rnd == 1, trg, Wt_est))


start_time <- Sys.time()
reduction_50 <- do.call("rbind", mapply(funScenario, 
                                        id = obese$X,
                                        w0 =obese$Wt_est, 
                                        height = obese$Ht_est, 
                                        sex = obese$Sex_letter, 
                                        age = obese$Age_est, 
                                        w1 = obese$target, 
                                        days = 365*3, 
                                        calibration = F,
                                        SIMPLIFY = F)) %>% 
  merge(., obese, by.x = "id", by.y = "X") %>% 
  mutate(final_BMI = weight/Ht_est/Ht_est*10000,
         final_BMI_class = cut(final_BMI, 
                               c(0, 18.5, 24.9, 29.9, 100), 
                               labels = c("underweight", "normal", "overweight", "obese")))

end_time <- Sys.time()

end_time - start_time

beep(3)

write_csv(reduction_50, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_scenario_3.csv")



# Summary statistics

reduction_50 %>% 
  group_by(Sex) %>% 
  summarise(intakeBefore = round(mean(baseIntake), -1),
            intakeAfter = round(mean(newIntake), -1),
            intakeBeforeSe = round(sd(baseIntake), -1),
            intakeAfterSe = round(sd(newIntake), -1))

reduction_50 %>% 
  group_by(Sex, rnd) %>% 
  summarise(intakeBefore = round(mean(baseIntake), -1),
            intakeAfter = round(mean(newIntake), -1),
            intakeBeforeSe = round(sd(baseIntake), -1),
            intakeAfterSe = round(sd(newIntake), -1)) %>% 
  filter(rnd == 1)

reduction_50 %>% 
  group_by(Sex, rnd) %>% 
  summarise(intakeBefore = round(mean(baseIntake), -1),
            intakeAfter = round(mean(newIntake), -1),
            intakeBeforeSe = round(sd(baseIntake), -1),
            intakeAfterSe = round(sd(newIntake), -1)) %>% 
  filter(rnd == 0)
 
# chart distribution

reduction_50 %>% 
  select(id, baseIntake, newIntake, Sex) %>% 
  melt(., id.vars = c("id", "Sex")) %>% 
  mutate(intake_type = ifelse(variable == "baseIntake", "Now", "In 3 Years")) %>% 
  ggplot(., aes(x = value, group = intake_type, fill = intake_type)) +
  geom_density(alpha = 0.5) + 
  facet_wrap( ~ Sex) +
  theme_minimal() +
  labs(fill = "Daily Intake",
       x = "Daily kcal")

# Scenario 4
# 50% obesity reduction in 3 years among the 50% of obese people with the lowest BMI (below the median)

set.seed(3)

obese <- df %>% 
  filter(BMI_class == "obese") %>% 
  mutate(trg = 29*(Ht_est/100)^2) %>% 
  arrange(BMI_est) %>% 
  mutate(count = seq_len(n()),
         half = n()/2) %>% 
  mutate(rnd = count <= half) %>% 
  mutate(target = ifelse(rnd == 1, trg, Wt_est))


start_time <- Sys.time()
scenario4 <- do.call("rbind", mapply(funScenario, 
                                        id = obese$X,
                                        w0 =obese$Wt_est, 
                                        height = obese$Ht_est, 
                                        sex = obese$Sex_letter, 
                                        age = obese$Age_est, 
                                        w1 = obese$target, 
                                        days = 365*3, 
                                        calibration = F,
                                        SIMPLIFY = F)) %>% 
  merge(., obese, by.x = "id", by.y = "X") %>% 
  mutate(final_BMI = weight/Ht_est/Ht_est*10000,
         final_BMI_class = cut(final_BMI, 
                               c(0, 18.5, 24.9, 29.9, 100), 
                               labels = c("underweight", "normal", "overweight", "obese")))
<<<<<<< HEAD
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18

end_time <- Sys.time()

end_time - start_time

beep(3)


<<<<<<< HEAD
<<<<<<< HEAD
write_csv(obese, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_obese_weight_loss.csv")


dat_over <- dat %>% filter(BMI_class == "overweight")

list <- list(id = dat_over$X,
             w0 =dat_over$Wt_est, 
             height = dat_over$Ht_est, 
             sex = dat_over$Sex_letter, 
             age = dat_over$Age_est, 
             w1 = dat_over$target, 
             days = 365*5 )

start_time <- Sys.time()
over <- pmap_dfr(list, funScenario)%>% 
  merge(., dat_over, by = "X") %>% 
  mutate(final_BMI_class = cut(bmi_final, 
                               c(0, 18.5, 24.9, 29.9, 100), 
                               labels = c("underweight", "normal", "overweight", "obese"))) %>% 
  mutate(calRed = (newIntake - baseIntake)/baseIntake)

end_time <- Sys.time()

end_time - start_time

beep(3)

write_csv(over, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_over_weight_loss.csv")

dat_norm <- dat %>% filter(BMI_class == "normal")

list <- list(id = dat_norm$X,
             w0 =dat_norm$Wt_est, 
             height = dat_norm$Ht_est, 
             sex = dat_norm$Sex_letter, 
             age = dat_norm$Age_est, 
             w1 = dat_norm$target, 
             days = 365*5 )

start_time <- Sys.time()
normal <- pmap_dfr(list, funScenario)%>% 
  merge(., dat_norm, by = "X") %>% 
  mutate(final_BMI_class = cut(bmi_final, 
                               c(0, 18.5, 24.9, 29.9, 100), 
                               labels = c("underweight", "normal", "overweight", "obese"))) %>% 
  mutate(calRed = (newIntake - baseIntake)/baseIntake)

end_time <- Sys.time()

end_time - start_time

beep(3)

write_csv(normal, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_normal_weight_loss.csv")

full <- rbind(normal, over, obese)

write_csv(full, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_full_weight_loss.csv")



=======
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
# Summary statistics



scenario4 %>% 
  group_by(Sex) %>% 
  summarise(intakeBefore = round(mean(baseIntake), -1),
            intakeAfter = round(mean(newIntake), -1),
            intakeBeforeSe = round(sd(baseIntake), -1),
            intakeAfterSe = round(sd(newIntake), -1))

scenario4 %>% 
  group_by(Sex, rnd) %>% 
  summarise(intakeBefore = round(mean(baseIntake), -1),
            intakeAfter = round(mean(newIntake), -1),
            intakeBeforeSe = round(sd(baseIntake), -1),
            intakeAfterSe = round(sd(newIntake), -1)) %>% 
  filter(rnd == 1)

scenario4 %>% 
  group_by(Sex, rnd) %>% 
  summarise(intakeBefore = round(mean(baseIntake), -1),
            intakeAfter = round(mean(newIntake), -1),
            intakeBeforeSe = round(sd(baseIntake), -1),
            intakeAfterSe = round(sd(newIntake), -1)) %>% 
  filter(rnd == 0)

# chart distribution

scenario4 %>% 
  select(id, baseIntake, newIntake, Sex) %>% 
  melt(., id.vars = c("id", "Sex")) %>% 
  mutate(intake_type = ifelse(variable == "baseIntake", "Now", "In 3 Years")) %>% 
  ggplot(., aes(x = value, group = intake_type, fill = intake_type)) +
  geom_density(alpha = 0.5) + 
  facet_wrap( ~ Sex) +
  theme_minimal() +
  labs(fill = "Daily Intake",
       x = "Daily kcal")

write_csv(scenario4, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_scenario_4.csv")
<<<<<<< HEAD
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18
=======
>>>>>>> c5da22913e5cd0b1e5050dc7decbd81518260d18

