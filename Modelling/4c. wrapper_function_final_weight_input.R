library(deSolve)
library(tidyverse)
library(plotly)
library(reshape2)
library(beepr)

# Read flat file

df <- read.csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse2019_clean_no_outliers.csv", sep = ",", fileEncoding="UTF-8-BOM") 

# Recode sex

df$Sex_letter <- ifelse(df$sex_label == "female", "F", "M")

# Define target weight by gender and BMI class


# mean weight change

w_change <- read.csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/weight_change_table_median.csv") %>% 
  dplyr::select(bmi_class, female, male) %>% 
  melt(.)

dat <- merge(df, w_change, by.x = c("bmi_class", "sex_label"), by.y = c("bmi_class", "variable")) %>% 
  mutate(target = wtval*(1 - value/100))


funScenario <- function(id, w0, height, sex, age, w1, days){
  
  bmi <- w0/height/height*10000
  
  EI0 <- if_else(sex == "M",
                 -.0971*(w0^2) + 40.853*w0 + 323.59,
                 .0278*(w0^2) + 9.2893*w0 + 1528.9) # at baseline
  
  a1 <- if_else(sex == "M",293,248)
  y1 <- if_else(sex == "M",5.92,5.09)
  p <- if_else(sex == "M",.4330,.4356)
  
  RMR0 <- a1*(w0^(p))-y1*(age)
  
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
  
  
 # tout <- (seq(0, days, by = days/60)) # calculate 1 month intervals (30 days approximately)
 tout <- (seq(0, days, by = days/50)) # calculate 1 month intervals (30 days approximately)
 
  
  CalRedShare <- seq(0.01,0.30,by=0.01)  # List of potential calorie reduction values
  
  funSelect <- function(x) {
    Cr <- EI0*x            # Updated calorie intake
    
    NI1 <- EI0 - Cr                  # New energy intake 
    
    C = F0 * 1/exp(FFM0/10.4)        # Estimate the constant of the Forbes Equation
    
    CC <- SPA0 - 2*(RMR0+PA0+DIT0)   # Determine constant of integration (note that s = 0.67 )
    
    DIT1 <- .075 * NI1               # New DIT
    
    cl <- 1020                       # energy in 1kg of lean muscle
    cf <- 9500                       # energy in 1kg of fat
    
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
    
    
    out <- as.data.frame(SolveFF(out_pars, tout)) %>% 
      mutate(FFM = 10.4*log(f/C),
             weight = f + FFM,
             startWeight = w0,
             cal = x, 
             tarW = w1)
    
    tail(out,1)
  }
  
  
  optimCal <- pmap_dfr(list(CalRedShare), funSelect) %>%
    filter(abs(weight - tarW) == min(abs(weight - tarW)))
  
  finalCal <- optimCal$cal
  
  
  
  Cr <- EI0*finalCal               # Calorie reduction
  NI1 <- EI0 - Cr                  # New energy intake 
  
  
  C = F0 * 1/exp(FFM0/10.4)        # Estimate the constant of the Forbes Equation
  
  CC <- SPA0 - 2*(RMR0+PA0+DIT0)   # Determine constant of integration (note that s = 0.67 )
  
  DIT1 <- .075 * NI1               # New DIT
  
  cl <- 1020                       # energy in 1kg of lean muscle
  cf <- 9500                       # energy in 1kg of fat
  
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
  
  
  
  state <- c(f=F0) 
  finalPar <- ode(y = state,
                  times = tout,
                  func = derivs,
                  parms = out_pars,
                  method = rk4)
  
  
  final <- merge(out, finalPar, by = c("time", "f"))   %>%          # merge
    mutate(diff = abs(EExp - newIntake))  %>%                         # difference between new intake and TEE
    mutate(thisMin = diff == min(diff))  %>%                          # minimum value of the difference, this is the time when weight is achieved
    arrange(time)  %>%                                                # sort by time
    mutate(cum = cumsum(thisMin))  %>%                                # flag all times after weight is achieved
    mutate(TEE_final = ifelse(time == 0, baseIntake, ifelse(cum == 1, newIntake, EExp)))             # update TEE so that is becomes stable after achieving target weight
  
  newWeight <- (final  %>% filter(thisMin == T))$finalWeight             # target weight
  dayAchieve <- (final  %>% filter(thisMin == T))$time              # day target weight is achieved
  rmrBase <- (final %>% filter(time == 0))$RMR
  
  finalOut <- final  %>% 
    mutate(weight_final = ifelse(cum == 1, cum*newWeight, startWeight))  %>%  # add target weight
    mutate(intake_final = ifelse(time == 0, baseIntake, newIntake))  %>% # update intake at day 0 it is the TEE
    mutate(day_final = dayAchieve)  %>% 
    mutate(bmi_final = weight_final/height/height*10000) %>% 
    mutate(RMR_base = rmrBase) %>% 
    tail(1)
  
  return(finalOut)
  }



###############################################################################
# FULL SAMPLE
###############################################################################

dat_morb_obese <- dat %>% filter(bmi_class == "morb obese")

list <- list(id = dat_morb_obese$X,
             w0 =dat_morb_obese$wtval, 
             height = dat_morb_obese$htval, 
             sex = dat_morb_obese$Sex_letter, 
             age = dat_morb_obese$age_est, 
             w1 = dat_morb_obese$target, 
             days = 365*4)


start_time <- Sys.time()
morb_obese <- pmap_dfr(list, funScenario) %>% 
  merge(., dat_morb_obese, by = "X") %>% 
  mutate(final_BMI_class = cut(bmi_final, 
                               c(0, 18.5, 24.9, 29.9, 40, 100), 
                               labels = c("underweight", "normal", "overweight", "obese", "morb obese"))) %>% 
  mutate(calRed = (newIntake - baseIntake)/baseIntake)

end_time <- Sys.time()

end_time - start_time

beep(3)

write_csv(morb_obese, "/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse_morb_obese_weight_loss.csv")

dat_obese <- dat %>% filter(bmi_class == "obese")

list <- list(id = dat_obese$X,
             w0 =dat_obese$wtval, 
             height = dat_obese$htval, 
             sex = dat_obese$Sex_letter, 
             age = dat_obese$age_est, 
             w1 = dat_obese$target, 
             days = 365*4)


start_time <- Sys.time()
obese <- pmap_dfr(list, funScenario) %>% 
  merge(., dat_obese, by = "X") %>% 
  mutate(final_BMI_class = cut(bmi_final, 
                               c(0, 18.5, 24.9, 29.9, 40, 100), 
                               labels = c("underweight", "normal", "overweight", "obese", "morb obese"))) %>%
  mutate(calRed = (newIntake - baseIntake)/baseIntake)

end_time <- Sys.time()

end_time - start_time

beep(3)

rbind(obese %>% dplyr::select(bmi_class, sex, baseIntake, wt_int) %>% mutate(type = "base") %>% rename(intake = baseIntake),
      obese %>% dplyr::select(bmi_class, sex, intake_final, wt_int) %>% mutate(type = "final") %>% rename(intake = intake_final)) %>% 
  group_by(bmi_class, sex, type) %>% 
  summarise(intakeM = round(wtd.mean(intake, weight = wt_int),0)) %>% 
  dcast(., bmi_class + sex ~ type) %>% 
  arrange(sex) %>% 
  mutate(diff = final - base,
         perc = round(diff/base*100,1))

rbind(obese %>% dplyr::select(bmi_class, sex, startWeight, wt_int) %>% mutate(type = "base") %>% rename(b_weight = startWeight),
      obese %>% dplyr::select(bmi_class, sex, weight_final, wt_int) %>% mutate(type = "final") %>% rename(b_weight = weight_final)) %>% 
  group_by(bmi_class, sex, type) %>% 
  summarise(weightM = round(wtd.mean(b_weight, weight = wt_int),0)) %>% 
  dcast(., bmi_class + sex ~ type) %>% 
  arrange(sex) %>% 
  mutate(diff = final - base,
         perc = round(diff/base*100,1))


write_csv(obese, "/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse_obese_weight_loss.csv")


dat_over <- dat %>% filter(bmi_class == "overweight")

list <- list(id = dat_over$X,
             w0 =dat_over$wtval,
             height = dat_over$htval,
             sex = dat_over$Sex_letter,
             age = dat_over$age_est,
             w1 = dat_over$target,
             days = 365*5 )

start_time <- Sys.time()
over <- pmap_dfr(list, funScenario)%>%
  merge(., dat_over, by = "X") %>%
  mutate(final_BMI_class = cut(bmi_final, 
                               c(0, 18.5, 24.9, 29.9, 40, 100), 
                               labels = c("underweight", "normal", "overweight", "obese", "morb obese"))) %>%
  mutate(calRed = (newIntake - baseIntake)/baseIntake)

end_time <- Sys.time()

end_time - start_time

beep(3)

write_csv(over,"/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse_overweight_weight_loss.csv")

dat_norm <- dat %>% filter(bmi_class == "normal")

list <- list(id = dat_norm$X,
             w0 =dat_norm$wtval,
             height = dat_norm$htval,
             sex = dat_norm$Sex_letter,
             age = dat_norm$age_est,
             w1 = dat_norm$target,
             days = 365*5 )

start_time <- Sys.time()
normal <- pmap_dfr(list, funScenario)%>%
  merge(., dat_norm, by = "X") %>%
  mutate(final_BMI_class = cut(bmi_final, 
                               c(0, 18.5, 24.9, 29.9, 40, 100), 
                               labels = c("underweight", "normal", "overweight", "obese", "morb obese"))) %>%
  mutate(calRed = (newIntake - baseIntake)/baseIntake)

end_time <- Sys.time()

end_time - start_time

beep(3)

write_csv(normal, "/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse_normal_weight_loss.csv")

full <- rbind(normal, over, obese, morb_obese)

write_csv(full, "/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse_full_weight_loss.csv")




