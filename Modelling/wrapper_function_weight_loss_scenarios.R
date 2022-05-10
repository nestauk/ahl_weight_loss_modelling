library(deSolve)
library(tidyverse)
library(plotly)
library(beepr)


# Read flat file Health Survey data

df <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Data\\calorie_deficit_scenarios_w_imd.csv", sep = ",", fileEncoding="UTF-8-BOM")

# Recode sex

df$Sex_letter <- ifelse(df$Sex == "female", "F", "M")

# Define function that takes in calorie reduction as a percentage 

funScenario <- function(id, w0, height, sex, age, CalRedShare, days){
  
EI0 <- if_else(sex == "M", 
               -.0971*(w0^2) + 40.853*w0 + 323.59,
               .0278*(w0^2) + 9.2893*w0 + 1528.9) # at baseline

a1 <- if_else(sex == "M",293,248)
y1 <- if_else(sex == "M",5.92,5.09)
p <- if_else(sex == "M",.4330,.4356)

RMR0 <- a1*(w0^(p))-y1*(age + days/365)

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

Cr <- EI0*CalRedShare            # Calorie reduction
NI1 <- EI0 - Cr                  # New energy intake 

C = F0 * 1/exp(FFM0/10.4)        # Estimate the constant of the Forbes Equation

CC <- SPA0 - 2*(RMR0+PA0+DIT0)   # Determine constant of integration (note that s = 0.67 )

DIT1 <- .075 * NI1               # New DIT

cl <- 1020                       # energy in 1kg of lean muscle
cf <- 9500                       # energy in 1kg of fat

# initial fat mass   
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

tout <- (seq(0, days, by = 1))
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
  tail(n=1) %>% 
  mutate(X = id,
         FFM = 10.4*log(f/C),
         finalWeight = f + FFM,
         startWeight = w0,
         sex = sex,
         height = height,
         age = age,
         CalRedShare = CalRedShare)

return(out)
}


# evaluate function for different scenarios
# merge with initial data

# 10% reduction

start_time <- Sys.time()
reduction_10 <- do.call("rbind", mapply(funScenario, 
                                        id = df$X, 
                                        w0 =df$Wt_est, 
                                        height = df$Ht_est, 
                                        sex = df$Sex_letter, 
                                        age = df$Age_est, 
                                        CalRedShare = 0.1, 
                                        days = 365*3, 
                                        SIMPLIFY = F)) %>% 
  merge(df, by = "X") %>% 
  mutate(final_BMI = finalWeight/Ht_est/Ht_est*10000,
         final_BMI_class = cut(final_BMI, 
                               c(0, 18.5, 24.9, 29.9, 100), 
                               labels = c("underweight", "normal", "overweight", "obese"))) 

end_time <- Sys.time()

end_time - start_time

beep(3)




# 15% reduction

start_time <- Sys.time()

reduction_15 <- do.call("rbind", mapply(funScenario, 
                                        id = df$X,
                                        w0 =df$Wt_est, 
                                        height = df$Ht_est, 
                                        sex = df$Sex_letter, 
                                        age = df$Age_est, 
                                        CalRedShare = 0.15, 
                                        days = 365*3, 
                                        SIMPLIFY = F)) %>% 
  merge(df, by = "X") %>%
  mutate(final_BMI = finalWeight/Ht_est/Ht_est*10000,
         final_BMI_class = cut(final_BMI, 
                               c(0, 18.5, 24.9, 29.9, 100), 
                               labels = c("underweight", "normal", "overweight", "obese")))

end_time <- Sys.time()

end_time - start_time

beep(3)

# 20% reduction

start_time <- Sys.time()

reduction_20 <- do.call("rbind", mapply(funScenario, 
                                        id = df$X,
                                        w0 =df$Wt_est, 
                                        height = df$Ht_est, 
                                        sex = df$Sex_letter, 
                                        age = df$Age_est, 
                                        CalRedShare = 0.20, 
                                        days = 365*3, 
                                        SIMPLIFY = F)) %>% 
  merge(df, by = "X") %>%
  mutate(final_BMI = finalWeight/Ht_est/Ht_est*10000,
         final_BMI_class = cut(final_BMI, 
                               c(0, 18.5, 24.9, 29.9, 100), 
                               labels = c("underweight", "normal", "overweight", "obese")))

end_time <- Sys.time()

end_time - start_time

beep(3)

# 25% reduction

start_time <- Sys.time()

reduction_25 <- do.call("rbind", mapply(funScenario, 
                                        id = df$X,
                                        w0 =df$Wt_est, 
                                        height = df$Ht_est, 
                                        sex = df$Sex_letter, 
                                        age = df$Age_est, 
                                        CalRedShare = 0.25, 
                                        days = 365*3, 
                                        SIMPLIFY = F)) %>% 
  merge(df, by = "X") %>%
  mutate(final_BMI = finalWeight/Ht_est/Ht_est*10000,
         final_BMI_class = cut(final_BMI, 
                               c(0, 18.5, 24.9, 29.9, 100), 
                               labels = c("underweight", "normal", "overweight", "obese")))

end_time <- Sys.time()

end_time - start_time

beep(3)

# save files as csv

write_csv(reduction_10, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_3years_10percent.csv")

write_csv(reduction_15, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_3years_15percent.csv")

write_csv(reduction_20, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_3years_20percent.csv")

write_csv(reduction_25, "C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_3years_25percent.csv")
