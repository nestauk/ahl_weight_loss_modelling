

w0 <- 90                  # weight at the start
height <- 170             # height
sex <- "M"                # gender (M = male, F = female)
age <- 30                 # age
CalRedShare <- 0        # set it as a percentage of the current intake
days <- 365            # duration measured in days

EI0 <- if_else(sex == "M", 
               -.0971*(w0^2) + 40.853*w0 + 323.59,
               .0278*(w0^2) + 9.2893*w0 + 1528.9) # at baseline

a1 <- if_else(sex == "M",293,248)
y1 <- if_else(sex == "M",5.92,5.09)
p <- if_else(sex == "M",.4330,.4356)

RMR0 <- a1*(w0^(p))-y1*(age)
RMR0

a <- 0.02


DIT0 <- 0.075*EI0
DIT0

SPA0 <- 0.326*EI0 
SPA0

PA0 <- if_else(EI0-DIT0-RMR0-SPA0 > 0, 
               EI0-DIT0-RMR0-SPA0,
               0)
PA0

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
cf <- 9500  


derivs <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    mff <- 10.4 * log(f / C)                    # fat free mass according to Forbes equation
    RMR <- a1 * (mff + f)^p - y1*(A + t/365)   # change in RMR with new FFM and FM
    PA <- PA0 / W0 * (mff + f)                 # change in PA with new FFM and FM
    SPA1 <- 2*((1-a)*RMR + DIT1 + PA) + CC     # change in SPA  - this is only 2 for weight loss, for weight gain it is 1.3

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
                EExp = (RMR + PA +SPA1 + DIT1)))
        }) 
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


state <- c(f=F0) 
finalPar <- ode(y = state,
                times = tout,
                func = derivs,
                parms = out_pars,
                method = rk4)
