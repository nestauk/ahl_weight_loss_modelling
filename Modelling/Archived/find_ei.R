#Individual's parameters
library(bw)



days <- 3650
bw      <- 80  #weight (kg)
ht      <- 1.8 #height (m)
age     <- 32
sex     <- "female"
deltaNA <- rep(0, days) #Sodium change
weight_goal <- 77.30

#Build a function to uniroot solve
weight_solve_energy <- function(x){
  
  #Matrix of energy intake change for 365 days
  deltaEI     <- rep(x, days)
  
  #Estimate weight change. 
  wtrajectory  <- adult_weight(bw, ht, age, sex, deltaEI, deltaNA, days = days)
  
  #Return weight by the end
  wtrajectory$Body_Weight[days] -weight_goal
  
}


#Energy interval for the solution
EI              <- c(-500, 500)
intake_solution <- uniroot(weight_solve_energy, interval = EI)$root

message(paste0("This person needs to reduce energy intake by ", intake_solution))

