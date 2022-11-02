#install.packages("mosaicCalc")
library(mosaicCalc)

PA <- 1.6
w0 <- 70

tau <- (230 + 9400 + 0.5*180+0.5*1800)/(1-0.24)*(1+0.5)

k <- 1/(1-0.24)*((3.6+0.5*22)/(1+0.5)+PA)



soln <- integrateODE(dx ~ (chi - k*(x-m))/tau, chi = -500, k = k, m = w0, tau = tau, x= 60, domain(t=0:3650))

plotFun(soln$x(t)~t, t.lim = range(0,3650))

soln$x(365)
