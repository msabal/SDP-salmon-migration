
# Parameterization of SDP model

# Outline
# 1. Growth
# 2. Survival
# 3. Terminal Fitness

# Set settings
options(scipen = 9999)

# Libraries
library(ggplot2)


# .........................................................................................
# 1. Growth

# River growth by speed and habitat
z     <- -0.01
ka    <- 0.9 # can vary btw 0.9 and 1.3
kn    <- 1.2 # can vary btw 0.9 and 1.3

RIVER.Q <- function(C, z, kh) { z*C+kh }  # Equations 4 and 5 in main text
curve(RIVER.Q(C, z=z, kh=ka), xname="C", xlim=c(0,40), ylim=c(0,1.5), , ylab = "Foraging gain in river", xlab = "travel speed (km)\n(corresponds with behavioral choice C)")
curve(RIVER.Q(C, z=z, kh=kn), xname="C", add=T, col="mediumpurple")
abline(h=0.5, col="gray24", lty="dashed")
# This plot shows how river growth potential declines with travel speed (behavioral choice C)
# and the intercept kh shifts the magnitude by shoreline habitat.

# Save plot for Appendix A
png("Figures/river_growth_FigS1.png", width = 4, height = 4, units = "in", res = 500)
curve(RIVER.Q(C, z=z, kh=ka), xname="C", xlim=c(0,40), ylim=c(0,1.5), , ylab = "River growth potential", xlab = "travel speed (km/d)\n(corresponds with behavioral choice C)")
curve(RIVER.Q(C, z=z, kh=kn), xname="C", add=T, col="mediumpurple")
abline(h=0.5, col="gray24", lty="dashed")
dev.off() # Close the PNG device

# Calculate the growth potential at different values of C
growth_C0 <- RIVER.Q(C=0, z=z, kh=ka)
growth_C20 <- RIVER.Q(C=20, z=z, kh=ka)
growth_C40 <- RIVER.Q(C=40, z=z, kh=ka)

# Calculate the percentage decline from C=0 to C=20
decline_0_to_20 <- ((growth_C0 - growth_C20) / growth_C0) * 100

# Calculate the percentage decline from C=20 to C=40
decline_20_to_40 <- ((growth_C20 - growth_C40) / growth_C20) * 100

# Look at overall % decline by choice to summarize in text (Appendix A: ~25%)
decline_0_to_20; decline_20_to_40 




# Ocean growth
f     <- 0.75
g     <- 1.2
c     <- 40
l     <- 0.05

OCEAN.Q <-    function(t, f, g, c, l) { f + g*exp(-(t-c)^2/2*l^2) }
curve(OCEAN.Q(t,f=f, g=g, c=c, l=l), xlim=c(0, 60), ylab="Ocean growth potential",
      xlab="Time (days)", xname = "t", ylim=c(0,2))
abline(h=RIVER.Q(C=0, z=z, kh=1.3), col="gray24", lty="dashed") #river value max (paused in natural)
abline(h=RIVER.Q(C=40, z=z, kh=0.9), col="gray24", lty="dashed") #river value min (fast in altered)

# Save plot for Appendix A
png("Figures/ocean_growth_FigS2.png", width = 4, height = 4, units = "in", res = 500)
curve(OCEAN.Q(t,f=f, g=g, c=c, l=l), xlim=c(0, 60), ylab="Ocean growth potential",
      xlab="Time (days)", xname = "t", ylim=c(0,2))
abline(h=RIVER.Q(C=0, z=z, kh=1.3), col="gray24", lty="dashed") #river value max (paused in natural)
abline(h=RIVER.Q(C=40, z=z, kh=0.9), col="gray24", lty="dashed") #river value min (fast in altered)
dev.off() # Close the PNG device


# Total Growth
E     <- 0.03
a     <- 0.86
Alpha <- 0.00607
d     <- 1
dn0   <- 0.7
v     <- 0.027

GROWTH.FUN <- function(W, E, q, a, Alpha, d, v, C)    { q*E*W^a - d*Alpha*W*exp(v*C) }

# Different growth trajectories depending on static q values
curve(GROWTH.FUN(W, E=E, q=1, a, Alpha, d, v, C=1), xname="W", xlim=c(7,70), ylim=c(-0.2,1))
curve(GROWTH.FUN(W, E=E, q=1.5, a, Alpha, d, v, C=1), xname="W", add=T, col="skyblue")


# Simulate Growth to Check for realistic growth rates.
sim.mix <- data.frame(t=seq(1,60, by=1), X=rep(NA, 60), 
                      h=rep(NA, 60), C=rep(NA, 60))     # set baseline values.

sim.mix[1,2] <- 7 # set starting values for mass in the river

sim.mix[31:60, 3] <- rep("o", 30)   # final 30 days in ocean
sim.mix[1:30, 3] <- sample(0:1, 30, replace=T, prob=c(0.5,0.5))  # first 30 days random between altered and natural
sim.mix$h[sim.mix$h == "1"] <- "a"   # change 1 from sample function to "a"
sim.mix$h[sim.mix$h == "0"] <- "n"   # change 0 from sample function to "n"

sim.mix[31:60, 4] <- rep(0, 30)   # last 30 days move 0 in ocean
sim.mix[1:30, 4] <- sample(0:2, 30, replace=T)  # first 30 days random sample between 0,1,2.
sim.mix$C[sim.mix$C == "1"] <- 20  # change 1 from sample function to "20" km/day
sim.mix$C[sim.mix$C == "2"] <- 40  # change 2 from sample function to "40" km/day


#for loop to simulate salmon mass over 60 days in a mix of habitats.
for(t in 1:59){
  sim.mix[t+1,2]<- sim.mix[t,2] + GROWTH.FUN(W = sim.mix[t,2], E=E, a=a, Alpha=Alpha, v=v,
                                             d = ifelse(sim.mix[t,3] == "n" & sim.mix[t,4] == 0, dn0, d),
                                             q= ifelse(sim.mix[t,3] == "o", OCEAN.Q(t=sim.mix[t,1], f=f, g=g, c=c, l=l),
                                                       ifelse(sim.mix[t,3] == "a", RIVER.Q(C=sim.mix[t,4], z=z, kh=ka), 
                                                              RIVER.Q(C=sim.mix[t,4], z=z, kh=kn))),
                                             C=sim.mix[t,4]) } # end loop.

sim.mix # look at trajectories.
(sim.mix[30,2] - sim.mix[1,2]) / 30   # daily river growth
(sim.mix[60,2] - sim.mix[31,2]) / 30  # daily ocean growth

# Summary: salmon enters the ocean at 8.6 g, grows a total of 12.8 g, which equates to
# an ocean growth rate of 0.43 g/day (in Appendix).


# .........................................................................................
# 2. Survival

# Survival
SURV.FUN <-   function(W, Bu, Bh, Bw, M, m, y, P)     { (1-M*(Bu + Bh + Bw*W^m))^(y*P) }

#Test RISK.FUN
SURV.FUN(W=12, Bu=1, Bh=1, Bw=2, M=0.002, m=-0.37, y=1, P=20) # good.

# Solving for Beta-W
Beta.W <- function(X, Bw){ Bw*X^-0.37 }
curve(Beta.W(X, Bw=2), xname="X", xlim=c(7,20), ylim=c(0,1.1), ylab="contrib. daily mortality rate")
abline(h=1,lty="dashed")

Beta.W(X=7, Bw=2)

# Save plot for Appendix A
png("Figures/survival_ssm_FigS3.png", width = 4, height = 4, units = "in", res = 500)
curve(Beta.W(X, Bw=2), xname="X", xlim=c(7,20), ylim=c(0,1.1), 
      ylab="Size-selective contribution to escape ability",
      xlab="Salmon weight (g)")
abline(h=1,lty="dashed")
dev.off() # Close the PNG device


# .........................................................................................
# 3. Terminal Fitness

# Terminal Fitness Function
Ws    <- 40
r     <- 0.1
Smax  <- 0.3

TERM.FUN <- function(W, Ws, r, Smax){ Smax/(1+exp(-r*(W-Ws))) }

curve(TERM.FUN(W, Ws=Ws , r=r, Smax=Smax), xname="W", xlim=c(7,80), ylim=c(0,0.31), ylab="adult marine survival (to age 3)")

# Save plot for Appendix A
png("Figures/term_fit_FigS4.png", width = 4, height = 4, units = "in", res = 500)
curve(TERM.FUN(W, Ws=Ws , r=r, Smax=Smax), xname="W", xlim=c(7,80), ylim=c(0,0.31), 
      ylab="Adult marine survival (to age 3)", xlab="Salmon weight (g); W(T)")
dev.off() # Close the PNG device



