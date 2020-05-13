library(deSolve)
library(tidyverse)

# Write a function that runs the Lotka Volterra competition model
lv_competition <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters/state variables:
    # r1 = per-capita growth rate of Sp. 1
    # N1 = population size of Sp. 1
    # K1 = carrying capacity of Sp 1
    # r2, N2, and K2 are all the analogous parameter for Sp 2

    # Differential equations
    dN1 <- (r1*N1)*(1-(N1+a*N2)/K1)
    dN2 <- (r2*N2)*(1-(N2+b*N1)/K2)

    # return both dN1 and dN2
    return(list(c(dN1, dN2)))
  })
}

# Set the initial population sizes
init <- c(N1 = 1, N2 = 1)
# Set the parameter values
params <- c(r1 = .5, r2 = .4, b = .1, a = .2,
            K1 = 500, K2 = 700)
# Time over which to simulate model dynamics
time <- seq(0,75,by = .1)

# Use the lv_competition function above to run
# the lotka-volterra competition model using the
# parameter estimates defined above
lv_out <- data.frame(deSolve::ode(func = lv_competition,
                        y=init, parms=params, times = time))

# Reshape the data so that population sizes of both
# species are in one column, and an extra column to define
# species name. This helps with the plotting...
lv_out <- pivot_longer(lv_out, c(N1,N2), "species")

# Plot
ggplot(lv_out) +
  geom_line(aes(x = time, y = value, color = species), size = 2) +
  scale_color_brewer(palette = "Set1") +
  ylab("Population size") +
  ecoevoapps::theme_apps()

