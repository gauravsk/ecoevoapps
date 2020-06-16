library(deSolve)
library(tidyr)
library(ggplot2)

# Write a function that runs the Lotka Volterra competition model
tilman_comp <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters/state variables:
    # d1 = intrinsic mortality rate of sp1
    # a1 = uptake efficiency of sp1
    # d2 = intrinsic mortality rate of sp2
    # a2 = uptake efficiency of sp2
    # r = intrinsic growth rate of resource

    # Differential equations
    dN1 <- N1*(a1*R-d1)
    dN2 <- N2*(a2*R-d2)
    dR <- r-R*(a1*N1 + a2*N2)

    # return both dN1 and dN2
    return(list(c(dN1, dN2, dR)))

  })
}

# Set the initial population sizes
init <- c(N1 = 50, N2 = 25, R = 100)

# Set the parameter values
params <- c(r = .01, d1 = .001, d2 = .001, a1 = .001, a2 = .001)

# Time over which to simulate model dynamics
time <- seq(0,750,by = .1)

# Use the tilman_comp function above to run
# the tilman resource competition model using the
# parameter estimates defined above
tilman_out <- data.frame(deSolve::ode(func = ecoevoapps::tilman_comp,
                                      y=init, parms=params, times = time))

# Reshape the data so that population sizes of both
# species are in one column, and an extra column to define
# species name. This helps with the plotting...

tilman_out <- pivot_longer(tilman_out, c(N1,N2,R), "species")

# Plot
ggplot(tilman_out) +
  geom_line(aes(x = time, y = value, color = species), size = 2) +
  scale_color_brewer(palette = "Set1") +
  ylab("Population size") +
  ecoevoapps::theme_apps()

