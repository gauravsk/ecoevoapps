library(deSolve)
library(tidyverse)

source("~/Dropbox/UCLA/ecoevo_models/R/SIR_ODE_variants.R")

##### run SIR model without vaccination #####
# Set the initial population sizes
init <- c(S = 500, I = 1, R = 0)
# Set the parameter values
params <- c(m = .1, beta = .01, v = 0, gamma = .2)
# Time over which to simulate model dynamics
time <- seq(0,100,by = .1)

# Use the SIR function above to run 
# the SIR model using the 
# parameter estimates defined above
out <- data.frame(deSolve::ode(func = SIR,
                                  y=init, parms=params, times = time))

# Reshape the data so that population sizes of both 
# species are in one column, and an extra column to define
# species name. This helps with the plotting...
out_long <- pivot_longer(out, c(S, I, R), "group") %>% 
  mutate(group = factor(group, levels = c("S", "I", "R")))

# Plot abundance through time
ggplot(out_long) + 
  geom_line(aes(x = time, y = value, color = group), size = 2) + 
  scale_color_brewer(palette = "Set1") +
  ylab("Population size") + 
  ecoevoapps::theme_apps()

# Plot S vs I
ggplot(out) + 
  geom_path(aes(x = S, y = I), size = 2) + 
  scale_color_brewer(palette = "Set1") +
  ylab("I size") + 
  xlab("S size") + 
  ecoevoapps::theme_apps()


##### run SIS model without vaccination #####
# Set the initial population sizes
init <- c(S = 50, I = 20)
# Set the parameter values
params <- c(m = .10, beta = .01, gamma = .02)
# Time over which to simulate model dynamics
time <- seq(0,100,by = .1)

# Use the SIS function above to run 
# the SIS model using the 
# parameter estimates defined above
out <- data.frame(deSolve::ode(func = SIS,
                               y=init, parms=params, times = time))

# Reshape the data so that population sizes of both 
# species are in one column, and an extra column to define
# species name. This helps with the plotting...
out_long <- pivot_longer(out, c(S, I), "group") %>% 
  mutate(group = factor(group, levels = c("S", "I")))

# Plot abundance through time
ggplot(out_long) + 
  geom_line(aes(x = time, y = value, color = group), size = 2) + 
  scale_color_brewer(palette = "Set1") +
  ylab("Population size") + 
  ecoevoapps::theme_apps()

# Plot S vs I
ggplot(out) + 
  geom_path(aes(x = S, y = I), size = 2) + 
  scale_color_brewer(palette = "Set1") +
  ylab("I size") + 
  xlab("S size") + 
  ecoevoapps::theme_apps()
