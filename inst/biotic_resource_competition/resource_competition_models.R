library(deSolve)
library(tidyverse)
library(ecoevoapps)


init <- c(H = 1, P = 1)

pars <- c(r = .3, a = .3, T_h = .3, e = .2, d = .2)

params <- c(r = .5, K = 500, a = .2, T_h = .2, e = .1, d = .1)

time <- seq(0,50, by =.1)


# Running the functions

lv_pred2_out <- data.frame(deSolve::ode(func = lv_pred2, y = init, parms = pars, times = time))

rm_pred_out <- data.frame(deSolve::ode(func = rm_predation, y = init, parms = params, times = time))

# Reshaping data

lv_pred2_out <- pivot_longer(lv_pred2_out, c(H,P), "Population")

rm_pred_out <- pivot_longer(rm_pred_out, c(H,P), "Population")

# Plot

ggplot(lv_pred2_out) +
  geom_line(aes(x = time, y = value, color = Population), size = 2) +
  scale_color_brewer(palette = "Set1") +
  ylab("Population size")

ggplot(rm_pred_out) +
  geom_line(aes(x = time, y = value, color = Population), size = 2) +
  scale_color_brewer(palette = "Set1") +
  ylab("Population size")


