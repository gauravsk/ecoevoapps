# testing ggplot code to make phase plane
library(tidyverse)
library(RColorBrewer)

lv_pred1 <- function(time,init,pars) {
  with (as.list(c(time,init,pars)), {
    # description of parameters:
    # r = per capita growth rate (prey)
    # a = attack rate
    # e = conversion efficiency
    # d = predator death rate

    dH_dt = r*H - (a*H*P)
    dP_dt = e*(a*H*P) - d*P
    return(list(c(dH = dH_dt, dP = dP_dt)))

  })
}

lv_pred1_eq <- function(H, P, pars){
  with(as.list(pars), {
    dH_dt = r*H - (a*H*P)
    dP_dt = e*(a*H*P) - d*P
    return(data.frame(dH = dH_dt, dP = dP_dt))
  })
}


init_lv_pred1 <- c(H = 10, P = 10)

r_lv_pred1 <- 0.5
a_lv_pred1 <- 0.1
e_lv_pred1 <- 0.2
d_lv_pred1 <- 0.3

pars_lv_pred1 <- c(r = r_lv_pred1, a = a_lv_pred1, e = e_lv_pred1, d = d_lv_pred1)

# Time over which to simulate model dynamics
time_lv_pred1 <- seq(0, 100, by = .1)



# Use the lv_competition function above to run
# the lotka-volterra competition model using the
# parameter estimates defined above

out_lv_pred1 <- data.frame(deSolve::ode(func = lv_pred1,
                                                  y = init_lv_pred1, parms = pars_lv_pred1, times = time_lv_pred1))


# Reshape the data so that population sizes of both
# species are in one column, and an extra column to define
# species name. This helps with the plotting...

out_long_lv_pred1 <- pivot_longer(out_lv_pred1, c(H,P), "Population")


# Plots
plot_lvpred1 <- ggplot(out_long_lv_pred1) +
    geom_line(aes(x = time, y = value, color = Population), size = 2) +
    scale_color_brewer(palette = "Set1") +
    ylab("Population size") +
    ecoevoapps::theme_apps()


np_lvpred1 <- ggplot(out_lv_pred1) +
    geom_path(aes(x = H, y = P), size = 2) +
    xlab("Number of Prey") +
    ylab("Number of Predators") +
    geom_segment(x = out_lv_pred1$H[min(5, round(length(time_lv_pred1))/50)], #did this min thing in case someone asks for very few timesteps
                 y = out_lv_pred1$P[min(5, round(length(time_lv_pred1))/50)],
                 xend = out_lv_pred1$H[min(5 + 1, round(length(time_lv_pred1)/50) + 1)],
                 yend = out_lv_pred1$P[min(5 + 1, round(length(time_lv_pred1)/50) + 1)],
                 arrow = arrow(length = unit(0.1, "npc"))) +
    geom_hline(yintercept = r_lv_pred1/a_lv_pred1, col = brewer.pal(n = 3, name = "Set1")[1], size = 2) +
    geom_vline(xintercept = d_lv_pred1/(e_lv_pred1*a_lv_pred1), col = brewer.pal(n = 3, name = "Set1")[2], size = 2) +
    ecoevoapps::theme_apps()

## attempt the phase plane -------

# determine the min and max of the number of prey and predators
lowH <- round(min(out_lv_pred1$H), 0)
hiH <- round(max(out_lv_pred1$H), 0)
lowP <- round(min(out_lv_pred1$P), 0)
hiP <- round(max(out_lv_pred1$P), 0)

# select a sequence of points between those values
seqH <- seq(lowH, hiH, length.out = 5)
seqP <- seq(lowP, hiP, length.out = 5)

# find all the combinations of those H and P coordinates, make that a df w/Hstart and Pstart
hpcoords <- expand.grid(Hstart = seqH, Pstart = seqP)

# use those values to solve dP and dH and calculate pend and hend
lv_pred1_eq(H = 4, P = 2, pars_lv_pred1)

hpcoords %>% map2_df(~ lv_pred1_eq(H = .Hstart, P = .Pstart, pars_lv_pred1))
map2_df(hpcoords$Hstart, hpcoords$Pstart, pars_lv_pred1, ~lv_pred1_eq)

# give those to geom_segment
