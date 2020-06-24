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

## make function to generate data for vector field/phase plane -------

vector_field_input <- function(sim_df, eq_func, pars_for_eq_func, vec_density = 20) {

  # INPUTS
  # sim_df is a df with the simulated values of H & P in separate columns
  # eq_func is the function with the system of equations that calculate dH and dP for one time step
  # pars_for_eq_func is the vector of named parameters to use in the eq_func
  # vec_density determines the number of arrows (vec_density^2)

  # OUTPUT:
  # a df with Hstart, Hend, Pstart, Pend (and dH, dP) for drawing vectors

  # BODY:
  # add error checks here

  # determine the min and max of the number of prey and predators
  lowH <- round(min(sim_df$H), 0)
  hiH <- round(max(sim_df$H), 0)
  lowP <- round(min(sim_df$P), 0)
  hiP <- round(max(sim_df$P), 0)

  # select a sequence of points between (and a little beyond) those values
  seqH <- seq(0.9*lowH, 1.4*hiH, length.out = vec_density)
  seqP <- seq(0.9*lowP, 1.4*hiP, length.out = vec_density)

  # find all the combinations of those H and P coordinates, make that a df w/Hstart and Pstart
  hpcoords <- expand.grid(Hstart = seqH, Pstart = seqP)

  # use those values to solve dP and dH and calculate pend and hend
  hpcoords <- bind_cols(hpcoords, map2_df(hpcoords$Hstart, hpcoords$Pstart, eq_func, pars_for_eq_func))
  hpcoords <- hpcoords %>% mutate(Hend = Hstart + dH, Pend = Pstart + dP)

  return(hpcoords)
}

hpcoords2 <- vector_field_input(sim_df = out_lv_pred1, eq_func = lv_pred1_eq, pars_for_eq_func = pars_lv_pred1)

# function to make the vector field with ggplot
vector_field <- function(sim_df, vector_field_input_data) {

  # INPUT
  # sim_df is a df with the simulated values of H & P in separate columns
  # vector_field_input_data has the output from vector_field_input, which is a list of start and end coordinates for each vector segment

  # OUTPUT
  # is a ggplot

  ggplot(sim_df) +

    # vector field
    geom_segment(data = vector_field_input_data,
                 aes(x = Hstart, y = Pstart, xend = Hend, yend = Pend),
                 arrow = arrow(length = unit(0.02, "npc")),
                 color = "light gray")

}

# plot vector field

# ggplot(out_lv_pred1) +
#
#   # vector field
#   geom_segment(data = hpcoords2, aes(x = Hstart, y = Pstart, xend = Hend, yend = Pend), arrow = arrow(length = unit(0.02, "npc")), color = "light gray") +

vector_field(out_lv_pred1, hpcoords2) +

  # the isoclines
  geom_hline(yintercept = r_lv_pred1/a_lv_pred1, col = brewer.pal(n = 3, name = "Set1")[1], size = 2) +
  geom_vline(xintercept = d_lv_pred1/(e_lv_pred1*a_lv_pred1), col = brewer.pal(n = 3, name = "Set1")[2], size = 2) +

  # the trace of the simulation and arrow for direction of the trace
  geom_path(aes(x = H, y = P), size = 2) +
  geom_segment(x = out_lv_pred1$H[min(5, round(length(time_lv_pred1))/50)], #did this min thing in case someone asks for very few timesteps
               y = out_lv_pred1$P[min(5, round(length(time_lv_pred1))/50)],
               xend = out_lv_pred1$H[min(5 + 1, round(length(time_lv_pred1)/50) + 1)],
               yend = out_lv_pred1$P[min(5 + 1, round(length(time_lv_pred1)/50) + 1)],
               arrow = arrow(length = unit(0.1, "npc")),
               cex = 2) +

  # plot appearance
  xlab("Number of Prey") +
  ylab("Number of Predators") +
  coord_cartesian(xlim = c(min(out_lv_pred1$H), max(out_lv_pred1$H) + 1), ylim = c(min(out_lv_pred1$P), max(out_lv_pred1$P) + 1)) + #need this line to show all vectors that go beyond plot limits
  ecoevoapps::theme_apps()

