library(deSolve)
library(tidyr)
library(ggplot2)

# Write a function that runs the Lotka Volterra competition model


# Set the initial population sizes
init <- c(N1 = 10, N2 = 10, R1 = 20, R2 = 20)

# Set the parameter values
# params <- c(S1 = 12, S2 = 12, r1 = 1.6, r2 = 1, k11 = 18, k12 = 4,
#             k21 = 2, k22 = 14, m1 = .2, m2 = .2, c11 = .25,
#             c12 = .08, c21 = .1, c22 = .2, a1 = .5, a2 = .5)
#
params <- c(S1 = 12, S2 = 12, r1 = 1, r2 = 1.6,
            k11 = 5, k12 = 4, k21 = 4, k22 = 5,
            m1 = .2, m2 = .2, c11 = .25,
            c12 = .08, c21 = .1, c22 = .2, a1 = .5, a2 = .5)

# Time over which to simulate model dynamics
time <- seq(0,5000,by = .5)

# Use the tilman_comp function above to run
# the tilman resource competition model using the
# parameter estimates defined above
tilman_out <- data.frame(deSolve::ode(func = ecoevoapps::tilman_comp_essential,
                                      y=init, parms=params, times = time))

tilman_out <- tilman_out %>% dplyr::mutate_if(is.numeric, round, digits = 3)

# Reshape the data so that population sizes of both
# species are in one column, and an extra column to define
# species name. This helps with the plotting...
R11 = (params["k11"] * params["m1"])/(params["r1"] - params["m1"])
R12 = (params["k12"] * params["m1"])/(params["r1"] - params["m1"])
R21 = (params["k21"] * params["m2"])/(params["r2"] - params["m2"])
R22 = (params["k22"] * params["m2"])/(params["r2"] - params["m2"])

tilman_out_long <- pivot_longer(tilman_out, c(R1,R2,N1,N2), "species")

# Plot
ggplot(tilman_out_long) +
  geom_line(aes(x = time, y = value, color = species), size = 2) +
  scale_color_brewer(palette = "Set1") +
  ylab("Population size") +
  ecoevoapps::theme_apps()

Rstar_vec <- ecoevoapps::Rstar_essential(params)
Rstar_df <- data.frame(species = c("N1", "N1", "N2", "N2"),
                       resource = c("R1", "R2", "R1", "R2"),
                       Rstar = Rstar_vec)
Rstar_df2 <- data.frame(species = c("N1", "N2"),
                        R1star = c(R11, R21),
                        R2star = c(R12, R22))
ggplot(tilman_out) +
  geom_path(aes(x = R1, y = R2)) +
  geom_segment(data = Rstar_df2, aes(x = R1star, xend = R1star, y = R2star, yend = Inf, color = species)) +
  geom_segment(data = Rstar_df2, aes(x = R1star, xend = Inf, y = R2star, yend = R2star, color = species)) +
  scale_color_brewer(name = "Consumer\nspecies", palette = "Set1") +
  ecoevoapps::theme_apps()

ggplot(tilman_out) +
  geom_path(aes(x = N1, y = N2))
