#' Lotka-Volterra Competition with carrying capacities
#' @param time vector of time units over which to run model
#' @param init vector of initial population sizes for both species
#' @param params vector of model parameters
lotka_volterra_competition <- function(time, init, params) {
  with (as.list(c(time, init, params)), {
    # description of parameters
    # r1 = per capita growth rate of species 1
    # N1 = population size of species 1
    # K1 = carying capacity of species 1
    # a = relative per capita effect of species 2 on species 1
    # r2 = per capita growth rate of species 2
    # N2 = population size of species 2
    # K2 = carrying capacity of species 2
    # b = relative per capita effect of species 1 on species 2

    # Differential equations
    dN1 <- r1*N1*(1 - (N1 + a*N2)/K1)
    dN2 <- r2*N2*(1 - (N2 + b*N1)/K2)

    # Return dN1 and dN2
    return(list(c(dN1, dN2)))
  })
}

#' Lotka-Volterra Competition without carrying capacities
#' @param time vector of time units over which to run model
#' @param init vector of initial population sizes for both species
#' @param params vector of model parameters
lotka_volterra_competition_wo_K <- function(time, init, params) {
  with (as.list(c(time, init, params)), {
    # description of parameters
    # r1 = per capita growth rate of species 1
    # N1 = population size of species 1
    # a11 = per capita effect of species 1 on itself
    # a12 = per capita effect of species 2 on species 1
    # r2 = per capita growth rate of species 2
    # N2 = population size of species 2
    # a22 = per capita effect of species 2 on itself
    # a21 = per capita effect of species 1 on species 2

    # Differential equations
    dN1 <- r1*N1*(1 - a11*N1 - a12*N2)
    dN2 <- r2*N2*(1 - a22*N2 - a21*N1)

    # Return dN1 and dN2
    return(list(c(dN1, dN2)))
  })
}

#' Run the Lotka-Volterra competition model (with carrying capacity and relative effects)
#' @param time vector of time units over which to run model, starting from 0.
#' `time` can also be supplied as just the total length of the simulation (i.e. tmax)
#' @param init vector of initial population sizes for both species, with names N1 and N2
#' @param params vector of model parameters
#' Note that carrying capacity for both species can be defined in `params`
#' either as `K1` and `K2`, or in the inverse, as `a11` and `a22`.
#' If carrying capacities are defined as `K1` and `K2`, interspecific competition
#' should be defined as `a` and `b`; otherwise, `a12` and `a21`.
#' @import deSolve
#' @examples
#' # Define full time series, and run model in terms of carrying capacities
#' # and relative competitive effects
#' run_lvcomp_model(time = 0:5, init = c(N1 = 1, N2 = 5),
#' params = c(r1 = .15, r2 = .2, K1 = 1000, K2 = 800, a = 0.9, b = 1.05))
#'
#' # Run model in terms of absolute competition coefficients
#' # (i.e. a11, a12, a21, a22)
#' run_lvcomp_model(time = 0:5, init = c(N1 = 1, N2 = 5),
#' params = c(r1 = .15, r2 = .2, a11 = .001, a22 = 0.00125, a12 = .0005, a21 = .0007))
#'
#' # Give only the final time step rather than full time series
#' run_lvcomp_model(time = 0:5, init = c(N1 = 1, N2 = 5),
#' params = c(r1 = .15, r2 = .2, K1 = 1000, K2 = 800, a = 0.9, b = 1.05))
#' run_lvcomp_model(time = 0:5, init = c(N1 = 1, N2 = 5),
#' params = c(r1 = .15, r2 = .2, a11 = .001, a22 = 0.00125, a12 = .0005, a21 = .0007))
#' @export
run_lvcomp_model <- function(time = 0:100, init = c(N1 = 20, N2 = 15),
                             params = c(r1 = .15, r2 = .2, K1 = 1000, K2 = 800, a = 0.9, b = 1.05)) {

  # Check how time has been defined (if just Tmax, then make vector)
  # and if vector was supplied, check that it starts at t = 0
  if(length(time) == 1) {
    tmax <- time
    time <- seq(0, tmax)
  } else if(time[1] != 0) {
    stop("The time vector should start at 0.")
  }

  # Check that init has been defined properly
  if(!(is.numeric(init))) stop("init should be a numeric vector of length 2, e.g. c(N1 = 10, N2 = 20)")
  if(length(init) != 2) stop("init should be a numeric vector of length 2, e.g. c(N1 = 10, N2 = 20)")
  if(!(all(names(init) %in% c("N1","N2")))) stop("init should be a numeric vector of length 2, e.g. c(N1 = 10, N2 = 20)")

  # Check that params is correctly defined (just r)
  if(!(is.numeric(params))) stop("params should be a numeric vector")
  if(!(all(c("r1", "r2", "K1", "K2", "a", "b") %in% names(params)) |
       all(c("r1", "r2", "a11", "a12", "a22", "a21") %in% names(params)))) {
    stop("paramaters vector should be defined either in terms of carrying capacity and relative interspecific effects (r1, r2, K1, K2, a, b), or absolute intra and interspecific competition effects (r1, r2, a11, a12, a22, a21)")
  }

  # If carrying capacity defined in terms of K1 and K2, use the lotka_volterra_competition fnc
  if("K1" %in% names(params)) {
    ode(func = lotka_volterra_competition,
        y = init, times = time, parms = params)
  } else { # use the lotka_volterra_competition_wo_K fnc
    ode(func = lotka_volterra_competition_wo_K,
        y = init, times = time, parms = params)
  }
}



#' Generate a phase portrait (N1 vs N2) plot for the Lotka-Volterra model
#' @param sim_df data frame of lokta-volterra model simulation
#' (created by run_lvcomp_model())
#' @param params vector of model parameters
#' Note that carrying capacity for both species can be defined in `params`
#' either as `K1` and `K2`, or in the inverse, as `a11` and `a22`.
#' If carrying capacities are defined as `K1` and `K2`, interspecific competition
#' should be defined as `a` and `b`; otherwise, `a12` and `a21`.
#' @examples
#' params_vec = c(r1 = .5, r2 = .6, K1 = 1000, K2 = 1050, a = 0.5, b = 0.7)
#' sim_df <- run_lvcomp_model(time = 0:50, init = c(N1 = 1, N2 = 5), params = params_vec)
#' plot_lvcomp_portrait(sim_df, params_vec)
#' @import ggplot2
#' @import dplyr
#' @export
plot_lvcomp_portrait <- function(sim_df, params) {

  if("K1" %in% names(params)) {
    ZNGI_sp1 <- data.frame(x1 = 0, y1 = params["K1"]/params["a"],
                           xend1 = params["K1"], yend1 = 0)
    ZNGI_sp2 <- data.frame(x2 = 0, y2 = params["K2"],
                           xend2 = params["K2"]/params["b"], yend2 = 0)
  } else {
    ZNGI_sp1 <- data.frame(x1 = 0, y1 = 1/params["a12"],
                           xend1 = 1/params["a11"], yend1 = 0)
    ZNGI_sp2 <- data.frame(x2 = 0, y2 = 1/params["a22"],
                           xend2 = 1/params["a21"], yend2 = 0)
  }

  sim_df <- data.frame(sim_df)

  potrait_plot <-
    ggplot(data = sim_df) +
    geom_segment(data = ZNGI_sp1,
                 aes(x = x1, y = y1, xend = xend1, yend = yend1,
                     color = "Species 1"), size = 2) +
    geom_segment(data = ZNGI_sp2,
                 aes(x = x2, y = y2, xend = xend2, yend = yend2,
                     color = "Species 2"), size = 2) +
    scale_color_manual(values = brewer.pal(3, name = "Set1"),
                       labels = c("Species 1", "Species 2")) +
    geom_path(aes(x = N1, y = N2), size = 1) +
    geom_point(x = first(sim_df$N1), y = first(sim_df$N2),
               pch = 21, size = 3, fill = "black") +
    geom_point(x = last(sim_df$N1), y = last(sim_df$N2),
               pch = 21, size = 3, fill = "white", stroke = 1) +
    geom_segment(x = sim_df$N1[round(nrow(sim_df)/4)],
                 y = sim_df$N2[round(nrow(sim_df)/4)],
                 xend = sim_df$N1[round(nrow(sim_df)/4) + 1],
                 yend = sim_df$N2[round(nrow(sim_df)/4) + 1],
                 arrow = arrow(length = unit(0.15, "inches"), type = "open")) +
    labs(x = expression(N[1]), y = expression(N[2]), color = "ZNGI for") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_apps()

  return(potrait_plot)
}



#' Generate a trajectory of population size (N1 vs N2) over time for the Lotka-Volterra model
#' @param sim_df data frame of lokta-volterra model simulation
#' (created by run_lvcomp_model())
#' @examples
#' sim_df <- run_lvcomp_model()
#' plot_lvcomp_time(sim_df)
#' @import ggplot2
#' @import tidyr
#' @export
plot_lvcomp_time <- function(sim_df) {
  sim_df <- data.frame(sim_df)
  sim_df_long <-
    pivot_longer(data = sim_df, cols = c(N1, N2), names_to = "species")

  N_over_time <-
    ggplot(data = sim_df_long) +
    geom_line(aes(x = time, y = value, color = species), size = 2) +
    scale_color_brewer(palette = "Set1", labels = c("1", "2")) +
    labs(x = "Time", y = "Population size (N)", color = "Species") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_apps()

  return(N_over_time)
}
