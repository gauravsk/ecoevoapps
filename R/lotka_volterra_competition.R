#' Lotka-Volterra Competition with carrying capacities
#' @param time vector of time units over which to run model
#' @param init vector of initial population sizes for both species
#' @param params vector of model parameters
#' @keywords internal
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
#' @keywords internal
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

#' Run the Lotka-Volterra competition model
#'
#' Given a vector of time, intiial values, and parameters, this
#' function runs the Lotka-Volterra competition model. Note that the
#' competition parameters can either be given as the relative effects of
#' one species on the other (in terms of alpha (a)  and beta (b)),
#' or in terms of the absolute intraspecific and interspecific competition
#' coefficients (a11, a12, a22, a21).
#' Note that the output of this function can be plotted
#' with the functions [ecoevoapps::plot_lvcomp_time()] (plot of N1 and N2 over
#' time) or [ecoevoapps::plot_lvcomp_portrait()] (phase portriat of N1 vs N2).
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
#' @param sim_df data frame of lokta-volterra model simulation (created by
#'   run_lvcomp_model())
#' @param params vector of model parameters Note that carrying capacity for both
#'   species can be defined in `params` either as `K1` and `K2`, or in the
#'   inverse, as `a11` and `a22`. If carrying capacities are defined as `K1` and
#'   `K2`, interspecific competition should be defined as `a` and `b`;
#'   otherwise, `a12` and `a21`.
#' @param margin_text add text annotations to margins of isoclines?
#' @examples
#' params_vec = c(r1 = .5, r2 = .6, K1 = 1000, K2 = 1050, a = 0.5, b = 0.7)
#' sim_df <- run_lvcomp_model(time = 0:50, init = c(N1 = 1, N2 = 5), params =
#' params_vec)
#' plot_lvcomp_portrait(sim_df, params_vec)
#' @import ggplot2
#' @import dplyr
#' @export
plot_lvcomp_portrait <- function(sim_df, params, margin_text = FALSE) {

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
  # brewer.pal(3, name = "Set1") to generate:
  color_pal <- c("#E41A1C", "#377EB8", "#4DAF4A")
  potrait_plot <-
    ggplot(data = sim_df) +
    geom_segment(data = ZNGI_sp1,
                 aes(x = x1, y = y1, xend = xend1, yend = yend1,
                     color = "Species 1"), size = 2) +
    geom_segment(data = ZNGI_sp2,
                 aes(x = x2, y = y2, xend = xend2, yend = yend2,
                     color = "Species 2"), size = 2) +
    scale_color_manual(values = color_pal,
                       labels = c("Species 1", "Species 2")) +
    geom_path(aes(x = N1, y = N2), size = 1) +
    geom_point(x = first(sim_df$N1), y = first(sim_df$N2),
               pch = 21, size = 3, fill = "black") +
    geom_point(x = last(sim_df$N1), y = last(sim_df$N2),
               pch = 21, size = 3, fill = "white", stroke = 1) +

    geom_point(x = ZNGI_sp1[["xend1"]], y = 0, fill = "#E41A1C",
               pch = 21, size = 3, color = "black", stroke = 0.5) +
    geom_point(x = 0, y = ZNGI_sp1[["y1"]], fill = "#E41A1C",
               pch = 21, size = 3, color = "black", stroke = 0.5) +
    geom_point(x = ZNGI_sp2[["xend2"]], y = 0, fill = "#377EB8",
               pch = 21, size = 3, color = "black", stroke = 0.5) +
    geom_point(x = 0, y = ZNGI_sp2[["y2"]], fill = "#377EB8",
               pch = 21, size = 3, color = "black", stroke = 0.5) +

    geom_segment(x = sim_df$N1[round(nrow(sim_df)/4)],
                 y = sim_df$N2[round(nrow(sim_df)/4)],
                 xend = sim_df$N1[round(nrow(sim_df)/4) + 1],
                 yend = sim_df$N2[round(nrow(sim_df)/4) + 1],
                 arrow = arrow(length = unit(0.15, "inches"), type = "open")) +
    labs(x = expression(N[1]), y = expression(N[2]), color = "ZNGI for") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_apps() +
    theme(plot.margin = margin(l = 15, t = 5, b = 7))

  # option to add margin_text
  if(margin_text == TRUE) {
    ZNGI_sp1 <- round(ZNGI_sp1)
    ZNGI_sp2 <- round(ZNGI_sp2)

    if("K1" %in% names(params)) {
      potrait_plot <-
        potrait_plot +
        annotate("text", x = ZNGI_sp1[["xend1"]], y = 0, vjust = 2, hjust = 0,
                 label = bquote(K[1] == .(ZNGI_sp1[["xend1"]]))) +
        annotate("text", y = ZNGI_sp1[["y1"]], x = 0, vjust = 1, hjust = 1,
                 label = bquote(frac(K[1], alpha) == .(ZNGI_sp1[["y1"]]))) +
        annotate("text",  x = ZNGI_sp2[["xend2"]], y = 0, vjust = 1.25, hjust = 0,
                 label = bquote(frac(K[2],beta) == .(ZNGI_sp2[["xend2"]]))) +
        annotate("text", y = ZNGI_sp2[["y2"]], x = 0, vjust = 1, hjust = 1,
                 label = bquote(K[2] == .(ZNGI_sp2[["y2"]]))) +

      coord_cartesian(clip = "off")

    } else {
      potrait_plot <-
        potrait_plot +

        annotate("text", x = ZNGI_sp1[["xend1"]], y = 0,  vjust = 1.25, hjust = 0,
                 label = bquote(~frac(1, alpha[11]) == .(ZNGI_sp1[["xend1"]]))) +
        annotate("text", y = ZNGI_sp1[["y1"]], x = 0, vjust = 1, hjust = 1,
                 label = bquote(~frac(1, alpha[12]) == .(ZNGI_sp1[["y1"]]))) +
        annotate("text",  x = ZNGI_sp2[["xend2"]], y = 0, vjust = 1.25, hjust = 0,
                 label = bquote(~frac(1, alpha[21]) == .(ZNGI_sp2[["xend2"]]))) +
        annotate("text", y = ZNGI_sp2[["y2"]], x = 0, vjust = 1, hjust = 1,
                 label = bquote(~frac(1, alpha[22]) == .(ZNGI_sp2[["y2"]]))) +
        coord_cartesian(clip = "off")
    }
  }
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
    theme_apps() +
    theme(plot.margin = margin(t = 10))

  return(N_over_time)
}
