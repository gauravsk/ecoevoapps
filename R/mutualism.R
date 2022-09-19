#' Model of mutualism with saturating functional response
#'
#' Used internally by [run_mutualism()].
#' @param time Vector of time units over which to run model, starting at 0.
#' @param init Named vector of initial population sizes.
#' * `N1`: Initial population size of species 1
#' * `N2`: Initial population size of species 2
#' @param params Named vector of model parameters.
#' * `r1`: Per capita growth rate of species 1
#' * `r2`: Per capita growth rate of species 1
#' * `a12`: Effect of species 2 on species 1
#' * `a21`: Effect of species 1 on species 2
#' * `b1`: Half-saturation constant for species 1
#' * `b2`: Half-saturation constant for species 2
#' * `d1`: Death rate of self-limitation for species 1
#' * `d2`: Death rate of self-limitation for species 2
#' @return A list where the first and only element is a vector of derivatives
#'   of the state variables (`N1`, `N2`) with respect to `time`, given `params`.
#' @seealso [run_mutualism()]
#' @keywords internal
mutualism <- function(time, init, params) {
  with(as.list(c(time, init, params)), {
    dN1 <- r1*N1 + (a12*N2*N1)/(b2 + N2) - d1*N1^2
    dN2 <- r2*N2 + (a21*N1*N2)/(b1 + N1) - d2*N2^2
    return(list(c(dN1, dN2)))
  })
}

#' Run model of mutualism with saturating functional response
#'
#' Given a vector of time, initial values, and parameters, this function runs
#' a phenomenological model of direct mutualism between two species as
#' described by Holland (2015). This model features a saturating functional
#' response for interspecific density dependence, which produces a stable
#' equilibrium where both mutualist species can coexist above their respective
#' population carrying capacities.
#' @inheritParams mutualism
#' @param time Vector of time units over which to run the model, starting at 0.
#'   `time` can also be supplied as the total length of the simulation.
#' @return
#' A data frame of population sizes (`N1`, `N2`) simulated through time,
#' produced by solving [mutualism()] with [deSolve::ode()] given `time`, `init`,
#' and `params`. This data frame has additional attributes recording the model
#' run (`model`), input parameter values (`params`) and population carrying
#' capacities calculated from the parameter values (`K`).
#' @export
#' @examples
#' # Define full time series and run model with default parameter values
#' run_mutualism(time = 0:10)
#'
#' # Define length of simulation and run model with default parameter values
#' run_mutualism(time = 10)
#'
#' # Run model with custom input values
#' tmax <- 10
#' start <- c(N1 = 100, N2 = 50)
#' pars <- c(r1 = 1, r2 = 1, a12 = 1.2, a21 = 0.8, b1 = 20, b2 = 20, d1 = 0.04, d2 = 0.02)
#' run_mutualism(time = tmax, init = start, params = pars)
#' @seealso [plot_mutualism_time()], [plot_mutualism_portrait()]
run_mutualism <- function(time = 0:50,
                          init = c(N1 = 20, N2 = 40),
                          params = c(r1 = 0.5, r2 = 0.5, a12 = 0.8, a21 = 0.4, b1 = 10, b2 = 10, d1 = 0.02, d2 = 0.01)) {

  # Check time
  if (!is.numeric(time)) {
    stop("time should be a numeric vector of length 1 or more.")
  }
  if (length(time) == 1) {
    tmax <- time
    time <- seq(0, tmax)
  } else if (time[1] != 0) {
    stop("time vector should start at 0.")
  }
  if (any(time < 0)) {
    stop("time should not contain negative values.")
  }

  # Check init
  if (!is.numeric(init)) {
    stop("init should be a named numeric vector of length 2.
         e.g. c(N1 = 20, N2 = 40)")
  }
  if (length(init) != 2) {
    stop("init should be a named numeric vector of length 2.
         e.g. c(N1 = 20, N2 = 40)")
  }
  if (!all(c("N1", "N2") %in% names(init))) {
    stop("Elements of init should be named N1 and N2.
         e.g. c(N1 = 20, N2 = 40)")
  }
  if (any(init <= 0)) {
    stop("init should only contain positive values.")
  }

  # Check params
  if (!is.numeric(params)) {
    stop("params should be a named numeric vector of length 8.
         e.g. c(r1 = 0.5, r2 = 0.5, a12 = 0.8, a21 = 0.4, b1 = 10, b2 = 10, d1 = 0.02, d2 = 0.01)")
  }
  if (length(params) != 8) {
    stop("params should be a named numeric vector of length 8.
         e.g. c(r1 = 0.5, r2 = 0.5, a12 = 0.8, a21 = 0.4, b1 = 10, b2 = 10, d1 = 0.02, d2 = 0.01)")
  }
  if (!all(c("r1", "r2", "a12", "a21", "b1", "b2", "d1", "d2") %in% names(params))) {
    stop("Elements of params should be named r1, r2, a12, a21, b1, b2, d1, d2.
         e.g. c(r1 = 0.5, r2 = 0.5, a12 = 0.8, a21 = 0.4, b1 = 10, b2 = 10, d1 = 0.02, d2 = 0.01)")
  }
  if (any(params[c("a12", "a21", "b1", "b2", "d1", "d2")] <= 0)) {
    stop("params a12, a21, b1, b2, d1, d2 should be positive.")
  }

  # Simulate
  sim <- deSolve::ode(func = mutualism, y = init, times = time, parms = params)
  sim <- as.data.frame(sim)
  attr(sim, "model") <- "mutualism"
  attr(sim, "params") <- params
  attr(sim, "K") <- c(K1 = as.numeric(params["r1"]/params["d1"]),
                      K2 = as.numeric(params["r2"]/params["d2"]))
  return(sim)
}

#' Plot population trajectories over time for mutualism model
#'
#' Generate a plot of trajectories of population sizes simulated through time
#' for the model of mutualism with saturating functional response.
#' @param sim_df Data frame of mutualism model simulation returned by
#'   [run_mutualism()].
#' @details
#' The plot generated with this function is constrained to display only the
#' first quadrant of the plane defined by `N` and `time` (i.e. values of 0 and
#' above) because negative values of `N` or `time` do not make biological sense.
#' As a consequence, when `sim_df` is generated using negative values for `r1`
#' and/or `r2`, this function may generate warning messages that look like:
#' 1. `Removed n rows containing missing values (geom_hline). `
#' 2. `Removed n row(s) containing missing values (geom_path). `
#'
#' These warnings are generated by `ggplot2` when (1) population carrying
#' capacities are calculated to be negative values or (2) negative population
#' size values are present in `sim_df`, respecitvely. Here, these warnings
#' typically do not mean that there is a problem with `sim_df`, but rather are
#' reporting that lines (or portions of lines) with negative values have been
#' omitted from the plot. Inspect `sim_df` to further diagnose these warnings.
#' @return
#' A ggplot object with trajectories of population sizes (`N1`, `N2`) plotted
#' against time. Population carrying capacities for each species, calculated
#' from model parameters, are also plotted.
#' @export
#' @examples
#' sim <- run_mutualism()
#' plot_mutualism_time(sim)
#' @seealso [run_mutualism()], [plot_mutualism_portrait()]
plot_mutualism_time <- function(sim_df) {

  # Check sim_df
  if (any(attributes(sim_df)$model != "mutualism")) {
    stop("sim_df should be a data frame returned by run_mutualism().")
  }

  # Plot
  sim_df_long <- tidyr::pivot_longer(sim_df, c(N1, N2), names_to = "species")
  y_upper <- max(c(sim_df_long$value, attr(sim_df, "K")))
  K_df <- data.frame(species = c("N1", "N2"), K = attr(sim_df, "K"))
  plot <-
    ggplot2::ggplot(sim_df_long) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = K, color = species), K_df, lty = 2) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = value, color = species), size = 2) +
    ggplot2::scale_color_brewer(palette = "Set1", labels = c("1", "2")) +
    ggplot2::labs(x = "Time", y = "Population size (N)", color = "Species",
                  caption = "Solid curves: population trajectories
                             Dashed lines: carrying capacities") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, y_upper*1.06)) +
    theme_apps() +
    ggplot2::theme(plot.caption = element_text(size = 12, color = "gray50", hjust = 1))
  return(plot)
}

#' Generate vector field for phase portrait of mutualism model
#'
#' Used internally by [plot_mutualism_portrait()].
#' @inheritParams plot_mutualism_time
#' @param vec.density Density of vectors. e.g. If `vec.density = 20` (default),
#'   generates a 20 \eqn{\times} 20 grid evenly spaced along each axis, where
#'   each grid point represents the starting point of a vector.
#' @param vec.scale Value to scale magnitude of vectors by. e.g. If
#'   `vec.scale = 0.1` (default) the magnitude of each vector is 0.1 times the
#'   magnitude of the vector corresponding to running [mutualism()] for one time
#'   step given each starting point selected via `vec.density` and `params`
#'   inherited from `sim_df`.
#' @return A data frame of start and end point values representing the vector
#'   field for a phase portrait of the model of mutualism with saturating
#'   functional response.
#' @seealso [plot_mutualism_portrait()]
#' @keywords internal
mutualism_vector_field <- function(sim_df, vec.density = 20, vec.scale = 0.1) {
  N1 <- seq(1, max(sim_df$N1)*2, length.out = vec.density)
  N2 <- seq(1, max(sim_df$N2)*2, length.out = vec.density)
  start <- expand.grid(N1 = N1, N2 = N2)
  end <- sapply(1:nrow(start), function(i) {
    run_mutualism(1, c(N1 = start[i, 1], N2 = start[i, 2]), attr(sim_df, "params"))[2, -1]
  })
  N1_end <- as.numeric(end[1, ])
  N2_end <- as.numeric(end[2, ])
  N1_end <- start$N1 + (N1_end - start$N1)*vec.scale
  N2_end <- start$N2 + (N2_end - start$N2)*vec.scale
  pts <- cbind(start, data.frame(N1_end, N2_end))
  return(pts)
}

plot_mutualism_portrait <- function(sim_df, vec = TRUE, traj = TRUE, ...) {
  params <- as.list(attr(sim_df, "params"))
  N1_lim <- c(0, max(sim_df$N1)*2)
  N2_lim <- c(0, max(sim_df$N2)*2)
  N1_seq <- seq(N1_lim[1], N1_lim[2], length.out = 100)
  N2_seq <- seq(N2_lim[1], N2_lim[2], length.out = 100)
  ZNGIs <- with(params, {
    ZNGI1 <- data.frame(species = "1",
                         N1 = sapply(N2_seq, function(N2) (r1 + (a12*N2)/(b2 + N2))/d1),
                         N2 = N2_seq)
    ZNGI2 <- data.frame(species = "2",
                         N1 = N1_seq,
                         N2 = sapply(N1_seq, function(N1) (r2 + (a21*N1)/(b1 + N1))/d2))
    rbind(ZNGI1, ZNGI2)
  })
  plot <-
    ggplot2::ggplot(ZNGIs, ggplot2::aes(x = N1, y = N2)) +
    ggplot2::geom_line(ggplot2::aes(group = species, color = species), size = 2) +
    ggplot2::scale_color_brewer(palette = "Set1", labels = c("1", "2")) +
    ggplot2::labs(x= expression(N[1]), y = expression(N[2]), color = "Species",
                  caption = "Colored curves: zero net growth isoclines") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = N1_lim, ylim = N2_lim) +
    theme_apps() +
    ggplot2::theme(plot.caption = element_text(size = 12, color = "gray50", hjust = 1))
  if (vec == TRUE) {
    vec_pts <- mutualism_vector_field(sim_df, ...)
    plot <-
      plot +
      ggplot2::geom_segment(ggplot2::aes(xend = N1_end, yend = N2_end), vec_pts,
                            arrow = arrow(length = unit(0.01, "npc")), color = "gray50")
  }
  if (traj == TRUE) {
    plot <-
      plot +
      ggplot2::geom_path(data = sim_df, size = 1) +
      ggplot2::geom_point(ggplot2::aes(x = sim_df$N1[1], y = sim_df$N2[1]),
                          size = 3, pch = 21, fill = "black") +
      ggplot2::geom_point(ggplot2::aes(x = sim_df$N1[nrow(sim_df)], y = sim_df$N2[nrow(sim_df)]),
                          size = 3, pch = 21, fill = "white", stroke = 1) +
      ggplot2::labs(x= expression(N[1]), y = expression(N[2]),
                    caption = "Colored curves: zero net growth isoclines
                               Black curve: population trajectory
                               Black point: initial population
                               White point: final population")
  }
  return(plot)
}
