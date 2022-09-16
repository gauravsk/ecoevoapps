mutualism <- function(time, init, params) {
  with(as.list(c(time, init, params)), {
    dN1 <- r1*N1 + (a12*N2*N1)/(b2 + N2) - d1*N1^2
    dN2 <- r2*N2 + (a21*N1*N2)/(b1 + N1) - d2*N2^2
    return(list(c(dN1, dN2)))
  })
}

run_mutualism <- function(time = 0:50,
                          init = c(N1 = 20, N2 = 40),
                          params = c(r1 = 1, a12 = 0.8, b2 = 20, d1 = 0.02, r2 = 1, a21 = 0.4, b1 = 20, d2 = 0.01)) {

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
         e.g. c(N1 = 50, N2 = 100)")
  }
  if (length(init) != 2) {
    stop("init should be a named numeric vector of length 2.
         e.g. c(N1 = 50, N2 = 100)")
  }
  if (!all(c("N1", "N2") %in% names(init))) {
    stop("Elements of init should be named N1 and N2.
         e.g. c(N1 = 50, N2 = 100).")
  }
  if (any(init <= 0)) {
    stop("init should only contain positive values.")
  }

  # Check params
  if (!is.numeric(params)) {
    stop("params should be a named numeric vector of length 8.
         e.g. c(r1 = 1, a12 = 0.2, b2 = 20, d1 = 0.01, r2 = 1, a21 = 0.4, b1 = 20, d2 = 0.01)")
  }
  if (length(params) != 8) {
    stop("params should be a named numeric vector of length 8.
         e.g. c(r1 = 1, a12 = 0.2, b2 = 20, d1 = 0.01, r2 = 1, a21 = 0.4, b1 = 20, d2 = 0.01)")
  }
  if (!all(c("r1", "r2", "a12", "a21", "b1", "b2", "d1", "d2") %in% names(params))) {
    stop("Elements of params should be named r1, r2, a12, a21, b1, b2, d1, d2.
         e.g. c(r1 = 1, a12 = 0.2, b2 = 20, d1 = 0.01, r2 = 1, a21 = 0.4, b1 = 20, d2 = 0.01)")
  }
  if (any(params <= 0)) {
    stop("params should only contain positive values.")
  }

  # Simulate
  sim <- deSolve::ode(func = mutualism, y = init, times = time, parms = params)
  sim <- as.data.frame(sim)
  attr(sim, "K") <- c(K1 = as.numeric(params["r1"]/params["d1"]),
                      K2 = as.numeric(params["r2"]/params["d2"]))
  return(sim)
}

plot_mutualism_time <- function(sim_df) {
  sim_df_long <- tidyr::pivot_longer(sim_df, c(N1, N2), names_to = "species")
  y_upper <- max(c(sim_df_long$value, attr(sim_df, "K")))
  K_df <- data.frame(species = c("N1", "N2"), K = attr(sim_df, "K"))
  N_over_time <-
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
  return(N_over_time)
}
