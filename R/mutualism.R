mutualism <- function(time, init, params) {
  with(as.list(c(time, init, params)), {
    dN1 <- r1*N1 + (a12*N2*N1)/(b2 + N2) - d1*N1^2
    dN2 <- r2*N2 + (a21*N1*N2)/(b1 + N1) - d2*N2^2
    return(list(c(dN1, dN2)))
  })
}

run_mutualism <- function(time = 0:50,
                          init = c(N1 = 20, N2 = 40),
                          params = c(r1 = 0.5, a12 = 0.8, b2 = 10, d1 = 0.02, r2 = 0.5, a21 = 0.4, b1 = 10, d2 = 0.01)) {

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
  attr(sim, "params") <- params
  attr(sim, "K") <- c(K1 = as.numeric(params["r1"]/params["d1"]),
                      K2 = as.numeric(params["r2"]/params["d2"]))
  return(sim)
}

plot_mutualism_time <- function(sim_df) {
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
  N1_seq <- seq(0, max(sim_df$N1)*2, length.out = 100)
  N2_seq <- seq(0, max(sim_df$N2)*2, length.out = 100)
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
      ggplot2::geom_line(data = sim_df, size = 1) +
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
