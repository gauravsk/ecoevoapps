#' SIR model with vital rates
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate),
#' m (natural birth/death rate), and v (vaccination rate)
#' @keywords internal
SIR <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # v is the newborn vaccination rate
    # gamma is the recovery rate
    dS_dt = m*(S + I + R)*(1 - v) - m*S - beta*S*I
    dI_dt = beta*S*I - m*I - gamma*I
    dR_dt = gamma*I - m*R + m*(S + I + R)*v
    return(list(c(dS = dS_dt, dI = dI_dt, dR = dR_dt)))
  })
}

#' SIR model without vital rates
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate),
#' and v (vaccination rate)
#' @keywords internal
SIR_no_vitals <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # v is the vaccination rate
    # gamma is the recovery rate
    dS_dt = -1*beta*S*I - v*S
    dI_dt = beta*S*I - gamma*I
    dR_dt = gamma*I + v*S ### BUT REALLY delete this line and then R = N - S + I
    return(list(c(dS = dS_dt, dI = dI_dt, dR = dR_dt)))
  })
}

#' SEIR model with vital rates
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate),
#' m (natural birth/death rate), v (vaccination rate),
#' and a (inverse of incubation period)
#' @keywords internal
SEIR <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # v is the newborn vaccination rate
    # a is the inverse of the incubation period
    # gamma is the recovery rate
    dS_dt = m*(S + E + I + R)*(1 - v) - m*S - beta*S*I
    dE_dt = beta*S*I - a*E - m*E
    dI_dt = a*E - m*I - gamma*I
    dR_dt = gamma*I - m*R + m*(S + E + I + R)*v
    return(list(c(dS = dS_dt, dE = dE_dt, dI = dI_dt, dR = dR_dt)))
  })
}

#' SIRD model with vital rates
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate),
#' m (natural birth/death rate), v (vaccination rate),
#' a (inverse of incubation period), and mu (death to infections)
#' @keywords internal
SIRD <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # v is the newborn vaccination rate
    # mu is the death rate due to infection
    # gamma is the recovery rate
    dS_dt = m*(S + I + R)*(1 - v) - m*S - beta*S*I
    dI_dt = beta*S*I - m*I - gamma*I - mu*I
    dR_dt = gamma*I - m*R + m*(S + I + R)*v
    dD_dt = mu*I
    return(list(c(dS = dS_dt, dI = dI_dt, dR = dR_dt, dD = dD_dt)))
  })
}

#' SIS model with vital rates
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate),
#' m (natural birth/death rate), and gamma (recovery rate)
#' @keywords internal
SIS <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # gamma is the recovery rate
    dS_dt = m*(S + I) - m*S - beta*S*I + gamma*I
    dI_dt = beta*S*I - m*I - gamma*I
    return(list(c(dS = dS_dt, dI = dI_dt)))
  })
}

#' SIR model with vital rates and frequency-dependent transmission
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate),
#' m (natural birth/death rate), and v(vaccination rate)
#' @keywords internal
SIR_ft <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # v is the newborn vaccination rate
    # gamma is the recovery rate
    dS_dt = m*(S + I + R)*(1 - v) - m*S - beta*S*I/(S+I+R)
    dI_dt = beta*S*I/(S+I+R) - m*I - gamma*I
    dR_dt = gamma*I - m*R + m*(S + I + R)*v
    return(list(c(dS = dS_dt, dI = dI_dt, dR = dR_dt)))
  })
}


#' SEIR model with vital rates and frequency-dependent transmission
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate),
#' m (natural birth/death rate), v (vaccination rate),
#' and a (inverse of incubation period)
#' @keywords internal
SEIR_ft <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # v is the newborn vaccination rate
    # a is the inverse of the incubation period
    # gamma is the recovery rate
    dS_dt = m*(S + E + I + R)*(1 - v) - m*S - beta*S*I/(S+E+I+R)
    dE_dt = beta*S*I/(S+E+I+R) - a*E - m*E
    dI_dt = a*E - m*I - gamma*I
    dR_dt = gamma*I - m*R + m*(S + E + I + R)*v
    return(list(c(dS = dS_dt, dE = dE_dt, dI = dI_dt, dR = dR_dt)))
  })
}

#' SIRD model with vital rates and frequency-dependent transmission
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate),
#' m (natural birth/death rate), v (vaccination rate),
#' a (inverse of incubation period), and mu (death to infections)
#' @keywords internal
SIRD_ft <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # v is the newborn vaccination rate
    # mu is the death rate due to infection
    # gamma is the recovery rate
    dS_dt = m*(S + I + R)*(1 - v) - m*S - beta*S*I/(S+I+R)
    dI_dt = beta*S*I/(S+I+R) - m*I - gamma*I - mu*I
    dR_dt = gamma*I - m*R + m*(S + I + R)*v
    dD_dt = mu*I
    return(list(c(dS = dS_dt, dI = dI_dt, dR = dR_dt, dD = dD_dt)))
  })
}

#' SIS model with vital rates and frequency-dependent transmission
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), m (natural birth/death rate),
#' and gamma (recovery rate)
#' @keywords internal
SIS_ft <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # gamma is the recovery rate
    dS_dt = m*(S + I) - m*S - beta*S*I/(S+I) + gamma*I
    dI_dt = beta*S*I/(S+I) - m*I - gamma*I
    return(list(c(dS = dS_dt, dI = dI_dt)))
  })
}


#' Run infectious disease models models
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), m (natural birth/death rate),
#'   and gamma (recovery rate)
#' @param model_type which type of model to run? (should be one of `SIR`,
#'   `SIR_ft`, `SEIR`, `SEIR_ft`, `SIRD`, `SIRD_ft`, `SIS`, or `SIS_ft`)
#' @seealso [plot_infectiousdisease_time()] to plot trajectories over time, and
#'   [plot_infectiousdisease_portrait()] to plot pairwise portrait diagrams
#' @import deSolve
#' @examples
#' # Run the SIR model
#' params_vec <- c(m = .1, beta = .01, v = .2, gamma = 0)
#' init_vec <- c(S = 100, I = 1, R = 0)
#' time_vec <- seq(0, 10, 0.1)
#' run_infectiousdisease_model(time = time_vec, init = init_vec, params =
#' params_vec, model_type = "SIR")
#' @export
run_infectiousdisease_model <- function(time, init, params, model_type) {
  if(model_type == "SIR") {
    data.frame(ode(func = SIR, y = init,
                            parms = params, times = time))
  } else if(model_type == "SIR_ft") {
    data.frame(ode(func = SIR_ft, y = init,
                            parms = params, times = time))
  } else if(model_type == "SEIR") {
    data.frame(ode(func = SEIR, y = init,
                            parms = params, times = time))
  } else if(model_type == "SEIR_ft") {
    data.frame(ode(func = SEIR_ft, y = init,
                            parms = params, times = time))
  } else if(model_type == "SIRD") {
    data.frame(ode(func = SIRD, y = init,
                            parms = params, times = time))
  } else if(model_type == "SIRD_ft") {
    data.frame(ode(func = SIRD_ft, y = init,
                            parms = params, times = time))
  } else if(model_type == "SIS") {
    data.frame(ode(func = SIS, y = init,
                            parms = params, times = time))
  } else if(model_type == "SIS_ft") {
    data.frame(ode(func = SIS_ft, y = init,
                            parms = params, times = time))
  } else {
    stop("The specified model_type is not supported.")
  }

}


#' Plot phase portrait for infectious disease model model
#' @param sim_df simulated data frame generated from
#'   run_infectiousdisease_model()
#' @param x_axis name of the column in `sim_df` to plot on X-axis of trajectory
#' @param y_axis name of the column in `sim_df` to plot on Y-axis of trajectory
#' @import ggplot2
#' @seealso [run_infectiousdisease_model()] to simulate the dynamics of the
#'   model, and [plot_infectiousdisease_time()] to plot trajectories over time
#' @examples
#' # Run SIR model
#' params_vec <- c(m = .1, beta = .01, v = .2, gamma = 0)
#' init_vec <- c(S = 100, I = 1, R = 0)
#' time_vec <- seq(0, 100, 0.1)
#' sir_out <- run_infectiousdisease_model(time = time_vec, init = init_vec,
#' params = params_vec, model_type = "SIR")
#' plot_infectiousdisease_portrait(sir_out, "S", "I")
#' @export
plot_infectiousdisease_portrait <- function(sim_df, x_axis, y_axis) {
  if(!(x_axis %in% colnames(sim_df)) | !(y_axis %in% colnames(sim_df))) {
    stop(paste0(x_axis," and/or ", y_axis, " are not column names in sim_df"))
  }
  nrows_df <- nrow(sim_df)
  sim_df$x_axis <- sim_df[, x_axis]
  sim_df$y_axis <- sim_df[, y_axis]
  traj <-
    ggplot(sim_df) +
    geom_segment(x = sim_df$x_axis[round(nrows_df)/25],
                 y = sim_df$y_axis[round(nrows_df)/25],
                 xend = sim_df$x_axis[round(nrows_df/25) + 1],
                 yend = sim_df$y_axis[round(nrows_df/25) + 1],
                 arrow = arrow(length = unit(0.15, "npc"))) +
    geom_path(aes(x = x_axis, y = y_axis), size = 2) +
    ylab(paste0(y_axis, " size")) +
    xlab(paste0(x_axis, " size")) +
    theme_apps()
  return(traj)
}


#' Plot phase portrait for infectious disease model model
#' @param sim_df simulated data frame generated from
#'   run_infectiousdisease_model()
#' @param model_type which type of model to run? (should be one of `SIR`,
#'   `SIR_ft`, `SEIR`, `SEIR_ft`, `SIRD`, `SIRD_ft`, `SIS`, or `SIS_ft`)
#' @import ggplot2
#' @seealso [run_infectiousdisease_model()] to simulate the dynamics of the
#'   model, and [plot_infectiousdisease_portrait()] to plot pairwise portrait
#'   diagrams
#' @examples
#' # Run SIR model
#' params_vec <- c(m = .1, beta = .01, v = .2, gamma = 0)
#' init_vec <- c(S = 100, I = 1, R = 0)
#' time_vec <- seq(0, 100, 0.1)
#' sir_out <- run_infectiousdisease_model(time = time_vec, init = init_vec,
#' params = params_vec, model_type = "SIR")
#' plot_infectiousdisease_time(sir_out, model_type = "SIR")
#' @export
plot_infectiousdisease_time <- function(sim_df, model_type) {

  # To suppress CMD Check
  R <- S <- grou <- E <- D <- time <- group <- value <- NULL


  if(model_type %in% c("SIR", "SIR_ft")) {
    sim_df_long <-
      pivot_longer(sim_df, c(S, I, R), "group") %>%
      mutate(group = factor(group, levels = c("S", "I", "R")))
  } else if(model_type %in% c("SEIR", "SEIR_ft")) {
    sim_df_long <-
      pivot_longer(sim_df, c(S, E, I, R), "group") %>%
      mutate(group = factor(group, levels = c("S", "E", "I", "R")))
  } else if(model_type %in% c("SIRD", "SIRD_ft")) {
    sim_df_long <-
      pivot_longer(sim_df, c(S, I, R, D), "group") %>%
      mutate(group = factor(group, levels = c("S", "I", "R", "D")))
  } else if(model_type %in% c("SIS", "SIS_ft")) {
    sim_df_long <-
      pivot_longer(sim_df, c(S, I), "group") %>%
      mutate(group = factor(group, levels = c("S", "I")))
  } else {
    stop("The specified model_type is not supported.")
  }

  ggplot(sim_df_long) +
    geom_line(aes(x = time, y = value, color = group), size = 2) +
    scale_color_brewer(palette = "Set1") +
    ylab("Population size") +
    theme_apps()
}
