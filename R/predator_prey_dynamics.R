#' Run L-V Pred prey model, Type I Functional Response
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param pars intrinsic growth rate r, a, e, d
#' @keywords internal
lv_predprey_t1 <- function(time,init,pars) {
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

#' Internal function used for generating vector field
#' @param H density of the prey
#' @param P density of the predator
#' @param pars vector of model parameters (r,a,e,d)
#' @noRd
lv_predprey_t1_1step <- function(H, P, pars) {
  with (as.list(pars), {
    dH_dt = r*H - (a*H*P)
    dP_dt = e*(a*H*P) - d*P
    return(data.frame(dH = dH_dt, dP = dP_dt))
  })
}

#' Run L-V Pred prey model, Type I FR + logistic prey
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param pars intrinsic growth rate r, a, e, d, K
#' @keywords internal
lv_predprey_logPrey <- function(time,init,pars) {
  with (as.list(c(time,init,pars)), {
    # description of parameters:
    # r = per capita growth rate (prey)
    # a = attack rate
    # e = conversion efficiency
    # d = predator death rate
    # K = carrying capacity of the prey

    dH_dt = r*H*(1 - H/K) - (a*H*P)
    dP_dt = e*(a*H*P) - d*P
    return(list(c(dH = dH_dt, dP = dP_dt)))

  })
}

#' Internal function used for generating vector field
#' @param H density of the prey
#' @param P density of the predator
#' @param pars vector of model parameters (r,a,e,d,K)
#' @noRd
lv_predprey_logPrey_1step <- function(H, P, pars) {

  with (as.list(pars), {
    dH_dt = r*H*(1 - H/K) - (a*H*P)
    dP_dt = e*(a*H*P) - d*P
    return(data.frame(dH = dH_dt, dP = dP_dt))
  })
}

#' Run L-V Pred prey model, Type II Functional Response
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param pars intrinsic growth rate r, a, e, d, T_h
#' @keywords internal
lv_predprey_t2 <- function(time,init,pars) {
  with (as.list(c(time,init,pars)), {
    # description of parameters:
    # r = per capita growth rate (prey)
    # a = attack rate
    # T_h = handling time
    # e = conversion efficiency
    # d = predator death rate
    dH_dt = r*H - (a*H*P)/(1 + a*T_h*H)
    dP_dt = e*(a*H*P)/(1 + a*T_h*H) - d*P
    return(list(c(dH = dH_dt, dP = dP_dt)))

  })
}


#' Internal function used for generating vector field
#' @param H density of the prey
#' @param P density of the predator
#' @param pars vector of model parameters (r,a,e,d,T_h)
#' @noRd
lv_predprey_t2_1step <- function(H, P, pars) {
  with (as.list(pars), {
    # description of parameters:
    # r = per capita growth rate (prey)
    # a = attack rate
    # T_h = handling time
    # e = conversion efficiency
    # d = predator death rate

    dH_dt = r*H - (a*H*P)/(1 + a*T_h*H)
    dP_dt = e*(a*H*P)/(1 + a*T_h*H) - d*P

    return(data.frame(dH = dH_dt, dP = dP_dt))

  })
}

#' Run Rosenzweig-MacArthur model
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param pars intrinsic growth rate r, a, e, d, T_h, K
#' @keywords internal
rm_predprey <- function(time,init,pars) {
  with (as.list(c(time,init,pars)), {
    # description of parameters:
    # r = per capita growth rate (prey)
    # K = prey carrying capacity
    # a = attack rate
    # T_h = handling time
    # e = conversion efficiency
    # d = predator death rate

    dH_dt = r*H*(1 - H/K) - (a*H*P)/(1 + a*T_h*H)
    dP_dt = e*(a*H*P)/(1 + a*T_h*H) - d*P
    return(list(c(dH = dH_dt, dP = dP_dt)))

  })
}

#' Internal function used for generating vector field
#' @param H density of the prey
#' @param P density of the predator
#' @param pars vector of model parameters (r,a,e,d,T_h)
#' @noRd
rm_predprey_1step <-function(H, P, pars) {
  with (as.list(pars), {
    dH_dt = r*H*(1 - H/K) - (a*H*P)/(1 + a*T_h*H)
    dP_dt = e*(a*H*P)/(1 + a*T_h*H) - d*P
    return(data.frame(dH = dH_dt, dP = dP_dt))
  })
}


#' Run predator-prey model
#' @param time vector of time units over which to run model, starting from 0.
#'   `time` can also be supplied as just the total length of the simulation
#'   (i.e. tmax)
#' @param init vector of initial population sizes for both species, with names H
#'   and P
#' @param params vector of parameters. If the param vector has entries of c(r,
#'   a, e, d), the function runs the classic Lotka-Volterra model (type 1
#'   functional response of predator; exponential growth for prey). If the
#'   parameter vector has entries c(r, a, e, d, K), supplied, then the function
#'   runs the Lotka-Volterra model with logistic growth in the prey. If the
#'   parameter vector has entries c(r, a, e, d, T_h), the function runs the
#'   Lotka-Volterra model with a Type II function response in the predator.
#'   Finally, if the parameter vector has entries c(r, a, e, d, K, T_h), the
#'   function runs the Lotka-Volterra model with logistic growth in the prey and
#'   Type II functional response for the predator (this is sometimes called the
#'   Rosenzweig-Macarthur model).
#' @import deSolve
#' @seealso [plot_predprey_time()] for plots of the population dynamics over
#'   time, and [plot_predprey_portrait()] for making portrait plots of the
#'   predator and prey (including visualizations of the ZNGIs)
#' @examples
#' # Lotka-Volterra predator-prey model
#' params_lv <- c(r = .1, a = .01, e = .01, d = .001)
#' run_predprey_model(5, init = c(H = 10, P = 5), params = params_lv)
#' # Lotka-Volterra model with logistic growth of prey
#' params_lv_logprey <- c(r = .1, a = .01, e = .01, d = .001, K = 1000)
#' run_predprey_model(5, init = c(H = 10, P = 5), params = params_lv_logprey)
#' # Lotka-Volterra model with Type 2 functional response
#' params_lvt2 <- c(r = .1, a = .01, e = .01, d = .001, T_h = .1)
#' run_predprey_model(5, init = c(H = 10, P = 5), params = params_lvt2)
#' # Rosenzweig-Macarthur model (logistic prey and Type 2 FR predator)
#' params_rm <- c(r = .1, a = .01, e = .01, d = .001, K = 1000, T_h = .1)
#' run_predprey_model(5, init = c(H = 10, P = 5), params = params_rm)
#' @export
run_predprey_model <- function(time, init, params) {

  # Check how time has been defined (if just Tmax, then make vector)
  # and if vector was supplied, check that it starts at t = 0
  if(length(time) == 1) {
    tmax <- time
    time <- seq(0, tmax)
  } else if(time[1] != 0) {
    stop("The time vector should start at 0.")
  }
  # Check that init has been defined properly
  if(!(is.numeric(init))) stop("init should be a numeric vector of length 2, e.g. c(H = 10, P = 20)")
  if(length(init) != 2) stop("init should be a numeric vector of length 2, e.g. c(H = 10, P = 20)")
  if(!(all(names(init) %in% c("H","P")))) stop("init should be a numeric vector of length 2, e.g. c(H = 10, P = 20)")

  # Check that params is correctly defined
  if(!(is.numeric(params))) {
    stop("params should be a numeric vector, see ?run_predprey_model for details")
  }
  if(!(all(names(params) %in% c("r", "e", "a", "d")) |
       all(names(params) %in% c("r", "e", "a", "d", "K")) |
       all(names(params) %in% c("r", "e", "a", "d", "T_h")) |
       all(names(params) %in% c("r", "e", "a", "d", "K", "T_h")))) {
    stop("please check the parameter vector, see ?run_predprey_model for details")
  }

  if(all(c("T_h", "K") %in% names(params))) {
    # R-M
    ode(rm_predprey, y = init, times = time, parms = params)
  } else if ("T_h" %in% names(params)) {
    # Type II
    ode(lv_predprey_t2, y = init, times = time, parms = params)
  } else if ("K" %in% names(params)) {
    # logistic prey
    ode(lv_predprey_logPrey, y = init, times = time, parms = params)
  } else {
    # Type I + exponential prey
    ode(lv_predprey_t1, y = init, times = time, parms = params)
  }
}



#' Generate a grid for plotting a vector field under predator-prey trajectories
#' @param sim_df data frame generated from run_XXX
#' @param params parameter values used to generate `sim_df`
#' @param vec_density density of grid to generate
#' @return a data frame, with Hstart, Hend, Pstart, Pend,
#' and corresponding values of dH and dP for drawing vectors
#' @keywords internal
#' @importFrom purrr map2_df
vector_field_input <- function(sim_df, params, vec_density = 20) {

  # determine the min and max of the number of prey and predators
  lowH <- 1
  hiH <- max(sim_df$H)
  lowP <- 1
  hiP <- max(sim_df$P)

  # select a sequence of points between (and a little beyond) those values
  seqH <- seq(0.9*lowH, 1.4*hiH, length.out = vec_density)
  seqP <- seq(0.9*lowP, 1.4*hiP, length.out = vec_density)

  # find all the combinations of those H and P coordinates, make that a df
  # w/Hstart and Pstart
  hpcoords <- expand.grid(Hstart = seqH, Pstart = seqP)


  # identify model type
  if(all(c("T_h", "K") %in% names(params))) {
    # R-M
    hpcoords <- bind_cols(hpcoords,
                          map2_df(hpcoords$Hstart, hpcoords$Pstart,
                                  rm_predprey_1step, params))
  } else if ("T_h" %in% names(params)) {
    # Type II
    hpcoords <- bind_cols(hpcoords,
                          map2_df(hpcoords$Hstart, hpcoords$Pstart,
                                  lv_predprey_t2_1step, params))
  } else if ("K" %in% names(params)) {
    # logistic prey
    hpcoords <- bind_cols(hpcoords,
                          map2_df(hpcoords$Hstart, hpcoords$Pstart,
                                  lv_predprey_logPrey_1step, params))
  } else {
    # Type I + exponential prey
    hpcoords <- bind_cols(hpcoords,
                          map2_df(hpcoords$Hstart, hpcoords$Pstart,
                                  lv_predprey_t1_1step, params))
  }

  # use those values to solve dP and dH and calculate pend and hend
  hpcoords <- hpcoords %>%
    mutate(Hend = Hstart + dH, Pend = Pstart + dP)

  return(hpcoords)
}


#' generates a vector field
#' @param sim_df data frame generated from run_predprey
#' @param params parameter values used to generate `sim_df`
#' @param vec_density density of grid to generate
#' @import ggplot2
#' @keywords internal
plot_vector_field <- function(sim_df, params, vec_density = 20) {

  vector_field_input_data <- vector_field_input(sim_df, params,
                                                vec_density = vec_density)
  ggplot(sim_df) +

    # vector field
    geom_segment(data = vector_field_input_data,
                 aes(x = Hstart, y = Pstart, xend = Hend, yend = Pend),
                 arrow = arrow(length = unit(0.01, "npc")),
                 color = "light gray")

}

#' Function for plotting phase portrait of predator-prey model
#' @param sim_df data frame generated from run_XXX
#' @param params parameter values used to generate `sim_df`
#' @param vectors_field set to TRUE to see the vector field under the phase
#'   portrait
#' @param ... additional arguments to pass to [ecoevoapps::plot_vector_field]
#'   (mostly this is the density of the vector field, set by vec_density)
#' @import ggplot2
#' @seealso [run_predprey_model()] for simulating the dynamics of a
#'   predator-prey system, and [plot_predprey_portrait()] for making portrait
#'   plots of the predator and prey (including visualizations of the ZNGIs)
#' @examples
#' # Define parameters for the Rosenzweig-Macarthur model:
#' params <- c(r = .1, a = .01, e = .01, d = .001, K = 1000, T_h = .1)
#' sim_df <- run_predprey_model(200, init = c(H = 10, P = 5), params = params)
#' plot_predprey_portrait(sim_df = sim_df, params = params, vectors_field
#' = TRUE)
#' @export
plot_predprey_portrait <- function(sim_df, params, vectors_field = FALSE,...) {
  sim_df <- data.frame(sim_df)
  if(vectors_field) {
    base_plot <- plot_vector_field(sim_df, params, ...)
  } else {
    base_plot <- ggplot(sim_df)
  }

  traj <-
    base_plot +

    # the trace of the simulation and arrow for direction of the trace
    geom_path(data = sim_df, aes(x = H, y = P), size = 2) +
    geom_segment(x = sim_df$H[5], y = sim_df$P[5],
                 xend = sim_df$H[6], yend = sim_df$P[6],
                 arrow = arrow(length = unit(0.05, "npc")),
                 size = 1.5) +

    # plot appearance
    xlab("Number of Prey") +
    ylab("Number of Predators") +
    scale_y_continuous(limits = c(0, max(sim_df$P) + 1), expand = c(0,0)) +
    scale_x_continuous(limits = c(0, max(sim_df$H) + 1), expand = c(0,0)) +
    theme_apps()

  # Add isoclines based on the model type

  if(all(c("T_h", "K") %in% names(params))) {
    # R-M
    traj <-
      traj +
      stat_function(fun = function(x) (params["r"]/params["a"])*
                      (1 - x/params["K"])*(1 + params["a"] * params["T_h"] * x),
                    col = "#E41A1C", size = 2) +
      geom_vline(xintercept = params["d"]/(params["e"]*params["a"] - params["a"]*params["d"]*params["T_h"]),
                 col = "#377EB8", size = 2)

  } else if ("T_h" %in% names(params)) {
    # Type II
    traj <- traj +
          geom_abline(intercept = params["r"]/params["a"],
                      slope = params["r"]*params["T_h"],
                      col = "#E41A1C", size = 2) +
          geom_vline(xintercept = params["d"]/
                       (params["e"]*params["a"] -
                          params["a"]*params["d"]*params["T_h"]),
                     col = "#377EB8", size = 2)
  } else if ("K" %in% names(params)) {
    # logistic prey
    traj <- traj +
      geom_abline(intercept = params["r"]/params["a"],
                  slope = (-1 * params["r"])/(params["a"] * params["K"]),
                  col = "#E41A1C", size = 2) +
      geom_vline(xintercept = params["d"]/(params["e"]*params["a"]),
                 col = "#377EB8", size = 2)
  } else {
    # Type I + exponential prey
    traj <- traj +
      geom_hline(yintercept = params["r"]/params["a"],
                 col = "#E41A1C", size = 2) +
      geom_vline(xintercept = params["d"]/(params["e"]*params["a"]),
                 col = "#377EB8", size = 2)
  }
  return(traj)
}

#' Function for plotting phase portrait of predator-prey model
#' @param sim_df data frame generated from run_XXX
#' @import ggplot2
#' @seealso [run_predprey_model()] for simulating the dynamics of a
#'   predator-prey system, and [plot_predprey_portrait()] for making portrait
#'   plots of the predator and prey (including visualizations of the ZNGIs)
#' @examples
#' # Define parameters for the Rosenzweig-Macarthur model:
#' params <- c(r = .1, a = .01, e = .01, d = .01, K = 1000, T_h = .1)
#' sim_df <- run_predprey_model(200, init = c(H = 10, P = 5), params = params)
#' plot_predprey_time(sim_df = sim_df)
#' @export
plot_predprey_time <- function(sim_df) {
  sim_df <- data.frame(sim_df)
  sim_df_long <- pivot_longer(sim_df, c(H,P), "Population")
  ggplot(sim_df_long) +
    geom_line(aes(x = time, y = value, color = Population), size = 2) +
    scale_color_brewer(palette = "Set1") +
    ylab("Population size") +
    scale_y_continuous(expand = c(0,0), limits = c(0, max(sim_df_long$value)*1.05)) +
    scale_x_continuous(expand = c(0,0)) +
    theme_apps()
}

