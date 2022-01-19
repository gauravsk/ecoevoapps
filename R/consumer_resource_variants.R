#' Run L-V Pred prey model, Type I Functional Response
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param pars intrinsic growth rate r, a, e, d
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
rm_predprey_1step <-function(H, P, pars) {
  with (as.list(pars), {
    dH_dt = r*H*(1 - H/K) - (a*H*P)/(1 + a*T_h*H)
    dP_dt = e*(a*H*P)/(1 + a*T_h*H) - d*P
    return(data.frame(dH = dH_dt, dP = dP_dt))
  })
}


#' Run predator-prey model
#' @param time vector of time units over which to run model, starting from 0.
#' `time` can also be supplied as just the total length of the simulation (i.e. tmax)
#' @param init vector of initial population sizes for both species, with names H and P
#' @param params vector of parameters. Minimum requirements are r, a, e, d.
#' If K supplied, then prey has logistic growth;
#' if T_h supplied, Type II functional response of predator;
#' if both K and T_h, this is the Rosenzweig-Macarthur  model
#' @examples
#' # Classic Lotka-Volterra model
#' run_predprey_model(200, init = c(H = 10, P = 5),
#' params = c(r = .1, a = .01, e = .01, d = .001))
#' # Lotka-Volterra model with logistic growth of prey
#' run_predprey_model(200, init = c(H = 10, P = 5),
#' params = c(r = .1, a = .01, e = .01, d = .001, K = 1000))
#' # Lotka-Volterra model with Type 2 functional response
#' run_predprey_model(200, init = c(H = 10, P = 5),
#' params = c(r = .1, a = .01, e = .01, d = .001, T_h = .1))
#' # Rosenzweig-Macarthur model (logistic prey and Type 2 FR predator)
#' run_predprey_model(200, init = c(H = 10, P = 5),
#' params = c(r = .1, a = .01, e = .01, d = .001, K = 1000, T_h = .1))
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
  if(!(is.numeric(params))) stop("params should be a numeric vector")
  if(!(all(names(params) %in% c("r", "e", "a", "d")) |
       all(names(params) %in% c("r", "e", "a", "d", "K")) |
       all(names(params) %in% c("r", "e", "a", "d", "T_h")) |
       all(names(params) %in% c("r", "e", "a", "d", "K", "T_h")))) {
    stop("please check the parameter vector.")
  }

  if(all(c("T_h", "K") %in% names(params))) {
    # R-M
    deSolve::ode(rm_predprey, y = init, times = time, parms = params)
  } else if ("T_h" %in% names(params)) {
    # Type II
    deSolve::ode(lv_predprey_t2, y = init, times = time, parms = params)
  } else if ("K" %in% names(params)) {
    # logistic prey
    deSolve::ode(lv_predprey_logPrey, y = init, times = time, parms = params)
  } else {
    # Type I + exponential prey
    deSolve::ode(lv_predprey_t1, y = init, times = time, parms = params)
  }
}


# FOR PLOTTING VECTOR FIELDS

#' generates a grid for plotting a vector field
#' @param sim_df data frame generated from run_XXX
#' @param pars_for_eq_func parameter values used to generate `sim_df`
#' @param vec_density density of grid to generate
#' @return a data frame, with Hstart, Hend, Pstart, Pend,
#' and corresponding values of dH and dP for drawing vectors
#' @importFrom purrr map2_df
vector_field_input <- function(sim_df, pars_for_eq_func, vec_density = 20) {

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


  # identify model type
  if(all(c("T_h", "K") %in% names(pars_for_eq_func))) {
    # R-M
    hpcoords <- bind_cols(hpcoords,
                          map2_df(hpcoords$Hstart, hpcoords$Pstart,
                                  ecoevoapps:::rm_predprey_1step, pars_for_eq_func))
  } else if ("T_h" %in% names(pars_for_eq_func)) {
    # Type II
    hpcoords <- bind_cols(hpcoords,
                          map2_df(hpcoords$Hstart, hpcoords$Pstart,
                                  ecoevoapps:::lv_predprey_t2_1step, pars_for_eq_func))
  } else if ("K" %in% names(pars_for_eq_func)) {
    # logistic prey
    hpcoords <- bind_cols(hpcoords,
                          map2_df(hpcoords$Hstart, hpcoords$Pstart,
                                  ecoevoapps:::lv_predprey_logPrey_1step, pars_for_eq_func))
  } else {
    # Type I + exponential prey
    hpcoords <- bind_cols(hpcoords,
                          map2_df(hpcoords$Hstart, hpcoords$Pstart,
                                  ecoevoapps:::lv_predprey_t1_1step, pars_for_eq_func))
  }

  # use those values to solve dP and dH and calculate pend and hend
  hpcoords <- hpcoords %>%
    mutate(Hend = Hstart + dH, Pend = Pstart + dP)

  return(hpcoords)
}


#' generates a vector field
#' @param sim_df data frame generated from run_predprey
#' @param pars_for_eq_func parameter values used to generate `sim_df`
#' @param vec_density density of grid to generate
#' @import ggplot2
#' @return a ggplot2 object
plot_vector_field <- function(sim_df, pars_for_eq_func, vec_density = 20) {

  vector_field_input_data <- vector_field_input(sim_df, pars_for_eq_func,
                                                vec_density = vec_density)
  ggplot(sim_df) +

    # vector field
    geom_segment(data = vector_field_input_data,
                 aes(x = Hstart, y = Pstart, xend = Hend, yend = Pend),
                 arrow = arrow(length = unit(0.02, "npc")),
                 color = "light gray")

}

#' Function for plotting trajectory of predator-prey model
#' @param sim_df data frame generated from run_XXX
#' @param pars_for_eq_func parameter values used to generate `sim_df`
#' @param vectors_field set to TRUE to see the vector field under the trajectory
#' @import ggplot2
#' @export
plot_predprey_trajectory <- function(sim_df, param_vec, vectors_field = F,...) {

  if(vectors_field) {
    base_plot <- plot_vector_field(sim_df, param_vec, ...)
  } else {
    base_plot <- ggplot(sim_df)
  }

  traj <-
    base_plot +

    # the trace of the simulation and arrow for direction of the trace
    geom_path(data = sim_df, aes(x = H, y = P), size = 2) +
    geom_segment(x = sim_df$H[5], y = sim_df$P[5],
                 xend = sim_df$H[6], yend = sim_df$P[6],
                 arrow = arrow(length = unit(0.1, "npc")),
                 cex = 2) +

    # plot appearance
    xlab("Number of Prey") +
    ylab("Number of Predators") +
    coord_cartesian(xlim = c(min(sim_df$H), max(sim_df$H) + 1),
                    ylim = c(min(sim_df$P), max(sim_df$P) + 1)) +  #need this line to show all vectors that go beyond plot limits
    theme_apps()

  # Add isoclines based on the model type

  if(all(c("T_h", "K") %in% names(param_vec))) {
    # R-M
    traj <-
      traj +
      stat_function(fun = function(x) (param_vec["r"]/param_vec["a"])*
                      (1 - x/param_vec["K"])*(1 + param_vec["a"] * param_vec["T_h"] * x),
                    col = "#E41A1C", size = 2) +
      geom_vline(xintercept = param_vec["d"]/(param_vec["e"]*param_vec["a"] - param_vec["a"]*param_vec["d"]*param_vec["T_h"]),
                 col = "#377EB8", size = 2)

  } else if ("T_h" %in% names(param_vec)) {
    # Type II
    traj <- traj +
          geom_abline(intercept = param_vec["r"]/param_vec["a"],
                      slope = param_vec["r"]*param_vec["T_h"],
                      col = "#E41A1C", size = 2) +
          geom_vline(xintercept = param_vec["d"]/
                       (param_vec["e"]*param_vec["a"] -
                          param_vec["a"]*param_vec["d"]*param_vec["T_h"]),
                     col = "#377EB8", size = 2)
  } else if ("K" %in% names(param_vec)) {
    # logistic prey
    traj <- traj +
      geom_abline(intercept = param_vec["r"]/param_vec["a"],
                  slope = (-1 * param_vec["r"])/(param_vec["a"] * param_vec["K"]),
                  col = "#E41A1C", size = 2) +
      geom_vline(xintercept = param_vec["d"]/(param_vec["e"]*param_vec["a"]),
                 col = "#377EB8", size = 2)
  } else {
    # Type I + exponential prey
    traj <- traj +
      geom_hline(yintercept = param_vec["r"]/param_vec["a"],
                 col = "#E41A1C", size = 2) +
      geom_vline(xintercept = param_vec["d"]/(param_vec["e"]*param_vec["a"]),
                 col = "#377EB8", size = 2)
  }
  return(traj)
}
