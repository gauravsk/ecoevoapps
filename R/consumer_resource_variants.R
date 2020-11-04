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


#' Run predator-prey model
#' @param time vector of time units over which to run model, starting from 0.
#' `time` can also be supplied as just the total length of the simulation (i.e. tmax)
#' @param init vector of initial population sizes for both species, with names H and P
#' @param params vector of parameters. Minimum requirements are r, a, e, d. If K supplied, then prey has logistic growth; if T_h supplied, Type II functional response of predator; if both K and T_h, R-M model
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
