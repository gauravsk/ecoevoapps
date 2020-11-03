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
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param pars intrinsic growth rate r, a, e, d, T_h, K
#' @export
run_predprey_model <- function(time, init, params) {
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
