#' Exponential growth model
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params intrinsic growth rate r
exponential_growth <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # r = per-capita growth rate of Sp. 1
    # N1 = population size of Sp. 1
    dN1 <- (r*N1)
    return(list(c(dN1)))
  })
}

#' Run exponential growth model
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params intrinsic growth rate r
#' @export
run_exponential_model <- function(time,init,params) {
  deSolve::ode(func = exponential_growth,
               y = init, times = time, parms = params)
}

#' Logistic growth model
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of r (intrinsic growth rate) and K (carrying capacity)
logistic_growth <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # r = per-capita growth rate of Sp. 1
    # N1 = population size of Sp. 1
    # K = carrying capacity of Sp 1
    dN1 <- (r*N1)*(1-N1/K)
    return(list(c(dN1)))
  })
}


#' Lagged logistic growth model
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of r (intrinsic growth rate), K (carrying capacity), and tau (time lag)
lagged_logistic_growth <- function(time, init, params) {
  with (as.list(c(time,init,params)), {
    tlag <- time - tau
    if (tlag < 0) {
      Nlag <- N1
    } else {
      Nlag <- deSolve::lagvalue(tlag)
    }
    dN1 <- r * N1 * (1-(Nlag/K))
    return(list(c(dN1)))
  })
}


#' Run exponential growth model
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params intrinsic growth rate r
#' @export
run_logistic_model <- function(time, init, params) {
  if("tau" %in% names(params)) {
    deSolve::dede(func = lagged_logistic_growth,
                 y = init, times = time, parms = params)
  } else {
    deSolve::ode(func = logistic_growth,
                 y = init, times = time, parms = params)
  }
}
