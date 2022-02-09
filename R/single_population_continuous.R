#' Exponential growth model
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params intrinsic growth rate r
#' @keywords internal
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
#' @param time vector of time units over which to run model, starting from 0.
#'   `time` can also be supplied as just the total length of the simulation
#'   (i.e. tmax)
#' @param init initial population size of population, in a vector with name `N1`
#' @param params intrinsic growth rate r, in a vector with name `r`
#' @seealso [run_logistic_model()] for simulating the dynamics of a population
#'   with logistic growth to a carrying capacity, and
#'   [run_discrete_exponential_model()] for a simulating exponential growth in
#'   discrete time
#' @examples
#' run_exponential_model(time = 0:10, init = c(N1 = 1), params = c(r = .1))
#' run_exponential_model(time = 10, init = c(N1 = 1), params = c(r = .1))
#' @export
run_exponential_model <- function(time, init, params) {

  # Run checks on user inputs

  # Check how time has been defined (if just Tmax, then make vector)
  # and if vector was supplied, check that it starts at t = 0
  if(length(time) == 1) {
    tmax <- time
    time <- seq(0, tmax)
  } else if(time[1] != 0) {
    stop("The time vector should start at 0.")
  }

  # Check that init has been defined properly
  if(!(is.numeric(init))) stop("init should be a numeric vector of length 1, e.g. c(N1 = 10)")
  if(length(init) != 1) stop("init should be a numeric vector of length 1, e.g. c(N1 = 10)")
  if(names(init) != "N1") names(init) <- "N1"

  # Check that params is correctly defined (just r)
  if(!(is.numeric(params))) stop("init should be a named numeric vector of length 1, e.g. c(r = 0.1)")
  if(length(params) != 1) stop("init should be a named numeric vector of length 1, e.g. c(r = 0.1)")
  if(names(params) != "r") stop("init should be a named numeric vector of length 1, e.g. c(r = 0.1)")

  # Run the model
  deSolve::ode(func = exponential_growth,
               y = init, times = time, parms = params)
}

#' Logistic growth model
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of r (intrinsic growth rate) and K (carrying capacity)
#' @keywords internal
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
#' @keywords internal
lagged_logistic_growth <- function(time, init, params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # r = per-capita growth rate of Sp. 1
    # N1 = population size of Sp. 1
    # K = carrying capacity of Sp 1
    # tau = time lag of density dependence
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


#' Run logistic growth model
#' @param time vector of time units over which to run model, starting from 0.
#'   `time` can also be supplied as just the total length of the simulation
#'   (i.e. tmax)
#' @param init initial population size of population, in a vector with name `N1`
#' @param params vector of intrinsic growth rate,  carrying capacity and, if
#'   simulating a lagged-logistic model, `tau` in a vector with names `r`, `K`,
#'   and `tau` (if applicable)
#' @import deSolve
#' @seealso [run_exponential_model()] for simulating the dynamics of a
#'   population with no carrying capacity, and [run_discrete_logistic_model()],
#'   [run_beverton_holt_model()], and [run_ricker_model()] for discrete-time
#'   models of population growth with population regulation
#' @examples
#' run_logistic_model(time = 0:10, init = c(N1 = 1), params = c(r = .15, K = 1000))
#' run_logistic_model(time = 10, init = c(N1 = 1), params = c(r = .15, K = 1000))
#' run_logistic_model(time = 0:10, init = c(N1 = 1), params = c(r = .15, K = 1000, tau = 2.1))
#' @export
run_logistic_model <- function(time, init, params) {

  # Run checks on user inputs

  # Check how time has been defined (if just Tmax, then make vector)
  # and if vector was supplied, check that it starts at t = 0
  if(length(time) == 1) {
    tmax <- time
    time <- seq(0, tmax)
  } else if(time[1] != 0) {
    stop("The time vector should start at 0.")
  }

  # Check that init has been defined properly
  if(!(is.numeric(init))) stop("init should be a numeric vector of length 1, e.g. c(N1 = 10)")
  if(length(init) != 1) stop("init should be a numeric vector of length 1, e.g. c(N1 = 10)")
  if(names(init) != "N1") names(init) <- "N1"

  # Check that params is correctly defined (r, K, and maybe tau)
  if(!(is.numeric(params))) stop("params should be a numeric vector")
  if(!(length(params) %in% c(2,3))) stop("params should be of length 2 or 3 (if simulating lagged-logistic growth)")
  if(!(all(c("r","K") %in% names(params)))) stop("params should have elements named `r` and `K`")
  if(!(all(names(params) %in% c("r", "K", "tau")))) stop("params should have elements named `r`, `K`, and `tau` (if simulating lagged-logistic growth)")


  if("tau" %in% names(params)) {
    deSolve::dede(func = lagged_logistic_growth,
                 y = init, times = time, parms = params)
  } else {
    deSolve::ode(func = logistic_growth,
                 y = init, times = time, parms = params)
  }
}
