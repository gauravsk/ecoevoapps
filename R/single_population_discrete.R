#' Discrete exponential growth model
#' @param N0 initial population size of population
#' @param lambda discrete population growth rate
#' @param time Number of time steps over which to project the model
#' @export
discrete_exponential <- function(N0, lambda, time) {
  to_return <-   c(N0,N0*(lambda^(1:time)))
  return(to_return)
}

#' Equation that returns Nt+1 from Nt for the Ricker model
#' @param Nt Nt
#' @param r Population growth rate
#' @param K Carrying capacity
#' @export
ricker_eqn <- function(Nt, r, K) Nt*exp(r*(1-Nt/K))

#' Ricker model of discrete population growth with a carrying capacity
#' @param N0 initial population size of population
#' @param K Carrying capacity
#' @param r Population growth rate
#' @param time Number of time steps over which to project the model
#' @export
ricker_model <- function(N0, K, r, time) {
  to_return <- numeric(time)
  to_return[1] <- N0
  for(current_time in 2:time) {
    Nt <- to_return[current_time-1]
    to_return[current_time] <- ricker_eqn(Nt, r, K)
  }
  return(to_return)
}

#' Equation that returns Nt+1 from Nt for the "standard" Discrete Logistic model
#' @param Nt Nt
#' @param rd Population growth rate
#' @param K Carrying capacity
#' @export
discretelogistic_eqn <- function(Nt, rd, K) rd*Nt*(1-Nt/K)

#' Discrete logistic model
#' @param N0 initial population size of population
#' @param K Carrying capacity
#' @param rd Discrete growth factor
#' @param time Number of time steps over which to project the model
#' @export
discrete_logistic <- function(N0, K, rd, time) {
  to_return <- numeric(time)
  to_return[1] <- N0
  for(current_time in 2:time) {
    Nt <- to_return[current_time-1]
    to_return[current_time] <-discretelogistic_eqn(Nt, rd, K)
  }
  return(to_return)

}

#' Equation that returns Nt+1 from Nt for the Beverton-Holt model
#' @param x Nt
#' @param r Population growth rate
#' @param K Carrying capacity
#' @export
bevertonholt_eqn <- function(Nt, Rd, K) (Rd*Nt)/(1+((Rd-1)/K)*Nt)

#' Beverton Holt model
#' @param N0 initial population size of population
#' @param K Carrying capacity
#' @param Rd population growth rate
#' @param time Number of time steps over which to project the model
#' @export
beverton_holt_model <- function(N0, K, Rd, time) {
  to_return <- numeric(time)
  to_return[1] <- N0
  for(current_time in 2:time) {
    Nt <- to_return[current_time-1]
    to_return[current_time] <- bevertonholt_eqn(Nt, Rd, K)
  }
  return(to_return)

}
