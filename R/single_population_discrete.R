#' Discrete exponential growth model
#' @param N0 initial population size of population
#' @param lambda discrete population growth rate
#' @param time Number of time steps over which to project the model
#' @export
discrete_exponential <- function(N0, lambda, time) {
  to_return <-   c(N0,N0*(lambda^(1:time)))
  return(to_return)
}


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
    to_return[current_time] <-Nt*exp(r*(1-Nt/K))
  }
  return(to_return)
}


