#' Discrete exponential growth model
#' @param N0 initial population size of population
#' @param lambda discrete population growth rate
#' @param time Number of time steps over which to project the model
#' @examples
#' run_discrete_exponential_model(N0 = 1, lambda = 1.1, time = 100)
#' @return A data frame with colums time and Nt
#' @export
run_discrete_exponential_model <- function(N0 = 1, lambda = 1.1, time = 100) {
  # Check that N0, lambda, and time are all single numbers
  if (!(length(N0) == 1 & is.numeric(N0))) stop("N0 should be a single number")
  if (!(length(lambda) == 1 & is.numeric(lambda))) stop("lambda should be a single number")
  if (!(length(time) == 1 & is.numeric(time))) stop("time should be a single number")

  to_return <- c(N0,N0*(lambda^(1:time)))
  to_return_df <- data.frame(time = 0:time,
                             Nt = to_return)
  return(to_return_df)
}

#' Equation that returns Nt+1 from Nt for the Ricker model
#' @param Nt Population size
#' @param r Population growth rate
#' @param K Carrying capacity
#' @keywords internal
ricker_eqn <- function(Nt, r, K) Nt*exp(r*(1-Nt/K))

#' Ricker model of discrete population growth with a carrying capacity
#' @param N0 initial population size of population
#' @param K Carrying capacity
#' @param r Population growth rate
#' @param time Number of time steps over which to project the model
#' @examples
#' run_ricker_model(N0 = 1, K = 100, r = 1.1, time = 100)
#' @return A data frame with colums time and Nt
#' @export
run_ricker_model <- function(N0 = 1, K = 100, r = 1.1, time = 100) {
  # Check that N0, K, r, and time are all single numbers
  if (!(length(N0) == 1 & is.numeric(N0))) stop("N0 should be a single number")
  if (!(length(K) == 1 & is.numeric(K))) stop("K should be a single number")
  if (!(length(r) == 1 & is.numeric(r))) stop("r should be a single number")
  if (!(length(time) == 1 & is.numeric(time))) stop("time should be a single number")

  to_return <- numeric(time)
  to_return[1] <- N0
  for(current_time in 2:time) {
    Nt <- to_return[current_time-1]
    to_return[current_time] <- ricker_eqn(Nt, r, K)
  }
  to_return_df <- data.frame(time = 1:time,
                             Nt = to_return)
  return(to_return_df)
  return(to_return)
}

#' Equation that returns Nt+1 from Nt for the "standard" Discrete Logistic model
#' @param Nt Nt
#' @param rd Population growth rate
#' @param K Carrying capacity
#' @keywords internal
discretelogistic_eqn <- function(Nt, rd, K) rd*Nt*(1-Nt/K)

#' Discrete logistic model
#' @param N0 initial population size of population
#' @param K Carrying capacity
#' @param rd Discrete growth factor
#' @param time Number of time steps over which to project the model
#' @examples
#' run_discrete_logistic_model(N0 = 1, K = 100, rd = 1.1, time = 100)
#' @return A data frame with colums time and Nt
#' @export
run_discrete_logistic_model <- function(N0 = 1, K = 100, rd = 1.1, time = 100) {

  # Check that N0, K, rd, and time are all single numbers
  if (!(length(N0) == 1 & is.numeric(N0))) stop("N0 should be a single number")
  if (!(length(K) == 1 & is.numeric(K))) stop("K should be a single number")
  if (!(length(rd) == 1 & is.numeric(rd))) stop("rd should be a single number")
  if (!(length(time) == 1 & is.numeric(time))) stop("time should be a single number")


  to_return <- numeric(time)
  to_return[1] <- N0
  for(current_time in 2:time) {
    Nt <- to_return[current_time-1]
    to_return[current_time] <-discretelogistic_eqn(Nt, rd, K)
  }
  to_return_df <- data.frame(time = 1:time,
                             Nt = to_return)
  return(to_return_df)

  return(to_return)

}

#' Equation that returns Nt+1 from Nt for the Beverton-Holt model
#' @param x Nt
#' @param r Population growth rate
#' @param K Carrying capacity
#' @keywords internal
bevertonholt_eqn <- function(Nt, Rd, K) (Rd*Nt)/(1+((Rd-1)/K)*Nt)

#' Beverton Holt model
#' @param N0 initial population size of population
#' @param K Carrying capacity
#' @param Rd population growth rate
#' @param time Number of time steps over which to project the model
#' @examples
#' run_beverton_holt_model(N0 = 1, K = 100, Rd = 1.01, time = 100)
#' @return A data frame with colums time and Nt
#' @export
run_beverton_holt_model <- function(N0 = 1, K = 100, Rd = 1.01, time = 100) {

  # Check that N0, K, Rd, and time are all single numbers
  if (!(length(N0) == 1 & is.numeric(N0))) stop("N0 should be a single number")
  if (!(length(K) == 1 & is.numeric(K))) stop("K should be a single number")
  if (!(length(Rd) == 1 & is.numeric(Rd))) stop("Rd should be a single number")
  if (!(length(time) == 1 & is.numeric(time))) stop("time should be a single number")

  to_return <- numeric(time)
  to_return[1] <- N0
  for(current_time in 2:time) {
    Nt <- to_return[current_time-1]
    to_return[current_time] <- bevertonholt_eqn(Nt, Rd, K)
  }
  to_return_df <- data.frame(time = 1:time,
                             Nt = to_return)
  return(to_return_df)

}

#' Plot the dynamics of a discrete population growth model
#' @param sim_df data frame of discrete population growth
#' generated by run_run_beverton_holt_model(), run_ricker_model(),
#' run_discrete_logistic_model(), run_discrete_exponential_model()
#' @examples
#' sim_df <- run_beverton_holt_model(N0 = 1, K = 100, Rd = 1.01, time = 100)
#' plot_discrete_population_growth(sim_df)
#' @import ggplot2
#' @export
plot_discrete_population_growth <- function(sim_df) {
  ggplot(sim_df) +
    geom_line(aes(x = time, y = Nt), size = .2) +
    geom_point(aes(y = Nt, x = time), size = 5, shape = 21,
               fill = "white", stroke = 0) +
    geom_point(aes(x = time, y = Nt), size = 2, shape = 21,
               fill = "white", stroke = .8 ) +
    theme_apps()
}
