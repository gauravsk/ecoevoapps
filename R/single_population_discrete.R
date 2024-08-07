#' Discrete exponential growth model
#' @param N0 initial population size of population
#' @param lambda discrete population growth rate
#' @param time Number of time steps over which to project the model
#' @seealso [run_exponential_model()] for simulation exponential growth
#'   in continuous time, and see [run_discrete_logistic_model()],
#'   [run_beverton_holt_model()], and [run_ricker_model()] for discrete time
#'   models with population regulation
#' @examples
#' run_discrete_exponential_model(N0 = 1, lambda = 1.1, time = 10)
#' @return A data frame with columns time and Nt
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
#' @noRd
ricker_eqn <- function(Nt, r, K) Nt*exp(r*(1-Nt/K))

#' Ricker model of discrete population growth with a carrying capacity
#' @param N0 initial population size of population
#' @param params a vector of carrying capacity (K) and discrete growth rate (rd)
#' @param time Number of time steps over which to project the model
#' @seealso [run_discrete_logistic_model()] and [run_beverton_holt_model()] for
#'   other discrete-time models of population growth with population regulation,
#'   and [run_logistic_model()] for a continuous time model of logistic growth.
#'   Also see [plot_discrete_population_growth()] and
#'   [plot_discrete_population_cobweb()] for plotting trajectory over time and
#'   cobweb plots.
#' @examples
#' run_ricker_model(N0 = 1, params = c(K = 100, rd = 1.1), time = 10)
#' @return A data frame with columns time and Nt
#' @export
run_ricker_model <- function(N0 = 1, params = c(rd = 1.1, K = 100), time = 100) {

  to_return <- numeric(time)
  to_return[1] <- N0
  for(current_time in 2:time) {
    Nt <- to_return[current_time-1]
    to_return[current_time] <- ricker_eqn(Nt, params["rd"], params["K"])
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
#' @noRd
discretelogistic_eqn <- function(Nt, rd, K) (Nt + (rd*Nt*(1-Nt/K)))

#' Discrete logistic model
#' @param N0 initial population size of population
#' @param params a vector of carrying capacity (K) and discrete growth rate (rd)
#' @param rd Discrete growth factor
#' @param time Number of time steps over which to project the model
#' @seealso [run_ricker_model()] and [run_beverton_holt_model()] for other
#'   discrete-time models of population growth with population regulation, and
#'   [run_logistic_model()] for a continuous time model of logistic growth. Also
#'   see [plot_discrete_population_growth()] and
#'   [plot_discrete_population_cobweb()] for plotting trajectory over time and
#'   cobweb plots.
#' @examples
#' run_discrete_logistic_model(N0 = 1, params = c(rd = 1.1, K = 100), time = 10)
#' @return A data frame with columns time and Nt
#' @export
run_discrete_logistic_model <- function(N0 = 1, params = c(rd = 1.1, K = 100), time = 100) {

  to_return <- numeric(time)
  to_return[1] <- N0
  for(current_time in 2:time) {
    Nt <- to_return[current_time-1]
    to_return[current_time] <- discretelogistic_eqn(Nt, params["rd"], params["K"])
  }
  to_return_df <- data.frame(time = 1:time,
                             Nt = to_return)
  return(to_return_df)

  return(to_return)

}

#' Equation that returns Nt+1 from Nt for the Beverton-Holt model
#' @param x Nt
#' @param rd Population growth rate
#' @param K Carrying capacity
#' @noRd
bevertonholt_eqn <- function(Nt, rd, K) (rd*Nt)/(1+((rd-1)/K)*Nt)

#' Beverton Holt model
#' @param N0 initial population size of population
#' @param params a vector of carrying capacity (K) and discrete growth rate (rd)
#' @param time Number of time steps over which to project the model
#' @seealso [run_discrete_logistic_model()] and [run_ricker_model()] for other
#'   discrete-time models of population growth with population regulation, and
#'   [run_logistic_model()] for a continuous time model of logistic growth.
#'   Also see [plot_discrete_population_growth()] and
#'   [plot_discrete_population_cobweb()] for plotting trajectory over time and
#'   cobweb plots.
#' @examples
#' run_beverton_holt_model(N0 = 1, params = c(rd = 1.1, K = 100), time = 10)
#' @return A data frame with columns time and Nt
#' @export
run_beverton_holt_model <- function(N0 = 1, params = c(rd = 1.1, K = 100), time = 100) {

  to_return <- numeric(time)
  to_return[1] <- N0
  for(current_time in 2:time) {
    Nt <- to_return[current_time-1]
    to_return[current_time] <- bevertonholt_eqn(Nt, params["rd"], params["K"])
  }
  to_return_df <- data.frame(time = 1:time,
                             Nt = to_return)
  return(to_return_df)

}

#' Plot the dynamics of a discrete population growth model
#' @param sim_df data frame of discrete population growth
#' @seealso [run_discrete_exponential_model()], [run_discrete_logistic_model()],
#'   [run_beverton_holt_model()], [run_ricker_model()] for functions that
#'   generate the \code{sim_df} objects plotted by this function
#' @examples
#' dl_params <- c(rd = 1.1, K = 1000)
#' sim_df_dl <- run_discrete_logistic_model(N0 = 1, params = dl_params, time = 100)
#' plot_discrete_population_growth(sim_df_dl)
#' bh_params <- c(rd = 1.1, K = 1000)
#' sim_df_bh <- run_beverton_holt_model(N0 = 1, params = bh_params, time = 100)
#' plot_discrete_population_growth(sim_df_bh)
#' ri_params <- c(rd = 1.1, K = 1000)
#' sim_df_ri <- run_ricker_model(N0 = 1, params = ri_params, time = 100)
#' plot_discrete_population_growth(sim_df_ri)
#' @import ggplot2
#' @export
plot_discrete_population_growth <- function(sim_df) {
  # To suppress R CMD check
  time <- Nt <- NULL

  ggplot(sim_df) +
    geom_line(aes(x = time, y = Nt), linewidth = .2) +
    geom_point(aes(y = Nt, x = time), size = 5, shape = 21,
               fill = "white", stroke = 0) +
    geom_point(aes(x = time, y = Nt), size = 2, shape = 21,
               fill = "white", stroke = .8 ) +
    theme_apps()
}


#' Plot a cobweb digram of a discrete population growth model
#' @param sim_df data frame of discrete population growth
#' @param params_vec a vector with entries c(rd = XX and K = YY) describing the
#'   parameters used to generate sim_df
#' @param model_type a string: either "discrete_logistic", "beverton_holt", or
#'   "ricker"
#' @seealso  [run_discrete_logistic_model()], [run_beverton_holt_model()],
#'   [run_ricker_model()] for functions that generate the \code{sim_df} objects
#'   plotted by this function
#' @examples
#' dl_params <- c(rd = 1.1, K = 1000)
#' sim_df_dl <- run_discrete_logistic_model(N0 = 1, params = dl_params, time = 100)
#' plot_discrete_population_cobweb(sim_df_dl, params_vec = dl_params, model_type = "discrete_logistic")
#' bh_params <- c(rd = 1.1, K = 1000)
#' sim_df_bh <- run_beverton_holt_model(N0 = 1, params = bh_params, time = 100)
#' plot_discrete_population_cobweb(sim_df_bh, params_vec = bh_params, model_type = "beverton_holt")
#' ri_params <- c(rd = 1.1, K = 1000)
#' sim_df_ri <- run_ricker_model(N0 = 1, params = ri_params, time = 100)
#' plot_discrete_population_cobweb(sim_df_ri, params_vec = ri_params, model_type = "ricker")
#' @import ggplot2
#' @export
plot_discrete_population_cobweb <- function(sim_df, params_vec, model_type) {
  # To suppress CMD Check
  Ntm1 <- Nt <- NULL

  if(!(model_type %in% c("discrete_logistic", "beverton_holt", "ricker"))) {
    stop("model_type should be one of 'discrete_logstic', 'beverton_holt' or 'ricker'")
  }

  sim_df$Ntm1 <- dplyr::lag(sim_df$Nt)

  base_plot <-
    ggplot(sim_df)  +
    geom_segment(aes(x = Ntm1, xend = Ntm1, y = Ntm1, yend = Nt), linewidth = 0.5, color = "red") +
    geom_segment(aes(x = Ntm1, xend = Nt, y = Nt, yend = Nt), linewidth = 0.5, color = "red") +
    geom_abline(linewidth = 0.2, linetype = 2) +
    ylab(expression('N'["t+1"])) +
    xlab(expression('N'["t"])) +
    labs(title = "Cobweb plot") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, max(sim_df$Nt)*1.25)) +
    scale_x_continuous(expand = c(0,0)) +
    theme_apps()

  if(model_type == "discrete_logistic") {
    to_return <- base_plot +
      stat_function(fun = function(x)
        discretelogistic_eqn(x, rd = params_vec["rd"], K = params_vec["K"]),
        aes(x = seq(1, max(Nt)*1.25, length.out = nrow(sim_df)))) +
      annotate("text", x = Inf, y = -Inf, hjust = 1.2, vjust = -.8,
               label = "Solid black line is the Discrete Logistic function,
               the red line is the population trajectory,
               and the dashed line is the 1:1 line ")
  } else if(model_type == "beverton_holt") {
    to_return <- base_plot +
      stat_function(fun = function(x)
        bevertonholt_eqn(x, rd = params_vec["rd"], K = params_vec["K"]),
        aes(x = seq(1, max(Nt)*1.25, length.out = nrow(sim_df)))) +
      annotate("text", x = Inf, y = -Inf, hjust = 1.2, vjust = -.8,
               label = "Solid black line is the Beverton Holt function,
               the red line is the population trajectory,
               and the dashed line is the 1:1 line")

  } else {
    to_return <- base_plot +
      stat_function(fun = function(x)
        ricker_eqn(x, r = params_vec["rd"], K = params_vec["K"]),
        aes(x = seq(1, max(Nt)*1.25, length.out = nrow(sim_df)))) +
      annotate("text", x = Inf, y = -Inf, hjust = 1.2, vjust = -.8,
               label = "The solid black line is the Ricker function,
               the red line is the population trajectory
               and the dashed line is the 1:1 line")

  }
  return(to_return)

}

