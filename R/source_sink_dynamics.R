#' check if input parameters violates model(Pulliam 1988) assumptions
#' @param params a vector of model parameters
#' @examples
#' VALID parameter example 1: lambdaSource > 1, lambdaSink < 1
#' assumption_check(params = c(pa = 0.6, pj = 0.15, betaSource = 4, betaSink = 1))
#' INVALID parameter example 1: lambdaSource < 1, lambdaSInk < 1
#' assumption_check(params = c(pa = 0.6, pj = 0.15, betaSource = 2, betaSink = 2))
#' INVALID parameter example 2: lambdaSource < 1, lambdaSink > 1
#' assumption_check(params = c(pa = 0.6, pj = 0.15, betaSource = 2, betaSink = 3))
#' @keywords internal
assumption_check <- function(params){
  with(as.list(params), {
    if ((pa + pj * betaSource >1)&(pa + pj * betaSink <1)) {
      return(TRUE)} else return(FALSE)
  })
}


#' Simulate dynamics of the source and sink populations under Pulliam (1988)'s model
#' @param endtime a number to indicate at what timesetp to end the simulation
#' @param init a vector of initial population sizes of the source and sink population
#' @param params a vector of model parameters
#' @examples
#' # in this example, source and sink population start at the same size,
#' # the sink population dies out, but is later replenished by immigration
#' # from the source patch once all source breeding sites are occupied
#' run_source_sink(endtime = 50, init = c(n0Source = 100, n0Sink = 100),
#' params = c(pa = 0.6, pj = 0.15, betaSource = 3, betaSink = 1, nSource = 300))
#' @export
run_source_sink <- function(endtime,init,params) {
  with (as.list(c(endtime,init,params)), {
    # description of parameters/state variables:
    # pa = probability of adults surviving winter
    # pj = probability of juvenile surviving winter
    # betaSource = fecundity of source population
    # betaSink = fecundity of sink population
    # n0Source <- initual source population size
    # NSource <- limiting breeding site for source population (equilibrium source population)
    # n0Sink <- initial sink population size
    # assume sink population has unlimited breeding sites, equilibrium nSink will be calculated
    # when all breading sites are occupied, emigration happens

    # warnings for parameter inputs that violate Pulliam model's requirements
    if(assumption_check(params) == F) {
      warning("Your parameters violate Pulliam model's requirements; \n  lambda for source population must be greater than 1;\n  lambda for sink population must be smaller than 1")
    }

    # initialize empty vectors to store population size at each time step
    nSource <- numeric(endtime)
    nSink <- numeric(endtime)
    e <- 0 # before breeding sites are all occupied at the source, emigration is 0

    # initial population sizes:
    nSource[1] <- n0Source
    nSink[1] <- n0Sink

    # annual cycles:
    for (t in 2:endtime) {
      # at the start of the year, population = end of last year
      n0Source <- nSource[t-1]
      n0Sink <- nSink[t-1]

      # growth & survival cycle  of the year
      nSource[t] <- n0Source*(pa + pj*betaSource)
      nSink[t] <- n0Sink*(pa + pj*betaSink)
      # optional: rounding, keep populations integers
      # nSource[t] <- round( nSource[t])
      # nSink[t] <- round( nSink[t])

      # migration cycle of the year
      if (nSource[t] >= NSource){
        #if source population reaches carrying capacity, emigration away from source, to sink
        e <- nSource[t] - NSource
      }
      nSource[t] <- nSource[t] - e
      nSink[t] <- nSink[t] + e
    }
    # return both nSource and nSink
    return(list(1:endtime, nSource, nSink))
  })
}


#' plot population trajectories of Pulliams' source sink meta-population
#' @param sim_df a 3-column data frame, or a list of 3:
#'  at each time step, the source and sink population sizes;
#' can directly use the output from run_source_sink()
#' @param assumption_status a Boolean value, TRUE if assumptions are met, FALSE if violated
#' @import ggplot2
#' @import tidyr
#' @examples
#' a valid example
#' Params <- c(pa = 0.6, pj = 0.15, betaSource = 3, betaSink = 1, NSource = 300)
#' Sim_df <- run_source_sink(endtime = 50, init = c(n0Source = 100, n0Sink = 100),
#' params = Params)
#' Assumption_status <- assumption_check(Params)
#' plot_source_sink(sim_df = Sim_df, assumption_status = Assumption_status)
#' an invalid example with warning
#' Params <- c(pa = 0.6, pj = 0.15, betaSource = 3, betaSink = 3, NSource = 300)
#' Sim_df <- run_source_sink(endtime = 50, init = c(n0Source = 100, n0Sink = 100),
#' params = Params)
#' Assumption_status <- assumption_check(Params)
#' plot_source_sink(sim_df = Sim_df, assumption_status = Assumption_status)
#' @export
plot_source_sink <- function(sim_df, assumption_status){
  # if the input is a list, convert to data frame
  sim_df <- data.frame(sim_df)
  # reshape data & give column names
  colnames(sim_df) <- c("year", "source", "sink")
  sim_df <- pivot_longer(sim_df, c(source,sink), "population")

  plot <- ggplot(sim_df) +
    geom_line(aes(x = year, y = value, color = population), size = 2) +
    theme_apps() +
    scale_x_continuous(expand = c(0, 0, .1, 0)) +
    scale_y_continuous(expand = c(0, 0, .1, 0)) +
    scale_color_brewer(palette = "Set1") +
    ylab("Population size")

  # project warning if assumptions are violated
  if(assumption_status == FALSE){
    # show warning at the middle of the plot
    x_center = max(sim_df$year)/2
    y_center = max(sim_df$value)/2
    plot <- plot +
      annotate("text", x = x_center, y = y_center,
               label = "Your parameters violate \nPulliam model's requirements")+
      # show warning as the title
      labs(title = "Your parameters violate Pulliam model's requirements")+
      theme(plot.title = element_text(color = "red", face = "bold"))

  }
  return(plot)
}
