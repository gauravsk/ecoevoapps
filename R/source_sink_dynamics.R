#' Simulate dynamics of the source and sink populations under Pulliam (1988)'s model
#' @param endtime a number to indicate at what timesetp to end the simulation
#' @param init a vector of initial population sizes of the source and sink population
#' @param params a vector of model parameters
#' @examples
#' # in this example, source and sink population start at the same size,
#' # the sink population dies out, but is later replenished by immigration
#' # from the source patch once all source breeding sites are occupied
#' run_source_sink(endtime = 50, init = c(100, 100),
#' params = c(pa = 0.6, pj = 0.15, beta1 = 3, beta2 = 1, N1 = 300))
#' # the following is an INVALID example, where the model assumption is violated
#' # the model requires a sink site with negative population growth (lambda < 1),
#' # but here lambdas for both sites exceeds 1.
#' # Note that the violation does not stop the simulation.
#' # But it no longer satisfies the definition of a Pulliam sink population
#' run_source_sink(endtime = 50, init = c(100, 100),
#' params = c(pa = 0.6, pj = 0.15, beta1 = 4, beta2 = 3, N1 = 300))
#' @export
run_source_sink <- function(endtime,init,params) {
  with (as.list(c(endtime,init,params)), {
    # description of parameters/state variables:
    # pa = probability of adults surviving winter
    # pj = probability of juvenile surviving winter
    # beta1 = fecundity of population 1
    # beta2 = fecundity of population 2
    # n1 <- population of n1 (source by default)
    # N1 <- limiting breeding site for source population (equilibrium source population)
    # n2 <- population of n2 (sink by default)
    # assume sink population has unlimited breeding sites, equilibrium N2 will be calculated
    # when all breading sites are occupied, emigration happens

    # initialize empty vectors to store population size at each time step
    n1 <- numeric(endtime)
    n2 <- numeric(endtime)
    e <- 0 # before breeding sites are all occupied at the source, emigration is 0

    # initial population sizes:
    n1[1] <- n10
    n2[1] <- n20

    # annual cycles:
    for (t in 2:endtime) {
      # at the start of the year, population = end of last year
      n10 <- n1[t-1]
      n20 <- n2[t-1]

      # growth & survival cycle  of the year
      n1[t] <- n10*(pa + pj*beta1)
      n2[t] <- n20*(pa + pj*beta2)
      # optional: rounding, keep populations integers
      # n1[t] <- round( n1[t])
      # n2[t] <- round( n2[t])

      # migration cycle of the year
      if (n1[t] >= N1){
        #if source population reaches carrying capacity, emigration away from source, to sink
        e <- n1[t] - N1
      }
      n1[t] <- n1[t] - e
      n2[t] <- n2[t] + e
    }
    # return both n1 and n2
    return(list(n1, n2))
  })
}

