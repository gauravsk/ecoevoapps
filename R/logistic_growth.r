#' Exponential growth model
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of r (intrinsic growth rate) and K (carrying capacity)
#' @export
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
