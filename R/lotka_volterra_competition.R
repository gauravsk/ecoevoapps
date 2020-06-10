#' Lotka-Volterra Competition with carrying capacities
#'
#' @param time vector of time units over which to run model
#' @param init vector of initial population sizes for both species
#' @param params vector of model parameters
#' @export
lotka_volterra_competition <- function(time, init, params) {
  with (as.list(c(time, init, params)), {
    # description of parameters
    # r1 = per-capita growth rate of species 1
    # N1 = population size of species 1
    # K1 = carying capacity of species 1
    # a = relative effect of species 2 on species 1
    # r2 = per-capita growth rate of species 2
    # N2 = population size of species 2
    # K2 = carrying capacity of species 2
    # b = relative effect of species 1 on species 2

    # Differential equations
    dN1 <- r1*N1*(1 - (N1 + a*N2)/K1)
    dN2 <- r2*N2*(1 - (N2 + b*N1)/K2)

    # Return dN1 and dN2
    return(list(c(dN1, dN2)))
  })
}
