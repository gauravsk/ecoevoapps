#' Exponential growth model
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of parameters: intrinsic growth rate r for the resource, d1 and d2 for intrinsic mortality rate of sp1 and sp2; a1 and a2 for resource uptake rate of sp1 and sp2
#' @export
tilman_comp <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters/state variables:
    # d1 = intrinsic mortality rate of sp1
    # a1 = uptake efficiency of sp1
    # d2 = intrinsic mortality rate of sp2
    # a2 = uptake efficiency of sp2
    # r = intrinsic growth rate of resource

    # Differential equations
    dN1 <- N1*(a1*R-d1)
    dN2 <- N2*(a2*R-d2)
    dR <- r-R*(a1*N1 + a2*N2)

    # return both dN1 and dN2
    return(list(c(dN1, dN2, dR)))

  })
}
