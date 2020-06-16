#' Competition between two species for two essential resources
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of parameters: intrinsic growth rate r for the resource, d1 and d2 for intrinsic mortality rate of sp1 and sp2; a1 and a2 for resource uptake rate of sp1 and sp2
#' @export
tilman_comp_essential <- function(time,init,params) {
with (as.list(c(time,init,params)), {
  # description of parameters/state variables:
  # d1 = intrinsic mortality rate of sp1
  # a1 = uptake efficiency of sp1
  # d2 = intrinsic mortality rate of sp2
  # a2 = uptake efficiency of sp2
  # r = intrinsic growth rate of resource

  # Differential equations
  dN1 <- min((r1*R1)/(R1+k11)-m1, (r1*R2)/(R2+k12)-m1)
  dN2 <- min((r2*R1)/(R1+k21)-m2, (r2*R2)/(R2+k22)-m2)
  dR1 <- a1*(S1-R1) - N1*c11*(dN1/N1 + m1) - N2*c21*(dN2/N2 + m1)
  dR2 <- a2*(S2-R2) - N1*c12*(dN1/N1 + m2) - N2*c22*(dN2/N2 + m2)
  # return both dN1 and dN2
  return(list(c(dN1, dN2, dR1, dR2)))

})
}

#' Calculate RStar for the 2-consumer, 2-essential resource competition
#' @param params vector of parameters: k11, k12, k21, k22, r1, r2, m1, m2
#' @export
Rstar_essential <- function(params) {
  R11 = unname(params["k11"] * params["m1"])/(params["r1"] - params["m1"])
  R12 = unlist(params["k12"] * params["m1"])/(params["r1"] - params["m1"])
  R21 = unlist(params["k21"] * params["m2"])/(params["r2"] - params["m2"])
  R22 = unlist(params["k22"] * params["m2"])/(params["r2"] - params["m2"])
  Rstar_vec <- c(R11 = R11, R12 = R12, R21 = R21, R22 = R22)
  names(Rstar_vec) <- c("R11", "R12", "R21", "R22")
  return(Rstar_vec)
}
