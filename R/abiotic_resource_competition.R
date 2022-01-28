#' Internal function for running the abiotic resource
#' competition between two species and two essential resources
#' @param time vector of time units over which to run model
#' @param init initial population size of both species and both resources
#' @param params vector of parameters:
#' intrinsic growth rate r for the resource,
#' d1 and d2 for intrinsic mortality rate of sp1 and sp2;
#' a1 and a2 for resource uptake rate of sp1 and sp2
#' @keywords internal
tilman_comp_essential <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters/state variables:
    # d1 = intrinsic mortality rate of sp1
    # a1 = uptake efficiency of sp1
    # d2 = intrinsic mortality rate of sp2
    # a2 = uptake efficiency of sp2
    # r = intrinsic growth rate of resource

    # Differential equations
    dN1 <- N1*(min((r1*R1)/(R1+k11)-m1, (r1*R2)/(R2+k12)-m1))
    dN2 <- N2*(min((r2*R1)/(R1+k21)-m2, (r2*R2)/(R2+k22)-m2))
    dR1 <- a1*(S1-R1) - N1*c11*(dN1/N1 + m1) - N2*c21*(dN2/N2 + m2)
    dR2 <- a2*(S2-R2) - N1*c12*(dN1/N1 + m1) - N2*c22*(dN2/N2 + m2)
    # return both dN1 and dN2
    return(list(c(dN1, dN2, dR1, dR2)))
  })
}

#' Calculate RStar for the 2-consumer, 2-essential resource competition
#' @param params vector of model parameters (`S1`, `S2`, `r1`, `r2`, `k11`, `k12`,
#' `k21`, `k22`, `m1`, `m2`, `c11`, `c12`,
#' `c21`, `c22`, `a1`, `a1`)
#' @examples
#' params_vec = c(r1 = 1.6, r2 = 1,
#' k11 = 18, k12 = 4, k21 = 2, k22 = 14,
#' m1 = .2, m2 = .2,c11 = .25, c12 = .08,
#' c21 = .1, c22 = .2, a1 = .5, a2 = .5)
#' run_abiotic_comp_rstar(params = params_vec)
#' @export
run_abiotic_comp_rstar <- function(params) {

  if(!(is.numeric(params))) stop("params should be a numeric vector")
  if(!(all(c("r1", "r2", "k11", "k12",
             "k21", "k22", "m1", "m2", "c11", "c12",
             "c21", "c22", "a1", "a1") %in%
           names(params)))) {
    stop("Please provide a complete parameter vector (see ?run_abiotic_comp_model for details)")
  }

  R11 = unname(params["k11"] * params["m1"])/(params["r1"] - params["m1"])
  R12 = unlist(params["k12"] * params["m1"])/(params["r1"] - params["m1"])
  R21 = unlist(params["k21"] * params["m2"])/(params["r2"] - params["m2"])
  R22 = unlist(params["k22"] * params["m2"])/(params["r2"] - params["m2"])
  Rstar_vec <- c(R11 = R11, R12 = R12, R21 = R21, R22 = R22)
  names(Rstar_vec) <- c("R11", "R12", "R21", "R22")
  return(Rstar_vec)
}


#' Run the Tilman abiotic resource competition model
#'
#' This function runs the abiotic resource model for two species
#' competing for two essential resources.
#' See page 42 onwards of Don Alstad's Populus guide for a thorough
#' overview of the model:
#' https://cbs.umn.edu/sites/cbs.umn.edu/files/public/downloads/PopulusHelp_e.pdf
#' Note that the output of the model can be plotted with
#' @param time vector of time units over which to run model, starting from 0.
#' `time` can also be supplied as just the total length of the simulation (i.e. tmax)
#' @param init vector of initial population sizes for both species,
#' with names N1 and N2, AND initial resource pool sizes for both resources,
#' with names R1 and R2
#' @param params vector of model parameters (`S1`, `S2`, `r1`, `r2`, `k11`, `k12`,
#' `k21`, `k22`, `m1`, `m2`, `c11`, `c12`,
#' `c21`, `c22`, `a1`, `a1`)
#' @examples
#' # Define full time series, and run model in terms of carrying capacities
#' # and relative competitive effects
#' run_abiotic_comp_model(time = seq(0,10),
#'  init = c(N1 = 10, N2 = 10, R1 = 20, R2 = 20),
#'  params = c(S1 = 12, S2 = 12, r1 = 1.6, r2 = 1,
#' k11 = 18, k12 = 4, k21 = 2, k22 = 14,
#' m1 = .2, m2 = .2,c11 = .25, c12 = .08,
#' c21 = .1, c22 = .2, a1 = .5, a2 = .5))
#' @import deSolve
#' @export
run_abiotic_comp_model <- function(time = seq(0,100,0.1),
                                   init = c(N1 = 10, N2 = 10,
                                            R1 = 20, R2 = 20),
                                   params = c(S1 = 12, S2 = 12,
                                              r1 = 1.6, r2 = 1,
                                              k11 = 18, k12 = 4,
                                              k21 = 2, k22 = 14,
                                              m1 = .2, m2 = .2,
                                              c11 = .25, c12 = .08,
                                              c21 = .1, c22 = .2,
                                              a1 = .5, a2 = .5)) {
  # Check how time has been defined (if just Tmax, then make vector)
  # and if vector was supplied, check that it starts at t = 0
  if(length(time) == 1) {
    tmax <- time
    time <- seq(0, tmax, 0.1)
  } else if(time[1] != 0) {
    stop("The time vector should start at 0.")
  }

  # Check that init has been defined properly
  if(!(is.numeric(init))) stop("init should be a numeric vector of length 4, e.g. c(N1 = 10, N2 = 20, R1 = 20, R2 = 20)")
  if(length(init) != 4) stop("init should be a numeric vector of length 4, e.g. c(N1 = 10, N2 = 20, R1 = 20, R2 = 20)")
  if(!(all(names(init) %in% c("N1","N2", "R1","R2")))) stop("init should be a numeric vector of length 4, e.g. c(N1 = 10, N2 = 20, R1 = 20, R2 = 20)")

  # Check that params is correctly defined (just r)
  if(!(is.numeric(params))) stop("params should be a numeric vector")
  if(!(all(c("S1", "S2", "r1", "r2", "k11", "k12",
             "k21", "k22", "m1", "m2", "c11", "c12",
             "c21", "c22", "a1", "a1") %in%
           names(params)))) {
    stop("Please provide a complete parameter vector (see ?run_abiotic_comp_model for details)")
  }

  ode(func = tilman_comp_essential,
               y = init, parms = params, times = time)
}

#' Plot population size over time for the aboitic resource competition model
plot_abiotic_comp_time <- function(sim_df) {
  pivot_longer(sim_df, c(R1,R2,N1,N2), "species") %>%
    dplyr::filter(species %in% c("N1", "N2"))
  ggplot(tilman_out_long) +
    geom_line(aes(x = time, y = value, color = species), size = 2) +
    scale_color_brewer(palette = "Set1") +
    ylab("Population size") +
    theme_apps()

}
