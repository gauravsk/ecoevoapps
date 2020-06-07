#' SIR model with vital rates
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate), m(natural birth/death rate), and v(vaccination rate)
#' @export
SIR <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # v is the newborn vaccination rate
    # gamma is the recovery rate
    dS_dt = m*(S + I + R)*(1 - v) - m*S - beta*S*I
    dI_dt = beta*S*I - m*I - gamma*I
    dR_dt = gamma*I - m*R + m*(S + I + R)*v
    return(list(c(dS = dS_dt, dI = dI_dt, dR = dR_dt)))
  })
}

#' SIR model without vital rates
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate), and v(vaccination rate)
#' @export
SIR_no_vitals <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # v is the vaccination rate
    # gamma is the recovery rate
    dS_dt = -1*beta*S*I - v*S
    dI_dt = beta*S*I - gamma*I
    dR_dt = gamma*I + v*S ### BUT REALLY delete this line and then R = N - S + I
    return(list(c(dS = dS_dt, dI = dI_dt, dR = dR_dt)))
  })
}

#' SEIR model with vital rates
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate), m(natural birth/death rate), v (vaccination rate), and a (inverse of incubation period)
#' @export
SEIR <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # v is the newborn vaccination rate
    # a is the inverse of the incubation period
    # gamma is the recovery rate
    dS_dt = m*(S + E + I + R)*(1 - v) - m*S - beta*S*I
    dE_dt = beta*S*I - a*E - m*E
    dI_dt = a*E - m*I - gamma*I
    dR_dt = gamma*I - m*R + m*(S + I + R)*v
    return(list(c(dS = dS_dt, dE = dE_dt, dI = dI_dt, dR = dR_dt)))
  })
}

#' SIRD model with vital rates
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate), m (natural birth/death rate), v (vaccination rate), a (inverse of incubation period), and mu (death to infections)
#' @export
SIRD <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # v is the newborn vaccination rate
    # mu is the death rate due to infection
    # gamma is the recovery rate
    dS_dt = m*(S + I + R)*(1 - v) - m*S - beta*S*I
    dI_dt = beta*S*I - m*I - gamma*I - mu*I
    dR_dt = gamma*I - m*R + m*(S + I + R)*v
    dD_dt = mu*I
    return(list(c(dS = dS_dt, dI = dI_dt, dR = dR_dt, dD = dD_dt)))
  })
}

#' SIS model with vital rates
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), m(natural birth/death rate), and gamma (recovery rate)
#' @export
SIS <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # gamma is the recovery rate
    dS_dt = m*(S + I) - m*S - beta*S*I + gamma*I
    dI_dt = beta*S*I - m*I - gamma*I
    return(list(c(dS = dS_dt, dI = dI_dt)))
  })
}
