#' SIR model with vital rates
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate), m(natural birth/death rate), and v(vaccination rate)
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
    dR_dt = gamma*I - m*R + m*(S + E + I + R)*v
    return(list(c(dS = dS_dt, dE = dE_dt, dI = dI_dt, dR = dR_dt)))
  })
}

#' SIRD model with vital rates
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate), m (natural birth/death rate), v (vaccination rate), a (inverse of incubation period), and mu (death to infections)
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

#' SIR model with vital rates and frequency-dependent transmission
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate), m(natural birth/death rate), and v(vaccination rate)
SIR_ft <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # v is the newborn vaccination rate
    # gamma is the recovery rate
    dS_dt = m*(S + I + R)*(1 - v) - m*S - beta*S*I/(S+I+R)
    dI_dt = beta*S*I/(S+I+R) - m*I - gamma*I
    dR_dt = gamma*I - m*R + m*(S + I + R)*v
    return(list(c(dS = dS_dt, dI = dI_dt, dR = dR_dt)))
  })
}


#' SEIR model with vital rates and frequency-dependent transmission
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate), m(natural birth/death rate), v (vaccination rate), and a (inverse of incubation period)
SEIR_ft <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # v is the newborn vaccination rate
    # a is the inverse of the incubation period
    # gamma is the recovery rate
    dS_dt = m*(S + E + I + R)*(1 - v) - m*S - beta*S*I/(S+E+I+R)
    dE_dt = beta*S*I/(S+E+I+R) - a*E - m*E
    dI_dt = a*E - m*I - gamma*I
    dR_dt = gamma*I - m*R + m*(S + E + I + R)*v
    return(list(c(dS = dS_dt, dE = dE_dt, dI = dI_dt, dR = dR_dt)))
  })
}

#' SIRD model with vital rates and frequency-dependent transmission
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), gamma (recovery rate), m (natural birth/death rate), v (vaccination rate), a (inverse of incubation period), and mu (death to infections)
SIRD_ft <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # v is the newborn vaccination rate
    # mu is the death rate due to infection
    # gamma is the recovery rate
    dS_dt = m*(S + I + R)*(1 - v) - m*S - beta*S*I/(S+I+R)
    dI_dt = beta*S*I/(S+I+R) - m*I - gamma*I - mu*I
    dR_dt = gamma*I - m*R + m*(S + I + R)*v
    dD_dt = mu*I
    return(list(c(dS = dS_dt, dI = dI_dt, dR = dR_dt, dD = dD_dt)))
  })
}

#' SIS model with vital rates and frequency-dependent transmission
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), m(natural birth/death rate), and gamma (recovery rate)
SIS_ft <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # beta is the infection rate
    # m is the natural mortality/birth rate
    # gamma is the recovery rate
    dS_dt = m*(S + I) - m*S - beta*S*I/(S+I) + gamma*I
    dI_dt = beta*S*I/(S+I) - m*I - gamma*I
    return(list(c(dS = dS_dt, dI = dI_dt)))
  })
}


#' Run infectious disease models models
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of beta (infection rate), m(natural birth/death rate), and gamma (recovery rate)
#' @export
run_infectiousdisease_model <- function(time, init, params, model_type) {
  if(model_type == "SIR") {
    data.frame(deSolve::ode(func = SIR, y = init,
                            parms = params, times = time))
  } else if(model_type == "SIR_ft") {
    data.frame(deSolve::ode(func = SIR_ft, y = init,
                            parms = params, times = time))
  } else if(model_type == "SEIR") {
    data.frame(deSolve::ode(func = SEIR, y = init,
                            parms = params, times = time))
  } else if(model_type == "SEIR_ft") {
    data.frame(deSolve::ode(func = SEIR_ft, y = init,
                            parms = params, times = time))
  } else if(model_type == "SIRD") {
    data.frame(deSolve::ode(func = SIRD, y = init,
                            parms = params, times = time))
  } else if(model_type == "SIRD_ft") {
    data.frame(deSolve::ode(func = SIRD_ft, y = init,
                            parms = params, times = time))
  } else if(model_type == "SIS") {
    data.frame(deSolve::ode(func = SIS, y = init,
                            parms = params, times = time))
  } else if(model_type == "SIS_ft") {
    data.frame(deSolve::ode(func = SIS_ft, y = init,
                            parms = params, times = time))
  }

}
