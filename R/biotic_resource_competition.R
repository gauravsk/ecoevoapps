biotic_competition <- function(time, init, params) {
  with (as.list(c(time, init, params)), {
    # description of parameters:
    # r = per capita growth rate (prey)
    # q = 1/prey carrying capacity
    # a = attack rate
    # T_h = handling time
    # e = conversion efficiency
    # d = predator death rate

    dH_dt = r*H*(1 - q*H) - (a1*H*P1)/(1 + a1*T_h1*H) - (a2*H*P2)/(1 + a2*T_h2*H)
    dP1_dt = e1*(a1*H*P1)/(1 + a1*T_h1*H) - d1*P1
    dP2_dt = e2*(a2*H*P2)/(1 + a2*T_h2*H) - d2*P2
    return(list(c(dH = dH_dt, dP1 = dP1_dt, dP2 = dP2_dt)))

  })
}

run_biotic_competition_model <- function(time, init, params) {
  deSolve::ode(func = biotic_competition,
                y = init, times = time, parms = params)
}


