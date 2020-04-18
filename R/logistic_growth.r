#' Logistic growth model
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


#' Lagged logistic growth model
#' @param time vector of time units over which to run model
#' @param init initial population size of population
#' @param params vector of r (intrinsic growth rate), K (carrying capacity), and tau (time lag)
#' @export
lagged_logistic_growth <- function(time, init, params) {
  with (as.list(c(time,init,params)), {
    tlag <- time - tau
    if (tlag < 0) {
      ylag <- N1
    } else {
      ylag <- lagvalue(tlag)
    }
    dN1 <- r * N1[1] * (1-(ylag[1]/K))
    return(list(c(dN1)))
  })
}

