#' functions for Pullian's source sink dynamics model
#' @param endtime how long to simulate the dynamics
#' @param init initial population size of population
#' @param params model parameters
#' @export
# Write a function to calculate Nt+1 for both populations, using DISCRETE time
source_sink <- function(endtime,init,params) {
  with (as.list(c(endtime,init,params)), {
    # description of parameters/state variables:
    # pa = probability of adults surviving winter
    # pj = probability of juvenile surviving winter
    # beta1 = fecundity of population 1
    # beta2 = fecundity of population 2
    # n1 <- population of n1 (source by default)
    # N1 <- limiting breeding site for source population (equilibrium source population)
    # n2 <- population of n2 (sink by default)
    # assume sink population has unlimited breeding sites, equilibirum N2 will be calculated
    # when all breading sites are occupied, emigration happens
    
    n1 <- numeric(endtime)
    n2 <- numeric(endtime)
    e <- 0 # before breeding sites are all occupied at the source, emmigration is 0
    
    n1[1] <- n10
    n2[1] <- n20
    
    for (t in 2:endtime) {
      # at the start of the year, population = end of last year
      n10 <- n1[t-1]
      n20 <- n2[t-1]
      
      # growth cycle  of the year
      n1[t] <- n10*(pa + pj*beta1) 
      n2[t] <- n20*(pa + pj*beta2)
      # optional: rounding, keep populations integers 
      # n1[t] <- round( n1[t])
      # n2[t] <- round( n2[t])
      
      # migration cycle of the year
      if (n1[t] >= N1){
        
        e <- n1[t] - N1 #emmigration away from source, to sink
      }
      n1[t] <- n1[t] - e 
      n2[t] <- n2[t] + e
    }
    # return both n1 and n2
    return(list(n1, n2))
  })
}