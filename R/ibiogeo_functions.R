#' Immigration rate for Island Biogeography model
#' @param S number of species on island
#' @param D distance from mainland (km)
#' @param M number of species in the mainland
#' @param k scaling constant
#' @export
ibiogeo_I <- function(S,D,M,k){
  exp(-(k/D)*(S-M))-1
}

#' Extinction rate for Island Biogeography model
#' @param S number of species on island
#' @param A size of island (km^2)
#' @param k scaling constant
#' @export
ibiogeo_E <- function(S,A,k) {
  exp(k*S/A)-1
}

#' Equilibrium number of species on island for Island Biogeography model
#' @param D distance from mainland (km)
#' @param A size of island (km^2)
#' @param M number of species in the mainland
#' @param k scaling constant
#' @export
ibiogeo_Sx <- function(D,A,M,k){
  A*M/(D+A)
}

#' equilibrium immigration rate for Island Biogeography model
#' @param D distance from mainland (km)
#' @param A size of island (km^2)
#' @export
ibiogeo_Ix <- function(D,A,M,k){
  exp(-(k/D)*(ibiogeo_Sx(D,A,M,k)-M))-1
}

#' equilibrium extinction rate for Island Biogeography model
#' @param D distance from mainland (km)
#' @param A size of island (km^2)
#' @export
ibiogeo_Ex <- function(D,A,M,k){
  exp(k*A*ibiogeo_Sx(D,A,M,k))-1
}

