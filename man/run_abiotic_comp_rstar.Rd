% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abiotic_resource_competition.R
\name{run_abiotic_comp_rstar}
\alias{run_abiotic_comp_rstar}
\title{Calculate RStar for the 2-consumer, 2-essential resource competition}
\usage{
run_abiotic_comp_rstar(params)
}
\arguments{
\item{params}{vector of model parameters (\code{S1}, \code{S2}, \code{r1}, \code{r2}, \code{k11}, \code{k12},
\code{k21}, \code{k22}, \code{m1}, \code{m2}, \code{c11}, \code{c12},
\code{c21}, \code{c22}, \code{a1}, \code{a1})}
}
\description{
Calculate RStar for the 2-consumer, 2-essential resource competition
}
\examples{
params_vec = c(r1 = 1.6, r2 = 1,
k11 = 18, k12 = 4, k21 = 2, k22 = 14,
m1 = .2, m2 = .2,c11 = .25, c12 = .08,
c21 = .1, c22 = .2, a1 = .5, a2 = .5)
run_abiotic_comp_rstar(params = params_vec)
}
