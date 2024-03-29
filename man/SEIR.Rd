% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infectious_disease_dynamics.R
\name{SEIR}
\alias{SEIR}
\title{SEIR model with vital rates}
\usage{
SEIR(time, init, params)
}
\arguments{
\item{time}{vector of time units over which to run model}

\item{init}{initial population size of population}

\item{params}{vector of beta (infection rate), gamma (recovery rate),
m (natural birth/death rate), v (vaccination rate),
and a (inverse of incubation period)}
}
\description{
SEIR model with vital rates
}
\keyword{internal}
