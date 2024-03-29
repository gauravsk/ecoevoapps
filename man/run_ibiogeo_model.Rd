% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ibiogeo_functions.R
\name{run_ibiogeo_model}
\alias{run_ibiogeo_model}
\title{Runs the island biogeography model.}
\usage{
run_ibiogeo_model(D = c(1, 4), A = c(1, 0.5), k = 0.015, M = 100)
}
\arguments{
\item{D}{distance from mainland (km)}

\item{A}{size of island (km^2)}

\item{k}{scaling parameter (defaults to 0.015)}

\item{M}{species richness of mainland}
}
\value{
Two ggplot objects: first, plot showing the equilibrium diversity of each island;
second, a "map" illustrating island size and distance from mainland.
}
\description{
The function takes as inputs the size of two islands as well as their
distance to the mainland, and uses these paramters to illustrate the equilibrium
diversity of two islands.
}
\examples{
run_ibiogeo_model(D = c(1,4), A = c(1,0.5))
}
