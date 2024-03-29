% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lotka_volterra_competition.R
\name{plot_lvcomp_time}
\alias{plot_lvcomp_time}
\title{Generate a trajectory of population size (N1 vs N2) over time for the
Lotka-Volterra model}
\usage{
plot_lvcomp_time(sim_df)
}
\arguments{
\item{sim_df}{data frame of lokta-volterra model simulation (created by
run_lvcomp_model())}
}
\description{
Generate a trajectory of population size (N1 vs N2) over time for the
Lotka-Volterra model
}
\examples{
sim_df <- run_lvcomp_model()
plot_lvcomp_time(sim_df)
}
\seealso{
\code{\link[=run_lvcomp_model]{run_lvcomp_model()}} for simulating Lotka-Volterra competition
dynamics between two species given a vector of parameters, and
\code{\link[=plot_lvcomp_portrait]{plot_lvcomp_portrait()}} for making a phase portrait of the both species
(including the ZNGIs) the ZNGIs)
}
