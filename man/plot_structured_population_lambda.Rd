% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/structured_population_growth.R
\name{plot_structured_population_lambda}
\alias{plot_structured_population_lambda}
\title{Generate a trajectory of the total population size over time}
\usage{
plot_structured_population_lambda(pop_growth_matrix, leslie_mat = NULL)
}
\arguments{
\item{pop_growth_matrix}{population growth matrix, as generated by
\code{run_structured_population_simulation} (each row is an age class and each column
is a time step)}

\item{leslie_mat}{Leslie matrix used to generate the population trajectory (optional).
If the Leslie matrix is provided, the trajectory also includes lines indicating
the stable age distribution}
}
\description{
Generate a trajectory of the total population size over time
}
\examples{
leslie_matrix <- matrix(c(0, 8,1, 1, 0.4,0,0,0,0,0.8,0,0,0,0,0.1,0),
ncol = 4, byrow = TRUE)
structured_pop_init <- c(10,10,10,10)
structured_pop_time <- 50
structured_pop_out <- run_structured_population_simulation(leslie_mat = leslie_matrix, init =
structured_pop_init, time = structured_pop_time)
plot_structured_population_lambda(structured_pop_out, leslie_matrix)
}
\seealso{
\code{\link[=run_structured_population_simulation]{run_structured_population_simulation()}} to simulate the growth of a
structured population given a Leslie matrix,
\code{\link[=plot_structured_population_size]{plot_structured_population_size()}} and
\code{\link[=plot_structured_population_agedist]{plot_structured_population_agedist()}} for plotting different aspects of the
population trajectory, and \code{\link[=plot_leslie_diagram]{plot_leslie_diagram()}} for plotting an transition
}
