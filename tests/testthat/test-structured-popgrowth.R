library(ecoevoapps)
test_that("Structured population growth simulations work", {
  leslie_mat <- matrix(c(0, 8, 1, 1,
                         0.4, 0, 0, 0,
                         0, 0.8, 0, 0,
                         0, 0, 0.1, 0), ncol =
                         4, byrow = TRUE)
  pop_out <- run_structured_population_simulation(leslie_mat = leslie_mat,
                                       init = c(5,5,5,5), time = 5)
  expect_equal(ncol(pop_out), 6)

  # Give a broken leslie matrix and check for error
  leslie_mat_break <- matrix(c(0, 8, 1, 0,
                         0.4, 0, 0, 0,
                         0, 0.8, 0, 0), ncol =
                         4, byrow = TRUE)
  expect_error(run_structured_population_simulation(leslie_mat = leslie_mat_break,
                                                    init = c(5,5,5,5), time = 5))

})

test_that("Structured population growth plots are generated as expected", {
  pop_out <- run_structured_population_simulation()

  plot_pop_out <- plot_structured_population_size(pop_out)
  expect_equal(plot_pop_out$labels$x, "Time step")

  plot_pop_lam <- plot_structured_population_lambda(pop_out)
  expect_equal(plot_pop_lam$labels$x, "Time step")

  plot_pop_age <- plot_structured_population_agedist(pop_out)
  expect_equal(plot_pop_age$labels$x, "Time step")

  leslie_mat <- matrix(c(0, 8, 1, 1,
                         0.4, 0, 0, 0,
                         0, 0.8, 0, 0,
                         0, 0, 0.1, 0), ncol =
                         4, byrow = TRUE)
  plot_leslie <- plot_leslie_diagram(leslie_mat)

  expect_length(plot_leslie$comp[,1], nrow(leslie_mat))
})

